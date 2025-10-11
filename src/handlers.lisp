(in-package :lisp-blog)

;;; JSON生成ヘルパー関数

(defun format-timestamp (timestamp)
  "タイムスタンプを ISO 8601 形式にフォーマット"
  (cond
    ((null timestamp) "")
    ((numberp timestamp)
     ;; Unix タイムスタンプの場合、local-time:timestamp に変換
     (local-time:format-timestring
       nil
       (local-time:unix-to-timestamp timestamp)
       :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2))))
    ((typep timestamp 'local-time:timestamp)
     ;; 既に local-time:timestamp の場合
     (local-time:format-timestring nil timestamp :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2))))
    (t
     ;; その他の場合は文字列として返す
     (format nil "~A" timestamp))))

(defun json-response (status &optional data)
  "JSON形式のレスポンスを生成"
  (setf (content-type*) "application/json")
  (format nil "{\"status\":\"~A\"~@[,~A~]}" 
          status 
          (when data
            (format nil "\"message\":\"~A\"" data))))

(defun json-error (message)
  "JSONエラーレスポンスを生成"
  (setf (content-type*) "application/json")
  (format nil "{\"error\":\"~A\"}" message))

(defun json-object (pairs)
  "連想リストからJSONオブジェクトを生成"
  (setf (content-type*) "application/json")
  (format nil "{~{\"~A\":~A~^,~}}"
          (loop for (key . value) in pairs
                collect key
                collect (cond
                          ((stringp value) (format nil "\"~A\"" value))
                          ((numberp value) (format nil "~A" value))
                          ((null value) "null")
                          (t (format nil "\"~A\"" value))))))

;;; ページネーションヘルパー関数

(defun get-query-param (name &optional (default nil))
  "クエリパラメータを取得して整数に変換

  Parameters:
    name    - パラメータ名（文字列）
    default - デフォルト値（パラメータがない場合または変換失敗時）

  Returns:
    整数値またはデフォルト値"
  (let ((value (hunchentoot:get-parameter name)))
    (if value
        (handler-case
            (parse-integer value)
          (error () default))
        default)))

(defun calculate-pagination (page per-page total)
  "ページネーション情報を計算

  Parameters:
    page     - 現在のページ番号
    per-page - 1ページあたりの件数
    total    - 総件数

  Returns:
    plist形式のページネーション情報
    (:page :per_page :total :total_pages :has_next :has_prev)"
  (let ((total-pages (if (zerop total) 0 (ceiling total per-page))))
    (list :page page
          :per_page per-page
          :total total
          :total_pages total-pages
          :has_next (< page total-pages)
          :has_prev (> page 1))))

;;; 認証API

(define-easy-handler (api-signup :uri "/api/auth/signup")
                     (username email password display-name)
                     "ユーザー登録"
                     (with-validation-handler
                       ;; 入力をサニタイズ
                       (let ((clean-username (sanitize-input username))
                             (clean-email (sanitize-input email))
                             (clean-display-name (when display-name (sanitize-input display-name))))
                         ;; バリデーション実行
                         (validate-user-input clean-username clean-email password clean-display-name)
                         ;; ユーザー作成
                         (create-user clean-username clean-email password clean-display-name nil)
                         ;; 成功レスポンス
                         (respond-json (make-success-response nil (t! "api.auth.account-created"))))))

(define-easy-handler (api-login :uri "/api/auth/login")
                     (username password)
                     "ログイン"
                     (handler-case
                       (with-db
                         (format t "Login attempt: username=~A~%" username)
                         (let ((result (query "SELECT id, password_hash FROM users WHERE username = $1"
                                              username :row)))
                           (format t "User lookup result: ~A~%" result)
                           (if result
                             (let ((user-id (first result))
                                   (password-hash (second result)))
                               (format t "Verifying password for user-id=~A~%" user-id)
                               (if (verify-password password password-hash)
                                 (progn
                                   (format t "Password verified, creating session for user-id=~A~%" user-id)
                                   (let ((session-id (generate-session-id)))
                                     (execute "INSERT INTO sessions (id, user_id, expires_at)
                                              VALUES ($1, $2, NOW() + INTERVAL '7 days')"
                                              session-id user-id)
                                     (format t "Session created: session-id=~A~%" session-id)
                                     (set-cookie "session_id" :value session-id :path "/" :max-age (* 7 24 60 60))
                                     (json-response "success" "Login successful")))
                                 (progn
                                   (setf (return-code*) hunchentoot:+http-authorization-required+)
                                   (json-error "Invalid username or password"))))
                             (progn
                               (setf (return-code*) hunchentoot:+http-authorization-required+)
                               (json-error "Invalid username or password")))))
                       (error (e)
                              (format t "Login error: ~A~%" e)
                              (setf (return-code*) hunchentoot:+http-internal-server-error+)
                              (json-error (format nil "Internal server error: ~A" e)))))

(define-easy-handler (api-logout :uri "/api/auth/logout") ()
                     "ログアウト"
                     (let ((session-id (cookie-in "session_id")))
                       (delete-session session-id)
                       (set-cookie "session_id" :value "" :path "/" :max-age 0)
                       (json-response "success" (t! "api.auth.logout-success"))))

(define-easy-handler (api-current-user :uri "/api/auth/me") ()
                     "現在のユーザー情報を取得"
                     (let ((user (get-current-user)))
                       (if user
                         (format nil "{\"id\":~A,\"username\":\"~A\",\"email\":\"~A\",\"display_name\":\"~A\",\"bio\":\"~A\"}"
                                 (user-id user)
                                 (user-username user)
                                 (user-email user)
                                 (user-display-name user)
                                 (or (user-bio user) ""))
                         (progn
                           (setf (return-code*) hunchentoot:+http-authorization-required+)
                           (json-error (t! "api.auth.login-required"))))))

;;; 投稿API

(define-easy-handler (api-posts :uri "/api/posts") (status)
                     "投稿を取得（statusパラメータでフィルタリング可能、ページネーション対応）"
                     (setf (content-type*) "application/json")

                     ;; 下書き一覧の場合はページネーション無し（既存動作を維持）
                     (when (string= status "draft")
                       (let ((user (get-current-user)))
                         (if user
                             (let ((posts (get-user-drafts (user-id user))))
                               (return-from api-posts
                                 (format nil "[~{~A~^,~}]"
                                         (mapcar (lambda (post)
                                                   (format nil "{\"id\":~A,\"user_id\":~A,\"title\":\"~A\",\"content\":\"~A\",\"author\":\"~A\",\"created_at\":\"~A\",\"status\":\"~A\"}"
                                                           (post-id post)
                                                           (post-user-id post)
                                                           (post-title post)
                                                           (post-content post)
                                                           (post-author-name post)
                                                           (format-timestamp (post-created-at post))
                                                           (post-status post)))
                                                 posts))))
                             (return-from api-posts "[]"))))

                     ;; 公開済み投稿の場合はページネーション対応
                     (let* ((page (or (get-query-param "page") 1))
                            (per-page (or (get-query-param "per_page") 10)))

                       ;; バリデーション
                       (when (or (<= page 0) (> per-page 100) (<= per-page 0))
                         (setf (return-code*) hunchentoot:+http-bad-request+)
                         (return-from api-posts
                           (json-error "Invalid pagination parameters")))

                       ;; 総件数を取得
                       (let* ((total (count-published-posts))
                              (total-pages (if (zerop total) 0 (ceiling total per-page))))

                         ;; ページ番号が範囲外の場合
                         (when (and (> page total-pages) (> total 0))
                           (setf (return-code*) hunchentoot:+http-bad-request+)
                           (return-from api-posts
                             (json-error (format nil "Page ~A does not exist (max: ~A)" page total-pages))))

                         ;; 投稿を取得
                         (let* ((posts (get-published-posts-paginated page per-page))
                                (pagination (calculate-pagination page per-page total)))

                           ;; JSONレスポンス生成
                           (format nil "{\"posts\":[~{~A~^,~}],\"pagination\":{\"page\":~A,\"per_page\":~A,\"total\":~A,\"total_pages\":~A,\"has_next\":~A,\"has_prev\":~A}}"
                                   (mapcar (lambda (post)
                                             (format nil "{\"id\":~A,\"user_id\":~A,\"title\":\"~A\",\"content\":\"~A\",\"author\":\"~A\",\"created_at\":\"~A\",\"status\":\"~A\"}"
                                                     (post-id post)
                                                     (post-user-id post)
                                                     (post-title post)
                                                     (post-content post)
                                                     (post-author-name post)
                                                     (format-timestamp (post-created-at post))
                                                     (post-status post)))
                                           posts)
                                   (getf pagination :page)
                                   (getf pagination :per_page)
                                   (getf pagination :total)
                                   (getf pagination :total_pages)
                                   (if (getf pagination :has_next) "true" "false")
                                   (if (getf pagination :has_prev) "true" "false"))))))

(define-easy-handler (api-create-post :uri "/api/posts/create")
                     (title content status)
                     "新しい投稿を作成（要ログイン、status指定可能）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response (t! "api.auth.login-required")))
                           (return-from api-create-post))
                         ;; 入力をサニタイズ
                         (let ((clean-title (sanitize-input title))
                               (clean-content (sanitize-input content)))
                           ;; バリデーション実行
                           (validate-post-input clean-title clean-content)
                           ;; 投稿作成（statusパラメータを含む）
                           (let ((post-status (if (and status (member status '("draft" "published") :test #'string=))
                                                   status
                                                   "published"))) ; デフォルトは公開
                             (create-post (user-id user) clean-title clean-content post-status))
                           ;; 成功レスポンス
                           (respond-json (make-success-response nil (t! "api.post.created")))))))

(define-easy-handler (api-update-post :uri "/api/posts/update")
                     (id title content status)
                     "投稿を更新（作成者のみ、status変更可能）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response (t! "api.auth.login-required")))
                           (return-from api-update-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response (t! "api.post.missing-id")))
                           (return-from api-update-post))
                         (let ((post-id (handler-case (parse-integer id)
                                          (error ()
                                            (setf (return-code*) +http-bad-request+)
                                            (respond-json (make-error-response (t! "api.post.invalid-id")))
                                            (return-from api-update-post)))))
                           ;; 入力をサニタイズ
                           (let ((clean-title (sanitize-input title))
                                 (clean-content (sanitize-input content)))
                             ;; バリデーション実行
                             (validate-post-input clean-title clean-content)
                             ;; 投稿更新（statusパラメータを含む）
                             (let ((post-status (when (and status (member status '("draft" "published") :test #'string=))
                                                   status)))
                               (if (update-post post-id (user-id user) clean-title clean-content post-status)
                                 ;; 成功レスポンス
                                 (respond-json (make-success-response nil (t! "api.post.updated")))
                                 ;; 権限エラー
                                 (progn
                                   (setf (return-code*) +http-forbidden+)
                                   (respond-json (make-error-response (t! "api.post.permission-denied")))))))))))

(define-easy-handler (api-delete-post :uri "/api/posts/delete") (id)
                     "投稿を削除（作成者のみ）"
                     (let ((user (get-current-user)))
                       (if user
                         (if id
                           (if (delete-post (parse-integer id) (user-id user))
                             (json-response "success")
                             (progn
                               (setf (return-code*) hunchentoot:+http-forbidden+)
                               (json-error (t! "api.post.permission-denied"))))
                           (progn
                             (setf (return-code*) hunchentoot:+http-bad-request+)
                             (json-error (t! "api.post.missing-id"))))
                         (progn
                           (setf (return-code*) hunchentoot:+http-authorization-required+)
                           (json-error (t! "api.auth.login-required"))))))

;; 下書き公開用API
(define-easy-handler (api-publish-post :uri "/api/posts/publish") (id)
                     "下書きを公開状態に変更（作成者のみ）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response (t! "api.auth.login-required")))
                           (return-from api-publish-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response (t! "api.post.missing-id")))
                           (return-from api-publish-post))
                         (handler-case
                           (let ((post-id-int (parse-integer id)))
                             ;; 下書き公開
                             (if (publish-draft post-id-int (user-id user))
                               ;; 成功
                               (respond-json (make-success-response nil (t! "api.post.published")))
                               ;; エラー（下書きが存在しないまたは権限なし）
                               (progn
                                 (setf (return-code*) +http-forbidden+)
                                 (respond-json (make-error-response (t! "api.post.draft-not-found"))))))
                           (error ()
                             (setf (return-code*) +http-bad-request+)
                             (respond-json (make-error-response (t! "api.post.invalid-id"))))))))

;; 投稿非公開用API
(define-easy-handler (api-unpublish-post :uri "/api/posts/unpublish") (id)
                     "公開記事を下書きに戻す（作成者のみ）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response (t! "api.auth.login-required")))
                           (return-from api-unpublish-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response (t! "api.post.missing-id")))
                           (return-from api-unpublish-post))
                         (handler-case
                           (let ((post-id-int (parse-integer id)))
                             ;; 投稿非公開
                             (if (unpublish-post post-id-int (user-id user))
                               ;; 成功
                               (respond-json (make-success-response nil (t! "api.post.unpublished")))
                               ;; エラー（投稿が存在しないまたは権限なし）
                               (progn
                                 (setf (return-code*) +http-forbidden+)
                                 (respond-json (make-error-response (t! "api.post.not-found"))))))
                           (error ()
                             (setf (return-code*) +http-bad-request+)
                             (respond-json (make-error-response (t! "api.post.invalid-id"))))))))

;; ユーザーの投稿一覧API（下書き含む、本人のみ）
(define-easy-handler (api-user-posts :uri "/api/user/posts") ()
                     "ログインユーザーの全投稿を取得（下書き含む）"
                     (setf (content-type*) "application/json")
                     (let ((user (get-current-user)))
                       (if user
                           (let ((posts (get-posts-by-user (user-id user))))
                             (format nil "[~{~A~^,~}]"
                                     (mapcar (lambda (post)
                                               (format nil "{\"id\":~A,\"user_id\":~A,\"title\":\"~A\",\"content\":\"~A\",\"author\":\"~A\",\"created_at\":\"~A\",\"status\":\"~A\"}"
                                                       (post-id post)
                                                       (post-user-id post)
                                                       (post-title post)
                                                       (post-content post)
                                                       (post-author-name post)
                                                       (format-timestamp (post-created-at post))
                                                       (post-status post)))
                                             posts)))
                           (progn
                             (setf (return-code*) +http-authorization-required+)
                             (json-error (t! "api.auth.login-required"))))))

;;; メインページ

(define-easy-handler (index :uri "/") ()
                     "メインページ"
                     (set-current-locale-from-request)
                     (setf (content-type*) "text/html")
                     (with-html-string
                       (:doctype)
                       (:html :lang (string-downcase (symbol-name *current-locale*))
                         (:head
                           (:meta :charset "utf-8")
                           (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
                           (:title (t! "main.page-title"))
                           (:link :rel "stylesheet" :href "/static/css/admin-card-layout.css")
                           (:script :src "https://unpkg.com/vue@3/dist/vue.global.js"))
                         (:body :class "admin-layout"
                           (:div :id "app"
                                 ;; Top Bar
                                 (:div :class "top-bar"
                                       (:div :class "breadcrumb"
                                             (:raw "<template v-if=\"showCreateView\">")
                                             (:span :class "breadcrumb-item" "📝 記事")
                                             (:span "›")
                                             (:span :class "breadcrumb-item active" "新規作成")
                                             (:raw "</template>")
                                             (:raw "<template v-else>")
                                             (:span :class "breadcrumb-item active" "📚 記事一覧")
                                             (:raw "</template>"))
                                       (:div :class "top-actions"
                                             (:raw "<template v-if=\"currentUser\">")
                                             (:raw "<span style=\"color: var(--text-secondary); margin-right: 15px;\">{{ currentUser.display_name }}</span>")
                                             (:raw "<button v-if=\"!showCreateView\" class=\"btn btn-gradient\" @click=\"showCreateView = true\">✏️ 新規作成</button>")
                                             (:raw "<button v-if=\"showCreateView\" class=\"btn btn-outline\" @click=\"showCreateView = false\">← 一覧に戻る</button>")
                                             (:raw (format nil "<button class=\"btn btn-outline\" @click=\"logout\">~A</button>" (t! "main.logout")))
                                             (:raw "</template>")
                                             (:raw "<template v-else>")
                                             (:a :href "/login" :class "btn btn-outline" (t! "common.login"))
                                             (:a :href "/signup" :class "btn btn-gradient" (t! "common.sign-up"))
                                             (:raw "</template>")))

                                 ;; Main Container - Create Post View
                                 (:raw "<div v-if=\"showCreateView && currentUser\" class=\"admin-container\">")
                                 (:div :class "admin-grid"
                                       ;; Editor Section
                                       (:div :class "editor-section"
                                             ;; Title Card
                                             (:div :class "card title-card"
                                                   (:raw "<input type=\"text\" class=\"title-input\" v-model=\"newPost.title\" placeholder=\"魅力的なタイトルを入力...\">"))

                                             ;; Content Card
                                             (:div :class "card content-card"
                                                   (:div :class "card-header"
                                                         (:div :class "card-title"
                                                               (:div :class "card-icon" "✏️")
                                                               (:span "本文")))
                                                   (:raw "<textarea class=\"content-textarea\" v-model=\"newPost.content\" placeholder=\"ここから書き始めましょう...\n\nMarkdownで自由に執筆できます。\n\n# 見出し\n## サブ見出し\n\n**太字** や *斜体* も使えます。\"></textarea>")))

                                       ;; Sidebar
                                       (:aside :class "sidebar"
                                               ;; Status Card
                                               (:div :class "card"
                                                     (:div :class "card-header"
                                                           (:div :class "card-title"
                                                                 (:div :class "card-icon" "📊")
                                                                 (:span "公開設定")))
                                                     (:div :class "status-options"
                                                           (:raw "<div class=\"status-btn\" :class=\"{active: newPost.status === 'draft'}\" @click=\"newPost.status = 'draft'\">")
                                                           (:div :class "status-btn-header"
                                                                 (:span :class "status-emoji" "📝")
                                                                 (:span :class "status-name" "下書き"))
                                                           (:div :class "status-desc" "非公開で保存")
                                                           (:raw "</div>")
                                                           (:raw "<div class=\"status-btn\" :class=\"{active: newPost.status === 'published'}\" @click=\"newPost.status = 'published'\">")
                                                           (:div :class "status-btn-header"
                                                                 (:span :class "status-emoji" "🚀")
                                                                 (:span :class "status-name" "公開"))
                                                           (:div :class "status-desc" "すぐに公開する")
                                                           (:raw "</div>"))
                                                     (:raw "<button class=\"btn btn-gradient\" style=\"width: 100%; margin-top: 15px;\" @click=\"createPost\">{{ newPost.status === 'draft' ? '下書き保存' : '公開する' }}</button>"))

                                               ;; Stats Card
                                               (:div :class "card"
                                                     (:div :class "card-header"
                                                           (:div :class "card-title"
                                                                 (:div :class "card-icon" "📈")
                                                                 (:span "統計")))
                                                     (:div :class "stats-grid"
                                                           (:div :class "stat-item"
                                                                 (:raw "<div class=\"stat-value\">{{ getWordCount() }}</div>")
                                                                 (:div :class "stat-label" "文字数"))
                                                           (:div :class "stat-item"
                                                                 (:raw "<div class=\"stat-value\">{{ getReadTime() }}</div>")
                                                                 (:div :class "stat-label" "分で読める"))))))
                                 (:raw "</div>")

                                 ;; Main Container - Posts List View
                                 (:raw "<div v-if=\"!showCreateView\" class=\"admin-container\">")
                                 (:div :class "posts-grid"
                                       (:raw "<div v-for=\"post in posts\" :key=\"post.id\" class=\"post-card\">")
                                       (:div :class "post-card-header"
                                             (:raw "<h3 class=\"post-title-link\">{{ post.title }}</h3>")
                                             (:raw "<span class=\"post-status-badge\" :class=\"post.status\">{{ post.status === 'draft' ? '下書き' : '公開済み' }}</span>"))
                                       (:div :class "post-meta"
                                             (:raw "<span class=\"post-meta-item\">✍️ {{ post.author }}</span>")
                                             (:raw "<span class=\"post-meta-item\">📅 {{ formatDate(post.created_at) }}</span>"))
                                       (:raw "<p class=\"post-content-preview\">{{ post.content.substring(0, 150) }}...</p>")
                                       (:raw "<div v-if=\"currentUser && currentUser.id === post.user_id\" class=\"post-actions\">")
                                       (:raw "<button class=\"post-action-btn edit\" @click=\"editPost(post)\">✏️ 編集</button>")
                                       (:raw "<button v-if=\"post.status === 'draft'\" class=\"post-action-btn publish\" @click=\"publishDraft(post.id)\">🚀 公開</button>")
                                       (:raw "<button v-if=\"post.status === 'published'\" class=\"post-action-btn\" style=\"background: rgba(255, 230, 109, 0.2); color: var(--accent-tertiary);\" @click=\"unpublishPost(post.id)\">📝 下書きに戻す</button>")
                                       (:raw "<button class=\"post-action-btn delete\" @click=\"deletePost(post.id)\">🗑️ 削除</button>")
                                       (:raw "</div>")
                                       (:raw "</div>"))

                                 ;; Pagination
                                 (:raw "<div v-if=\"!currentUser && pagination.total_pages > 1\" class=\"pagination-container\">")
                                 (:div :class "pagination"
                                       (:raw "<button @click=\"prevPage\" :disabled=\"!pagination.has_prev\" class=\"pagination-btn pagination-arrow\">")
                                       (:span "« 前へ")
                                       (:raw "</button>")

                                       (:raw "<button v-for=\"page in pageNumbers\" :key=\"page\" @click=\"typeof page === 'number' ? goToPage(page) : null\" :class=\"['pagination-btn', page === pagination.page ? 'active' : '', typeof page !== 'number' ? 'ellipsis' : '']\" :disabled=\"typeof page !== 'number'\">")
                                       (:raw "{{ page }}")
                                       (:raw "</button>")

                                       (:raw "<button @click=\"nextPage\" :disabled=\"!pagination.has_next\" class=\"pagination-btn pagination-arrow\">")
                                       (:span "次へ »")
                                       (:raw "</button>"))

                                 (:raw "<div v-if=\"!currentUser && pagination.total > 0\" class=\"pagination-info\">")
                                 (:raw "表示中: {{ ((pagination.page - 1) * pagination.per_page) + 1 }}-{{ Math.min(pagination.page * pagination.per_page, pagination.total) }} / 全{{ pagination.total }}件")
                                 (:raw "</div>")
                                 (:raw "</div>")
                                 (:raw "</div>")

                           (:script :src "https://unpkg.com/vue@3/dist/vue.global.js")
                           (:script :src "/static/js/i18n.js")
                           (:script :src "/static/js/app.js"))))))

;;; ログイン・サインアップページ

(define-easy-handler (login-page :uri "/login") ()
                     "ログインページ"
                     (set-current-locale-from-request)
                     (setf (content-type*) "text/html")
                     (with-html-string
                       (:doctype)
                       (:html :lang (string-downcase (symbol-name *current-locale*))
                         (:head
                           (:meta :charset "utf-8")
                           (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
                           (:title (t! "login.page-title"))
                           (:link :rel "stylesheet" :href "/static/css/login.css"))
                         (:body :class "login-page"
                           (:div :class "background")

                           (:div :class "login-header"
                                 (:div :class "logo"
                                       (:div :class "logo-icon" "B")
                                       (:span :class "logo-text" (t! "login.logo-text")))
                                 (:a :href "/signup" :class "signup-btn" (t! "common.signup")))

                           (:div :class "container"
                                 (:div :class "login-card"
                                       (:h1 :class "brand-name" (t! "login.brand"))
                                       (:p :class "brand-subtitle" (t! "login.subtitle"))

                                       (:div :id "error-message" :class "error-message" :style "display: none;")

                                       (:form :id "login-form" :action "/api/auth/login" :method "post"
                                             (:div :class "form-group"
                                                   (:input :type "text"
                                                           :name "username"
                                                           :class "form-input"
                                                           :placeholder (t! "login.username")
                                                           :required "required"))

                                             (:div :class "form-group"
                                                   (:input :type "password"
                                                           :name "password"
                                                           :class "form-input"
                                                           :placeholder (t! "login.password")
                                                           :required "required"))

                                             (:button :type "submit" :class "login-btn" (t! "login.button")))

                                       (:div :class "switch-link"
                                             (t! "login.no-account") " "
                                             (:a :href "/signup" (t! "common.signup")))))

                           (:script :src "/static/js/i18n.js")
                           (:script :src "/static/js/login.js")))))

(define-easy-handler (signup-page :uri "/signup") ()
                     "サインアップページ"
                     (set-current-locale-from-request)
                     (setf (content-type*) "text/html")
                     (with-html-string
                       (:doctype)
                       (:html :lang (string-downcase (symbol-name *current-locale*))
                         (:head
                           (:meta :charset "utf-8")
                           (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
                           (:title (t! "signup.page-title"))
                           (:link :rel "stylesheet" :href "/static/css/login.css"))
                         (:body :class "login-page"
                           (:div :class "background")

                           (:div :class "login-header"
                                 (:div :class "logo"
                                       (:div :class "logo-icon" "B")
                                       (:span :class "logo-text" (t! "login.logo-text")))
                                 (:a :href "/login" :class "login-btn-header" (t! "signup.login-btn-header")))

                           (:div :class "container"
                                 (:div :class "login-card"
                                       (:h1 :class "brand-name" (t! "signup.brand"))
                                       (:p :class "brand-subtitle" (t! "signup.subtitle"))

                                       (:div :id "error-message" :class "error-message" :style "display: none;")
                                       (:div :id "success-message" :class "success-message" :style "display: none;")

                                       (:form :id "signup-form" :action "/api/auth/signup" :method "post"
                                             (:div :class "form-group"
                                                   (:input :type "text"
                                                           :name "username"
                                                           :class "form-input"
                                                           :placeholder (t! "signup.username")
                                                           :required "required"
                                                           :minlength "3"
                                                           :maxlength "50"))

                                             (:div :class "form-group"
                                                   (:input :type "email"
                                                           :name "email"
                                                           :class "form-input"
                                                           :placeholder (t! "signup.email")
                                                           :required "required"))

                                             (:div :class "form-group"
                                                   (:input :type "password"
                                                           :name "password"
                                                           :class "form-input"
                                                           :placeholder (t! "signup.password")
                                                           :required "required"
                                                           :minlength "8"))

                                             (:div :class "form-group"
                                                   (:input :type "text"
                                                           :name "display-name"
                                                           :class "form-input"
                                                           :placeholder (t! "signup.display-name")))

                                             (:button :type "submit" :class "login-btn" (t! "signup.button")))

                                       (:div :class "switch-link"
                                             (t! "signup.has-account") " "
                                             (:a :href "/login" (t! "common.login")))))

                           (:script :src "/static/js/i18n.js")
                           (:script :src "/static/js/login.js")))))

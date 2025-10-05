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
                         (respond-json (make-success-response nil "アカウントが正常に作成されました")))))

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
                       (json-response "success" "Logged out")))

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
                           (json-error "Not logged in")))))

;;; 投稿API

(define-easy-handler (api-posts :uri "/api/posts") (status)
                     "投稿を取得（statusパラメータでフィルタリング可能）"
                     (setf (content-type*) "application/json")
                     (let ((posts (cond
                                    ((string= status "published") (get-published-posts))
                                    ((string= status "draft")
                                     ;; 下書きはログインユーザーのみアクセス可能
                                     (let ((user (get-current-user)))
                                       (if user
                                           (get-user-drafts (user-id user))
                                           '())))
                                    (t (get-published-posts))))) ; デフォルトは公開済みのみ
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

(define-easy-handler (api-create-post :uri "/api/posts/create")
                     (title content status)
                     "新しい投稿を作成（要ログイン、status指定可能）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response "ログインが必要です"))
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
                           (respond-json (make-success-response nil "投稿が正常に作成されました"))))))

(define-easy-handler (api-update-post :uri "/api/posts/update")
                     (id title content status)
                     "投稿を更新（作成者のみ、status変更可能）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response "ログインが必要です"))
                           (return-from api-update-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response "投稿IDが必要です"))
                           (return-from api-update-post))
                         (let ((post-id (handler-case (parse-integer id)
                                          (error ()
                                            (setf (return-code*) +http-bad-request+)
                                            (respond-json (make-error-response "無効な投稿IDです"))
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
                                 (respond-json (make-success-response nil "投稿が正常に更新されました"))
                                 ;; 権限エラー
                                 (progn
                                   (setf (return-code*) +http-forbidden+)
                                   (respond-json (make-error-response "この投稿を編集する権限がありません"))))))))))

(define-easy-handler (api-delete-post :uri "/api/posts/delete") (id)
                     "投稿を削除（作成者のみ）"
                     (let ((user (get-current-user)))
                       (if user
                         (if id
                           (if (delete-post (parse-integer id) (user-id user))
                             (json-response "success")
                             (progn
                               (setf (return-code*) hunchentoot:+http-forbidden+)
                               (json-error "Permission denied")))
                           (progn
                             (setf (return-code*) hunchentoot:+http-bad-request+)
                             (json-error "Missing post ID")))
                         (progn
                           (setf (return-code*) hunchentoot:+http-authorization-required+)
                           (json-error "Login required")))))

;; 下書き公開用API
(define-easy-handler (api-publish-post :uri "/api/posts/publish") (id)
                     "下書きを公開状態に変更（作成者のみ）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response "ログインが必要です"))
                           (return-from api-publish-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response "投稿IDが必要です"))
                           (return-from api-publish-post))
                         (handler-case
                           (let ((post-id-int (parse-integer id)))
                             ;; 下書き公開
                             (if (publish-draft post-id-int (user-id user))
                               ;; 成功
                               (respond-json (make-success-response nil "下書きが公開されました"))
                               ;; エラー（下書きが存在しないまたは権限なし）
                               (progn
                                 (setf (return-code*) +http-forbidden+)
                                 (respond-json (make-error-response "下書きが見つからないか、権限がありません")))))
                           (error ()
                             (setf (return-code*) +http-bad-request+)
                             (respond-json (make-error-response "無効な投稿IDです")))))))

;; 投稿非公開用API
(define-easy-handler (api-unpublish-post :uri "/api/posts/unpublish") (id)
                     "公開記事を下書きに戻す（作成者のみ）"
                     (with-validation-handler
                       (let ((user (get-current-user)))
                         (unless user
                           (setf (return-code*) +http-authorization-required+)
                           (respond-json (make-error-response "ログインが必要です"))
                           (return-from api-unpublish-post))
                         ;; IDの数値変換とチェック
                         (unless id
                           (setf (return-code*) +http-bad-request+)
                           (respond-json (make-error-response "投稿IDが必要です"))
                           (return-from api-unpublish-post))
                         (handler-case
                           (let ((post-id-int (parse-integer id)))
                             ;; 投稿非公開
                             (if (unpublish-post post-id-int (user-id user))
                               ;; 成功
                               (respond-json (make-success-response nil "投稿が下書きに戻されました"))
                               ;; エラー（投稿が存在しないまたは権限なし）
                               (progn
                                 (setf (return-code*) +http-forbidden+)
                                 (respond-json (make-error-response "投稿が見つからないか、権限がありません")))))
                           (error ()
                             (setf (return-code*) +http-bad-request+)
                             (respond-json (make-error-response "無効な投稿IDです")))))))

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
                             (json-error "Login required")))))

;;; メインページ

(define-easy-handler (index :uri "/") ()
                     "メインページ"
                     (setf (content-type*) "text/html")
                     (with-html-string
                       (:doctype)
                       (:html
                         (:head
                           (:meta :charset "utf-8")
                           (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
                           (:title "Common Lisp Blog")
                           (:script :src "https://cdn.tailwindcss.com")
                           (:script :src "https://unpkg.com/vue@3/dist/vue.global.js")
                           (:script :src "https://cdn.jsdelivr.net/npm/masonry-layout@4.2.2/dist/masonry.pkgd.min.js")
                           (:link :rel "stylesheet" :href "/static/css/style.css"))
                         (:body
                           (:div :id "app"
                                 (:header :class "bg-blue-600 text-white p-6 shadow-lg"
                                          (:div :class "container mx-auto flex justify-between items-center"
                                                (:div
                                                  (:h1 :class "text-3xl font-bold" "Common Lisp Blog")
                                                  (:p :class "text-blue-100" "Multi-user Blog System"))
                                                (:div :class "flex gap-4"
                                                      (:raw "<template v-if=\"currentUser\">")
                                                      (:raw "<span class=\"text-blue-100\">Hello, {{ currentUser.display_name }}</span>")
                                                      (:raw "<button class=\"bg-white text-blue-600 px-4 py-2 rounded hover:bg-blue-50\" @click=\"logout\">Logout</button>")
                                                      (:raw "</template>")
                                                      (:raw "<template v-else>")
                                                      (:raw "<button class=\"bg-white text-blue-600 px-4 py-2 rounded hover:bg-blue-50 mr-2\" @click=\"showLoginModal = true\">Login</button>")
                                                      (:raw "<button class=\"bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600\" @click=\"showSignupModal = true\">Sign Up</button>")
                                                      (:raw "</template>"))))


                                 ;       (:header :class "bg-blue-600 text-white p-6 shadow-lg"
                                 ;        (:div :class "container mx-auto flex justify-between items-center"
                                 ;         (:div
                                 ;          (:h1 :class "text-3xl font-bold" "Common Lisp Blog")
                                 ;          (:p :class "text-blue-100" "Multi-user Blog System"))
                                 ;         (:div :class "flex gap-4"
                                 ;          (:raw "<template v-if=\"currentUser\">")
                                 ;          (:span :class "text-blue-100" "Hello, {{ currentUser.display_name }}")
                                 ;          (:button :class "bg-white text-blue-600 px-4 py-2 rounded hover:bg-blue-50"
                                 ;                   :onclick "app.logout()" "Logout")
                                 ;          (:raw "</template>")
                                 ;          (:raw "<template v-else>")
                                 ;          (:button :class "bg-white text-blue-600 px-4 py-2 rounded hover:bg-blue-50 mr-2"
                                 ;                   :onclick "app.showLoginModal = true" "Login")
                                 ;          (:button :class "bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600"
                                 ;                   :onclick "app.showSignupModal = true" "Sign Up")
                                 ;          (:raw "</template>"))))


                                 (:main :class "container mx-auto p-6"
                                        (:raw "<div v-if=\"currentUser\" class=\"mb-8 bg-white rounded-lg shadow-md p-6\">")
                                        (:h2 :class "text-2xl font-bold mb-4" "新しい投稿を作成")
                                        (:raw "<form @submit.prevent=\"createPost\">")
                                        (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"newPost.title\" placeholder=\"タイトル\">")
                                        (:raw "<textarea class=\"w-full p-2 border rounded mb-3\" v-model=\"newPost.content\" placeholder=\"内容\" rows=\"4\"></textarea>")
                                        (:button :class "bg-blue-600 text-white px-6 py-2 rounded hover:bg-blue-700"
                                                 :type "submit" "投稿する")
                                        (:raw "</form>")
                                        (:raw "</div>")

                                        (:div :class "masonry-grid" :ref "masonryGrid"
                                              (:raw "<div class=\"masonry-item\" v-for=\"post in posts\" :key=\"post.id\">")
                                              (:div :class "bg-white rounded-lg shadow-md p-6 hover:shadow-xl transition-shadow"
                                                    (:raw "<h3 class=\"text-xl font-bold mb-2\">{{ post.title }}</h3>")
                                                    (:raw "<p class=\"text-gray-600 text-sm mb-3\">by {{ post.author }} - {{ formatDate(post.created_at) }}</p>")
                                                    (:raw "<p class=\"text-gray-700 mb-4\">{{ post.content }}</p>")
                                                    (:raw "<div v-if=\"currentUser && currentUser.id === post.user_id\" class=\"flex gap-2\">")
                                                    (:raw "<button class=\"text-blue-600 hover:text-blue-800\" @click=\"editPost(post)\">編集</button>")
                                                    (:raw "<button class=\"text-red-600 hover:text-red-800\" @click=\"deletePost(post.id)\">削除</button>")
                                                    (:raw "</div>"))
                                              (:raw "</div>")))

                                 (:raw "<div v-if=\"showLoginModal\" class=\"fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center\" @click.self=\"showLoginModal = false\">")
                                 (:div :class "bg-white rounded-lg p-8 max-w-md w-full"
                                       (:h2 :class "text-2xl font-bold mb-4" "Login")
                                       (:raw "<form @submit.prevent=\"login\">")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"loginForm.username\" placeholder=\"Username\" required>")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"loginForm.password\" type=\"password\" placeholder=\"Password\" required>")
                                       (:div :class "flex gap-2"
                                             (:button :class "bg-blue-600 text-white px-6 py-2 rounded hover:bg-blue-700"
                                                      :type "submit" "Login")
                                             (:button :class "bg-gray-300 text-gray-700 px-6 py-2 rounded hover:bg-gray-400"
                                                      :type "button"
                                                      :onclick "app.showLoginModal = false" "Cancel"))
                                       (:raw "</form>"))
                                 (:raw "</div>")

                                 (:raw "<div v-if=\"showSignupModal\" class=\"fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center\" @click.self=\"showSignupModal = false\">")
                                 (:div :class "bg-white rounded-lg p-8 max-w-md w-full"
                                       (:h2 :class "text-2xl font-bold mb-4" "Sign Up")
                                       (:raw "<form @submit.prevent=\"signup\">")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"signupForm.username\" placeholder=\"Username (3-50 characters)\" required>")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"signupForm.email\" type=\"email\" placeholder=\"Email\" required>")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"signupForm.password\" type=\"password\" placeholder=\"Password (min 8 characters)\" required>")
                                       (:raw "<input class=\"w-full p-2 border rounded mb-3\" v-model=\"signupForm.display_name\" placeholder=\"Display Name (optional)\">")
                                       (:div :class "flex gap-2"
                                             (:button :class "bg-green-500 text-white px-6 py-2 rounded hover:bg-green-600"
                                                      :type "submit" "Sign Up")
                                             (:button :class "bg-gray-300 text-gray-700 px-6 py-2 rounded hover:bg-gray-400"
                                                      :type "button"
                                                      :onclick "app.showSignupModal = false" "Cancel"))
                                       (:raw "</form>"))
                                 (:raw "</div>"))
                           (:script :src "/static/js/app.js")))))

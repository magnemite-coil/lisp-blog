(in-package :lisp-blog)

(defun json-response (data)
  "JSON形式のレスポンスを返す"
  (setf (content-type*) "application/json")
  (with-output-to-string (s)
    (yason:encode data s)))

(define-easy-handler (api-posts :uri "/api/posts") ()
  "全投稿を取得するAPIエンドポイント"
  (setf (content-type*) "application/json")
  (let ((posts (get-all-posts)))
    (with-output-to-string (s)
      (yason:encode 
       (mapcar (lambda (post)
                 (list (cons "id" (post-id post))
                       (cons "title" (post-title post))
                       (cons "content" (post-content post))
                       (cons "author" (post-author post))
                       (cons "created_at" (format nil "~a" (post-created-at post)))))
               posts)
       s))))

(define-easy-handler (api-post :uri "/api/posts/:id") (id)
  "特定の投稿を取得するAPIエンドポイント"
  (setf (content-type*) "application/json")
  (let ((post (get-post-by-id (parse-integer id))))
    (if post
        (with-output-to-string (s)
          (yason:encode 
           (list (cons "id" (post-id post))
                 (cons "title" (post-title post))
                 (cons "content" (post-content post))
                 (cons "author" (post-author post))
                 (cons "created_at" (format nil "~a" (post-created-at post))))
           s))
        (setf (return-code*) +http-not-found+))))

(define-easy-handler (api-create-post :uri "/api/posts/create") 
    (title content author)
  "新しい投稿を作成するAPIエンドポイント"
  (setf (content-type*) "application/json")
  (if (and title content author)
      (progn
        (create-post title content author)
        (with-output-to-string (s)
          (yason:encode (list (cons "status" "success")) s)))
      (progn
        (setf (return-code*) +http-bad-request+)
        (with-output-to-string (s)
          (yason:encode (list (cons "status" "error")
                             (cons "message" "Missing required fields")) s)))))

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
        (:div :class "container mx-auto"
         (:h1 :class "text-3xl font-bold" "Common Lisp Blog")
         (:p :class "text-blue-100" "Built with SBCL, Hunchentoot & Vue 3")))
       
       (:main :class "container mx-auto p-6"
        (:div :class "mb-8 bg-white rounded-lg shadow-md p-6"
         (:h2 :class "text-2xl font-bold mb-4" "新しい投稿を作成")
         (:form "@submit.prevent" "createPost"
          (:input :class "w-full p-2 border rounded mb-3" 
                  :v-model "newPost.title" 
                  :placeholder "タイトル")
          (:textarea :class "w-full p-2 border rounded mb-3" 
                     :v-model "newPost.content" 
                     :placeholder "内容" 
                     :rows "4")
          (:input :class "w-full p-2 border rounded mb-3" 
                  :v-model "newPost.author" 
                  :placeholder "著者名")
          (:button :class "bg-blue-600 text-white px-6 py-2 rounded hover:bg-blue-700"
                   :type "submit" "投稿する")))
        
        (:div :class "masonry-grid" :ref "masonryGrid"
         (:div :class "masonry-item" 
               :v-for "post in posts" 
               ":key" "post.id"
          (:div :class "bg-white rounded-lg shadow-md p-6 hover:shadow-xl transition-shadow"
           (:h3 :class "text-xl font-bold mb-2" "{{ post.title }}")
           (:p :class "text-gray-600 text-sm mb-3" 
               "by {{ post.author }} - {{ formatDate(post.created_at) }}")
           (:p :class "text-gray-700" "{{ post.content }}")
           (:button :class "mt-4 text-red-600 hover:text-red-800"
                    "@click" "deletePost(post.id)" "削除"))))))
      (:script :src "/static/js/app.js")))))

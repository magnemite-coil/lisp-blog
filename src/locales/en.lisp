;;;; en.lisp - English translations

(in-package :lisp-blog)

;;; Common
(set-translation :en "common.login" "Login")
(set-translation :en "common.logout" "Logout")
(set-translation :en "common.signup" "Sign Up")
(set-translation :en "common.cancel" "Cancel")
(set-translation :en "common.save" "Save")
(set-translation :en "common.delete" "Delete")
(set-translation :en "common.edit" "Edit")
(set-translation :en "common.sign-up" "Sign Up")

;;; Login page
(set-translation :en "login.page-title" "Login - Common Lisp Blog")
(set-translation :en "login.brand" "MyBlog")
(set-translation :en "login.subtitle" "Login to write your blog")
(set-translation :en "login.username" "Username")
(set-translation :en "login.password" "Password")
(set-translation :en "login.button" "Login")
(set-translation :en "login.no-account" "Don't have an account?")
(set-translation :en "login.logo-text" "Common Lisp Blog")
(set-translation :en "login.error.invalid" "Invalid username or password")

;;; Signup page
(set-translation :en "signup.page-title" "Sign Up - Common Lisp Blog")
(set-translation :en "signup.brand" "MyBlog")
(set-translation :en "signup.subtitle" "Create an account")
(set-translation :en "signup.username" "Username (3-50 characters)")
(set-translation :en "signup.email" "Email")
(set-translation :en "signup.password" "Password (8+ characters)")
(set-translation :en "signup.display-name" "Display Name (optional)")
(set-translation :en "signup.button" "Create Account")
(set-translation :en "signup.has-account" "Already have an account?")
(set-translation :en "signup.success" "Account created successfully!")
(set-translation :en "signup.login-btn-header" "Login")

;;; Main page
(set-translation :en "main.page-title" "Common Lisp Blog - Modern Admin")
(set-translation :en "main.breadcrumb.posts" "Posts")
(set-translation :en "main.breadcrumb.new" "New Post")
(set-translation :en "main.breadcrumb.list" "All Posts")
(set-translation :en "main.new-post" "New Post")
(set-translation :en "main.back-to-list" "Back to List")
(set-translation :en "main.logout" "Logout")

;;; Post editor
(set-translation :en "editor.title-placeholder" "Enter a catchy title...")
(set-translation :en "editor.content-label" "Content")
(set-translation :en "editor.content-placeholder" "Start writing here...

You can write freely in Markdown.

# Heading
## Subheading

**Bold** and *italic* are also available.")

;;; Publish settings
(set-translation :en "status.title" "Publish Settings")
(set-translation :en "status.draft" "Draft")
(set-translation :en "status.draft.desc" "Save as private")
(set-translation :en "status.published" "Publish")
(set-translation :en "status.published.desc" "Publish immediately")
(set-translation :en "status.save-draft" "Save Draft")
(set-translation :en "status.publish" "Publish")

;;; Statistics
(set-translation :en "stats.title" "Statistics")
(set-translation :en "stats.word-count" "Words")
(set-translation :en "stats.read-time" "min read")

;;; Post list
(set-translation :en "post.status.draft" "Draft")
(set-translation :en "post.status.published" "Published")
(set-translation :en "post.edit" "Edit")
(set-translation :en "post.publish-action" "Publish")
(set-translation :en "post.unpublish" "Unpublish")
(set-translation :en "post.delete" "Delete")

;;; Error messages
(set-translation :en "error.not-logged-in" "Login required")
(set-translation :en "error.invalid-credentials" "Invalid username or password")
(set-translation :en "error.server-error" "Server error occurred")

const { createApp } = Vue;

const app = createApp({
  data() {
    return {
      posts: [],
      currentUser: null,
      showCreateView: false,
      newPost: {
        title: '',
        content: '',
        status: 'draft'
      },
      pagination: {
        page: 1,
        per_page: 10,
        total: 0,
        total_pages: 0,
        has_next: false,
        has_prev: false
      }
    }
  },
  
  mounted() {
    this.checkAuth();
    this.fetchPosts();
  },
  
  methods: {
    async checkAuth() {
      try {
        const response = await fetch('/api/auth/me');
        if (response.ok) {
          this.currentUser = await response.json();
        }
      } catch (error) {
        console.error('認証チェックエラー:', error);
      }
    },

    async logout() {
      try {
        await fetch('/api/auth/logout', { method: 'POST' });
        this.currentUser = null;
        alert(i18n.t('auth.logged-out'));
        await this.fetchPosts();
      } catch (error) {
        console.error('ログアウトエラー:', error);
      }
    },
    
    async fetchPosts() {
      try {
        // ログイン中: 全ての投稿を取得（公開済み + 自分の下書き）
        const response = await fetch('/api/user/posts');
        if (response.ok) {
          this.posts = await response.json();
          // ユーザー投稿はページネーション無し（将来実装可能）
          this.pagination = {
            page: 1,
            per_page: this.posts.length,
            total: this.posts.length,
            total_pages: 1,
            has_next: false,
            has_prev: false
          };
        } else {
          // 未ログイン: 公開済みのみページネーション付きで取得
          const publicResponse = await fetch(`/api/posts?page=${this.pagination.page}&per_page=${this.pagination.per_page}`);
          const data = await publicResponse.json();

          // レスポンス形式が異なる場合の処理
          if (data.posts && data.pagination) {
            // 新しいページネーション対応API
            this.posts = data.posts;
            this.pagination = data.pagination;
          } else {
            // 旧形式（配列のみ）の場合
            this.posts = data;
          }
        }
      } catch (error) {
        console.error('投稿の取得に失敗しました:', error);
      }
    },
    
    async createPost() {
      if (!this.newPost.title || !this.newPost.content) {
        alert(i18n.t('validation.required'));
        return;
      }

      try {
        const formData = new URLSearchParams();
        formData.append('title', this.newPost.title);
        formData.append('content', this.newPost.content);
        formData.append('status', this.newPost.status);

        const response = await fetch('/api/posts/create', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });

        if (response.ok) {
          const status = this.newPost.status;
          this.newPost = { title: '', content: '', status: 'draft' };
          this.showCreateView = false;
          await this.fetchPosts();
          const message = status === 'draft' ? i18n.t('post.created.draft') : i18n.t('post.created.published');
          alert(message);
        } else {
          const result = await response.json();
          alert(result.error || i18n.t('post.created'));
        }
      } catch (error) {
        console.error('投稿の作成に失敗しました:', error);
        alert(i18n.t('login.error.server'));
      }
    },
    
    editPost(post) {
      const newTitle = prompt('新しいタイトル:', post.title);
      if (!newTitle) return;
      
      const newContent = prompt('新しい内容:', post.content);
      if (!newContent) return;
      
      this.updatePost(post.id, newTitle, newContent);
    },
    
    async updatePost(id, title, content) {
      try {
        const formData = new URLSearchParams();
        formData.append('id', id);
        formData.append('title', title);
        formData.append('content', content);
        
        const response = await fetch('/api/posts/update', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        if (response.ok) {
          await this.fetchPosts();
          alert(i18n.t('post.updated'));
        } else {
          const result = await response.json();
          alert(result.error || i18n.t('post.updated'));
        }
      } catch (error) {
        console.error('投稿の更新に失敗しました:', error);
        alert(i18n.t('login.error.server'));
      }
    },
    
    async deletePost(id) {
      if (!confirm(i18n.t('validation.confirm-delete'))) {
        return;
      }
      
      try {
        const formData = new URLSearchParams();
        formData.append('id', id);
        
        const response = await fetch('/api/posts/delete', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        if (response.ok) {
          await this.fetchPosts();
          alert(i18n.t('post.deleted'));
        } else {
          const result = await response.json();
          alert(result.error || i18n.t('post.deleted'));
        }
      } catch (error) {
        console.error('削除に失敗しました:', error);
        alert(i18n.t('login.error.server'));
      }
    },
    
    formatDate(dateString) {
      const date = new Date(dateString);
      return date.toLocaleDateString('ja-JP', {
        year: 'numeric',
        month: 'long',
        day: 'numeric'
      });
    },
    
    async publishDraft(postId) {
      try {
        const response = await fetch(`/api/posts/${postId}/publish`, {
          method: 'PUT'
        });

        if (response.ok) {
          alert(i18n.t('post.published'));
          await this.fetchPosts();
        } else {
          const result = await response.json();
          alert(result.error || i18n.t('post.published'));
        }
      } catch (error) {
        console.error('公開に失敗しました:', error);
        alert(i18n.t('login.error.server'));
      }
    },

    async unpublishPost(postId) {
      try {
        const response = await fetch(`/api/posts/${postId}/unpublish`, {
          method: 'PUT'
        });

        if (response.ok) {
          alert(i18n.t('post.unpublished'));
          await this.fetchPosts();
        } else {
          const result = await response.json();
          alert(result.error || i18n.t('post.unpublished'));
        }
      } catch (error) {
        console.error('下書きに戻す処理に失敗しました:', error);
        alert(i18n.t('login.error.server'));
      }
    },

    getWordCount() {
      const title = this.newPost.title || '';
      const content = this.newPost.content || '';
      return (title + ' ' + content).trim().length;
    },

    getReadTime() {
      const wordCount = this.getWordCount();
      // 日本語の場合、1分あたり約600文字と仮定
      const readTime = Math.ceil(wordCount / 600);
      return readTime || 1;
    },

    // ページネーション関連メソッド
    goToPage(page) {
      if (page < 1 || page > this.pagination.total_pages) return;
      this.pagination.page = page;
      this.fetchPosts();
      // ページ最上部にスクロール
      window.scrollTo({ top: 0, behavior: 'smooth' });
    },

    nextPage() {
      if (this.pagination.has_next) {
        this.goToPage(this.pagination.page + 1);
      }
    },

    prevPage() {
      if (this.pagination.has_prev) {
        this.goToPage(this.pagination.page - 1);
      }
    }
  },

  computed: {
    pageNumbers() {
      const pages = [];
      const total = this.pagination.total_pages;
      const current = this.pagination.page;

      if (total <= 1) {
        return pages; // ページが1つ以下の場合は空配列
      }

      if (total <= 7) {
        // 7ページ以下の場合は全て表示
        for (let i = 1; i <= total; i++) {
          pages.push(i);
        }
      } else {
        // 7ページ超の場合は省略表示
        // 例: [1] [2] [3] ... [8] [9] [10]
        // 例: [1] ... [5] [6] [7] ... [10]
        pages.push(1);

        if (current > 3) {
          pages.push('...');
        }

        const start = Math.max(2, current - 1);
        const end = Math.min(total - 1, current + 1);

        for (let i = start; i <= end; i++) {
          pages.push(i);
        }

        if (current < total - 2) {
          pages.push('...');
        }

        pages.push(total);
      }

      return pages;
    }
  }
});

app.mount('#app');

window.app = app;

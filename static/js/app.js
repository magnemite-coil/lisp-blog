const { createApp } = Vue;

const app = createApp({
  data() {
    return {
      posts: [],
      currentUser: null,
      newPost: {
        title: '',
        content: ''
      },
      loginForm: {
        username: '',
        password: ''
      },
      signupForm: {
        username: '',
        email: '',
        password: '',
        display_name: ''
      },
      showLoginModal: false,
      showSignupModal: false
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
    
    async login() {
      try {
        const formData = new URLSearchParams();
        formData.append('username', this.loginForm.username);
        formData.append('password', this.loginForm.password);
        
        const response = await fetch('/api/auth/login', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        const result = await response.json();
        
        if (response.ok) {
          alert('ログイン成功！');
          this.showLoginModal = false;
          this.loginForm = { username: '', password: '' };
          await this.checkAuth();
          await this.fetchPosts();
        } else {
          alert(result.message || 'ログインに失敗しました');
        }
      } catch (error) {
        console.error('ログインエラー:', error);
        alert('ログインに失敗しました');
      }
    },
    
    async signup() {
      try {
        const formData = new URLSearchParams();
        formData.append('username', this.signupForm.username);
        formData.append('email', this.signupForm.email);
        formData.append('password', this.signupForm.password);
        formData.append('display-name', this.signupForm.display_name || this.signupForm.username);
        
        const response = await fetch('/api/auth/signup', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        const result = await response.json();
        
        if (response.ok) {
          alert('アカウント作成成功！ログインしてください。');
          this.showSignupModal = false;
          this.signupForm = { username: '', email: '', password: '', display_name: '' };
          this.showLoginModal = true;
        } else {
          alert(result.message || 'アカウント作成に失敗しました');
        }
      } catch (error) {
        console.error('サインアップエラー:', error);
        alert('アカウント作成に失敗しました');
      }
    },
    
    async logout() {
      try {
        await fetch('/api/auth/logout', { method: 'POST' });
        this.currentUser = null;
        alert('ログアウトしました');
        await this.fetchPosts();
      } catch (error) {
        console.error('ログアウトエラー:', error);
      }
    },
    
    async fetchPosts() {
      try {
        const response = await fetch('/api/posts');
        this.posts = await response.json();
        
        this.$nextTick(() => {
          this.initMasonry();
        });
      } catch (error) {
        console.error('投稿の取得に失敗しました:', error);
      }
    },
    
    async createPost() {
      if (!this.newPost.title || !this.newPost.content) {
        alert('タイトルと内容を入力してください');
        return;
      }
      
      try {
        const formData = new URLSearchParams();
        formData.append('title', this.newPost.title);
        formData.append('content', this.newPost.content);
        
        const response = await fetch('/api/posts/create', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        if (response.ok) {
          this.newPost = { title: '', content: '' };
          await this.fetchPosts();
          alert('投稿が作成されました！');
        } else {
          const result = await response.json();
          alert(result.error || '投稿の作成に失敗しました');
        }
      } catch (error) {
        console.error('投稿の作成に失敗しました:', error);
        alert('投稿の作成に失敗しました');
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
          alert('投稿が更新されました！');
        } else {
          const result = await response.json();
          alert(result.error || '投稿の更新に失敗しました');
        }
      } catch (error) {
        console.error('投稿の更新に失敗しました:', error);
        alert('投稿の更新に失敗しました');
      }
    },
    
    async deletePost(id) {
      if (!confirm('本当に削除しますか？')) {
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
          alert('投稿が削除されました');
        } else {
          const result = await response.json();
          alert(result.error || '削除に失敗しました');
        }
      } catch (error) {
        console.error('削除に失敗しました:', error);
        alert('削除に失敗しました');
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
    
    initMasonry() {
      const grid = this.$refs.masonryGrid;
      if (grid && window.Masonry) {
        new Masonry(grid, {
          itemSelector: '.masonry-item',
          columnWidth: '.masonry-item',
          percentPosition: true
        });
      }
    }
  }
});

app.mount('#app');

window.app = app;

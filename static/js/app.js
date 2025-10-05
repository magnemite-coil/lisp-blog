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
        alert('ログアウトしました');
        await this.fetchPosts();
      } catch (error) {
        console.error('ログアウトエラー:', error);
      }
    },
    
    async fetchPosts() {
      try {
        // 全ての投稿を取得（公開済み + 自分の下書き）
        const response = await fetch('/api/user/posts');
        if (response.ok) {
          this.posts = await response.json();
        } else {
          // ログインしていない場合は公開済みのみ取得
          const publicResponse = await fetch('/api/posts?status=published');
          this.posts = await publicResponse.json();
        }
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
        formData.append('status', this.newPost.status);

        const response = await fetch('/api/posts/create', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });

        if (response.ok) {
          this.newPost = { title: '', content: '', status: 'draft' };
          this.showCreateView = false;
          await this.fetchPosts();
          const message = this.newPost.status === 'draft' ? '下書きが保存されました！' : '投稿が公開されました！';
          alert(message);
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
    
    async publishDraft(postId) {
      try {
        const response = await fetch(`/api/posts/${postId}/publish`, {
          method: 'PUT'
        });

        if (response.ok) {
          alert('下書きが公開されました！');
          await this.fetchPosts();
        } else {
          const result = await response.json();
          alert(result.error || '公開に失敗しました');
        }
      } catch (error) {
        console.error('公開に失敗しました:', error);
        alert('公開に失敗しました');
      }
    },

    async unpublishPost(postId) {
      try {
        const response = await fetch(`/api/posts/${postId}/unpublish`, {
          method: 'PUT'
        });

        if (response.ok) {
          alert('投稿が下書きに戻されました！');
          await this.fetchPosts();
        } else {
          const result = await response.json();
          alert(result.error || '下書きに戻す処理に失敗しました');
        }
      } catch (error) {
        console.error('下書きに戻す処理に失敗しました:', error);
        alert('下書きに戻す処理に失敗しました');
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
    }
  }
});

app.mount('#app');

window.app = app;

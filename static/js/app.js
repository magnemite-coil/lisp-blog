const { createApp } = Vue;

createApp({
  data() {
    return {
      posts: [],
      newPost: {
        title: '',
        content: '',
        author: ''
      }
    }
  },
  
  mounted() {
    this.fetchPosts();
  },
  
  methods: {
    async fetchPosts() {
      try {
        const response = await fetch('/api/posts');
        this.posts = await response.json();
        
        // Masonryレイアウトを次のティックで更新
        this.$nextTick(() => {
          this.initMasonry();
        });
      } catch (error) {
        console.error('投稿の取得に失敗しました:', error);
      }
    },
    
    async createPost() {
      if (!this.newPost.title || !this.newPost.content || !this.newPost.author) {
        alert('全ての項目を入力してください');
        return;
      }
      
      try {
        const formData = new URLSearchParams();
        formData.append('title', this.newPost.title);
        formData.append('content', this.newPost.content);
        formData.append('author', this.newPost.author);
        
        const response = await fetch('/api/posts/create', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
          },
          body: formData
        });
        
        if (response.ok) {
          // フォームをリセット
          this.newPost = { title: '', content: '', author: '' };
          
          // 投稿一覧を再取得
          await this.fetchPosts();
          
          alert('投稿が作成されました！');
        }
      } catch (error) {
        console.error('投稿の作成に失敗しました:', error);
        alert('投稿の作成に失敗しました');
      }
    },
    
    async deletePost(id) {
      if (!confirm('本当に削除しますか？')) {
        return;
      }
      
      try {
        // この例では削除APIを実装していないため、
        // 実際の実装では適切なAPIエンドポイントを作成してください
        alert('削除機能は未実装です');
      } catch (error) {
        console.error('削除に失敗しました:', error);
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
      // Masonryレイアウトの初期化
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
}).mount('#app');

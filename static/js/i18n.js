/**
 * i18n.js - Simple internationalization helper
 * Lightweight i18n implementation without external dependencies
 */

const i18n = {
  // Current locale (from cookie or default to 'ja')
  locale: getCookie('locale') || 'ja',

  // Translation messages
  messages: {
    ja: {
      // Login
      'login.error.invalid': 'ユーザー名またはパスワードが正しくありません',
      'login.error.server': 'サーバーエラーが発生しました',

      // Signup
      'signup.success': 'アカウントが作成されました！',
      'signup.redirect': '2秒後にログインページに移動します...',
      'signup.error.exists': 'ユーザー名またはメールアドレスが既に使用されています',
      'signup.error.validation': '入力内容に誤りがあります',

      // Post
      'post.created': '投稿が作成されました！',
      'post.created.draft': '下書きが保存されました！',
      'post.created.published': '投稿が公開されました！',
      'post.updated': '投稿が更新されました！',
      'post.deleted': '投稿が削除されました',
      'post.published': '下書きが公開されました！',
      'post.unpublished': '投稿が下書きに戻されました！',

      // Validation
      'validation.required': 'タイトルと内容を入力してください',
      'validation.confirm-delete': '本当に削除しますか？',

      // Auth
      'auth.logged-out': 'ログアウトしました'
    },

    en: {
      // Login
      'login.error.invalid': 'Invalid username or password',
      'login.error.server': 'Server error occurred',

      // Signup
      'signup.success': 'Account created successfully!',
      'signup.redirect': 'Redirecting to login page in 2 seconds...',
      'signup.error.exists': 'Username or email already exists',
      'signup.error.validation': 'Invalid input',

      // Post
      'post.created': 'Post created!',
      'post.created.draft': 'Draft saved!',
      'post.created.published': 'Post published!',
      'post.updated': 'Post updated!',
      'post.deleted': 'Post deleted',
      'post.published': 'Draft published!',
      'post.unpublished': 'Post unpublished!',

      // Validation
      'validation.required': 'Please enter title and content',
      'validation.confirm-delete': 'Are you sure you want to delete?',

      // Auth
      'auth.logged-out': 'Logged out'
    }
  },

  /**
   * Get translation
   * @param {string} key - Translation key
   * @returns {string} Translated text
   */
  t(key) {
    const message = this.messages[this.locale][key];
    return message !== undefined ? message : key;
  },

  /**
   * Switch locale
   * @param {string} locale - 'ja' or 'en'
   */
  switchLocale(locale) {
    if (locale !== 'ja' && locale !== 'en') {
      console.error('Unsupported locale:', locale);
      return;
    }

    this.locale = locale;
    setCookie('locale', locale, 365); // Valid for 1 year
    location.reload(); // Reload page to reflect changes
  },

  /**
   * Get current locale
   * @returns {string} Current locale ('ja' or 'en')
   */
  getLocale() {
    return this.locale;
  }
};

/**
 * Get cookie value
 * @param {string} name - Cookie name
 * @returns {string|null} Cookie value or null
 */
function getCookie(name) {
  const value = `; ${document.cookie}`;
  const parts = value.split(`; ${name}=`);
  if (parts.length === 2) {
    return parts.pop().split(';').shift();
  }
  return null;
}

/**
 * Set cookie
 * @param {string} name - Cookie name
 * @param {string} value - Cookie value
 * @param {number} days - Expiration in days
 */
function setCookie(name, value, days) {
  const expires = new Date(Date.now() + days * 864e5).toUTCString();
  document.cookie = `${name}=${encodeURIComponent(value)}; expires=${expires}; path=/`;
}

// Make i18n available globally
window.i18n = i18n;

// Log current locale on load
console.log('[i18n] Current locale:', i18n.locale);

/**
 * Create and inject language switcher UI
 */
function createLanguageSwitcher() {
  // Check if already exists
  if (document.querySelector('.lang-switcher')) {
    return;
  }

  const switcher = document.createElement('div');
  switcher.className = 'lang-switcher';
  
  // Japanese button
  const jaBtn = document.createElement('button');
  jaBtn.className = 'lang-btn' + (i18n.locale === 'ja' ? ' active' : '');
  jaBtn.textContent = '日本語';
  jaBtn.onclick = () => i18n.switchLocale('ja');
  
  // English button
  const enBtn = document.createElement('button');
  enBtn.className = 'lang-btn' + (i18n.locale === 'en' ? ' active' : '');
  enBtn.textContent = 'English';
  enBtn.onclick = () => i18n.switchLocale('en');
  
  switcher.appendChild(jaBtn);
  switcher.appendChild(enBtn);
  
  // Inject into body
  document.body.appendChild(switcher);
}

// Auto-create language switcher when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', createLanguageSwitcher);
} else {
  createLanguageSwitcher();
}

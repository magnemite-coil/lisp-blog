// Login and Signup Page JavaScript

document.addEventListener('DOMContentLoaded', function() {
    const loginForm = document.getElementById('login-form');
    const signupForm = document.getElementById('signup-form');
    const errorMessage = document.getElementById('error-message');
    const successMessage = document.getElementById('success-message');

    // Login Form Handler
    if (loginForm) {
        loginForm.addEventListener('submit', async function(e) {
            e.preventDefault();

            const formData = new FormData(loginForm);
            const data = new URLSearchParams(formData);

            try {
                const response = await fetch('/api/auth/login', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded',
                    },
                    body: data
                });

                const result = await response.json();

                if (response.ok) {
                    // ログイン成功 - メインページにリダイレクト
                    window.location.href = '/';
                } else {
                    // エラー表示
                    showError(result.error || result.message || i18n.t('login.error.invalid'));
                }
            } catch (error) {
                console.error('Login error:', error);
                showError(i18n.t('login.error.server'));
            }
        });
    }

    // Signup Form Handler
    if (signupForm) {
        signupForm.addEventListener('submit', async function(e) {
            e.preventDefault();

            const formData = new FormData(signupForm);
            const data = new URLSearchParams(formData);

            try {
                const response = await fetch('/api/auth/signup', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded',
                    },
                    body: data
                });

                const result = await response.json();

                if (response.ok) {
                    // サインアップ成功
                    showSuccess(i18n.t('signup.success') + ' ' + i18n.t('signup.redirect'));
                    setTimeout(() => {
                        window.location.href = '/login';
                    }, 2000);
                } else {
                    // エラー表示
                    showError(result.error || result.message || i18n.t('signup.error.validation'));
                }
            } catch (error) {
                console.error('Signup error:', error);
                showError(i18n.t('login.error.server'));
            }
        });
    }

    function showError(message) {
        if (errorMessage) {
            errorMessage.textContent = message;
            errorMessage.style.display = 'block';
            if (successMessage) {
                successMessage.style.display = 'none';
            }
        }
    }

    function showSuccess(message) {
        if (successMessage) {
            successMessage.textContent = message;
            successMessage.style.display = 'block';
            if (errorMessage) {
                errorMessage.style.display = 'none';
            }
        }
    }
});

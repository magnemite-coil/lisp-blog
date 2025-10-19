import { describe, it, expect, vi, beforeEach } from 'vitest'
import { screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { render } from '../test/test-utils'
import { RegisterPage } from './RegisterPage'
import * as authApi from '../api/auth'
import type { User } from '../types/User'

vi.mock('../api/auth')

const mockNavigate = vi.fn()
vi.mock('react-router-dom', async () => {
  const actual = await vi.importActual('react-router-dom')
  return {
    ...actual,
    useNavigate: () => mockNavigate,
  }
})

describe('RegisterPage', () => {
  const mockUser: User = {
    id: 1,
    username: 'newuser',
    created_at: '2025-01-01T00:00:00Z',
  }

  beforeEach(() => {
    vi.clearAllMocks()
    vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
  })

  describe('初期表示', () => {
    it('登録フォームが表示される', async () => {
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByRole('heading', { name: '新規登録' })).toBeInTheDocument()
      })

      expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      expect(screen.getByPlaceholderText('パスワード（8-100文字）')).toBeInTheDocument()
      expect(screen.getByPlaceholderText('パスワード（確認）')).toBeInTheDocument()
      expect(screen.getByRole('button', { name: '登録' })).toBeInTheDocument()
    })

    it('ログインへのリンクが表示される', async () => {
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByRole('link', { name: 'ログイン' })).toBeInTheDocument()
      })

      const loginLink = screen.getByRole('link', { name: 'ログイン' })
      expect(loginLink).toHaveAttribute('href', '/login')
    })
  })

  describe('バリデーション', () => {
    it('空のフォームを送信するとバリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: '登録' })).toBeInTheDocument()
      })

      const submitButton = screen.getByRole('button', { name: '登録' })
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByText('ユーザー名を入力してください')).toBeInTheDocument()
      })

      expect(screen.getByText('パスワードを入力してください')).toBeInTheDocument()
    })

    it('ユーザー名が短すぎる場合、バリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      await user.type(usernameInput, 'ab')
      await user.tab()

      await waitFor(() => {
        expect(screen.getByText('ユーザー名は3文字以上で入力してください')).toBeInTheDocument()
      })
    })

    it('パスワードが短すぎる場合、バリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('パスワード（8-100文字）')).toBeInTheDocument()
      })

      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      await user.type(passwordInput, '1234567')
      await user.tab()

      await waitFor(() => {
        expect(screen.getByText('パスワードは8文字以上で入力してください')).toBeInTheDocument()
      })
    })

    it('パスワードが一致しない場合、バリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('パスワード（8-100文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      const passwordConfirmInput = screen.getByPlaceholderText('パスワード（確認）')
      const submitButton = screen.getByRole('button', { name: '登録' })

      await user.type(usernameInput, 'testuser')
      await user.type(passwordInput, 'password123')
      await user.type(passwordConfirmInput, 'password456')
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByText('パスワードが一致しません')).toBeInTheDocument()
      })
    })
  })

  describe('登録機能', () => {
    it('正しい情報で登録すると成功する', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.register).mockResolvedValue(mockUser)

      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      const passwordConfirmInput = screen.getByPlaceholderText('パスワード（確認）')
      const submitButton = screen.getByRole('button', { name: '登録' })

      await user.type(usernameInput, 'newuser')
      await user.type(passwordInput, 'password123')
      await user.type(passwordConfirmInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(authApi.register).toHaveBeenCalledWith({
          username: 'newuser',
          password: 'password123',
        })
      })

      expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
    })

    it('登録中はボタンが無効化され、テキストが変わる', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.register).mockImplementation(
        () => new Promise((resolve) => setTimeout(() => resolve(mockUser), 100))
      )

      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      const passwordConfirmInput = screen.getByPlaceholderText('パスワード（確認）')
      const submitButton = screen.getByRole('button', { name: '登録' })

      await user.type(usernameInput, 'newuser')
      await user.type(passwordInput, 'password123')
      await user.type(passwordConfirmInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: '登録中...' })).toBeInTheDocument()
      })

      const submittingButton = screen.getByRole('button', { name: '登録中...' })
      expect(submittingButton).toBeDisabled()
    })

    it('登録に失敗するとエラーメッセージが表示される', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.register).mockRejectedValue({
        response: {
          data: {
            message: 'このユーザー名は既に使用されています',
          },
        },
      })

      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      const passwordConfirmInput = screen.getByPlaceholderText('パスワード（確認）')
      const submitButton = screen.getByRole('button', { name: '登録' })

      await user.type(usernameInput, 'existinguser')
      await user.type(passwordInput, 'password123')
      await user.type(passwordConfirmInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(
          screen.getByText('このユーザー名は既に使用されています')
        ).toBeInTheDocument()
      })

      expect(mockNavigate).not.toHaveBeenCalled()
    })

    it('登録エラー時、エラーメッセージがない場合はデフォルトメッセージを表示', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.register).mockRejectedValue(new Error('Network error'))

      render(<RegisterPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名（3-50文字）')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名（3-50文字）')
      const passwordInput = screen.getByPlaceholderText('パスワード（8-100文字）')
      const passwordConfirmInput = screen.getByPlaceholderText('パスワード（確認）')
      const submitButton = screen.getByRole('button', { name: '登録' })

      await user.type(usernameInput, 'newuser')
      await user.type(passwordInput, 'password123')
      await user.type(passwordConfirmInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByText('ユーザー登録に失敗しました。')).toBeInTheDocument()
      })
    })
  })

  describe('リダイレクト', () => {
    it('すでにログイン済みの場合、ダッシュボードにリダイレクトされる', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)

      render(<RegisterPage />)

      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
      })
    })
  })
})

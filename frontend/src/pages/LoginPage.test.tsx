import { describe, it, expect, vi, beforeEach } from 'vitest'
import { screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { render } from '../test/test-utils'
import { LoginPage } from './LoginPage'
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

describe('LoginPage', () => {
  const mockUser: User = {
    id: 1,
    username: 'testuser',
    created_at: '2025-01-01T00:00:00Z',
  }

  beforeEach(() => {
    vi.clearAllMocks()
    mockNavigate.mockClear()
    vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
  })

  describe('初期表示', () => {
    it('ログインフォームが表示される', async () => {
      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByRole('heading', { name: 'ログイン' })).toBeInTheDocument()
      })

      expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      expect(screen.getByPlaceholderText('パスワード')).toBeInTheDocument()
      expect(screen.getByRole('button', { name: 'ログイン' })).toBeInTheDocument()
    })

    it('新規登録へのリンクが表示される', async () => {
      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByRole('link', { name: '新規登録' })).toBeInTheDocument()
      })

      const registerLink = screen.getByRole('link', { name: '新規登録' })
      expect(registerLink).toHaveAttribute('href', '/register')
    })
  })

  describe('バリデーション', () => {
    it('空のフォームを送信するとバリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: 'ログイン' })).toBeInTheDocument()
      })

      const submitButton = screen.getByRole('button', { name: 'ログイン' })
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByText('ユーザー名を入力してください')).toBeInTheDocument()
      })

      expect(screen.getByText('パスワードを入力してください')).toBeInTheDocument()
    })

    it('ユーザー名が短すぎる場合、バリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名')
      await user.type(usernameInput, 'ab')
      await user.tab() // フォーカスを外す

      await waitFor(() => {
        expect(screen.getByText('ユーザー名は3文字以上で入力してください')).toBeInTheDocument()
      })
    })

    it('パスワードが短すぎる場合、バリデーションエラーが表示される', async () => {
      const user = userEvent.setup()
      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('パスワード')).toBeInTheDocument()
      })

      const passwordInput = screen.getByPlaceholderText('パスワード')
      await user.type(passwordInput, '1234567')
      await user.tab() // フォーカスを外す

      await waitFor(() => {
        expect(screen.getByText('パスワードは8文字以上で入力してください')).toBeInTheDocument()
      })
    })
  })

  describe('ログイン機能', () => {
    it('正しい情報でログインすると成功する', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.login).mockResolvedValue(mockUser)

      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名')
      const passwordInput = screen.getByPlaceholderText('パスワード')
      const submitButton = screen.getByRole('button', { name: 'ログイン' })

      await user.type(usernameInput, 'testuser')
      await user.type(passwordInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(authApi.login).toHaveBeenCalledWith({
          username: 'testuser',
          password: 'password123',
        })
      })

      expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
    })

    it('ログイン中はボタンが無効化され、テキストが変わる', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.login).mockImplementation(
        () => new Promise((resolve) => setTimeout(() => resolve(mockUser), 100))
      )

      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名')
      const passwordInput = screen.getByPlaceholderText('パスワード')
      const submitButton = screen.getByRole('button', { name: 'ログイン' })

      await user.type(usernameInput, 'testuser')
      await user.type(passwordInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: 'ログイン中...' })).toBeInTheDocument()
      })

      const submittingButton = screen.getByRole('button', { name: 'ログイン中...' })
      expect(submittingButton).toBeDisabled()

      // ログインが完了するまで待つ（次のテストへの影響を防ぐ）
      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
      }, { timeout: 1000 })
    })

    it('ログインに失敗するとエラーメッセージが表示される', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.login).mockRejectedValue({
        response: {
          data: {
            message: 'ユーザー名またはパスワードが正しくありません',
          },
        },
      })

      render(<LoginPage />)

      // 初期ローディングが完了するまで待つ
      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      })

      // useEffectのリダイレクトが起きないよう少し待つ
      await new Promise(resolve => setTimeout(resolve, 100))

      // この時点ではまだナビゲートされていないはず
      const initialNavigateCalls = mockNavigate.mock.calls.length

      const usernameInput = screen.getByPlaceholderText('ユーザー名')
      const passwordInput = screen.getByPlaceholderText('パスワード')
      const submitButton = screen.getByRole('button', { name: 'ログイン' })

      await user.type(usernameInput, 'wronguser')
      await user.type(passwordInput, 'wrongpassword')
      await user.click(submitButton)

      await waitFor(() => {
        expect(
          screen.getByText('ユーザー名またはパスワードが正しくありません')
        ).toBeInTheDocument()
      })

      // ログイン失敗時は新たなナビゲート呼び出しがないはず
      expect(mockNavigate).toHaveBeenCalledTimes(initialNavigateCalls)
    })

    it('ログインエラー時、エラーメッセージがない場合はデフォルトメッセージを表示', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.login).mockRejectedValue(new Error('Network error'))

      render(<LoginPage />)

      await waitFor(() => {
        expect(screen.getByPlaceholderText('ユーザー名')).toBeInTheDocument()
      })

      const usernameInput = screen.getByPlaceholderText('ユーザー名')
      const passwordInput = screen.getByPlaceholderText('パスワード')
      const submitButton = screen.getByRole('button', { name: 'ログイン' })

      await user.type(usernameInput, 'testuser')
      await user.type(passwordInput, 'password123')
      await user.click(submitButton)

      await waitFor(() => {
        expect(screen.getByText('ログインに失敗しました。')).toBeInTheDocument()
      })
    })
  })

  describe('リダイレクト', () => {
    it('すでにログイン済みの場合、ダッシュボードにリダイレクトされる', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)

      render(<LoginPage />)

      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/dashboard')
      })
    })
  })
})

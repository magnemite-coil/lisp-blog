import { describe, it, expect, vi, beforeEach } from 'vitest'
import { screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { render } from '../test/test-utils'
import { Header } from './Header'
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

describe('Header', () => {
  const mockUser: User = {
    id: 1,
    username: 'testuser',
    created_at: '2025-01-01T00:00:00Z',
  }

  beforeEach(() => {
    vi.clearAllMocks()
  })

  describe('ログイン前の表示', () => {
    it('ログインしていない場合、ユーザー情報とログアウトボタンが表示されない', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)

      render(<Header />)

      await waitFor(() => {
        expect(screen.queryByText(/ようこそ/)).not.toBeInTheDocument()
      })

      expect(screen.queryByRole('button', { name: /ログアウト/ })).not.toBeInTheDocument()
    })

    it('ロゴとタイトルが常に表示される', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)

      render(<Header />)

      await waitFor(() => {
        expect(screen.getByText('Lisp Blog')).toBeInTheDocument()
      })

      const logoLink = screen.getByRole('link', { name: 'Lisp Blog' })
      expect(logoLink).toHaveAttribute('href', '/dashboard')
    })
  })

  describe('ログイン後の表示', () => {
    it('ログインしている場合、ユーザー名が表示される', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)

      render(<Header />)

      await waitFor(() => {
        expect(screen.getByText(/ようこそ/)).toBeInTheDocument()
      })

      expect(screen.getByText('testuser')).toBeInTheDocument()
    })

    it('ログインしている場合、ログアウトボタンが表示される', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)

      render(<Header />)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: /ログアウト/ })).toBeInTheDocument()
      })
    })
  })

  describe('ログアウト機能', () => {
    it('ログアウトボタンをクリックするとログアウト処理が実行される', async () => {
      const user = userEvent.setup()
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)
      vi.mocked(authApi.logout).mockResolvedValue()

      render(<Header />)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: /ログアウト/ })).toBeInTheDocument()
      })

      const logoutButton = screen.getByRole('button', { name: /ログアウト/ })
      await user.click(logoutButton)

      await waitFor(() => {
        expect(authApi.logout).toHaveBeenCalledTimes(1)
      })

      expect(mockNavigate).toHaveBeenCalledWith('/login')
    })

    it('ログアウトに失敗してもエラーがスローされない', async () => {
      const user = userEvent.setup()
      const consoleErrorSpy = vi.spyOn(console, 'error').mockImplementation(() => {})

      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)
      vi.mocked(authApi.logout).mockRejectedValue(new Error('Logout failed'))

      render(<Header />)

      await waitFor(() => {
        expect(screen.getByRole('button', { name: /ログアウト/ })).toBeInTheDocument()
      })

      const logoutButton = screen.getByRole('button', { name: /ログアウト/ })
      await user.click(logoutButton)

      await waitFor(() => {
        expect(consoleErrorSpy).toHaveBeenCalled()
      })

      expect(mockNavigate).not.toHaveBeenCalled()

      consoleErrorSpy.mockRestore()
    })
  })
})

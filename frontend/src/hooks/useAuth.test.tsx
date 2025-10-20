import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, waitFor } from '../test/test-utils'
import { useAuth } from './useAuth'
import * as authApi from '../api/auth'
import type { User } from '../types/User'

vi.mock('../api/auth')

describe('useAuth', () => {
  const mockUser: User = {
    id: 1,
    username: 'testuser',
    created_at: '2025-01-01T00:00:00Z',
  }

  beforeEach(() => {
    vi.clearAllMocks()
  })

  describe('初期化', () => {
    it('初期状態ではユーザーがnullでローディング中である', () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)

      const { result } = renderHook(() => useAuth())

      expect(result.current.user).toBeNull()
      expect(result.current.isAuthenticated).toBe(false)
      expect(result.current.isLoading).toBe(true)
    })

    it('初期化時にログイン状態を確認する', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      expect(result.current.user).toEqual(mockUser)
      expect(result.current.isAuthenticated).toBe(true)
      expect(authApi.getCurrentUser).toHaveBeenCalledTimes(1)
    })

    it('getCurrentUserが失敗した場合、ユーザーはnullのまま', async () => {
      vi.mocked(authApi.getCurrentUser).mockRejectedValue(new Error('Unauthorized'))

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      expect(result.current.user).toBeNull()
      expect(result.current.isAuthenticated).toBe(false)
    })
  })

  describe('login', () => {
    it('ログインに成功するとユーザー情報が設定される', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
      vi.mocked(authApi.login).mockResolvedValue(mockUser)

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      await result.current.login({ username: 'testuser', password: 'password123' })

      expect(result.current.user).toEqual(mockUser)
      expect(result.current.isAuthenticated).toBe(true)
      expect(authApi.login).toHaveBeenCalledWith({
        username: 'testuser',
        password: 'password123',
      })
    })

    it('ログインに失敗するとエラーがスローされる', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
      vi.mocked(authApi.login).mockRejectedValue(new Error('Invalid credentials'))

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      await expect(
        result.current.login({ username: 'wronguser', password: 'wrongpass' })
      ).rejects.toThrow('Invalid credentials')

      expect(result.current.user).toBeNull()
      expect(result.current.isAuthenticated).toBe(false)
    })
  })

  describe('register', () => {
    it('登録に成功するとユーザー情報が設定される', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
      vi.mocked(authApi.register).mockResolvedValue(mockUser)

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      await result.current.register({ username: 'newuser', password: 'password123' })

      expect(result.current.user).toEqual(mockUser)
      expect(result.current.isAuthenticated).toBe(true)
      expect(authApi.register).toHaveBeenCalledWith({
        username: 'newuser',
        password: 'password123',
      })
    })

    it('登録に失敗するとエラーがスローされる', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(null)
      vi.mocked(authApi.register).mockRejectedValue(new Error('Username already exists'))

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      await expect(
        result.current.register({ username: 'existinguser', password: 'password123' })
      ).rejects.toThrow('Username already exists')

      expect(result.current.user).toBeNull()
      expect(result.current.isAuthenticated).toBe(false)
    })
  })

  describe('logout', () => {
    it('ログアウトするとユーザー情報がnullになる', async () => {
      vi.mocked(authApi.getCurrentUser).mockResolvedValue(mockUser)
      vi.mocked(authApi.logout).mockResolvedValue()

      const { result } = renderHook(() => useAuth())

      await waitFor(() => {
        expect(result.current.isLoading).toBe(false)
      })

      expect(result.current.user).toEqual(mockUser)
      expect(result.current.isAuthenticated).toBe(true)

      await result.current.logout()

      expect(result.current.user).toBeNull()
      expect(result.current.isAuthenticated).toBe(false)
      expect(authApi.logout).toHaveBeenCalledTimes(1)
    })
  })

  describe('エラーハンドリング', () => {
    it('AuthProvider外でuseAuthを使用するとエラーがスローされる', () => {
      expect(() => {
        renderHook(() => useAuth())
      }).toThrow('useAuth must be used within an AuthProvider')
    })
  })
})

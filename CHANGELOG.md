# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Internationalization (i18n) support for Japanese and English
  - Hash-table based translation system
  - Cookie-based locale persistence
  - Automatic language switcher UI on all pages
  - Translation support for login, signup, and main pages
  - API message localization (authentication and post management endpoints)
- Dedicated login and signup pages with modern UI
  - Gradient background design
  - Responsive card layout
  - Enhanced user experience
- Draft functionality for blog posts
  - Save posts as drafts
  - Publish drafts to public
  - Unpublish posts back to drafts
- Modern admin interface with card layout
  - Dark theme design
  - Real-time post status management
  - Word count and reading time statistics
- Comprehensive mockup gallery (44 designs)
  - 15 engineer-focused blog themes
  - 19 creator-focused blog themes
  - 10 admin panel layouts
  - GitHub Pages deployment for preview
- Automated test suite with FiveAM framework (Phase 1 completed)
  - 97 test cases covering utility functions (100% pass rate)
  - PostgreSQL-based test database with transaction isolation
  - Comprehensive validation and security function tests
  - Password hashing, session ID generation, input validation, HTML sanitization tests
  - Test fixtures for database isolation (with-empty-db, with-transaction)

### Changed
- Improved session management for better performance
- Enhanced error handling across the application
- Removed modal-based authentication in favor of dedicated pages
- Optimized database queries with JOINs

### Security
- Strengthened input validation and sanitization
- Improved password hashing with PBKDF2 (100,000 iterations)
- Enhanced session security with 7-day expiration
- Protected sensitive files via .gitignore
- SQL injection prevention with parameterized queries
- Fixed HTML sanitization function for proper XSS protection

### Fixed
- Session timeout issues
- Database connection handling
- Timestamp formatting errors
- Masonry layout rendering

## [0.1.0] - 2025-10-04

### Added
- Initial release
- User authentication system (registration, login, logout)
- Blog post CRUD operations
- PostgreSQL database integration
- Vue 3 frontend with reactive UI
- Basic security measures
- Session-based authentication

---

**Note**: For detailed development progress and internal notes, see `PROGRESS.md` (local only, not tracked in git).

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Updated authentication UI design with modern Tumblr-inspired styling
  - Login and registration pages now feature a navy blue gradient background
  - Semi-transparent card design with improved visual hierarchy
  - Bright blue accent color for interactive elements
  - Preparation for future OAuth integration (Google sign-in)

### Added
- Test coverage reporting for backend and frontend
  - Backend coverage using SBCL sb-cover (Expression and Branch coverage)
  - Frontend coverage using Vitest @vitest/coverage-v8 (Lines, Functions, Branches, Statements)
  - Automated coverage report generation scripts (run-tests-coverage.sh)
  - HTML reports for visual coverage inspection
  - Coverage thresholds: Backend (Service 90%+, Util 80%+), Frontend (Lines 30%, Functions 45%, Branches 85%)
  - Comprehensive coverage documentation in docs/testing/ directory
  - Backend guide: sb-cover execution, report interpretation, color coding, FAQs
  - Frontend guide: Vitest coverage execution, metrics explanation, threshold management
  - Quick start README for both coverage systems
- CI/CD pipeline with GitHub Actions
  - Backend CI: Automated testing with SBCL 2.5.9, PostgreSQL 15, and Redis 7 service containers
  - Frontend CI: Lint, type check, unit tests, build verification, and security audit
  - 307 backend test cases automated execution (FiveAM)
  - 36 frontend test cases automated execution (Vitest)
  - Node.js 20 LTS support (dropped Node.js 18 due to dependency compatibility)
  - Parallel job execution for frontend CI (Lint/Test/Build/Security)
  - Dependency caching for faster builds (Quicklisp, npm)
  - Path-based workflow triggers for efficiency
  - Manual workflow dispatch support
  - Concurrency control to cancel outdated workflows
  - CI status badges in README
- Comprehensive test suite for error handling system (Phase 1-4)
  - 71 test cases with 196 checks covering all error handling components
  - Condition system tests (validation-error, authentication-error, authorization-error, resource-not-found-error, resource-conflict-error, business-logic-error, system-error)
  - Error response generation tests (make-error-response, validation-error-response, auth-error-response, resource-error-response, business-error-response, system-error-response)
  - Service layer error handling tests (create-post, update-post, publish-draft, unpublish-post validation and business logic errors)
  - Handler integration tests (Condition-to-Response conversion, HTTP status codes, error ID uniqueness, JSON format validation)
  - Edge case tests (empty messages, special characters, Unicode, nil/empty details)
  - All tests passing with 100% success rate
- Advanced error handling and user feedback system (Phase 4)
  - Toast notification system for user-friendly error and success messages
  - Automatic retry functionality for network errors with exponential backoff
  - Error monitoring foundation with structured logging and statistics tracking
  - Enhanced API error handling with AppError class for consistent error management
  - Global error handlers for unhandled Promise rejections and runtime errors
  - Error log export functionality for debugging purposes
- Condition-based error handling system (Phase 2-3)
  - Migrated authentication service from plist-based errors to Condition signals
  - Migrated post service from string-based errors to Condition signals
  - Four Condition types: validation-error, business-logic-error, authentication-error, resource-conflict-error
  - Field-specific validation error reporting (e.g., :field "username")
  - Business logic error codes (e.g., :code :post-already-published)
  - Comprehensive test coverage with Condition type verification (92 test cases)
- Database performance optimization with strategic indexes
  - Added indexes on post table for improved query performance
  - Individual indexes: user_id, status, created_at
  - Composite index: (user_id, status, created_at) for complex queries
  - Database migration script for existing installations
  - Performance verification tools (EXPLAIN ANALYZE)
  - Benchmark tools with test data generation (1000 posts)
  - Verified 10-100x performance improvement for key queries (0.024ms - 0.120ms execution time)
- Test execution script (run-tests.sh) for easy test suite running
  - Normal mode with summary output
  - Verbose mode (-v flag) for full output
  - Automatic test environment setup and cleanup
- Migration to Caveman2 web framework and Mito ORM (Phase 1: Foundation)
  - User model with Mito's `deftable` macro
  - Automatic ID and timestamp generation
  - Connection pooling built into Mito
- Syntax highlighting for code blocks with highlight.js
  - Client-side highlighting with highlight.js v11.9.0
  - Tokyo Night Dark theme for consistent design
  - Support for 200+ programming languages
  - Automatic language detection
- Test data with 10 programming language articles
  - Rust, Python, JavaScript, Go, TypeScript, Java, C#, Swift, Kotlin, Common Lisp
  - Practical code examples with Japanese explanations
- Pagination system for blog posts
  - Offset-based pagination with configurable items per page (default: 10)
  - Smart page number display with ellipsis for large page counts
  - Previous/Next navigation buttons
  - Automatic scroll to top on page change
  - DoS prevention with maximum per_page limit (100)
  - Total items and page count display
  - Responsive mobile design
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
- Automated test suite with FiveAM framework
  - Error handling tests: 71 test cases with 196 checks (100% pass rate)
    - Condition system tests: 31 tests covering all 7 error condition types
    - Error response generation tests: 40 tests covering all response functions
    - Service layer error tests: Validation and business logic error handling
    - Handler integration tests: HTTP status codes, JSON format, error IDs
  - Utility function tests: 97 test cases (100% pass rate)
    - Password hashing, session ID generation, input validation, HTML sanitization tests
  - PostgreSQL-based test database with transaction isolation
  - Test fixtures for database isolation (with-empty-db, with-transaction)
- REST API authentication system (Phase 2: Authentication API)
  - Redis-based session management with 7-day TTL
  - JSON response utilities with standardized format
  - Four authentication endpoints (register, login, logout, me)
  - Cryptographically secure session ID generation (128-bit)
  - HttpOnly and SameSite=Lax cookies for session security
  - Timing attack prevention in authentication flow
  - Input validation for username and password
- REST API post management system (Phase 3.2-3.3: Post API Handlers)
  - Seven REST API endpoints for complete post lifecycle management
  - POST /api/posts - Create new post (draft or published)
  - GET /api/posts - List posts with status filtering (draft/published/all)
  - GET /api/posts/:id - Get post details with ownership-based access control
  - PUT /api/posts/:id - Update post title and content
  - DELETE /api/posts/:id - Delete post
  - PUT /api/posts/:id/publish - Publish draft to public
  - PUT /api/posts/:id/unpublish - Unpublish post back to draft
  - Caveman2 routing configuration with proper path parameter handling
  - Username inclusion in post list and detail responses (Phase 3.3)
- Production build and deployment system (Phase 5: Frontend Integration)
  - Automated build script (build.sh) for one-command frontend deployment
  - Vite production build configuration with code splitting
  - Manual chunk splitting for vendor libraries (React, Axios, React Hook Form)
  - Environment variable support for development and production
  - Static file serving from Caveman2 with proper Content-Type headers
  - SPA routing fallback for React Router client-side navigation
  - Production deployment guide with systemd, Nginx, and SSL configuration

### Changed
- **Improved test reliability and component cleanup:**
  - Added useRef and useEffect cleanup for setTimeout in LoginPage and RegisterPage
  - Proper timer cleanup on component unmount prevents test interference
  - All 36 frontend tests now pass reliably in CI environment
- **Enhanced user experience with modern error feedback (Phase 4):**
  - Toast notifications replace inline error messages for better visibility
  - Automatic retry on transient network failures improves reliability
  - Comprehensive error logging helps with debugging and monitoring
  - Unified error handling across authentication and API layers
- **Improved error handling architecture (Phase 2-3):**
  - Simplified handler code by removing 37 lines of legacy plist error checking
  - Unified service layer return values (success path only)
  - Centralized error handling with handler-case macro
  - Enhanced test robustness with specific Condition type verification
- **Technology Stack Migration (Phase 1):**
  - Replaced Hunchentoot with Caveman2 web framework
  - Replaced Postmodern with Mito ORM
  - Added Clack/Lack middleware layer
  - Replaced Yason with Jonathan for JSON handling
  - Improved connection pooling with Mito's built-in support
- Migrated from server-side colorize to client-side highlight.js for syntax highlighting
  - Disabled 3bmd colorize library completely
  - Cleaner HTML output without inline style spans
  - Better performance with client-side rendering
- Improved session management for better performance
- Enhanced error handling across the application
- Removed modal-based authentication in favor of dedicated pages
- Optimized database queries with JOINs
- Improved database connection management for better flexibility

### Security
- Strengthened input validation and sanitization
- Improved password hashing with PBKDF2 (100,000 iterations)
- Enhanced session security with Redis-based storage and 7-day expiration
- HttpOnly cookies to prevent XSS attacks
- SameSite=Lax cookie attribute for CSRF mitigation
- Timing attack prevention in user authentication
- Protected sensitive files via .gitignore
- SQL injection prevention with parameterized queries
- Fixed HTML sanitization function for proper XSS protection
- Post ownership authorization (Phase 3.2)
  - Users can only modify or delete their own posts
  - Draft posts only accessible to their owner
  - Published posts publicly readable, but only owner can modify
  - Consistent permission checks across all post management endpoints

### Fixed
- Frontend test isolation issues in CI environment
  - Fixed timer cleanup in LoginPage and RegisterPage components
  - Resolved mockNavigate interference between tests
  - All tests now pass consistently in both local and CI environments
- Test suite failures (11 tests fixed, achieving 100% pass rate)
  - Fixed keyword argument usage in session middleware tests
  - Corrected create-test-user function calls to use keyword arguments
  - All 377 test checks now passing successfully
- Session cookie security enhancement
  - Added missing SameSite=Lax attribute to session cookies
  - Improves CSRF attack protection
- Vue directive rendering in Spinneret templates (logout and create post buttons)
- Session timeout issues
- Database connection handling
- Timestamp formatting errors
- Masonry layout rendering
- Caveman2 request handling in authentication handlers (Phase 2)
- Lack response format for proper Woo/Fast-HTTP compatibility (Phase 2)
- Mito accessor functions for auto-generated fields (Phase 2)
- Caveman2 path parameter handling (Phase 3.2)
  - Use &key instead of &rest for path parameters in route definitions
  - Pipe-symbol keywords (:|id|) for proper parameter access in handlers
- Static file path handling in wildcard route (Phase 5)
  - Fixed absolute path creation when merging paths with leading slash
  - Implemented slash stripping for proper relative path resolution
  - Resolved vite.svg and other root-level static files returning HTML instead of correct MIME types

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

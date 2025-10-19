-- Performance Verification Script for Post Table Indexes
-- Run this script to verify that indexes are being used correctly

\echo '=== Post Table Index Verification ==='
\echo ''

\echo '1. List all indexes on post table:'
\d+ post

\echo ''
\echo '=== Query Plan Analysis ==='
\echo ''

\echo '2. Query: Get user posts (should use idx_post_user_id)'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE user_id = 1
ORDER BY created_at DESC;

\echo ''
\echo '3. Query: Get published posts (should use idx_post_status or idx_post_created_at)'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE status = 'published'
ORDER BY created_at DESC;

\echo ''
\echo '4. Query: Get user drafts (should use idx_post_user_status_created - composite index)'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE user_id = 1 AND status = 'draft'
ORDER BY created_at DESC;

\echo ''
\echo '5. Query: Get all posts ordered by date (should use idx_post_created_at)'
EXPLAIN ANALYZE
SELECT * FROM post
ORDER BY created_at DESC
LIMIT 10;

\echo ''
\echo '=== Index Usage Statistics ==='
\echo ''

\echo '6. Table statistics:'
SELECT
    schemaname,
    tablename,
    n_tup_ins AS inserts,
    n_tup_upd AS updates,
    n_tup_del AS deletes,
    n_live_tup AS live_rows,
    n_dead_tup AS dead_rows,
    last_vacuum,
    last_autovacuum,
    last_analyze,
    last_autoanalyze
FROM pg_stat_user_tables
WHERE tablename = 'post';

\echo ''
\echo '7. Index scan statistics:'
SELECT
    schemaname,
    tablename,
    indexname,
    idx_scan AS index_scans,
    idx_tup_read AS tuples_read,
    idx_tup_fetch AS tuples_fetched
FROM pg_stat_user_indexes
WHERE tablename = 'post'
ORDER BY indexname;

\echo ''
\echo '=== Performance Recommendations ==='
\echo ''
\echo 'Expected results:'
\echo '- User posts query should use "Index Scan using idx_post_user_id"'
\echo '- Published posts query should use index scan'
\echo '- User drafts query should use "Index Scan using idx_post_user_status_created"'
\echo '- Ordered posts query should use "Index Scan using idx_post_created_at"'
\echo ''
\echo 'If you see "Seq Scan" instead of "Index Scan", the query planner'
\echo 'chose a full table scan. This is normal for small tables (<1000 rows).'
\echo ''

-- Database Index Performance Benchmark
-- Run this after benchmark-setup.sql to measure index effectiveness

\timing on

\echo '========================================='
\echo 'Database Index Performance Benchmark'
\echo '========================================='
\echo ''

-- Warm up PostgreSQL cache
SELECT COUNT(*) FROM post;

\echo ''
\echo '=== Test 1: Get User Posts (idx_post_user_id) ==='
\echo 'Query: SELECT * FROM post WHERE user_id = ? ORDER BY created_at DESC LIMIT 20'
\echo ''

-- Get a sample user_id
SELECT id INTO TEMPORARY TABLE sample_user FROM "user" WHERE username = 'benchuser100' LIMIT 1;

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE user_id = (SELECT id FROM sample_user)
ORDER BY created_at DESC
LIMIT 20;

\echo ''
\echo '=== Test 2: Get Published Posts (idx_post_status + idx_post_created_at) ==='
\echo 'Query: SELECT * FROM post WHERE status = ''published'' ORDER BY created_at DESC LIMIT 20'
\echo ''

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE status = 'published'
ORDER BY created_at DESC
LIMIT 20;

\echo ''
\echo '=== Test 3: Get User Drafts (idx_post_user_status_created - Composite) ==='
\echo 'Query: SELECT * FROM post WHERE user_id = ? AND status = ''draft'' ORDER BY created_at DESC'
\echo ''

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT * FROM post
WHERE user_id = (SELECT id FROM sample_user)
  AND status = 'draft'
ORDER BY created_at DESC;

\echo ''
\echo '=== Test 4: Chronological Listing (idx_post_created_at) ==='
\echo 'Query: SELECT * FROM post ORDER BY created_at DESC LIMIT 50'
\echo ''

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT * FROM post
ORDER BY created_at DESC
LIMIT 50;

\echo ''
\echo '=== Test 5: Count by Status (idx_post_status) ==='
\echo 'Query: SELECT status, COUNT(*) FROM post GROUP BY status'
\echo ''

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT status, COUNT(*) as count
FROM post
GROUP BY status;

\echo ''
\echo '=== Test 6: User Post Count (idx_post_user_id) ==='
\echo 'Query: SELECT user_id, COUNT(*) FROM post GROUP BY user_id'
\echo ''

\echo 'Execution plan:'
EXPLAIN ANALYZE
SELECT user_id, COUNT(*) as post_count
FROM post
GROUP BY user_id
ORDER BY post_count DESC
LIMIT 10;

\echo ''
\echo '=== Index Usage Statistics ==='
\echo ''

SELECT
    schemaname,
    tablename,
    indexname,
    idx_scan as scans,
    idx_tup_read as tuples_read,
    idx_tup_fetch as tuples_fetched,
    CASE
        WHEN idx_scan > 0 THEN ROUND((idx_tup_fetch::numeric / idx_scan), 2)
        ELSE 0
    END as avg_tuples_per_scan
FROM pg_stat_user_indexes
WHERE tablename = 'post'
ORDER BY idx_scan DESC;

\echo ''
\echo '=== Table Statistics ==='
\echo ''

SELECT
    n_tup_ins as inserts,
    n_tup_upd as updates,
    n_tup_del as deletes,
    n_live_tup as live_rows,
    n_dead_tup as dead_rows,
    ROUND(100.0 * n_dead_tup / NULLIF(n_live_tup, 0), 2) as dead_row_pct,
    last_analyze,
    last_autoanalyze
FROM pg_stat_user_tables
WHERE tablename = 'post';

\echo ''
\echo '=== Index Sizes ==='
\echo ''

SELECT
    indexname,
    pg_size_pretty(pg_relation_size(indexname::regclass)) as index_size
FROM pg_indexes
WHERE tablename = 'post'
ORDER BY pg_relation_size(indexname::regclass) DESC;

\echo ''
\echo '=== Performance Summary ==='
\echo ''
\echo 'Expected Results:'
\echo '- Index Scan usage: Should see "Index Scan using idx_post_..." in plans above'
\echo '- Execution time: Should be <10ms for all queries with 1000 rows'
\echo '- Index efficiency: idx_scan > 0 indicates index is being used'
\echo ''
\echo 'Key Metrics to Watch:'
\echo '- "Seq Scan" = Full table scan (slow for large tables)'
\echo '- "Index Scan" = Using index (fast)'
\echo '- "Index Only Scan" = Using index without table access (fastest)'
\echo '- "Bitmap Index Scan" = Using multiple indexes (also fast)'
\echo ''

\timing off

\echo '========================================='
\echo 'Benchmark Complete'
\echo '========================================='

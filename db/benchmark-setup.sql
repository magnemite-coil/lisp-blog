-- Benchmark Setup: Generate test data for index performance testing
-- This script creates sample data to demonstrate index effectiveness

\echo '=== Benchmark Setup ==='
\echo ''

\echo 'Current post count:'
SELECT COUNT(*) as current_posts FROM post;

\echo ''
\echo 'Generating test data...'
\echo 'Note: This will create 1000 sample posts for performance testing'
\echo ''

-- Create test users if they don't exist (user_id 100-109)
DO $$
DECLARE
    i INTEGER;
    username_val VARCHAR(50);
    password_hash VARCHAR(255);
BEGIN
    password_hash := '$pbkdf2-sha256$i=100000,l=32$D7rMJLXtPxmwVqjU2psAAA$5TlFPKLW8Tn0vRXLXJKJgcLhJ5N9z3gT9sGJhHgLXJI'; -- 'password123'

    FOR i IN 100..109 LOOP
        username_val := 'benchuser' || i;

        INSERT INTO "user" (username, password, created_at, updated_at)
        VALUES (username_val, password_hash, NOW(), NOW())
        ON CONFLICT (username) DO NOTHING;
    END LOOP;

    RAISE NOTICE 'Created 10 test users (benchuser100-109)';
END $$;

-- Generate 1000 test posts with various statuses and users
DO $$
DECLARE
    i INTEGER;
    user_id_val BIGINT;
    status_val VARCHAR(20);
    title_val VARCHAR(255);
    content_val TEXT;
    created_offset INTERVAL;
BEGIN
    FOR i IN 1..1000 LOOP
        -- Rotate through test users (100-109)
        SELECT id INTO user_id_val FROM "user" WHERE username = 'benchuser' || (100 + (i % 10));

        -- 70% published, 30% draft (realistic distribution)
        IF i % 10 < 7 THEN
            status_val := 'published';
        ELSE
            status_val := 'draft';
        END IF;

        title_val := 'Benchmark Post ' || i || ': Performance Testing';
        content_val := 'This is test content for post ' || i || '. ' ||
                      'Lorem ipsum dolor sit amet, consectetur adipiscing elit. ' ||
                      'Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ' ||
                      'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris. ' ||
                      REPEAT('Test data padding. ', 10);

        -- Spread posts across last 365 days
        created_offset := (i || ' hours')::INTERVAL;

        INSERT INTO post (user_id, title, content, status, created_at, updated_at)
        VALUES (
            user_id_val,
            title_val,
            content_val,
            status_val,
            NOW() - created_offset,
            NOW() - created_offset
        );

        -- Progress indicator every 100 posts
        IF i % 100 = 0 THEN
            RAISE NOTICE 'Generated % posts...', i;
        END IF;
    END LOOP;

    RAISE NOTICE 'Successfully generated 1000 test posts';
END $$;

\echo ''
\echo 'Updating database statistics...'
ANALYZE post;
ANALYZE "user";

\echo ''
\echo 'Final post count:'
SELECT COUNT(*) as total_posts FROM post;

\echo ''
\echo 'Post distribution by status:'
SELECT status, COUNT(*) as count FROM post GROUP BY status ORDER BY status;

\echo ''
\echo 'Post distribution by user (top 10):'
SELECT u.username, COUNT(p.id) as post_count
FROM post p
JOIN "user" u ON p.user_id = u.id
GROUP BY u.username
ORDER BY post_count DESC
LIMIT 10;

\echo ''
\echo '=== Benchmark setup complete ==='
\echo 'You can now run db/benchmark.sql to measure index performance'

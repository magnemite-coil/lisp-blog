-- Migration: Add indexes to post table for performance optimization
-- Date: 2025-10-19
-- Description: Adds indexes on user_id, status, created_at columns and a composite index

-- Individual indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_post_user_id ON post(user_id);
CREATE INDEX IF NOT EXISTS idx_post_status ON post(status);
CREATE INDEX IF NOT EXISTS idx_post_created_at ON post(created_at DESC);

-- Composite index for complex queries (user_id, status, created_at)
-- This covers queries like: WHERE user_id = ? AND status = ? ORDER BY created_at DESC
CREATE INDEX IF NOT EXISTS idx_post_user_status_created ON post(user_id, status, created_at DESC);

-- Update table statistics for query planner
ANALYZE post;

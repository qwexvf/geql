-- Create posts table
CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    content TEXT,
    author_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
    published BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for better query performance
CREATE INDEX idx_posts_author_id ON posts(author_id);
CREATE INDEX idx_posts_published ON posts(published);

-- Insert sample posts
INSERT INTO posts (title, content, author_id, published) VALUES
    ('Getting Started with GraphQL', 'GraphQL is a query language for APIs...', 1, TRUE),
    ('Advanced Gleam Patterns', 'Gleam offers powerful pattern matching...', 1, TRUE),
    ('Database Design Tips', 'When designing your database schema...', 2, FALSE),
    ('Web Development with Wisp', 'Wisp is a fantastic web framework...', 2, TRUE),
    ('Functional Programming Benefits', 'Functional programming offers many advantages...', 4, TRUE);
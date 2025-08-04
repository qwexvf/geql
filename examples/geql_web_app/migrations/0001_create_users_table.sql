-- Create users table
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create index for faster email lookups
CREATE INDEX idx_users_email ON users(email);

-- Insert sample data
INSERT INTO users (name, email, active) VALUES
    ('John Doe', 'john@example.com', TRUE),
    ('Jane Smith', 'jane@example.com', TRUE),
    ('Bob Johnson', 'bob@example.com', FALSE),
    ('Alice Brown', 'alice@example.com', TRUE);
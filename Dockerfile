# Multi-stage Dockerfile for Elm Image Categorizer
# Build stage
FROM node:18-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    curl \
    bash

WORKDIR /app

# Copy package files first for better caching
COPY package*.json ./
COPY elm.json ./

# Install npm dependencies
RUN npm ci --only=production --no-audit

# Install Elm globally
RUN npm install -g elm@0.19.1 elm-test@0.19.1-revision16

# Copy source files
COPY src/ ./src/
COPY tests/ ./tests/
COPY scripts/ ./scripts/

# Create dist directory
RUN mkdir -p dist

# Build the Elm application
RUN npx elm make src/Main.elm --output=dist/main.js --optimize

# Production stage
FROM nginx:alpine AS production

# Install Node.js for environment variable injection
RUN apk add --no-cache nodejs npm

# Create non-root user
RUN addgroup -g 1001 -S appgroup && \
    adduser -S appuser -u 1001 -G appgroup

# Create app directory and set ownership
RUN mkdir -p /app && \
    chown -R appuser:appgroup /app

WORKDIR /app

# Copy built application from builder stage
COPY --from=builder /app/dist/main.js ./dist/
COPY --from=builder /app/node_modules/thumbhash/thumbhash.js ./node_modules/thumbhash/

# Copy static files
COPY src/index.html ./src/

# Create production HTML template and server script
COPY <<'EOF' ./production-server.js
#!/usr/bin/env node

const http = require('http');
const fs = require('fs');
const path = require('path');
const url = require('url');

const PORT = process.env.PORT || 8000;

// Read the HTML template
let htmlTemplate = '';
try {
  htmlTemplate = fs.readFileSync('/app/src/index.html', 'utf8');
} catch (error) {
  console.error('Failed to read HTML template:', error);
  process.exit(1);
}

function injectEnvironmentVariables(html) {
  const immichApiKey = process.env.IMMICH_API_KEY || '';
  const immichUrl = process.env.IMMICH_URL || 'https://localhost';
  
  // Validate required environment variables
  if (!immichApiKey) {
    console.warn('WARNING: IMMICH_API_KEY not set');
  }
  
  console.log(`Environment: IMMICH_URL=${immichUrl}, IMMICH_API_KEY=${immichApiKey ? '[SET]' : '[NOT SET]'}`);

  const envScript = `
    <script type="text/javascript">
      window.ENV = {
        IMMICH_API_KEY: '${immichApiKey}',
        IMMICH_URL: '${immichUrl}'
      };

      // Storage API with encryption support
      let encryptionKey = null;

      async function getEncryptionKey() {
        if (encryptionKey) return encryptionKey;
        
        const stored = sessionStorage.getItem('encKey');
        if (stored) {
          encryptionKey = await window.crypto.subtle.importKey(
            'raw',
            new Uint8Array(JSON.parse(stored)),
            { name: 'AES-GCM' },
            false,
            ['encrypt', 'decrypt']
          );
          return encryptionKey;
        }

        encryptionKey = await window.crypto.subtle.generateKey(
          { name: 'AES-GCM', length: 256 },
          true,
          ['encrypt', 'decrypt']
        );

        const exported = await window.crypto.subtle.exportKey('raw', encryptionKey);
        sessionStorage.setItem('encKey', JSON.stringify(Array.from(new Uint8Array(exported))));
        
        return encryptionKey;
      }

      async function encryptText(text) {
        if (!text) return text;
        
        const key = await getEncryptionKey();
        const iv = window.crypto.getRandomValues(new Uint8Array(12));
        const encoder = new TextEncoder();
        
        const encrypted = await window.crypto.subtle.encrypt(
          { name: 'AES-GCM', iv: iv },
          key,
          encoder.encode(text)
        );
        
        const combined = new Uint8Array(iv.length + encrypted.byteLength);
        combined.set(iv);
        combined.set(new Uint8Array(encrypted), iv.length);
        
        return btoa(String.fromCharCode(...combined));
      }

      async function decryptText(encryptedText) {
        if (!encryptedText) return encryptedText;
        
        try {
          const key = await getEncryptionKey();
          const combined = new Uint8Array(atob(encryptedText).split('').map(c => c.charCodeAt(0)));
          
          const iv = combined.slice(0, 12);
          const encrypted = combined.slice(12);
          
          const decrypted = await window.crypto.subtle.decrypt(
            { name: 'AES-GCM', iv: iv },
            key,
            encrypted
          );
          
          const decoder = new TextDecoder();
          return decoder.decode(decrypted);
        } catch (error) {
          return encryptedText;
        }
      }

      window.storageAPI = {
        save: async function(key, value) {
          try {
            let finalValue = value;
            
            if (key === 'immichApiKey' && value) {
              finalValue = await encryptText(value);
            }
            
            localStorage.setItem(key, finalValue);
          } catch (error) {
            console.warn('Storage save failed:', error);
          }
        },

        load: async function(key) {
          try {
            const value = localStorage.getItem(key);
            if (!value) return null;
            
            if (key === 'immichApiKey') {
              return await decryptText(value);
            }
            
            return value;
          } catch (error) {
            console.warn('Storage load failed:', error);
            return null;
          }
        },

        clear: function() {
          try {
            localStorage.removeItem('immichApiUrl');
            localStorage.removeItem('immichApiKey');
            sessionStorage.removeItem('encKey');
            encryptionKey = null;
          } catch (error) {
            console.warn('Storage clear failed:', error);
          }
        }
      };
    </script>`;

  return html.replace('<head>', `<head>${envScript}`);
}

const server = http.createServer((req, res) => {
  const parsedUrl = url.parse(req.url, true);
  const pathname = parsedUrl.pathname;

  // Add security headers
  res.setHeader('X-Frame-Options', 'DENY');
  res.setHeader('X-Content-Type-Options', 'nosniff');
  res.setHeader('X-XSS-Protection', '1; mode=block');
  res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');
  res.setHeader('Content-Security-Policy', "default-src 'self' 'unsafe-inline' 'unsafe-eval' data: blob:; connect-src 'self' https:; img-src 'self' data: blob:;");

  if (pathname === '/' || pathname === '/index.html') {
    const html = injectEnvironmentVariables(htmlTemplate);
    res.writeHead(200, {
      'Content-Type': 'text/html',
      'Cache-Control': 'no-cache, no-store, must-revalidate',
      'Pragma': 'no-cache',
      'Expires': '0'
    });
    res.end(html);
  } else if (pathname === '/dist/main.js') {
    try {
      const js = fs.readFileSync('/app/dist/main.js');
      res.writeHead(200, {
        'Content-Type': 'application/javascript',
        'Cache-Control': 'public, max-age=3600'
      });
      res.end(js);
    } catch (error) {
      res.writeHead(404);
      res.end('Not Found');
    }
  } else if (pathname === '/node_modules/thumbhash/thumbhash.js') {
    try {
      const js = fs.readFileSync('/app/node_modules/thumbhash/thumbhash.js');
      res.writeHead(200, {
        'Content-Type': 'application/javascript',
        'Cache-Control': 'public, max-age=3600'
      });
      res.end(js);
    } catch (error) {
      res.writeHead(404);
      res.end('Not Found');
    }
  } else if (pathname === '/health') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ status: 'healthy', timestamp: new Date().toISOString() }));
  } else {
    res.writeHead(404);
    res.end('Not Found');
  }
});

server.listen(PORT, '0.0.0.0', () => {
  console.log(`Image Categorizer server running on port ${PORT}`);
  console.log(`Health check available at http://localhost:${PORT}/health`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down gracefully');
  server.close(() => {
    console.log('Server closed');
    process.exit(0);
  });
});

process.on('SIGINT', () => {
  console.log('SIGINT received, shutting down gracefully');
  server.close(() => {
    console.log('Server closed');
    process.exit(0);
  });
});
EOF

# Make server script executable
RUN chmod +x production-server.js

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:8000/health || exit 1

# Start the production server
CMD ["node", "production-server.js"]
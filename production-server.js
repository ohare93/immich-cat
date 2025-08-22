#!/usr/bin/env node
/**
 * Production server for Elm Image Categorizer
 * Serves static files with environment variable injection
 */

const http = require('http');
const fs = require('fs');
const path = require('path');
const url = require('url');

const PORT = process.env.PORT || 8000;

// Read the HTML template
let htmlTemplate = '';
try {
  // Try different possible paths for the HTML template
  const possiblePaths = [
    path.join(__dirname, 'src', 'index.html'),
    path.join(__dirname, 'index.html'),
    '/app/src/index.html'
  ];
  
  for (const htmlPath of possiblePaths) {
    try {
      htmlTemplate = fs.readFileSync(htmlPath, 'utf8');
      console.log(`Loaded HTML template from: ${htmlPath}`);
      break;
    } catch (e) {
      // Continue to next path
    }
  }
  
  if (!htmlTemplate) {
    throw new Error('Could not find HTML template in any expected location');
  }
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
        IMMICH_API_KEY: '${immichApiKey.replace(/'/g, "\\'")}',
        IMMICH_URL: '${immichUrl.replace(/'/g, "\\'")}'
      };

      // Storage API with encryption support
      let encryptionKey = null;

      async function getEncryptionKey() {
        if (encryptionKey) return encryptionKey;
        
        const stored = sessionStorage.getItem('encKey');
        if (stored) {
          try {
            encryptionKey = await window.crypto.subtle.importKey(
              'raw',
              new Uint8Array(JSON.parse(stored)),
              { name: 'AES-GCM' },
              false,
              ['encrypt', 'decrypt']
            );
            return encryptionKey;
          } catch (error) {
            console.warn('Failed to import stored encryption key:', error);
            sessionStorage.removeItem('encKey');
          }
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
        
        try {
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
        } catch (error) {
          console.warn('Encryption failed:', error);
          return text;
        }
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
          console.warn('Decryption failed, returning original text:', error);
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

function findAssetFile(relativePath) {
  const possibleBasePaths = [
    __dirname,
    '/app',
    process.cwd()
  ];
  
  for (const basePath of possibleBasePaths) {
    const fullPath = path.join(basePath, relativePath);
    if (fs.existsSync(fullPath)) {
      return fullPath;
    }
  }
  return null;
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
    const jsFile = findAssetFile('dist/main.js');
    if (jsFile) {
      try {
        const js = fs.readFileSync(jsFile);
        res.writeHead(200, {
          'Content-Type': 'application/javascript',
          'Cache-Control': 'public, max-age=3600',
          'Content-Encoding': 'identity'
        });
        res.end(js);
      } catch (error) {
        console.error('Error reading main.js:', error);
        res.writeHead(500);
        res.end('Internal Server Error');
      }
    } else {
      console.error('Could not find dist/main.js');
      res.writeHead(404);
      res.end('Not Found');
    }
  } else if (pathname === '/node_modules/thumbhash/thumbhash.js') {
    const jsFile = findAssetFile('node_modules/thumbhash/thumbhash.js');
    if (jsFile) {
      try {
        const js = fs.readFileSync(jsFile);
        res.writeHead(200, {
          'Content-Type': 'application/javascript',
          'Cache-Control': 'public, max-age=3600'
        });
        res.end(js);
      } catch (error) {
        console.error('Error reading thumbhash.js:', error);
        res.writeHead(500);
        res.end('Internal Server Error');
      }
    } else {
      console.error('Could not find thumbhash.js');
      res.writeHead(404);
      res.end('Not Found');
    }
  } else if (pathname === '/health') {
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({ 
      status: 'healthy', 
      timestamp: new Date().toISOString(),
      version: process.env.npm_package_version || '0.1.0',
      environment: {
        nodeVersion: process.version,
        immichUrl: process.env.IMMICH_URL || 'not-set',
        hasApiKey: !!(process.env.IMMICH_API_KEY)
      }
    }));
  } else {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end('Not Found');
  }
});

server.listen(PORT, '0.0.0.0', () => {
  console.log(`🚀 Image Categorizer server running on port ${PORT}`);
  console.log(`📊 Health check available at http://localhost:${PORT}/health`);
  console.log(`🌐 Environment: ${process.env.NODE_ENV || 'development'}`);
  
  // Log configuration for debugging
  if (process.env.NODE_ENV !== 'production') {
    console.log(`🔧 Configuration:`);
    console.log(`   IMMICH_URL: ${process.env.IMMICH_URL || 'not set'}`);
    console.log(`   IMMICH_API_KEY: ${process.env.IMMICH_API_KEY ? '[SET]' : '[NOT SET]'}`);
  }
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

// Handle uncaught exceptions
process.on('uncaughtException', (error) => {
  console.error('Uncaught Exception:', error);
  process.exit(1);
});

process.on('unhandledRejection', (reason, promise) => {
  console.error('Unhandled Rejection at:', promise, 'reason:', reason);
  process.exit(1);
});
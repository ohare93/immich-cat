#!/usr/bin/env node

const http = require('http');
const url = require('url');
const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

require('dotenv').config();

const DEV_SERVER_PORT = 8000;
const ELM_LIVE_PORT = 8001;

// Generate or use provided encryption key
let ENCRYPTION_KEY = process.env.ENCRYPTION_KEY;
if (!ENCRYPTION_KEY) {
  // Auto-generate a random key if none provided
  ENCRYPTION_KEY = require('crypto').randomBytes(32).toString('hex');
  console.log('⚠️  No ENCRYPTION_KEY environment variable provided.');
  console.log('📋 Generated key:', ENCRYPTION_KEY);
  console.log('💡 Set ENCRYPTION_KEY=' + ENCRYPTION_KEY + ' to persist across restarts');
}

function startElmLive() {
  const elmLive = spawn('npx', [
    'elm-live',
    'src/Main.elm',
    '--host=0.0.0.0',
    '--port=' + ELM_LIVE_PORT,
    '--start-page=src/index.html',
    '--',
    '--debug',
    '--output=dist/main.js'
  ], {
    stdio: 'inherit',
    cwd: path.join(__dirname, '..')
  });

  elmLive.on('error', (err) => {
    process.exit(1);
  });

  elmLive.on('exit', (code) => {
    process.exit(code);
  });

  return elmLive;
}

function getHtmlWithEnvVars() {
  const htmlPath = path.join(__dirname, '..', 'src', 'index.html');
  
  try {
    let htmlTemplate = fs.readFileSync(htmlPath, 'utf8');

    const envScript = `
    <script type="text/javascript">
      window.ENV = {
        IMMICH_API_KEY: '${process.env.IMMICH_API_KEY || ''}',
        IMMICH_URL: '${process.env.IMMICH_URL || 'https://localhost/api'}',
        ENCRYPTION_KEY: '${ENCRYPTION_KEY}'
      };

      let encryptionKey = null;

      async function getEncryptionKey() {
        if (encryptionKey) return encryptionKey;
        
        // Pad or truncate the key to exactly 32 bytes for AES-256
        const keyMaterial = (window.ENV.ENCRYPTION_KEY + '0'.repeat(64)).slice(0, 64);
        const keyBytes = new Uint8Array(32);
        for (let i = 0; i < 32; i++) {
          keyBytes[i] = parseInt(keyMaterial.slice(i * 2, i * 2 + 2), 16) || 0;
        }
        
        encryptionKey = await window.crypto.subtle.importKey(
          'raw',
          keyBytes,
          'AES-GCM',
          false,
          ['encrypt', 'decrypt']
        );
        
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

      try {
        window.storageAPI = {
        save: async function(key, value) {
          try {
            let finalValue = value;
            
            if (key === 'immichApiKey' && value) {
              finalValue = await encryptText(value);
            }
            
            localStorage.setItem(key, finalValue);
          } catch (error) {
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
            return null;
          }
        },

        clear: function() {
          try {
            localStorage.removeItem('immichApiUrl');
            localStorage.removeItem('immichApiKey');
            encryptionKey = null;
          } catch (error) {
          }
        }
      };
      } catch (error) {
      }
    </script>`;

    htmlTemplate = htmlTemplate.replace('<head>', `<head>${envScript}`);
    
    return htmlTemplate;
  } catch (error) {
    console.error('Error processing HTML template:', error);
    throw error;
  }
}

function proxyToElmLive(req, res) {
  const options = {
    hostname: 'localhost',
    port: ELM_LIVE_PORT,
    path: req.url,
    method: req.method,
    headers: req.headers
  };

  const proxy = http.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res, { end: true });
  });

  proxy.on('error', (err) => {
    console.error('Proxy error:', err);
    res.writeHead(500);
    res.end('Proxy error: ' + err.message);
  });

  req.pipe(proxy, { end: true });
}

function createDevServer() {
  const server = http.createServer((req, res) => {
    const parsedUrl = url.parse(req.url, true);
    const pathname = parsedUrl.pathname;

    if (pathname === '/' || pathname === '/index.html') {
      try {
        const htmlContent = getHtmlWithEnvVars();
        res.writeHead(200, {
          'Content-Type': 'text/html',
          'Cache-Control': 'no-cache, no-store, must-revalidate',
          'Pragma': 'no-cache',
          'Expires': '0'
        });
        res.end(htmlContent);
      } catch (err) {
        res.writeHead(500);
        res.end('Error loading page');
      }
    } 
    else {
      proxyToElmLive(req, res);
    }
  });

  return server;
}

function main() {

  const elmLiveProcess = startElmLive();

  setTimeout(() => {
    const server = createDevServer();
    
    server.listen(DEV_SERVER_PORT, '0.0.0.0', () => {
    });

    server.on('error', (err) => {
      elmLiveProcess.kill();
      process.exit(1);
    });

    process.on('SIGINT', () => {
      server.close(() => {
        elmLiveProcess.kill();
        process.exit(0);
      });
    });

  }, 2000);
}

main();
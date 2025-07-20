#!/usr/bin/env node

const http = require('http');
const url = require('url');
const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

// Load environment variables from .env file
require('dotenv').config();

const DEV_SERVER_PORT = 8000;
const ELM_LIVE_PORT = 8001;

// Start elm-live on a different port
function startElmLive() {
  console.log('Starting elm-live on port', ELM_LIVE_PORT);
  const elmLive = spawn('npx', [
    'elm-live',
    'src/Main.elm',
    '--host=0.0.0.0',
    '--port=' + ELM_LIVE_PORT,
    '--',
    '--debug',
    '--output=dist/main.js'
  ], {
    stdio: 'inherit',
    cwd: path.join(__dirname, '..')
  });

  elmLive.on('error', (err) => {
    console.error('Failed to start elm-live:', err);
    process.exit(1);
  });

  elmLive.on('exit', (code) => {
    console.log(`elm-live exited with code ${code}`);
    process.exit(code);
  });

  return elmLive;
}

// Read HTML file and inject environment variables in memory
function getHtmlWithEnvVars() {
  const htmlPath = path.join(__dirname, '..', 'src', 'index.html');
  let htmlTemplate = fs.readFileSync(htmlPath, 'utf8');

  // Create environment script that will be injected
  const envScript = `
    <script type="text/javascript">
      // Secure runtime environment variable injection (never written to files)
      window.ENV = {
        IMMICH_API_KEY: '${process.env.IMMICH_API_KEY || ''}',
        IMMICH_URL: '${process.env.IMMICH_URL || 'https://localhost/api'}'
      };

      // Encryption key management (stored in sessionStorage, cleared on browser close)
      let encryptionKey = null;

      // Generate or retrieve encryption key
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

        // Generate new key
        encryptionKey = await window.crypto.subtle.generateKey(
          { name: 'AES-GCM', length: 256 },
          true,
          ['encrypt', 'decrypt']
        );

        // Store key in sessionStorage
        const exported = await window.crypto.subtle.exportKey('raw', encryptionKey);
        sessionStorage.setItem('encKey', JSON.stringify(Array.from(new Uint8Array(exported))));
        
        return encryptionKey;
      }

      // Encrypt text using AES-GCM
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
        
        // Combine IV and encrypted data
        const combined = new Uint8Array(iv.length + encrypted.byteLength);
        combined.set(iv);
        combined.set(new Uint8Array(encrypted), iv.length);
        
        // Return as base64
        return btoa(String.fromCharCode(...combined));
      }

      // Decrypt text using AES-GCM
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
          console.error('Decryption failed:', error);
          return encryptedText; // Return original if decryption fails
        }
      }

      // localStorage interface functions for Elm ports
      console.log('Creating storageAPI...');
      try {
        window.storageAPI = {
        save: async function(key, value) {
          try {
            let finalValue = value;
            
            // Encrypt API keys
            if (key === 'immichApiKey' && value) {
              finalValue = await encryptText(value);
            }
            
            localStorage.setItem(key, finalValue);
            console.log('Saved to localStorage:', key);
          } catch (error) {
            console.error('Failed to save to localStorage:', error);
          }
        },

        load: async function(key) {
          try {
            const value = localStorage.getItem(key);
            if (!value) return null;
            
            // Decrypt API keys
            if (key === 'immichApiKey') {
              return await decryptText(value);
            }
            
            return value;
          } catch (error) {
            console.error('Failed to load from localStorage:', error);
            return null;
          }
        },

        clear: function() {
          try {
            localStorage.removeItem('immichApiUrl');
            localStorage.removeItem('immichApiKey');
            sessionStorage.removeItem('encKey');
            encryptionKey = null;
            console.log('Cleared localStorage configuration');
          } catch (error) {
            console.error('Failed to clear localStorage:', error);
          }
        }
      };
      console.log('storageAPI created successfully');
      } catch (error) {
        console.error('Error creating storageAPI:', error);
      }
    </script>`;

  // Insert the environment script at the very beginning of <head> to ensure it loads first
  htmlTemplate = htmlTemplate.replace('<head>', `<head>${envScript}`);
  
  return htmlTemplate;
}

// Proxy requests to elm-live
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
    res.end('Proxy error');
  });

  req.pipe(proxy, { end: true });
}

// Create custom development server
function createDevServer() {
  const server = http.createServer((req, res) => {
    const parsedUrl = url.parse(req.url, true);
    const pathname = parsedUrl.pathname;

    // Serve main HTML page with environment variables injected in-memory
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
        console.error('Error serving HTML:', err);
        res.writeHead(500);
        res.end('Error loading page');
      }
    } 
    // Proxy all other requests to elm-live
    else {
      proxyToElmLive(req, res);
    }
  });

  return server;
}

// Main function
function main() {
  console.log('🔒 Secure Development Server - Environment variables stay in memory only');
  console.log('Environment variables loaded:');
  console.log('  IMMICH_API_KEY:', process.env.IMMICH_API_KEY ? (process.env.IMMICH_API_KEY.substring(0, 5) + '...') : 'NOT SET');
  console.log('  IMMICH_URL:', process.env.IMMICH_URL || '[DEFAULT]');
  console.log('');

  // Start elm-live
  const elmLiveProcess = startElmLive();

  // Wait a moment for elm-live to start, then start our proxy server
  setTimeout(() => {
    const server = createDevServer();
    
    server.listen(DEV_SERVER_PORT, '0.0.0.0', () => {
      console.log('');
      console.log('✅ Secure development server running at:');
      console.log(`   http://localhost:${DEV_SERVER_PORT}/`);
      console.log('');
      console.log('🔒 Security: Environment variables are injected in-memory only');
      console.log('💡 Hot reloading: Powered by elm-live');
      console.log('');
    });

    server.on('error', (err) => {
      console.error('Server error:', err);
      elmLiveProcess.kill();
      process.exit(1);
    });

    // Handle graceful shutdown
    process.on('SIGINT', () => {
      console.log('\nShutting down development server...');
      server.close(() => {
        elmLiveProcess.kill();
        process.exit(0);
      });
    });

  }, 2000); // Give elm-live 2 seconds to start
}

main();
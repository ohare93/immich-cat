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
#!/usr/bin/env node

const http = require('http');
const url = require('url');
const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

require('dotenv').config();

const DEV_SERVER_PORT = 8000;
const ELM_LIVE_PORT = 8001;

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
        IMMICH_URL: '${process.env.IMMICH_URL || 'https://localhost/api'}'
      };

      window.storageAPI = {
        save: function(key, value) {
          try {
            localStorage.setItem(key, value);
          } catch (error) {
          }
        },
        load: function(key) {
          try {
            return localStorage.getItem(key);
          } catch (error) {
            return null;
          }
        },
        clear: function() {
          try {
            localStorage.removeItem('immichApiUrl');
            localStorage.removeItem('immichApiKey');
          } catch (error) {
          }
        }
      };
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
#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');


const devServer = spawn('node', [path.join(__dirname, 'dev-server.js')], {
  stdio: 'inherit',
  cwd: path.join(__dirname, '..')
});

devServer.on('error', (err) => {
  process.exit(1);
});

devServer.on('exit', (code) => {
  process.exit(code);
});
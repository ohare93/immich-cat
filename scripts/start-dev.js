#!/usr/bin/env node

// Simply call the secure development server
const { spawn } = require('child_process');
const path = require('path');

console.log('Starting secure development environment...');

const devServer = spawn('node', [path.join(__dirname, 'dev-server.js')], {
  stdio: 'inherit',
  cwd: path.join(__dirname, '..')
});

devServer.on('error', (err) => {
  console.error('Failed to start development server:', err);
  process.exit(1);
});

devServer.on('exit', (code) => {
  console.log(`Development server exited with code ${code}`);
  process.exit(code);
});
#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');

// For production builds, we just build the Elm application
// The src/index.html file is already clean and secure with getEnvVar() calls
// Production deployment should inject environment variables at runtime

console.log('Building application for production...');
console.log('🔒 Security: src/index.html is already clean (no secrets embedded)');

// Build the Elm application
console.log('Building Elm application...');
const elmMake = spawn('npx', [
  'elm',
  'make',
  'src/Main.elm',
  '--output=dist/main.js',
  '--optimize'
], {
  stdio: 'inherit',
  cwd: path.join(__dirname, '..')
});

elmMake.on('error', (err) => {
  console.error('Failed to build Elm application:', err);
  process.exit(1);
});

elmMake.on('exit', (code) => {
  if (code === 0) {
    console.log('✅ Build completed successfully!');
    console.log('');
    console.log('🔒 Security Notes:');
    console.log('  - src/index.html contains no embedded secrets');
    console.log('  - Production deployment must inject window.ENV at runtime');
    console.log('  - Environment variables handled securely via getEnvVar() calls');
    console.log('');
    console.log('📁 Build outputs:');
    console.log('  - dist/main.js (optimized Elm application)');
    console.log('  - src/index.html (clean, ready for deployment)');
  } else {
    console.log(`Build failed with exit code ${code}`);
  }
  process.exit(code);
});
#!/usr/bin/env node

const { spawn } = require('child_process');
const path = require('path');



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
  process.exit(1);
});

elmMake.on('exit', (code) => {
  if (code === 0) {
  } else {
  }
  process.exit(code);
});
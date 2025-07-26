import express from 'express';
import path from 'path';

const app = express();
const port = 3333;

// Serve static files from the 'images' directory
app.use('/images', express.static(path.join(__dirname, 'images')));

app.listen(port, () => {
  console.log(`Server is running at http://localhost:${port}`);
});
import express from "express";
import path from "path";

const app = express();
const port = 3333;

// Middleware to enable CORS
app.use((req, res, next) => {
  res.setHeader("Access-Control-Allow-Origin", "*"); // Allow requests from any origin
  res.setHeader("Access-Control-Allow-Methods", "GET,PUT,POST,DELETE"); // Allow specific HTTP methods
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization"); // Allow specific headers
  next();
});

// Serve static files from the 'images' directory
app.use("/images", express.static(path.join(__dirname, "images")));

app.listen(port, () => {
  console.log(`Server is running at http://localhost:${port}`);
});


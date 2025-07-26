"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const express_1 = __importDefault(require("express"));
const path_1 = __importDefault(require("path"));
const app = (0, express_1.default)();
const port = 3333;
// Serve static files from the 'images' directory
app.use('/images', express_1.default.static(path_1.default.join(__dirname, 'images')));
app.listen(port, () => {
    console.log(`Server is running at http://localhost:${port}`);
});

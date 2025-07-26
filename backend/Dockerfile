# Use the official Node.js image as the base image
FROM node:14

# Create and set the working directory
WORKDIR /app

# Copy package.json and package-lock.json
COPY package.json package-lock.json tsconfig.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application code
COPY ./src ./src

# Expose the port the app runs on
EXPOSE 3333

# Command to run the application
CMD ["npm", "run", "serve"]
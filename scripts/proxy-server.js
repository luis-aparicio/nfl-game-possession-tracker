const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const path = require('path');
const fs = require('fs');
require('dotenv').config();

const app = express();
const PORT = 8000;

// Proxy API requests to Sportradar
app.use('/api', createProxyMiddleware({
  target: 'https://api.sportradar.us',
  changeOrigin: true,
  pathRewrite: {
    '^/api': '', // Remove /api prefix when forwarding
  },
  onProxyReq: (proxyReq, req, res) => {
    console.log(`Proxying: ${req.method} ${req.url} -> https://api.sportradar.us${req.url.replace('/api', '')}`);
  },
  onError: (err, req, res) => {
    console.error('Proxy error:', err.message);
    res.status(500).json({ error: 'Proxy error', message: err.message });
  }
}));

// Serve static files from dist directory (except index.html which we handle specially)
app.use(express.static(path.join(__dirname, '../dist'), {
  index: false // Don't serve index.html automatically
}));

// Inject config from .env into index.html when serving it
app.get('/', (req, res) => {
  const htmlPath = path.join(__dirname, '../dist/index.html');
  let htmlContent = fs.readFileSync(htmlPath, 'utf8');
  
  // Read config from .env (already loaded via dotenv.config())
  const apiKey = process.env.API_KEY || '';
  const testGameId = process.env.TEST_GAME_ID || '7d06369a-382a-448a-b295-6da9eab53245';
  const superBowlGameId = process.env.SUPER_BOWL_GAME_ID || 'ca9d8f84-8e7b-4ee7-a310-54c2e3ca4edc';
  const testMode = process.env.TEST_MODE || 'false';
  
  const configScript = `
    <script>
        // Configuration injected from .env file
        window.__APP_CONFIG__ = {
            API_KEY: ${JSON.stringify(apiKey)},
            TEST_GAME_ID: ${JSON.stringify(testGameId)},
            SUPER_BOWL_GAME_ID: ${JSON.stringify(superBowlGameId)},
            TEST_MODE: ${JSON.stringify(testMode)}
        };
    </script>`;
  
  // Replace or insert the config script
  if (htmlContent.includes('window.__APP_CONFIG__')) {
    htmlContent = htmlContent.replace(
      /<script>[\s\S]*?window\.__APP_CONFIG__[\s\S]*?<\/script>/,
      configScript.trim()
    );
  } else {
    // Insert before the module script if config doesn't exist
    htmlContent = htmlContent.replace(
      /(<script type="module")/,
      configScript.trim() + '\n    $1'
    );
  }
  
  res.send(htmlContent);
});

// Fallback to index.html for SPA routing (must be last)
app.use((req, res) => {
  // Only serve index.html for non-API routes
  if (!req.path.startsWith('/api')) {
    const htmlPath = path.join(__dirname, '../dist/index.html');
    let htmlContent = fs.readFileSync(htmlPath, 'utf8');
    
    // Inject config from .env
    const apiKey = process.env.API_KEY || '';
    const testGameId = process.env.TEST_GAME_ID || '7d06369a-382a-448a-b295-6da9eab53245';
    const superBowlGameId = process.env.SUPER_BOWL_GAME_ID || 'ca9d8f84-8e7b-4ee7-a310-54c2e3ca4edc';
    const testMode = process.env.TEST_MODE || 'false';
    
    const configScript = `
    <script>
        // Configuration injected from .env file
        window.__APP_CONFIG__ = {
            API_KEY: ${JSON.stringify(apiKey)},
            TEST_GAME_ID: ${JSON.stringify(testGameId)},
            SUPER_BOWL_GAME_ID: ${JSON.stringify(superBowlGameId)},
            TEST_MODE: ${JSON.stringify(testMode)}
        };
    </script>`;
    
    if (htmlContent.includes('window.__APP_CONFIG__')) {
      htmlContent = htmlContent.replace(
        /<script>[\s\S]*?window\.__APP_CONFIG__[\s\S]*?<\/script>/,
        configScript.trim()
      );
    } else {
      htmlContent = htmlContent.replace(
        /(<script type="module")/,
        configScript.trim() + '\n    $1'
      );
    }
    
    res.send(htmlContent);
  } else {
    res.status(404).json({ error: 'Not found' });
  }
});

app.listen(PORT, () => {
  console.log(`🚀 Server running at http://localhost:${PORT}`);
  console.log(`📡 API proxy: http://localhost:${PORT}/api/* -> https://api.sportradar.us/*`);
});

@echo off
echo Starting Fraud Detection Web Server...
echo.
echo Frontend: http://localhost:8000
echo API: http://localhost:8001
echo.

# Start the backend API (if plumber works)
Rscript -e "plumber::plumb('backend_server.R')$run(port=8001)" &

# Serve the frontend (using Python simple HTTP server)
python -m http.server 8000

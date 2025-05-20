#!/usr/bin/env python3
"""
Simple HTTP server to serve the AST visualizer
"""
import os
import sys
import http.server
import socketserver
import webbrowser
from urllib.parse import urlparse

class CustomHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET')
        self.send_header('Cache-Control', 'no-store, no-cache, must-revalidate')
        return super().end_headers()

def start_server(port=8000, directory='.'):
    """Start a simple HTTP server"""
    handler = CustomHTTPRequestHandler
    
    # Change to the specified directory
    os.chdir(directory)
    
    # Create the server
    with socketserver.TCPServer(("", port), handler) as httpd:
        print(f"Server started at http://localhost:{port}")
        print(f"Serving files from {os.path.abspath(directory)}")
        print("Press Ctrl+C to stop the server")
        
        # Open the visualizer in the default web browser
        webbrowser.open(f"http://localhost:{port}/cobol_ast_viewer_simple.html")
        
        # Start the server
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nServer stopped.")

if __name__ == "__main__":
    # Get port from command line arguments or use default
    port = int(sys.argv[1]) if len(sys.argv) > 1 and sys.argv[1].isdigit() else 8000
    
    # Start the server
    start_server(port)
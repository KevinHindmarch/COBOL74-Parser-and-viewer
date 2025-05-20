# COBOL 74 AST Parser and Visualizer

A tool for parsing COBOL 74 files, generating Abstract Syntax Trees (AST), and visualizing program structure.

Files from here: https://github.com/KevinHindmarch/COBOL74-Banking

## Core Components

### Main Parser (`cobol_parser.py`)
- Parses COBOL 74 files and generates an AST
- Identifies program structure: divisions, sections, paragraphs, statements, and data items
- Outputs the AST as JSON for further processing

### Testing Tool (`test_cobol_parser.py`)
- Processes multiple COBOL files in a directory
- Generates AST JSON files for each input file
- Reports success/failure statistics

### Analysis Tools
- `view_all_ast.py` - Summarizes all AST files in a directory
- `visualize_ast.py` - Displays the AST in a tree format
- `compare_ast.py` - Compares two AST files to identify similarities and differences

## Visualization Components

### AST to React Converter (`ast_to_react.py`)
- Converts AST JSON files to a format optimized for React visualization
- Creates an index file listing all available programs
- Saves converted files in a `react_data` directory

### Interactive Visualizers
- **Simple Tree Visualizer** (`cobol_ast_viewer_simple.html`)
  - Displays program structure as an expandable/collapsible tree
  - Color-codes different node types
  - Shows detailed information when clicking on a node

- **Advanced D3 Visualizer** (`cobol_ast_viewer.html`)
  - Uses D3.js for graphical tree representation
  - Supports zooming and panning for large programs
  - Visually distinguishes different node types with colors

### Web Server (`start_server.py`)
- Serves HTML files and JSON data
- Opens the visualizer automatically in your default browser
- Handles CORS and caching headers

## Usage

### Basic Parsing
```
# Parse a single file
python cobol_parser.py path/to/file.CBL74

# Parse all files in a directory
python test_cobol_parser.py [directory]
```

### Analysis
```
# View summary of all AST files
python view_all_ast.py [directory]

# View details of a specific AST file
python view_all_ast.py path/to/file.ast.json

# Compare two AST files
python compare_ast.py file1.ast.json file2.ast.json
```

### Visualization
```
# Convert AST files to React format
python ast_to_react.py

# Start the web server and open visualizer
python start_server.py
```

## AST Structure
- Program
  - Divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
    - Sections
      - Paragraphs (in PROCEDURE division)
        - Statements
      - Data items (in DATA division)

## Limitations
- Handles basic COBOL 74 syntax but may not support all language features
- Complex expressions are not fully parsed into separate nodes
- Copy statements are not expanded
- Conditional expressions are treated as simple text arguments

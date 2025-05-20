#!/usr/bin/env python3
"""
Test script for COBOL 74 AST Parser
"""
import os
import sys
import json
from cobol_ast_parser import CobolAstGenerator

def test_parser(directory='.', pattern='*.CBL74'):
    """Test the parser on files in the directory"""
    import glob
    
    # Find all COBOL 74 files
    search_pattern = os.path.join(directory, pattern)
    files = glob.glob(search_pattern)
    
    if not files:
        print(f"No files found matching pattern {search_pattern}")
        return
    
    print(f"Found {len(files)} files to parse")
    
    # Create generator
    generator = CobolAstGenerator()
    
    # Process each file
    success_count = 0
    for file_path in files:
        print(f"\nParsing {os.path.basename(file_path)}...")
        try:
            ast = generator.generate_ast(file_path)
            print(f"  Success! Program name: {ast.name if hasattr(ast, 'name') and ast.name else 'Unknown'}")
            
            # Print divisions found
            if hasattr(ast, 'divisions'):
                print(f"  Divisions found: {', '.join(ast.divisions.keys())}")
            
            # Save AST to JSON file for inspection
            output_file = f"{os.path.splitext(file_path)[0]}.ast.json"
            with open(output_file, 'w') as f:
                json.dump(ast, f, default=lambda o: o.__dict__, indent=2)
            print(f"  AST saved to {output_file}")
            
            success_count += 1
        except Exception as e:
            print(f"  Error parsing {file_path}: {str(e)}")
    
    print(f"\nSummary: Successfully parsed {success_count} out of {len(files)} files")

if __name__ == "__main__":
    # Use directory from command line or current directory
    directory = sys.argv[1] if len(sys.argv) > 1 else '.'
    test_parser(directory)
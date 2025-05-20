#!/usr/bin/env python3
"""
Visualize COBOL AST as a tree structure
"""
import os
import sys
import json

def print_tree(node, indent=0, prefix=""):
    """Print a tree representation of the AST"""
    if isinstance(node, dict):
        node_type = node.get('type', 'Unknown')
        node_name = node.get('name', '')
        
        # Print the current node
        if node_name:
            print(f"{' ' * indent}{prefix}{node_type}: {node_name}")
        else:
            print(f"{' ' * indent}{prefix}{node_type}")
        
        # Process divisions
        if 'divisions' in node and isinstance(node['divisions'], dict):
            for name, division in sorted(node['divisions'].items()):
                print(f"{' ' * (indent+2)}└── Division: {name}")
                for section_name, section in sorted(division.get('sections', {}).items()):
                    print(f"{' ' * (indent+4)}└── Section: {section_name}")
                    
                    # Process paragraphs
                    for para_name, paragraph in sorted(section.get('paragraphs', {}).items()):
                        print(f"{' ' * (indent+6)}└── Paragraph: {para_name}")
                        
                        # Process statements
                        for statement in paragraph.get('statements', []):
                            verb = statement.get('verb', '')
                            args = statement.get('arguments', [])
                            if args:
                                print(f"{' ' * (indent+8)}├── {verb}: {args[0][:50]}")
                            else:
                                print(f"{' ' * (indent+8)}├── {verb}")
                    
                    # Process data entries
                    for entry in section.get('entries', []):
                        level = entry.get('level', '')
                        name = entry.get('name', '')
                        pic = entry.get('picture', '')
                        if pic:
                            print(f"{' ' * (indent+6)}├── {level:02d} {name} PIC {pic}")
                        else:
                            print(f"{' ' * (indent+6)}├── {level:02d} {name}")

def visualize_ast(file_path):
    """Visualize AST from a JSON file"""
    if not file_path.endswith('.json'):
        print(f"Error: {file_path} is not a JSON file")
        return
    
    try:
        with open(file_path, 'r') as f:
            ast = json.load(f)
        
        # Print the tree
        print(f"\nAST Tree for {os.path.basename(file_path)}:")
        print("=" * 50)
        print(f"Program: {ast.get('name', 'Unknown')}")
        print(f"Divisions: {', '.join(ast.get('divisions', {}).keys())}")
        print("-" * 50)
        print_tree(ast)
        print("=" * 50)
    except Exception as e:
        print(f"Error visualizing AST: {str(e)}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python visualize_ast.py <ast_json_file>")
        sys.exit(1)
    
    visualize_ast(sys.argv[1])
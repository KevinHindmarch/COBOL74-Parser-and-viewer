#!/usr/bin/env python3
"""
Convert COBOL AST JSON files to a format suitable for React visualization
"""
import os
import sys
import json
import glob

def convert_ast_to_react_format(ast):
    """Convert AST to a format suitable for React visualization"""
    # Start with the program as the root node
    root = {
        "name": ast.get("name", "Unknown Program"),
        "attributes": {
            "type": "Program"
        },
        "children": []
    }
    
    # Add divisions as children
    for div_name, division in ast.get("divisions", {}).items():
        div_node = {
            "name": div_name,
            "attributes": {
                "type": "Division"
            },
            "children": []
        }
        
        # Add sections as children of divisions
        for section_name, section in division.get("sections", {}).items():
            section_node = {
                "name": section_name,
                "attributes": {
                    "type": "Section"
                },
                "children": []
            }
            
            # Add paragraphs as children of sections
            for para_name, paragraph in section.get("paragraphs", {}).items():
                para_node = {
                    "name": para_name,
                    "attributes": {
                        "type": "Paragraph",
                        "statements": len(paragraph.get("statements", []))
                    },
                    "children": []
                }
                
                # Add statements as children of paragraphs
                for statement in paragraph.get("statements", []):
                    verb = statement.get("verb", "Unknown")
                    args = statement.get("arguments", [])
                    
                    stmt_node = {
                        "name": verb,
                        "attributes": {
                            "type": "Statement",
                            "arguments": args[0][:50] + "..." if args and len(args[0]) > 50 else args[0] if args else ""
                        }
                    }
                    
                    para_node["children"].append(stmt_node)
                
                section_node["children"].append(para_node)
            
            # Add data items as children of sections (for DATA division)
            for entry in section.get("entries", []):
                level = entry.get("level", "")
                name = entry.get("name", "")
                pic = entry.get("picture", "")
                
                item_node = {
                    "name": name,
                    "attributes": {
                        "type": "DataItem",
                        "level": level,
                        "picture": pic
                    }
                }
                
                section_node["children"].append(item_node)
            
            div_node["children"].append(section_node)
        
        root["children"].append(div_node)
    
    return root

def process_ast_files(directory='.', pattern='*.ast.json'):
    """Process all AST files in the directory"""
    # Find all AST JSON files
    search_pattern = os.path.join(directory, pattern)
    files = glob.glob(search_pattern)
    
    if not files:
        print(f"No AST files found matching pattern {search_pattern}")
        return
    
    print(f"Found {len(files)} AST files")
    
    # Create output directory for React-compatible JSON files
    react_dir = os.path.join(directory, "react_data")
    if not os.path.exists(react_dir):
        os.makedirs(react_dir)
    
    # Process each file
    all_programs = []
    for file_path in files:
        try:
            with open(file_path, 'r') as f:
                ast = json.load(f)
            
            # Convert to React format
            react_data = convert_ast_to_react_format(ast)
            
            # Save as individual file
            base_name = os.path.basename(file_path)
            output_file = os.path.join(react_dir, f"{os.path.splitext(base_name)[0]}.react.json")
            
            with open(output_file, 'w') as f:
                json.dump(react_data, f, indent=2)
            
            print(f"Converted {base_name} to {os.path.basename(output_file)}")
            
            # Add to list of all programs
            all_programs.append({
                "name": ast.get("name", "Unknown"),
                "file": os.path.basename(output_file)
            })
            
        except Exception as e:
            print(f"Error processing {file_path}: {str(e)}")
    
    # Create index file with list of all programs
    index_file = os.path.join(react_dir, "index.json")
    with open(index_file, 'w') as f:
        json.dump(all_programs, f, indent=2)
    
    print(f"Created index file with {len(all_programs)} programs")
    print(f"React-compatible JSON files saved to {react_dir}")

if __name__ == "__main__":
    directory = sys.argv[1] if len(sys.argv) > 1 else '.'
    process_ast_files(directory)
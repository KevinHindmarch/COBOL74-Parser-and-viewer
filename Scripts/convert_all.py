#!/usr/bin/env python3
"""
Convert all COBOL files in a directory to COBOL-74 format
"""

import os
import sys
from cobol74_converter import Cobol74Converter

def convert_directory(directory):
    """Convert all .cbl files in a directory to COBOL-74 format"""
    converter = Cobol74Converter()
    converted_count = 0
    skipped_count = 0
    
    # Get all .cbl files in the directory
    for filename in os.listdir(directory):
        if filename.lower().endswith('.cbl'):
            input_path = os.path.join(directory, filename)
            output_path = os.path.join(directory, os.path.splitext(filename)[0] + '.CBL74')
            
            # Skip if the output file already exists
            if os.path.exists(output_path):
                print(f"Skipping {filename} - {os.path.basename(output_path)} already exists")
                skipped_count += 1
                continue
                
            try:
                print(f"Converting {filename}...")
                converter.convert_file(input_path, output_path)
                converted_count += 1
            except Exception as e:
                print(f"Error converting {filename}: {str(e)}")
    
    print(f"\nConversion complete: {converted_count} files converted, {skipped_count} files skipped")

def main():
    """Main function"""
    if len(sys.argv) < 2:
        print("Usage: python convert_all.py <directory>")
        sys.exit(1)
        
    directory = sys.argv[1]
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory")
        sys.exit(1)
        
    convert_directory(directory)

if __name__ == "__main__":
    main()
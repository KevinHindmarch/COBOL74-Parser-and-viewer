#!/usr/bin/env python3
"""
COBOL to COBOL-74 Converter

This script converts modern COBOL code to COBOL-74 standard format.
"""

import re
import sys
import os


class Cobol74Converter:
    """Converts modern COBOL code to COBOL-74 standard"""

    def __init__(self):
        self.in_procedure_division = False
        self.current_if_level = 0
        self.if_stack = []

    def convert_file(self, input_file, output_file=None):
        """Convert a COBOL file to COBOL-74 format"""
        if output_file is None:
            base, ext = os.path.splitext(input_file)
            output_file = f"{base}.CBL74"

        with open(input_file, 'r') as f:
            content = f.read()

        converted = self.convert_content(content)

        with open(output_file, 'w') as f:
            f.write(converted)

        return output_file

    def convert_content(self, content):
        """Convert COBOL content to COBOL-74 format"""
        lines = content.splitlines()
        converted_lines = []

        for i, line in enumerate(lines):
            # Process each line
            converted_line = self._process_line(line, i, lines)
            converted_lines.append(converted_line)

        return '\n'.join(converted_lines)

    def _process_line(self, line, line_idx, all_lines):
        """Process a single line of COBOL code"""
        # Skip empty lines
        if not line.strip():
            return line

        # Check if we're entering the PROCEDURE DIVISION
        if re.search(r'^\s*PROCEDURE\s+DIVISION', line, re.IGNORECASE):
            self.in_procedure_division = True

        # Handle PROGRAM-ID format
        if re.search(r'^\s*PROGRAM-ID\s*\.\s*$', line, re.IGNORECASE):
            next_line = all_lines[line_idx + 1] if line_idx + 1 < len(all_lines) else ""
            if next_line.strip():
                # Combine PROGRAM-ID with the next line
                program_name = next_line.strip().rstrip('.')
                return re.sub(r'(PROGRAM-ID\s*\.)\s*$', f"PROGRAM-ID. {program_name}.", line, flags=re.IGNORECASE)

        # Handle DATE-WRITTEN format
        if re.search(r'^\s*DATE-WRITTEN\s*\.\s*$', line, re.IGNORECASE):
            next_line = all_lines[line_idx + 1] if line_idx + 1 < len(all_lines) else ""
            if next_line.strip():
                # Combine DATE-WRITTEN with the next line
                date = next_line.strip().rstrip('.')
                return re.sub(r'(DATE-WRITTEN\s*\.)\s*$', f"DATE-WRITTEN. {date}.", line, flags=re.IGNORECASE)

        # Handle DATE-COMPILED format
        if re.search(r'^\s*DATE-COMPILED\s*\.\s*$', line, re.IGNORECASE):
            next_line = all_lines[line_idx + 1] if line_idx + 1 < len(all_lines) else ""
            if next_line.strip():
                # Combine DATE-COMPILED with the next line
                date = next_line.strip().rstrip('.')
                return re.sub(r'(DATE-COMPILED\s*\.)\s*$', f"DATE-COMPILED. {date}.", line, flags=re.IGNORECASE)

        # Skip processing if not in PROCEDURE DIVISION
        if not self.in_procedure_division:
            return line

        # Handle END-IF statements
        if re.search(r'^\s*END-IF\s*', line, re.IGNORECASE):
            # Remove END-IF statements
            return ""

        # Handle CONTINUE statements
        if re.search(r'^\s*CONTINUE\s*', line, re.IGNORECASE):
            # Replace CONTINUE with NEXT SENTENCE
            return re.sub(r'CONTINUE', "NEXT SENTENCE", line, flags=re.IGNORECASE)

        # Handle EXIT statements
        if re.search(r'^\s*EXIT\s*\.\s*$', line, re.IGNORECASE):
            # Remove standalone EXIT statements
            return ""

        # Handle PERFORM UNTIL
        if re.search(r'^\s*PERFORM\s+UNTIL\s+', line, re.IGNORECASE):
            # Convert "PERFORM UNTIL condition" to "PERFORM paragraph UNTIL condition"
            match = re.search(r'^\s*PERFORM\s+UNTIL\s+(.+)$', line, re.IGNORECASE)
            if match:
                condition = match.group(1)
                # Find the next line that contains PERFORM
                for j in range(line_idx + 1, len(all_lines)):
                    if re.search(r'^\s*PERFORM\s+\w', all_lines[j], re.IGNORECASE):
                        perform_match = re.search(r'^\s*PERFORM\s+(\w[\w-]*)', all_lines[j], re.IGNORECASE)
                        if perform_match:
                            paragraph = perform_match.group(1)
                            return f"      PERFORM {paragraph}\n          UNTIL {condition}"
                        break
            
        # Add periods to statements that should have them
        if self.in_procedure_division:
            # Check if the line contains a statement that should end with a period
            statement_patterns = [
                r'^\s*DISPLAY\s+.+',
                r'^\s*MOVE\s+.+\s+TO\s+.+',
                r'^\s*ADD\s+.+\s+TO\s+.+',
                r'^\s*SUBTRACT\s+.+\s+FROM\s+.+',
                r'^\s*MULTIPLY\s+.+\s+BY\s+.+',
                r'^\s*DIVIDE\s+.+\s+INTO\s+.+',
                r'^\s*COMPUTE\s+.+\s*=\s*.+',
                r'^\s*OPEN\s+.+',
                r'^\s*CLOSE\s+.+',
                r'^\s*READ\s+.+',
                r'^\s*WRITE\s+.+',
                r'^\s*REWRITE\s+.+',
                r'^\s*DELETE\s+.+',
                r'^\s*CALL\s+.+'
            ]
            
            for pattern in statement_patterns:
                if re.search(pattern, line, re.IGNORECASE) and not line.rstrip().endswith('.'):
                    # Check if this is the last line of a statement
                    next_line = all_lines[line_idx + 1] if line_idx + 1 < len(all_lines) else ""
                    if not next_line.strip() or re.search(r'^\s*(?:ELSE|END-IF|WHEN|END-EVALUATE|END-PERFORM)', next_line, re.IGNORECASE):
                        line = line.rstrip() + '.'
                    
        return line


def main():
    """Main function to run the converter from command line"""
    if len(sys.argv) < 2:
        print("Usage: python cobol74_converter.py <cobol_file> [output_file]")
        sys.exit(1)
        
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    converter = Cobol74Converter()
    output_path = converter.convert_file(input_file, output_file)
    
    print(f"Converted {input_file} to COBOL-74 format: {output_path}")


if __name__ == "__main__":
    main()
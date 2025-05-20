#!/usr/bin/env python3
"""
COBOL 74 Parser - Generates Abstract Syntax Tree from COBOL 74 files
"""
import os
import re
import json
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Optional, Any, Union

@dataclass
class Position:
    """Position in source code"""
    line: int
    column: int

@dataclass
class Node:
    """Base AST node"""
    type: str
    position: Optional[Position] = None

@dataclass
class Program(Node):
    """Program node"""
    name: str = ""
    divisions: Dict[str, Any] = field(default_factory=dict)

@dataclass
class Division(Node):
    """Division node"""
    name: str
    sections: Dict[str, Any] = field(default_factory=dict)

@dataclass
class Section(Node):
    """Section node"""
    name: str
    paragraphs: Dict[str, Any] = field(default_factory=dict)
    entries: List[Any] = field(default_factory=list)

@dataclass
class Paragraph(Node):
    """Paragraph node"""
    name: str
    statements: List[Any] = field(default_factory=list)

@dataclass
class Statement(Node):
    """Statement node"""
    verb: str
    arguments: List[Any] = field(default_factory=list)

@dataclass
class DataItem(Node):
    """Data item node"""
    level: int
    name: str
    picture: Optional[str] = None
    usage: Optional[str] = None
    value: Optional[str] = None
    redefines: Optional[str] = None
    children: List[Any] = field(default_factory=list)

class CobolParser:
    """Parser for COBOL 74 code"""
    
    def __init__(self):
        self.current_line = 0
        self.lines = []
        self.program = None
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
    
    def parse_file(self, filename):
        """Parse a COBOL file and return AST"""
        with open(filename, 'r', encoding='utf-8') as file:
            content = file.read()
        return self.parse(content, filename)
    
    def parse(self, content, filename=""):
        """Parse COBOL content and return AST"""
        self.lines = content.splitlines()
        self.current_line = 0
        self.program = Program(type="Program")
        
        # Extract program name from PROGRAM-ID
        for line in self.lines:
            if "PROGRAM-ID" in line:
                match = re.search(r'PROGRAM-ID\.\s*([A-Z0-9-]+)', line, re.IGNORECASE)
                if match:
                    self.program.name = match.group(1)
                break
        
        while self.current_line < len(self.lines):
            line = self.lines[self.current_line].strip()
            self.current_line += 1
            
            # Skip comments and empty lines
            if not line or line.startswith('*'):
                continue
                
            # Handle divisions
            if re.match(r'^\s*[A-Z-]+\s+DIVISION\s*\.$', line, re.IGNORECASE):
                self._parse_division(line)
            
            # Handle sections
            elif re.match(r'^\s*[A-Z-]+\s+SECTION\s*\.$', line, re.IGNORECASE):
                self._parse_section(line)
            
            # Handle paragraphs in procedure division
            elif self.current_division and self.current_division.name == "PROCEDURE" and re.match(r'^\s*[A-Z0-9-]+\s*\.$', line, re.IGNORECASE):
                self._parse_paragraph(line)
            
            # Handle data items
            elif self.current_division and self.current_division.name == "DATA" and re.match(r'^\s*\d+\s+', line):
                self._parse_data_item(line)
            
            # Handle statements in procedure division
            elif self.current_division and self.current_division.name == "PROCEDURE" and self.current_paragraph:
                self._parse_statement(line)
        
        return self.program
    
    def _parse_division(self, line):
        """Parse a division line"""
        division_name = re.match(r'^\s*([A-Z-]+)\s+DIVISION\s*\.$', line, re.IGNORECASE).group(1)
        self.current_division = Division(type="Division", name=division_name)
        self.program.divisions[division_name] = self.current_division
        self.current_section = None
        self.current_paragraph = None
    
    def _parse_section(self, line):
        """Parse a section line"""
        if not self.current_division:
            return
            
        section_name = re.match(r'^\s*([A-Z-]+)\s+SECTION\s*\.$', line, re.IGNORECASE).group(1)
        self.current_section = Section(type="Section", name=section_name)
        self.current_division.sections[section_name] = self.current_section
        self.current_paragraph = None
    
    def _parse_paragraph(self, line):
        """Parse a paragraph line"""
        if not self.current_division:
            return
            
        paragraph_name = re.match(r'^\s*([A-Z0-9-]+)\s*\.$', line, re.IGNORECASE).group(1)
        self.current_paragraph = Paragraph(type="Paragraph", name=paragraph_name)
        
        if self.current_section:
            self.current_section.paragraphs[paragraph_name] = self.current_paragraph
        else:
            # Handle paragraphs not in a section
            if "PARAGRAPHS" not in self.current_division.sections:
                self.current_division.sections["PARAGRAPHS"] = Section(type="Section", name="PARAGRAPHS")
            self.current_division.sections["PARAGRAPHS"].paragraphs[paragraph_name] = self.current_paragraph
    
    def _parse_data_item(self, line):
        """Parse a data item line"""
        if not self.current_division or not self.current_section:
            return
            
        match = re.match(r'^\s*(\d+)\s+([A-Z0-9-]+)(.*)$', line, re.IGNORECASE)
        if not match:
            return
            
        level = int(match.group(1))
        name = match.group(2)
        rest = match.group(3).strip()
        
        item = DataItem(type="DataItem", level=level, name=name)
        
        # Parse PIC clause
        pic_match = re.search(r'PIC\s+([X9\(\)]+)', rest, re.IGNORECASE)
        if pic_match:
            item.picture = pic_match.group(1)
        
        # Parse VALUE clause
        value_match = re.search(r'VALUE\s+[\'"]?([^\'"]*)[\'""]?', rest, re.IGNORECASE)
        if value_match:
            item.value = value_match.group(1)
        
        # Parse REDEFINES clause
        redefines_match = re.search(r'REDEFINES\s+([A-Z0-9-]+)', rest, re.IGNORECASE)
        if redefines_match:
            item.redefines = redefines_match.group(1)
        
        self.current_section.entries.append(item)
    
    def _parse_statement(self, line):
        """Parse a statement line"""
        if not self.current_paragraph:
            return
            
        # Simple statement parsing - just get the verb and the rest as arguments
        match = re.match(r'^\s*([A-Z-]+)\s*(.*)$', line, re.IGNORECASE)
        if not match:
            return
            
        verb = match.group(1)
        args = match.group(2).strip()
        
        statement = Statement(type="Statement", verb=verb)
        if args:
            statement.arguments.append(args)
        
        self.current_paragraph.statements.append(statement)

class CobolAstGenerator:
    """Generate AST from COBOL 74 files"""
    
    def __init__(self):
        self.parser = CobolParser()
    
    def generate_ast(self, file_path):
        """Generate AST from a single file"""
        return self.parser.parse_file(file_path)
    
    def generate_ast_from_files(self, file_paths):
        """Generate AST from multiple files"""
        results = {}
        for file_path in file_paths:
            try:
                ast = self.generate_ast(file_path)
                results[file_path] = ast
            except Exception as e:
                results[file_path] = {"error": str(e)}
        return results

def main():
    """Main function"""
    import sys
    import glob
    
    if len(sys.argv) < 2:
        print("Usage: python cobol_ast_parser.py <file_or_pattern>")
        sys.exit(1)
    
    generator = CobolAstGenerator()
    
    # Handle file patterns
    files = []
    for pattern in sys.argv[1:]:
        matched_files = glob.glob(pattern)
        if matched_files:
            files.extend(matched_files)
        else:
            files.append(pattern)
    
    # Generate AST for each file
    for file_path in files:
        try:
            ast = generator.generate_ast(file_path)
            print(f"Parsed {file_path}: Program {ast.name}")
            print(f"Divisions: {', '.join(ast.divisions.keys())}")
        except Exception as e:
            print(f"Error parsing {file_path}: {str(e)}")

if __name__ == "__main__":
    main()
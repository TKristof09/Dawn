#!/usr/bin/python3
import re
import subprocess
import sys
from collections import defaultdict

def run_menhir_compare(parser, new_messages, old_messages):
    cmd = ["menhir", parser, "--compare-errors", new_messages, "--compare-errors", old_messages]
    result = subprocess.run(cmd, capture_output=True, text=True)
    return result.stderr

def parse_error_locations(stderr_output):
    parse = re.compile(r'File "([^"]+)", line (\d+).*?\nError: this sentence.*?\nNo sentence that leads to this state exists in "([^"]+)".', re.DOTALL)
    matches = parse.findall(stderr_output)
    
    # Group updates by source file
    updates_by_file = defaultdict(list)
    for new_file, line_no, _ in matches:
        updates_by_file[new_file].append(int(line_no))
    return updates_by_file

def extract_new_messages(filename, line_numbers):
    messages = {}
    
    with open(filename) as f:
        lines = f.readlines()
        
    for target_line_no in line_numbers:
        current_message = []
        found_message = False
        
        for i in range(target_line_no - 1, len(lines)):
            if not found_message:
                current_message.extend([lines[i]])
                found_message = True
            elif lines[i].startswith("##"):
                current_message.append(lines[i])
            else:
                current_message.append("TODO: PARSER MESSAGE NEEDED HERE.\n")
                messages[target_line_no] = current_message
                break
                
        if target_line_no not in messages:
            current_message.append("TODO: PARSER MESSAGE NEEDED HERE.\n")
            messages[target_line_no] = current_message
    
    return messages

def update_messages(target_file_lines, updates_by_file):
    lines = target_file_lines.copy()
    
    for new_file, line_numbers in updates_by_file.items():
        # Sort updates by line number in descending order
        line_numbers.sort(reverse=True)
        messages = extract_new_messages(new_file, line_numbers)
        
        # Insert messages at appropriate positions
        for line_no in line_numbers:
            if line_no in messages:
                pos = line_no - 1
                lines[pos:pos] = messages[line_no]
    
    return lines

def main():
    if len(sys.argv) != 4:
        print("Usage: script.py parser new_messages old_messages", file=sys.stderr)
        sys.exit(1)
        
    parser, new_messages, old_messages = sys.argv[1:4]
    
    # Read target file
    with open(old_messages) as f:
        target_lines = f.readlines()
    
    # Run menhir comparison
    stderr_output = run_menhir_compare(parser, new_messages, old_messages)
    
    # Parse error locations
    updates_by_file = parse_error_locations(stderr_output)
    
    # Generate updated content
    updated_lines = update_messages(target_lines, updates_by_file)
    
    # Print to stdout without any additional output
    sys.stdout.writelines(updated_lines)

if __name__ == "__main__":
    main()

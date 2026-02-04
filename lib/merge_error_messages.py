#!/usr/bin/python3
import re
import subprocess
import sys

def run_menhir_compare(parser, new_messages, old_messages):
    cmd = ["menhir", parser, "--compare-errors", new_messages, "--compare-errors", old_messages]
    result = subprocess.run(cmd, capture_output=True, text=True)
    return result.stderr

def parse_missing_states(stderr_output):
    """Extract state numbers that are missing from old_messages"""
    pattern = re.compile(r'Error: this sentence leads to an error in state (\d+)\.')
    matches = pattern.findall(stderr_output)
    return set(int(state) for state in matches)

def parse_all_messages(filename):
    """Parse all error message entries, indexed by state number"""
    messages_by_state = {}
    
    with open(filename) as f:
        content = f.read()
    
    # Split into individual error entries
    # Each entry starts with "prog:" and continues until the next "prog:" or end
    entries = re.split(r'\n(?=prog: )', content)
    
    for entry in entries:
        if not entry.strip():
            continue
            
        # Extract state number from this entry
        state_match = re.search(r'## Ends in an error in state: (\d+)\.', entry)
        if state_match:
            state_num = int(state_match.group(1))
            # Ensure entry ends with newline
            if not entry.endswith('\n'):
                entry += '\n'
            messages_by_state[state_num] = entry
    
    return messages_by_state

def main():
    if len(sys.argv) != 4:
        print("Usage: script.py parser new_messages old_messages", file=sys.stderr)
        sys.exit(1)
        
    parser, new_messages, old_messages = sys.argv[1:4]
    
    # Run menhir comparison to find missing states
    stderr_output = run_menhir_compare(parser, new_messages, old_messages)
    missing_states = parse_missing_states(stderr_output)
    
    if not missing_states:
        # No missing states, just output the old messages
        with open(old_messages) as f:
            sys.stdout.write(f.read())
        return
    
    # Parse all messages from both files
    complete_messages = parse_all_messages(new_messages)
    old_messages_dict = parse_all_messages(old_messages)
    
    # Merge: old messages + new messages for missing states
    merged = {}
    for state, msg in old_messages_dict.items():
        merged[state] = msg
    
    for state in missing_states:
        if state in complete_messages:
            # Add the new message with TODO placeholder
            entry = complete_messages[state]
            # Replace any existing message with TODO
            entry = re.sub(r'\n[^\n#][^\n]*$', 'TODO: PARSER MESSAGE NEEDED HERE.\n', entry)
            merged[state] = entry
    
    # Output in sorted order by state number
    for state in sorted(merged.keys()):
        sys.stdout.write(merged[state])
        sys.stdout.write('\n')  # Add blank line between entries

if __name__ == "__main__":
    main()

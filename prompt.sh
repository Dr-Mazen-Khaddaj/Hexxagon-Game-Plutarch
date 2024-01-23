#!/bin/bash

FILE="./prompt.sh"
LINE="# This file is found in the Hexxagon-Game-Plutarch folder, And this is a unique SHA256 hash: 6c00c6dadbc909ac132279f33732f6fbdb0c692711505175b327ddec6346ed3e"

IGreen='\033[0;92m'     # Green
IBlue='\033[0;94m'      # Blue
NC='\033[0m'            # No Color

update_prompt() {
# Check if the file exists and the last line of the file matches the specific line
    if [ -f "$FILE" ] && [ "$(tail -n 1 "$FILE")" = "$LINE" ]; then
        export PS1="\[${IGreen}\][Hexxagon-Game\[${IBlue}\] On-Chain-Code\[${IGreen}\]]$ \[${NC}\]"
    else
        export PS1="\\[\\e[0;92m\\][\\[\\e[0;92m\\]nix develop:\\[\\e[0;92m\\]\\w\\[\\e[0;92m\\]]\\[\\e[0;92m\\]$ \\[\\e[0m\\]"
    fi
}
PROMPT_COMMAND=update_prompt

# This file is found in the Hexxagon-Game-Plutarch folder, And this is a unique SHA256 hash: 6c00c6dadbc909ac132279f33732f6fbdb0c692711505175b327ddec6346ed3e
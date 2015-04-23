# config.fish, stephen@niedzielski.com

[ -f ~/.sh_aliases ]; and . ~/.sh_aliases
[ -f /usr/share/autojump/autojump.fish ]; and . /usr/share/autojump/autojump.fish

# prompt
function fish_prompt -d "Prints the command prompt."
  prompt $status $COLUMNS
end
# config.fish, stephen@niedzielski.com

[ -f ~/.sh_aliases ]; and . ~/.sh_aliases
[ -f /usr/share/autojump/autojump.fish ]; and . /usr/share/autojump/autojump.fish

# prompt
function fish_prompt -d "Prints the command prompt."
  prompt $status $COLUMNS
end

complete -c chromium --wraps chromium-browser
complete -c f --wraps find
complete -c g --wraps grep
complete -c gradlew --wraps gradle
complete -c l --wraps ls
complete -c map --wraps x
complete -c s --wraps sed
complete -c v --wraps gvim
complete -c x --wraps xargs
# config.fish, stephen@niedzielski.com

[ -f ~/.sh_aliases ]; and . ~/.sh_aliases
[ -f /usr/share/autojump/autojump.fish ]; and . /usr/share/autojump/autojump.fish

# no welcome message
set -e fish_greeting

# prompt
function fish_prompt -d "Prints the command prompt."
  prompt $status $COLUMNS
end

# fish doesn't seem to fully support readline
function fish_user_key_bindings
  # ctrl-del
  bind \e\[3\;5~ kill-word

  # ctrl-]
  bind \c] backward-kill-word
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

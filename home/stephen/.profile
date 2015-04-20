# .profile, stephen@niedzielski.com
# for dash, bash3, bash4, and zsh. ~/.profile is sourced for login shells when
# ~/.bash_profile and ~/.bash_login do not exist

# ------------------------------------------------------------------------------
# os utils

case "$OSTYPE$(uname)" in
  [lL]inux*) export TUX_OS=1 ;;
    [dD]arwin*) export MAC_OS=1 ;;
     [cC]ygwin) export WIN_OS=1 ;;
          *) echo "unknown os=\"$OSTYPE$(uname)\"" >&2 ;;
esac

is_tux() { [ ${TUX_OS-0} -ne 0 ]; }
is_mac() { [ ${MAC_OS-0} -ne 0 ]; }
is_win() { [ ${WIN_OS-0} -ne 0 ]; }

# ------------------------------------------------------------------------------
# search path utils

# $1 path
# $@ dirs
prepend_dirs_to_search_path() {
  local ret="$1"
  shift
  for i; do
    ret="$(trim_search_path "$i:$(remove_dir_in_search_path "$ret" "$i")")"
    #if ! is_dir_in_search_path "$ret" "$i"; then
    #  ret="$i${ret:+":$ret"}"
    #fi
  done
  echo "$ret"
}

# $1 path
# $@ dirs
append_dirs_to_search_path() {
  local ret="$1"
  shift
  for i; do
    ret="$(trim_search_path "$(remove_dir_in_search_path "$ret" "$i")"):$i"
    #if ! is_dir_in_search_path "$ret" "$i"; then
    #  ret="${ret:+"$ret:"}$i"
    #fi
  done
  echo "$ret"
}

# $1 path
# $2 dir
# ex: is_dir_in_search_path "$PATH" "$SDK_DIR"
is_dir_in_search_path() {
  #[[ ":$1:" == *":$2:"* ]]
  is_substr ":$1:" ":$2:"
}

# $1 path
# $2 dir
remove_dir_in_search_path() {
  trim_search_path "$(repl_str ":$1:" ":$2:" :)"
}

# $1 path
# $2 paths to prepend
prepend_path_to_search_paths() (
  local search_path="$1"
  shift
  IFS=:
  set -f $@
  prepend_dirs_to_search_path "$search_path" "$@"
)

# $1 path
# $2 paths to append
append_path_to_search_paths() (
  local search_path="$1"
  shift
  IFS=:
  set -f $@
  append_dirs_to_search_path "$search_path" "$@"
)

# $1 path
trim_search_path() {
  local ret="$*"
  ret="${ret#"${1%%[![:space:]:]*}"}"
  ret="${ret%"${1##*[![:space:]:]}"}"
  echo "$ret"
}

# $1 path
print_search_path() {
  trim_search_path "$1"|sed -r 's%:%\n%g'
}

# ------------------------------------------------------------------------------
# path utils

# $1 dirs
set_path_prepend_dirs() {
  export PATH="$(prepend_dirs_to_search_path "$PATH" "$@")"
}

# $1 dirs
set_path_append_dirs() {
  export PATH="$(append_dirs_to_search_path "$PATH" "$@")"
}

# $1 paths to prepend
set_paths_prepend_path() {
  export PATH="$(prepend_path_to_search_paths "$PATH" "$@")"
}

# $1 paths to append
set_path_append_paths() {
  export PATH="$(append_path_to_search_paths "$PATH" "$@")"
}

print_path() {
  print_search_path "$PATH"
}

# ------------------------------------------------------------------------------
# misc utils

deref() { eval echo \"\${$1}\"; }

# $1 str
# $2 substr
# $3 repl
repl_str() {
  is_substr "$1" "$2" && echo "${1%%$2*}$3${1#*$2}" || echo "$1"
}

# $1 str
# $2 substr
is_substr() {
  case "$1" in
    *"$2"*) : ;;
    *) ! : ;;
  esac
}

glob_last() {
  ls -1 "$@"|tail -n1
}

glob_last_dir() {
  ls -1d "$@"|tail -n1
}

is_bash() { is_substr "$-" i; }

# ------------------------------------------------------------------------------
# dart

if [ -d ~/opt/dart/dart-sdk/bin ]; then
  export DART_SDK="$HOME/opt/dart/dart-sdk"
  set_path_prepend_dirs "$DART_SDK/bin"
fi

# ------------------------------------------------------------------------------
# android

if [ -f ~/.profile_android ]; then
  . ~/.profile_android
fi

# ------------------------------------------------------------------------------
# java
export JAVA_HOME=/usr/lib/jvm/default-java

# ------------------------------------------------------------------------------
# path
set_path_prepend_dirs ~/bin

# ------------------------------------------------------------------------------
if is_bash && [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi
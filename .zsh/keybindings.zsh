### Keybindings.
#
# This binds keys to Zle widgets (Zsh Line Editor widgets.).
# To get a list of all widgets run zle -la
#
# See this link for more information.
#
#   http://sgeb.io/posts/2014/04/zsh-zle-custom-widgets/
#   http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets
#
# You can use `bindkey 'M-x'` to see what command M-x is bound to. Use `bindkey`
# to get a list of all mappings.
#
# bindkey <keys> <widget>
# bindkey -s <keys> <input strokes>
#
# ^ is control.
# ^[ is alt.
#

bindkey '^[l' down-case-word
bindkey '^[u' up-case-word
bindkey -s '^[p' '^Uproject^M' # Ctrl+u to delete line, project, enter.
bindkey -s '^[s' '^Ugit-switch^M'
bindkey -s '^[e' '^Uedit^M'

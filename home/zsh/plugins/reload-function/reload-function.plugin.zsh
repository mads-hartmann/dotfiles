# Tiny function for reloading another function.
# This is useful when you're writing completion functions.
# e.g. reload-function _mycompletionfunction

function reload-function {
    local function_name=$1
    unfunction ${function_name}
    autoload -U ${function_name}
}

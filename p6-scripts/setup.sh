if which python2 &>/dev/null; then
    PYTHON=python2
else
    PYTHON=python
fi

function p {
    $PYTHON ./pygmentize -l perl6 -O outencoding=utf-8 "$1" 2>/dev/null | nl -ba | less -FRX
}

export PATH="$(pwd):$PATH"

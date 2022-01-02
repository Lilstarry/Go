# Go

#### Run
You can run script as `sbcl --script main.lsp`\
Or you can compile it by yourself.

To play the same version in version 2, run:
```shell
rlwrap sbcl --noinform --non-interactive --load "go2.lisp" --eval '(play-match 9)'
```

#### Not avaliable list:
- ko rule is not avaliable now (in ver2 its not available too)
- autocount scores at the end of the game is not avaliable now

#### Changes in ver 2:
- ko rule is still not avaliable
- autocount works only for "eaten" stones

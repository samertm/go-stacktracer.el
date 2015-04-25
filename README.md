[![License GPL 3][badge-license]][license]

go-stacktracer.el
=================

Jump through Go stacktraces as easily as grep-mode.

![what it looks like](http://i.imgur.com/JXhRieX.png)

When you hit a stacktrace, mark the portion that you want to jump
through and then call `go-stacktracer-region' with M-x. The
*go-stacktracer* buffer will look something like this:

main.go:20: main.AFunc()
main.go:15: main.AnotherFunc()
main.go:7: main.main()

Use "n" and "p" to go down and up (just like grep-mode, it's
*go-stacktracer* is literally a grep-mode buffer).

go-stacktracer uses a regexp to capture the file path and line number,
so you don't need to be super precise with the lines you call
`go-stacktracer-region' on (though you should be sure to capture the
entire line).

BUGS: "debug.PrintStack()" prints the function name and file path in
the opposite order that "panic" does. I don't handle that case right
now, so the function names are off-by-one in the view for
debug.PrintStack traces.


[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[license]: https://github.com/samertm/go-stacktracer.el/blob/master/LICENSE

# parseargs
Command-line argument parsing for Haskell programs  
Bart Massey <bart@cs.pdx.edu>

This library provides System.Console.Parseargs, a module to
assist in argument parsing for Haskell stand-alone command
line programs.

The package provides a Haskell command-line argument
"parser".  You supply a specification of the arguments to
your command-line program; `parseargs` reads the arguments
and checks that they meet your spec. It then fills in a data
structure that captures the relevant data, from which parsed
arguments can be extracted as needed. See the Haddock
documentation for the gory details.

I have used this code with `ghc` 6 and 7 on Linux.  It is a
fairly standard Hackage-ready package, to the extent I know
how to construct such.

The 0.1.2 release includes a typeclass for argument types for
easier use.

The 0.1.3 release includes more uniform and usable error
handling.

The various 0.1.3.x point releases include bug fixes and
various extra-minor enhancements. See the Git log.

The 0.1.4 release includes the ability to mix optional and
required positional arguments.

This library is not what I set out to build.  It definitely
could also use some work.  However, I use it all the time
for writing little programs. I thought others might find it
useful, and I also have released other code that depends on
it, so I put it out there.

Have fun with it, and let me know if there are problems.

This code is released under a "3-clause BSD" license. Please
see the file COPYING in this distribution for license terms.

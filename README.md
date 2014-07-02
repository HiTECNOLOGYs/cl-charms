
Introduction
============

cl-charms provides CFFI bindings to libcurses for most Common Lisp
implementations.

It is intended to succeed cl-ncurses, which used the less portable UFFI instead
of CFFI for foreign bindings.

Indeed, cl-charms includes portions of code from cl-ncurses, and is largely a
direct translation of the latter's UFFI definitions to CFFI.

Currently, it is probably feature-equivalent to cl-ncurses and compatible with
client code. In the future, cl-charms may break compatibility with cl-ncurses
and may provide additional functionality.

cl-charms has been developed by it's original author, Abhishek Reddy
(abhishek@abhishek.geek.nz), since beginning of October 2010 for quite some
time, then abandoned for unknown reason. After that, in 2014, Mark Fedurin
(hitecnologys@gmail.com) took responsibility for keeping the library in working
state. It is released under an MIT-style license. See the file COPYING for
details.

>    The imitator dooms himself to hopeless mediocrity. The
>    inventor did it because it was natural to him, and so in him
>    it has a charm. In the imitator something else is natural, and
>    he bereaves himself of his own beauty, to come short of
>    another man's.
          -- R.W. Emerson on originality.


Portability
===========

cl-charms in it's current state has been officially confirmed by me to fully
work at least with configurations listed below:

* SBCL 1.1.8 on Gentoo Linux (3.13.6-hardened-r3) x86-64
* CCL 1.9-r15769M on Gentoo Linux (3.13.6-hardened-r3) x86-64

It may work on other implementations and system but I can not guarantee you
that. More testing is really appreciated.

cl-charms ought to function on other implementations of Common Lisp and
libcurses on various distributions of GNU/Linux.  See the section on Bugs and
Contributing below for how to help test and realize this.

cl-charms is supposed to be only bindings: nothing more, nothing less. For
full-featured TUI see: https://bitbucket.org/naryl/cl-tui


Installation
============

If you're Quicklisp user, then simple
```lisp
(ql:quickload :cl-charms)
```
would suffice, as cl-charms is in QL.

If you're not, then install Quicklisp and follow to the previous paragraph.


Testing
=======

TODO


Usage
=====

TODO


Bugs and Contributing
=====================

You can use GitHub project's (which is located here:
https://github.com/HiTECNOLOGYs/cl-charms) issues tracker or wiki to contribute
bug reports/patches/etc. or just send me an email (but that's not recommended
way to do it, though).

Major areas of work to be done:

* Add bindings for all the functions from ncurses
* Do more testing on different OS and architectures
* Try it with non-libncurses libraries.
* Test cases and sample applications.
* Finally write documentation

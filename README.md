Chana - C Header ANAlyzer
================================================================================
Chana analyzes the influence of C header files on compilation.  By adding it to
your C compiler command-line, it will generate a set of files that represent
the impact of all C header files used in your program's compilation. The files
produced by chana are intentionally structured for easy comparison, allowing
you to readily contrast separate compilations that employ distinct headers or
toolchains.

How does Chana do this?
--------------------------------------------------------------------------------
C headers can be broken down into these fundamental operations:

1. Define (or Undefine) preprocessor macro symbols
2. Define types
3. Define functions
4. Provide (or don't provide) content for #include <PATH> or #include "PATH"

This set of operations gives us the criteria we can use to verify whether a set
of header files are "semantically equivalent" to another set. Namely, is the
set of macros, types and functions the same after every "include" directive?

Enter chana, which takes a C source file, preproceses it and subsequently
generates an output file that shows all macros, types and functions introduced
by each `#include` directive.  Chana also sorts entries within each section,
ensuring that semantic equivalence will appear identical to a diff tool.

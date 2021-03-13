# guile-psyntax-bootstrapping
Bootstrapping Guile's psyntax.pp without relying on pre-expanded code

About
-----

This is an ("auditable" / "full source") bootstrap of Guile's psyntax system.

You may replace psyntax.pp.scm by a file that contains only the line

    (primitive-load-path "psyntax-bootstrap/allsteps")
    
and in theory your Guile should still work (albeit start considerably slower).

(See `psyntax-bootstrapping.scm` for description what these steps do and provide).

This is done by first bootstrapping a simple (Lisp-style, unhygienic)
macro expander, which then loads @janneke's implementation of quasiquote.

This macro expander is used to define `syntax`, `syntax-case`, `define-syntax`, `with-syntax` and `syntax-rules`. These definitions
do not support `custom-ellipsis` and are also not hygienic (they will not avoid macro symbols to interfere with same symbols in macro usage),
but janneke's syntax-rules does not either. But as this code should only be used to load the "real" macro expander, this is not a
big limitation.

Next, some definitions from boot-9.scm are loaded as they are needed for the further bootstrap. After that, a patched
version of `psyntax.scm` is loaded, which will dynamically switch the macroexpand definition in the middle. This was
probably the trickiest part of the bootstrap. After that, you have fully hyginenic psyntax available, which is then
used to load the same definitions from boot-9.scm again, and finally loading the unpatched `psyntax.scm`.

(Some steps have been tried and discarded again as they were not fruitful. These are collected in the `unused` directory.)

Finally, you can use this bootstrap to 

    make ice-9/psyntax-pp.scm.gen
    
and it will dump a shiny new psyntax-pp.scm for you. Note that the last command is broken on guile 3.0.3 to 3.0.5, therefore
I will use 3.0.2 in the CI task. But as psyntax-pp.scm is idential in all four versions, it hopefully does not matter too much.

Testing
-------

When testing, you may see Guile segfaulting when it tries to print syntax objects (before the module system is
initialized) - you can use patch #2 to work around that.

License
-------

The whole code is licensed under the [GNU General Public License 3.0 or later](https://www.gnu.org/licenses/gpl-3.0.html).

Some files (with me as the sole author) are (dual-)licensed under the [MIT License](https://opensource.org/licenses/MIT).

If you want to contribute to those files and are offended by such a liberal license, mention this to me. Depending on the
contribution I may be willing to drop that dual license.

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

and it will dump a shiny new psyntax-pp.scm for you. The bootstrap was tested on Guile 3.0.7. For 3.0.2, see the corresponding
tag in this repo.

About Hygiene in psyntax
------------------------

You may wonder, what does it take to make this psyntax implementation a possible replacement of other syntax-case implementations?
Adding with-ellipsis support is straightforward (it was removed as it is unneccessary for the bootstrap, and keeping it in would
clutter the code and make it harder to follow), but what exactly makes this implementation unhygienic?

I will try to explain the problem with an example. There is another example in `psyntax-bootstrapping.scm`, but this one is
maybe easier to follow:

    ¹: (define-syntax-rule (myfunc1 func aval bval) (let ((a aval)) (func a bval)))
    ²: (define-syntax-rule (myfunc2 b aval) (let ((a aval)) (+ a b)))
    ³: (myfunc1 myfunc2 20 22)

Recall that define-syntax-rule internally maps to syntax-rule and then syntax-case where the right hand side is a new `(syntax)`
invocation. And when expanding syntax macros, the whole input is also wrapped into a syntax expression. So, there are three
syntax exxpressions in this example, one in each line.

When expanding this expression, the result looks like the follows (I have annotated each atom with the syntax expression
it comes from):

       (myfunc1³ myfunc2³ 20³ 22³)
    => (let¹ ((a¹ 20³) (myfunc2³ a¹ 22³))
    => (let² ((a² 20³)) (let¹ ((a¹ 22³)) (+² a¹ a²)))

This implementation does not track where syntax-objects come from, and will just call `syntax->datum` to strip the syntax objects
out when evaluation finished. Therefore the code that is actually executed is:

    => (let ((a 20)) (let ((a 22)) (+ a a)))

Resulting in 44 instead of 42.

So how could we fix this?

First, when creating syntax objects (in `s3*-gen-syntax`) we will need to distinguish the syntax expressions. This could be done
by using wraps like psyntax does it, or by simply incrementing a counter and remembering its value. The latter approach was
rejected by Guile as it makes compiled forms unreproducible. Second, `$sc-dispatch` and `datum->syntax` will need to retain
this information. And last, before calling `syntax->datum` in the define-syntax definition, some renaming needs to be performed:

- Determine all symbols that exist from more than one syntax expression (in our example `let` and `a`)
- Scan the expression (taking into account forms like `let` or `lambda`) to find if any of these symbols
  is freshly bound. When such a location is found, replace the symbol by a fresh symbol wherever it occurs
  from the same syntax expression (in our example, replace `a¹` by `newsym-87¹` and `a²` by `newsym-88²`)

The resulting expression can then be evaluated. I am not sure whether this covers every case where the current implementation
is unhygienic, but so far I cannot think of any counter example.

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

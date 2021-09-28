fmt
===

A code formatter for Racket.

Work in progress.

To run, install the package (`raco pkg install fmt`) and run `raco fmt <your-code.rkt>`.

### Principles 

- A reformat of an OK code (with potentially bad style) should look OK. A reformat of a not OK code (macro-expansion error, read error) can look bad or even error (in case of read error).
- Changes must be only cosmetic. No content addition or removal.

### Related efforts

- [DrRacket](https://github.com/racket/drracket/): Only indent. Customizable with limited styles
- [`raco-format`](https://github.com/mxork/raco-format/): Only indent. Using DrRacket's code internally
- [Racket Mode](https://racket-mode.com/): Only indent. Customizable with limited styles
- [`racket/pretty`]: Prints from an S-expression, meaning comments are not supported. Does not care about paren-shape. Tends to lump everything on one line.
- [`racket-pretty-printer`](https://github.com/Shuumatsu/racket-pretty-printer): Not customizable. Using Wadler's pretty printer, which is suboptimal and not expressive.

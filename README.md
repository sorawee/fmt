fmt
===

A code formatter for Racket.

Work in progress.

To run, install the package (`raco pkg install fmt`) and run `raco fmt <your-code.rkt>`.

### Principles 

- A reformat of an OK code (with potentially bad style) should look OK. A reformat of a not OK code (macro-expansion error, read error) can look bad or even error (in case of read error).
- Changes must be only cosmetic. No content addition or removal.

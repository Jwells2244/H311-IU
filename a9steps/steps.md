# Steps to convert a racket program to C/Python

- Pick an interesting program to convert
- CPS the program
- Make the program RI WRT Continuations
- Use DS representation for Continuations 
- ANF
- Registerize
- Convert match statements to `union-cases`
- Convert thunked defines to `define-label`
- Use `define-registers` to initialize the registers
- Define `pc` register and change all thunk invocations to setting pc and Trampolinize
- Use parenthec's trampoline 
- Remove all racket specific things and save
- Use `pc2c.rkt`/`pc2py.rkt` to convert your code to C/Python
- Run it!

## ParentheC specific syntax

- `define-union`
- `union-case`
- `define-label`
- `define-registers`
- `define-program-counter`
- `mount-trampoline`
- `dismount-trampoline`

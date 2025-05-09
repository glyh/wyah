# Write You a Haskell implemented in OCaml

### Rationale

Not everyone is familiar or like the syntax and semantic of Haskell, as to write anything useful, you basically have to learn a whole bunch of concepts. 

However, studying the underlying mechanics of a Haskell compiler is still valuable, as its architecture is vastly different from what I would like to call "everyday compilers". For that specific reason, I would like to revive the project "Write You a Haskell" by Stephen Diehl, but I want to have it implemented in a language that is closed to compiler writers and more generally everyday programmers.   

### Requirements
- Opam

### Building

- Clone the repo and init the switch
```sh
git clone https://github.com/glyh/wyah
cd wyah
opam switch create . 5.3.0 --deps-only --with-test -y
```
- For developing, you may want to have LSP and other stuffs available
```sh
opam install --switch=. -y ocamlformat ocaml-lsp-server utop
```
- Update the environment, for example if you're on bash: 
```bash
eval $(opam env)
```
- Build and run the package
```sh
dune exec wyah
```

# OCaml Graph Project : Debt calculator

Implementation project of an algorithm computing the max-flow of a flow graph and apply it in a debt calculator app.

## Requierements
- [opam](https://opam.ocaml.org/) package manager to install ocaml
- [dune](https://dune.build/) build system to build, test and execute this app
- [Graphviz](https://graphviz.org/) visualization software to generate image files from app results

## Getting started
### Installation
```bash
git clone https://github.com/arc-hugo/debt_calculator/
cd debt_calculator
dune install
```
### Test
```bash
dune test -f
```
### Usage
```bash
dune exec bin/debt.exe infile outfile
```
## Maintainers
- [Hugo Barral](https://github.com/arc-hugo/)
- [Aubry Dubois](https://github.com/adubois31/) 

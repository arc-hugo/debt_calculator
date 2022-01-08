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
dune build
```
### Test
```bash
dune test -f
```
### Usage
```bash
dune exec bin/debt.exe infile outfile
```
Given infile need to follow this format.
```
Name1 MoneyPaid1\n
Name2 MoneyPaid2\n
....
NameN MoneyPaidN
```
Resulting output file will be in [DOT language](https://www.graphviz.org/doc/info/lang.html).  
It can then be converted in an image by the [dot layout engine](https://www.graphviz.org/docs/layouts/dot/) or an equivalent.


## Maintainers
- [Hugo Barral](https://github.com/arc-hugo/)
- [Aubry Dubois](https://github.com/adubois31/) 

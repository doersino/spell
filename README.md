# Spelling Corrector in Haskell

Haskell implementation of `spell.py` by Peter Norvig, which is described in detail [in his blog post "How to Write a Spelling Corrector"](http://norvig.com/spell-correct.html).

TODO tests


## Setup

```bash
git clone https://github.com/doersino/spell.git
cd spell
cabal sandbox init
cabal install
```

## Usage

For taking a first look, it's probably easiest to run `cabal repl`:

```haskell
*Spell> correction "scandinaiva"
"scandinavia"
*Spell> correction "inndeed"
"indeed"
*Spell> correction "photograf"
"photograph"
```

TODO
You can use this Cabal package as a library: add it to your `build-depends`, if necessary, `cabal sandbox add-source` the containing directory, `import Spell` and there you go.

## Notes

* Performance is a bit of an issue here, so I've generated a file `small.txt` by running `cat big.txt | head -n 1000 > small.txt`. This is used by default. *If you manage to significantly improve performance or reduce dependencies, please don't hesitate before filing an issue or sending a pull request.*
* Haskell is lazy, so it won't parse the `small.txt` file until you call `correction`. As a result, the first correction might take a few seconds -- subsequent ones will be faster.

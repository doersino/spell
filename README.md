# Spelling Corrector in Haskell

Haskell implementation of the simple spelling corrector `spell.py` by [Peter Norvig](http://norvig.com), which is described in detail [in his article "How to Write a Spelling Corrector"](http://norvig.com/spell-correct.html).


## Setup

```bash
git clone https://github.com/doersino/spell.git
cd spell
cabal sandbox init
cabal install
```


## Usage

For taking a first look, run `cabal repl`. Some examples:

```haskell
*Spell> correction "scandinaiva"
"scandinavia"
*Spell> correction "inndeed"
"indeed"
*Spell> correction "photograf"
"photograph"
```


## Notes

* Performance is somewhat lacking. *If you manage to significantly improve performance or reduce dependencies without major structural modifications (or just add more unit tests), please don't hesitate before filing an issue or sending a pull request.*
* Haskell is lazy, so it won't parse the `big.txt` file until you call `correction`. As a result, the first correction might take a few seconds -- subsequent ones will be faster.
* If you want, you can use this Cabal package as a dependency in your project (of course, there are better spell checkers). Since it's not on Hackage, run `cabal sandbox add-source <path to local clone of spell package>` in your project directory before adding `spell` to the `build-depends` list in your `.cabal` file.
* If you want to be able to run the included Hspec unit tests, replace the `cabal install` step above with `cabal install --only-dependencies --enable-tests`. Run the tests by means of `cabal test` or, for incremental and more colorful output, `cabal exec -- runhaskell -isrc -itest test/Spec.hs`.
* Run `cabal haddock` to generate documentation.

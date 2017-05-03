module SpellSpec (main, spec) where

import Test.Hspec
import Spell

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "spell" $ do
    it "corrects speling to spelling (insert)" $ do
        correction "speling" `shouldBe` "spelling"
--    it "corrects korrectud to corrected (replace 2)" $ do
--        correction "korrectud" `shouldBe` "corrected"
    it "corrects bycycle to bicycle (replace)" $ do
        correction "bycycle" `shouldBe` "bicycle"
    it "corrects inconvient to inconvenient (insert 2)" $ do
        correction "inconvient" `shouldBe` "inconvenient"
    it "corrects arrainged to arranged (delete)" $ do
        correction "arrainged" `shouldBe` "arranged"
    it "corrects peotry to poetry (transpose)" $ do
        correction "peotry" `shouldBe` "poetry"
    it "corrects peotryy to poetry (transpose + delete)" $ do
        correction "peotryy" `shouldBe` "poetry"
    it "corrects word to word (known)" $ do
        correction "word" `shouldBe` "word"
    it "corrects quintessential to quintessential (unknown)" $ do
        correction "quintessential" `shouldBe` "quintessential"

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
import           Data.Reflection  (give)
import           GHC.TypeLits
import           Test.Hspec       (describe, hspec, it, shouldBe)

import           ReaderMonad      (unModular)
import qualified ReaderMonad      as RM (Modular)
import qualified Reflection       as Ref (Modular)
import qualified TypeLevelNatural as TL (Modular)

modulus :: Num a => a
modulus = 10 ^ (9 :: Int) + 7

n :: Num a => a
n = 10 ^ (30 :: Int)

main :: IO ()
main = hspec $ do
  describe "Reader Monad" $ do
    it "with Int" $
      unModular modulus (n :: RM.Modular Int) `shouldBe` 999657007
    it "with Integer" $
      unModular modulus (n :: RM.Modular Integer) `shouldBe` 999657007
    it "handle negative number" $
      unModular 7 (-1 :: RM.Modular Int) `shouldBe` 6
    it "calculate reciprocal" $
      unModular 11 (recip 7 :: RM.Modular Int) `shouldBe` 8

  describe "Reflection" $ do
    it "with Int" $
      give (modulus :: Int) $ (n :: Ref.Modular Int) `shouldBe` 999657007
    it "with Integer" $
      give (modulus :: Integer) $ (n :: Ref.Modular Integer) `shouldBe` 999657007
    it "handle negative number" $
      give (7 :: Int) $ (-1 :: Ref.Modular Int) `shouldBe` 6
    it "calculate reciprocal" $
      give (11 :: Int) $ (recip 7 :: Ref.Modular Int) `shouldBe` 8

  describe "Type-level Natural" $ do
    it "with Int" $
      (n :: TL.Modular Int (10 ^ 9 + 7)) `shouldBe` 999657007
    it "with Integer" $
      (n :: TL.Modular Integer (10 ^ 9 + 7)) `shouldBe` 999657007
    it "handle negative number" $
      (-1 :: TL.Modular Int 7) `shouldBe` 6
    it "calculate reciprocal" $
      (recip 7 :: TL.Modular Int 11) `shouldBe` 8

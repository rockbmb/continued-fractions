{-# LANGUAGE ExtendedDefaultRules #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- Orphan instance are disabled because of @Arbitrary/CoArbitrary@ instances for @CF@.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.CFTests
    ( cfTests
    , prop_modifiedLentzWith_log_sane
    ) where

import Math.ContinuedFraction

import Control.Applicative                  (liftA2)
import Data.List                            (isPrefixOf, genericLength)
import qualified Data.Set                   as S
import Test.QuickCheck                      (Arbitrary (..), CoArbitrary (..),
                                             NonNegative (..), Property, (==>))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

default (Integer, Rational, Double)

eps :: Double
eps = 2.220446049250313e-16

instance Arbitrary a => Arbitrary (CF a) where
    arbitrary = do
        mode <- arbitrary
        if mode
            then liftA2 cf arbitrary arbitrary
            else liftA2 gcf arbitrary arbitrary

instance (CoArbitrary a, Num a, Eq a) => CoArbitrary (CF a) where
    coarbitrary cF = coarbitrary b0 . coarbitrary terms
        where
            (b0, terms) = asGCF cF

cfTests :: [Test]
cfTests =
    [ testGroup "asCF"                  asCF_tests
    , testGroup "asGCF"                 asGCF_tests
    , testGroup "truncateCF"            truncateCF_tests
    , testGroup "partitionCF"           partitionCF_tests
    , testGroup "evenCF"                evenCF_tests
    , testGroup "oddCF"                 oddCF_tests
    , testGroup "equiv"                 equiv_tests
    , testGroup "setDenominators"       setDenominators_tests
    , testGroup "setNumerators"         setNumerators_tests
    , testGroup "convergents"           convergents_tests
    , testGroup "steed"                 steed_tests
    , testGroup "lentz"                 lentz_tests
    , testGroup "lentzWith"             lentzWith_tests
    , testGroup "modifiedLentz"         modifiedLentz_tests
    , testGroup "modifiedLentzWith"     modifiedLentzWith_tests
    , testGroup "sumPartialProducts"    sumPartialProducts_tests
    ]

asCF_tests :: [Test]
asCF_tests =
    [ testProperty "preserves convergents" prop_asCF_preserves_convergents
    ]

prop_asCF_preserves_convergents :: (Eq a, Fractional a) => CF a -> Bool
prop_asCF_preserves_convergents orig = convergents orig == convergents new
    where
        new = uncurry cf (asCF orig)

asGCF_tests :: [Test]
asGCF_tests =
    [ testProperty "preserves convergents" prop_asCF_preserves_convergents
    ]

truncateCF_tests :: [Test]
truncateCF_tests =
    [ testProperty "truncates convergents" prop_truncateCF_truncates_convergents
    ]

prop_truncateCF_truncates_convergents
    :: (Eq a, Fractional a) => CF a -> NonNegative Int -> Bool
prop_truncateCF_truncates_convergents orig (NonNegative n) = convergents truncated `isPrefixOf` convergents orig
    where
        truncated = truncateCF n orig

partitionCF_tests :: [Test]
partitionCF_tests =
    [ testProperty "preserves convergents" prop_partitionCF_preserves_convergents
    ]

prop_partitionCF_preserves_convergents :: (Ord a, Fractional a) => CF a -> Property
prop_partitionCF_preserves_convergents cF =
    length (snd (asGCF cF)) > 1
    ==> (origConvergents == S.union evenConvergents oddConvergents)
    where
        origConvergents = S.fromList $ convergents cF
        evenConvergents = S.fromList $ convergents evenCf
        oddConvergents  = S.fromList $ convergents oddC
        
        (evenCf, oddC) = partitionCF cF

evenCF_tests :: [Test]
evenCF_tests =
    [ testProperty "preserves last convergent" prop_evenCF_preserves_last_convergent
    ]

prop_evenCF_preserves_last_convergent :: (Fractional a, Eq a) => CF a -> Bool
prop_evenCF_preserves_last_convergent orig = origResult == evenResult
    where
        origResult = last $ convergents orig
        evenResult = last $ convergents (evenCF orig)

oddCF_tests :: [Test]
oddCF_tests =
    [ testProperty "preserves last convergent" prop_oddCF_preserves_last_convergent
    ]

prop_oddCF_preserves_last_convergent :: (Fractional a, Eq a) => CF a -> Bool
prop_oddCF_preserves_last_convergent orig = origResult == oddResult
    where
        origResult = last $ convergents orig
        oddResult  = last $ convergents (oddCF orig)

equiv_tests :: [Test]
equiv_tests =
    [ testProperty "preserves convergents" prop_equiv_preserves_convergents
    ]

prop_equiv_preserves_convergents :: (Fractional a, Eq a) => [a] -> CF a -> Bool
prop_equiv_preserves_convergents cs orig = convergents orig == convergents new
    where
        new = equiv cs orig

setDenominators_tests :: [Test]
setDenominators_tests =
    [ testProperty "preserves convergents" prop_setDenominators_preserves_convergents
    ]

prop_setDenominators_preserves_convergents :: (Fractional a, Eq a) => [a] -> CF a -> Bool
prop_setDenominators_preserves_convergents denoms orig = convergents orig == convergents new
    where
        new = setDenominators denoms orig

setNumerators_tests :: [Test]
setNumerators_tests =
    [ testProperty "preserves convergents" prop_setNumerators_preserves_convergents
    ]

prop_setNumerators_preserves_convergents :: (Fractional a, Eq a) => [a] -> CF a -> Bool
prop_setNumerators_preserves_convergents nums orig = convergents orig == convergents new
    where
        new = setNumerators nums orig

convergents_tests :: [Test]
convergents_tests =
    [ testProperty "not null" prop_convergents_not_null
    ]

prop_convergents_not_null :: CF Rational -> Bool
prop_convergents_not_null = not . null . convergents

steed_tests :: [Test]
steed_tests =
    [ testProperty "not null" prop_steed_not_null
    ]

prop_steed_not_null :: CF Rational -> Bool
prop_steed_not_null = not . null . steed

lentz_tests :: [Test]
lentz_tests =
    [ testProperty "not null" prop_lentz_not_null
    ]

prop_lentz_not_null :: CF Rational -> Bool
prop_lentz_not_null = not . null . lentz

lentzWith_tests :: [Test]
lentzWith_tests =
    [ testProperty "not null" prop_lentzWith_not_null
    , testProperty "lentzWith log agrees with map log . lentz" prop_lentzWith_log_sane
    ]

prop_lentzWith_not_null :: CF Rational -> Bool
prop_lentzWith_not_null = not . null . lentzWith id (*) (1/)

prop_lentzWith_log_sane :: CF Double -> Bool
prop_lentzWith_log_sane cF =
    let signLog x = (signum x, log (abs x))
        addSignLog (xS,xL) (yS,yL) = (xS*yS, xL+yL)
        negateSignLog (s,l) = (negate s, l)
        
        (sX, x) ~= (sY, y)   
            = sX == sY 
                && (absErr <= eps * max 1 n * 16
                || relErr  <= eps * max 1 n * 16
                || any isNaN [absErr, relErr])
            where
                absErr = abs (x-y)
                relErr = absErr / max (abs x) (abs y)
        
        n = genericLength a
        a = map signLog (lentz cF)
        b = lentzWith signLog addSignLog negateSignLog cF
     in and (zipWith (~=) a b)

modifiedLentz_tests :: [Test]
modifiedLentz_tests =
    [ testProperty "first convergent"   prop_modifiedLentz_first_convergent
    , testProperty "not null"           prop_modifiedLentz_result_not_null
    , testProperty "sublists not null"  prop_modifiedLentz_sublists_not_null
    ]

prop_modifiedLentz_first_convergent :: (Fractional a, Eq a) => CF a -> Property
prop_modifiedLentz_first_convergent x =
    b0 /= 0 ==> (head . head) (modifiedLentz 1e-30 x) == b0
    where b0 = fst (asGCF x)

prop_modifiedLentz_sublists_not_null :: CF Rational -> Bool
prop_modifiedLentz_sublists_not_null = not . any null . modifiedLentz 1e-30

prop_modifiedLentz_result_not_null :: CF Rational -> Bool
prop_modifiedLentz_result_not_null = not . null . modifiedLentz 1e-30

modifiedLentzWith_tests :: [Test]
modifiedLentzWith_tests =
    [ testProperty "sanity test with log"   prop_modifiedLentzWith_log_sane
    ]

prop_modifiedLentzWith_log_sane :: CF Double -> Bool
prop_modifiedLentzWith_log_sane cF =
    let signLog x = (signum x, log (abs x))
        addSignLog (xS,xL) (yS,yL) = (xS*yS, xL+yL)
        negateSignLog (s,l) = (negate s, l)
        
        (sX, x) ~= (sY, y) = (sX == sY)
            && (absErr <= eps * max 1 n * max 1 m
            || relErr  <= eps * max 1 n * max 1 m
            || any isNaN [absErr, relErr])
            where
                absErr = abs (x-y)
                relErr = absErr / max (abs x) (abs y)
        
        tiny = 1e-30
        n = genericLength a; m = genericLength (concat a)
        a = map (map signLog) (modifiedLentz tiny cF)
        b = modifiedLentzWith signLog addSignLog negateSignLog tiny cF
     in and (zipWith (~=) (concat a) (concat b))

sumPartialProducts_tests :: [Test]
sumPartialProducts_tests =
    [ testProperty "preserves partial sums"
                   prop_sumPartialProducts_preserves_partial_sums
    ]

prop_sumPartialProducts_preserves_partial_sums :: (Eq a, Fractional a) => [a] -> Bool
prop_sumPartialProducts_preserves_partial_sums xs =
    scanl (+) 0 (tail $ scanl (*) 1 xs) == convergents (sumPartialProducts xs)
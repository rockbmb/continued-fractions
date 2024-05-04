{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- Orphan instance are disabled because of @Arbitrary/CoArbitrary@ instances for @CF@.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.CFTests
    ( cfTests
    , prop_modifiedLentzWith_log_sane
    ) where

import Math.ContinuedFraction

import Control.Applicative                  (liftA2)
import Data.List                            (isPrefixOf, genericLength, zip4)
import qualified Data.Set                   as S
import Test.QuickCheck                      (Arbitrary (..), CoArbitrary (..),
                                             NonNegative (..), NonZero (..), Property, (==>), withMaxSuccess)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

default (Integer, Rational, Double)

eps :: Double
eps = 1.0e-14

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

{-
continuants :: (Fractional a, Eq a) => CF a -> [(a, a)]
continuants orig = drop 1 (zip nums denoms)
    where
        (b0, terms) = asGCF orig
        nums   = 1:b0:[b * x1 + a * x0 | x0:x1:_ <- tails nums   | (a,b) <- terms]
        denoms = 0:1 :[b * x1 + a * x0 | x0:x1:_ <- tails denoms | (a,b) <- terms]

hasValidContinuants :: (Fractional a, Eq a) => CF a -> Bool
hasValidContinuants orig = all (/=0) $ map snd (continuants orig)
-}

allPartialDenomsNonZero :: (Fractional a, Eq a) => CF a -> Bool
allPartialDenomsNonZero orig = all (/=0) $ map snd terms
    where
        (_, terms) = asGCF orig

atLeastOneBTermNonZero :: (Fractional a, Eq a) => CF a -> Bool
atLeastOneBTermNonZero orig = any (/=0) $ (b0 : map snd terms)
    where
        (b0, terms) = asGCF orig

allPartialNumsNonZero :: (Fractional a, Eq a) => CF a -> Bool
allPartialNumsNonZero orig = all (/=0) $ map fst terms
    where
        (_, terms) = asGCF orig

allLentzTermsNonZero :: (Fractional a, Num a, Eq a) => CF a -> Bool
allLentzTermsNonZero orig = b0 /= 0 && all (/=0) cs && all (/=0) is
    where
        (b0, terms) = asGCF orig
        
        cs = [ b + a/c  | (a,b) <- terms | c <- b0 : cs]
        is = case terms of 
               []             -> []
               (_, b1) : rest -> b1 : [ b + a*(1/i) | (a,b) <- rest | i <- is ]

{-
-- |A continued fraction represented through nesting.
data CFNested a
    = CFCont a a (CFNested a)
    | CFFinal a

eval :: (Num a, Fractional a) => CFNested a -> a
eval (CFFinal a) = a
eval (CFCont b a next) = b + a / (eval next)

asGCFNumsDenoms :: CFNested a -> ([a], [a])
asGCFNumsDenoms (CFFinal b) = ([], [b])
asGCFNumsDenoms (CFCont b a cfn) = (a : nums, b : denoms) where (nums, denoms) = asGCFNumsDenoms cfn
-}

{-
toGCF :: (Eq a, Num a) => Bool -> CFNested a -> CF a
toGCF trunc cfn = gcf b truncterms
  where (nums, denoms) = asGCFNumsDenoms cfn
        b = head denoms
        terms = zip nums (tail denoms)
        truncterms = if trunc
                     then takeWhile ((/=0) . fst) terms
                     else terms

toCF :: (Fractional a, Eq a) => Bool -> CFNested a -> CF a
toCF trunc = (uncurry cf) . asCF . toGCF trunc
-}

{-
instance (Arbitrary a, Num a, Fractional a, Eq a) => Arbitrary (CFNested a) where
    arbitrary = do
        NonNegative n <- arbitrary
        if n == 0
          then CFFinal <$> arbitrary
          else CFFinal <$> (getNonZero <$> arbitrary) >>= generate n 
        where generate n cfn 
                | n <= 0 = return cfn
                | otherwise = do
                    b <- arbitrary
                    a <- arbitrary
                    adj <- if (n > 1 && b + a / (eval cfn) == 0)
                             then getNonZero <$> arbitrary
                             else return 0
                    generate (n-1) (CFCont (b + adj) a cfn)
-}

{-
newtype ValidCF a = ValidCF (CF a)
  deriving (Show)

instance (Arbitrary a, Num a, Fractional a, Eq a) => Arbitrary (ValidCF a) where
    arbitrary = do
        mode <- arbitrary
        if mode
            then (ValidCF . toCF True) <$> arbitrary -- Must truncate to avoid partial numerator == 0
            else (ValidCF . toGCF False) <$> arbitrary
-}

{-
newtype NonZeroPNumsCF a = NonZeroPNumsCF (CF a)
  deriving (Show)

instance (Arbitrary a, Num a, Fractional a, Eq a) => Arbitrary (NonZeroPNumsCF a) where
    arbitrary = do
        mode <- arbitrary
        if mode
            then (NonZeroPNumsCF . toCF True) <$> arbitrary
            else (NonZeroPNumsCF . toGCF True) <$> arbitrary
-}

newtype ValidContinuantsCF a = ValidContinuantsCF (CF a)
  deriving (Show)

instance (Arbitrary a, Num a, Eq a, Fractional a) => Arbitrary (ValidContinuantsCF a) where
    arbitrary = do
        b0 <- arbitrary
        pairvals <- arbitrary
        let (nzas, nzcontdenoms) = unzip pairvals 
            as = map getNonZero nzas
            contdenoms = map getNonZero nzcontdenoms
            n = length contdenoms
            inputs = zip4 as contdenoms (take n (1:contdenoms)) (take n (0:1:contdenoms))
            bs = map calcBs inputs
            val = gcf b0 $ zip as bs
        mode <- arbitrary
        if mode
            then return (ValidContinuantsCF . uncurry cf . asCF $ val)
            else return $ ValidContinuantsCF val
        where calcBs (a, denom, denom1, denom2) = (denom - a * denom2) / denom1   

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

prop_asCF_preserves_convergents :: (Eq a, Fractional a) => ValidContinuantsCF a -> Bool
prop_asCF_preserves_convergents (ValidContinuantsCF orig) = convergents orig == convergents new
    where
        new = uncurry cf (asCF orig)

asGCF_tests :: [Test]
asGCF_tests =
    [ testProperty "preserves convergents" prop_asGCF_preserves_convergents
    ]

prop_asGCF_preserves_convergents :: (Eq a, Fractional a) => ValidContinuantsCF a -> Bool
prop_asGCF_preserves_convergents (ValidContinuantsCF orig) = convergents orig == convergents new
    where
        new = uncurry gcf (asGCF orig)

truncateCF_tests :: [Test]
truncateCF_tests =
    [ testProperty "truncates convergents" prop_truncateCF_truncates_convergents
    ]

prop_truncateCF_truncates_convergents
    :: (Eq a, Fractional a) => ValidContinuantsCF a -> NonNegative Int -> Bool
prop_truncateCF_truncates_convergents (ValidContinuantsCF orig) (NonNegative n) = convergents truncated `isPrefixOf` convergents orig
    where
        truncated = truncateCF n orig

partitionCF_tests :: [Test]
partitionCF_tests =
    [ testProperty "preserves convergents" prop_partitionCF_preserves_convergents
    ]

prop_partitionCF_preserves_convergents :: (Ord a, Fractional a) => ValidContinuantsCF a -> Property
prop_partitionCF_preserves_convergents (ValidContinuantsCF cF) =
    length (snd (asGCF cF)) > 1
    ==> allPartialDenomsNonZero cF
    {- TODO ==> (hasValidContinuants evenCf && hasValidContinuants oddC) -}
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

prop_evenCF_preserves_last_convergent :: (Fractional a, Eq a) => ValidContinuantsCF a -> Property
prop_evenCF_preserves_last_convergent (ValidContinuantsCF orig) =
    allPartialDenomsNonZero orig
    -- ==> hasValidContinuants (evenCF orig) -- TODO
    ==> origResult == evenResult
    where
        origResult = last $ convergents orig
        evenResult = last $ convergents (evenCF orig)

oddCF_tests :: [Test]
oddCF_tests =
    [ testProperty "preserves last convergent" prop_oddCF_preserves_last_convergent
    ]

prop_oddCF_preserves_last_convergent :: (Fractional a, Eq a) => ValidContinuantsCF a -> Property
prop_oddCF_preserves_last_convergent (ValidContinuantsCF orig) = 
    allPartialDenomsNonZero orig
    -- ==> hasValidContinuants (oddCF orig) -- TODO
    ==> origResult == oddResult
    where
        origResult = last $ convergents orig
        oddResult  = last $ convergents (oddCF orig)

equiv_tests :: [Test]
equiv_tests =
    [ testProperty "preserves convergents" prop_equiv_preserves_convergents
    ]

prop_equiv_preserves_convergents :: (Fractional a, Eq a) => [NonZero a] -> ValidContinuantsCF a -> Bool
prop_equiv_preserves_convergents cs (ValidContinuantsCF orig) = 
    convergents orig == convergents new
    where
        new = equiv cs' orig
        cs' = map getNonZero cs

setDenominators_tests :: [Test]
setDenominators_tests =
    [ testProperty "preserves convergents" (withMaxSuccess 20000 prop_setDenominators_preserves_convergents)
    ]

prop_setDenominators_preserves_convergents :: (Fractional a, Eq a) => [NonZero a] -> ValidContinuantsCF a -> Property
prop_setDenominators_preserves_convergents denoms (ValidContinuantsCF orig) = 
    {-
    hasValidContinuants orig
    ==> 
    -}
    allPartialDenomsNonZero orig
    ==> convergents orig == convergents new
    where
        new = setDenominators denoms' orig
        denoms' = map getNonZero denoms

setNumerators_tests :: [Test]
setNumerators_tests =
    [ testProperty "preserves convergents" (withMaxSuccess 20000 prop_setNumerators_preserves_convergents)
    ]

prop_setNumerators_preserves_convergents :: (Fractional a, Eq a) => [NonZero a] -> ValidContinuantsCF a -> Property
prop_setNumerators_preserves_convergents nums (ValidContinuantsCF orig) =
    {- hasValidContinuants orig ==> -}
    allPartialNumsNonZero orig
    ==> convergents orig == convergents new
    where
        new = setNumerators nums' orig
        nums' = map getNonZero nums

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

prop_lentzWith_log_sane :: CF Double -> Property
prop_lentzWith_log_sane cF =
    let signLog x = (signum x, log (abs x))
        addSignLog (xS,xL) (yS,yL) = (xS*yS, xL+yL)
        negateSignLog (s,l) = (s, negate l)
        
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
     in allLentzTermsNonZero cF ==> and (zipWith (~=) a b)

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
    [ testProperty "sanity test with log"   (withMaxSuccess 20000 prop_modifiedLentzWith_log_sane)
    ]

prop_modifiedLentzWith_log_sane :: CF Double -> Property
prop_modifiedLentzWith_log_sane cF =
    let signLog x = (signum x, log (abs x))
        addSignLog (xS,xL) (yS,yL) = (xS*yS, xL+yL)
        negateSignLog (s,l) = (s, negate l)
        
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
     in atLeastOneBTermNonZero cF ==> and (zipWith (~=) (concat a) (concat b))

sumPartialProducts_tests :: [Test]
sumPartialProducts_tests =
    [ testProperty "preserves partial sums"
                   prop_sumPartialProducts_preserves_partial_sums
    ]

prop_sumPartialProducts_preserves_partial_sums :: (Eq a, Fractional a) => [a] -> Bool
prop_sumPartialProducts_preserves_partial_sums xs =
    scanl (+) 0 (tail $ scanl (*) 1 xs') == convergents (sumPartialProducts xs)
    where xs' = takeWhile (/=0) xs

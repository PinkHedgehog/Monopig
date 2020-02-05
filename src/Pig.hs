{-# LANGUAGE FlexibleInstances  #-}

module Pig
    (
    Pig(..), Piggable(..), kindOf
    ) where


import Data.String (IsString(..))
import Text.Read (readMaybe)
import Data.List (isPrefixOf)

instance IsString Pig where
    fromString = PString

data Pig = PInt !Int
         | PString String
         | PBool !Bool
         | PObject [Pig]
         deriving (Eq, Ord)

instance Show Pig where
    show (PInt x) = show x
    show (PBool x) = show x
    show (PString x) = x
    show (PObject x) = show x

class Show a => Piggable a where
    piggify :: a -> Pig

instance Piggable Int where
    piggify = PInt

instance Piggable Bool where
    piggify = PBool

instance Piggable [Char] where
    piggify = PString

instance Piggable  where
    definition

instance Piggable Pig where
    piggify = id

--instance Piggable a => Num a where


-- instance Num a => Piggable a where
--     piggify = PInt

instance Num Pig where
    PInt x + PInt y = PInt $! x + y
    PInt _ + x = error $ "PInt expected, " ++ kindOf x ++ " found!"
    -- PInt _ + PString _ = error "Incorrect constructor!"
    -- PInt _ + PObject _ = error "Incorrect constructor!"
    -- PInt _ * PBool _ = error "Incorrect constructor!"
    -- PInt _ * PString _ = error "Incorrect constructor!"
    -- PInt _ * PObject _ = error "Incorrect constructor!"
    -- PInt _ + PBool _ = error "Incorrect constructor!"
    -- PInt _ + PString _ = error "Incorrect constructor!"
    -- PInt _ + PObject _ = error "Incorrect constructor!"
    PInt x - PInt y = PInt $! x - y
    PInt x * PInt y = PInt $! x * y
    abs (PInt x) = PInt (abs x)
    fromInteger = PInt . fromInteger
    signum (PInt x) = PInt (signum x)
    -- _ + _ = error "Incorrect constructor! PInt expected"
    -- _ * _ = error "Incorrect constructor! PInt expected"
    -- _ - _ = error "Incorrect constructor! PInt expected"
    -- signum _ = error "Incorrect constructor! PInt expected"
    -- fromInteger _ = error "Incorrect constructor! PInt expected"
    -- abs _ = error "Incorrect constructor! PInt expected"

instance Enum Pig where
    fromEnum (PInt x) = x
    fromEnum x = error $ "PInt expected, " ++ kindOf x ++ " found!"
    toEnum = PInt

instance Real Pig where
    toRational (PInt x) = toRational x
    toRational _ = error "Not-a-number found!"

instance Integral Pig where
    quotRem (PInt x) (PInt y) = (PInt a, PInt b)
        where (a, b) = quotRem x y
    quotRem (PInt _) x = error $ "PInt expected, " ++ kindOf x ++ " found!"
    quotRem x _ = error $! "PInt expected, " ++ kindOf x ++ " found!"
    toInteger (PInt x) = toInteger x
    toInteger x = error $ "PInt expected, " ++ kindOf x ++ " found!"


read' :: String -> Pig
read' str | isPrefixOf "PInt " str = PInt (read (drop 5 str) :: Int)
          | isPrefixOf "PBool " str = PBool (read $! drop 6 str )
          | isPrefixOf "PString " str = PString $ drop 8 str
          | isPrefixOf "PObject " str = PObject (readListPig (drop 8 str) :: [Pig])
          | otherwise = case ((readMaybe str) :: Maybe Int, (readMaybe str)  :: Maybe Bool) of
              (Just x, _) -> PInt x
              (_, Just y) -> PBool y
              (_, _) -> PString str
readListPig = undefined
-- readListPig :: String -> [Pig]
-- readListPig "" = []
-- readListPig str = let str' = map (\x -> if x == ',' then '\1' else x) str
--                       curr =
--                         in

kindOf :: Pig -> String
kindOf (PInt _) = "PInt"
kindOf (PBool _) = "PBool"
kindOf (PString _) = "PString"
kindOf (PObject _) = "PObject"

instance Read Pig where
    readsPrec _ str = let
      a = read' str
      b = ""
      in [(a, b)]

    -- read str | isPrefixOf "PInt " str = PInt $! read str
    --          | isPrefixOf "PBool " str = PBool $! read str
    --          | isPrefixOf "PString " str = PString str
    --          | isPrefixOf "PObject " str = PObject $ read str :: [Pig]
    --          | otherwise = case (readMaybe str :: Int, readMaybe str :: Bool) of
    --             (Just x, _) -> PInt x
    --             (_, Just y) -> PBool y
    --             (_, _) -> PString str
    --
    --
    --
    -- read str = case (readMaybe str :: Int) of
    --     Just x = PInt x
    --     Nothing ->
    -- read str | (readMaybe str :: Maybe Int) /= Nothing = PInt (read x)
    --          | (readMaybe str :: Maybe Bool) /= Nothing = PBool (read x)
    --          |

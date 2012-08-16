{-

  Simple turn-based game:

  - Characters start with some health points.
  - Two actions are possible ("attack", or "idle").
  - Attack always succeeds, and removes one health point. Idle does nothing.
  - A "turn" can't be completed unless all characters have specified an action.

-}


module Game where

import List (groupBy)
import Data.List (intercalate, isInfixOf)


data Character = Character String Int -- name, health
               deriving (Show, Eq)


data Activity = Attack
              | Idle
              deriving (Show, Eq)


data Action = Action Character Character Activity -- from, to, activity
            deriving (Show)


data Adjustment = Adjustment Character Int -- character, health adjustment
                deriving (Show)


-- Returns a friendly string for any given contest of characters.
tryTurn :: [Character] -> [Action] -> String
tryTurn [] _ = "Whoa, I need characters!"
tryTurn cs as = case turnReady cs as of
  Left  chars   -> ("I need a valid action from " ++ (showCharacters chars))
  Right actions -> ("Result: " ++ (showCharacters $ performActions actions))


showCharacters :: [Character] -> String
showCharacters [] = "No characters."
showCharacters cs = intercalate ", " (map showCharacter cs)

showCharacter :: Character -> String
showCharacter (Character n h) | h > 0      = n ++ " has " ++ (show h) ++ "hp"
                              | otherwise  = n ++ " is dead"

showActions :: [Action] -> String
showActions [] = "No actions."
showActions as = intercalate ", " (map showAction as)

showAction :: Action -> String
showAction (Action f t a) | a == Idle   = (nameOf f) ++ " idles" -- catches all idles; the rest are attacks
                          | f == t      = (nameOf f) ++ " engages in self abuse"
                          | otherwise   = (nameOf f) ++ " attacks " ++ (nameOf t)
                          where
                            nameOf (Character n _) = n
    

-- checks to see if we need actions from characters (Left [Character]) or if all characters have acted (Right [Action])
turnReady :: [Character] -> [Action] -> Either [Character] [Action]
turnReady cs as = case charactersMissingActions of
    []    -> Right $ enforceOneActionPerCharacter inclusiveActions
    chars -> Left chars
  where
    charactersMissingActions = filterCharactersWithoutActions cs inclusiveActions
    inclusiveActions = filterActionsForCharacters as cs

-- returns characters missing actions
filterCharactersWithoutActions :: [Character] -> [Action] -> [Character]
filterCharactersWithoutActions cs as = filter noActionTest cs 
  where
    noActionTest c = (isInfixOf [c] charactersWithActions) /= True 
    charactersWithActions = map extractCharacter as
    extractCharacter (Action f _ _ ) = f


-- removes any actions that aren't from or to a member of the group
filterActionsForCharacters :: [Action] -> [Character] -> [Action]
filterActionsForCharacters as cs = filter allowedActions as
  where
    allowedActions (Action f t _) = (isInfixOf [f] cs) && (isInfixOf [t] cs)

-- enforces that there can only be one action (the most recent action) from a character per turn 
enforceOneActionPerCharacter :: [Action] -> [Action]
enforceOneActionPerCharacter ts = map firstAction (groupActionByCharacter ts)
  where
    firstAction ts = head ts
    groupActionByCharacter ts = groupBy groupChar ts
    groupChar (Action c _ _) (Action c' _ _) = (c == c')


-- returns as set of characters as the net result of a series of actions
performActions :: [Action] -> [Character]
performActions as = map applyAdjustment (rolledUpAdjustments as)
  where
    rolledUpAdjustments as' = map rollUpAdjustments (groupedAdjustments as')
    groupedAdjustments as'' = groupAdjustmentsByCharacter (appliedActions as'')
    appliedActions as'''    = map applyAction as'''


-- creates an Adjustment as the result of an action
applyAction :: Action -> Adjustment
applyAction (Action f t a) = case a of
  Idle   -> Adjustment t 0
  Attack -> Adjustment t (-1)


-- takes a set of adjustments and combines the effect; input must be adjustments a single character!
rollUpAdjustments :: [Adjustment] -> Adjustment
rollUpAdjustments as = Adjustment (extractCharacter $ head as) (rollupValues as)
  where
    rollupValues as' = foldr (+) 0 (extractValues as')
    extractValues as' = map extractValue as'
    extractValue (Adjustment c v) = v
    extractCharacter (Adjustment c v) = c


-- takes a disordered list of adjustments, and groupsBy the character
groupAdjustmentsByCharacter :: [Adjustment] -> [[Adjustment]]
groupAdjustmentsByCharacter a = groupBy charGroup a
  where
    charGroup (Adjustment c _) (Adjustment c' _) = (c == c')


-- returns the adjusted character
applyAdjustment :: Adjustment -> Character
applyAdjustment (Adjustment (Character n h) a) = Character n (h + a) 


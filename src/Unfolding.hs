module Unfolding (
    Unfolding, Event, Condition, Label, Id, parseUnfolding,
    events, conditions, labels, conflicts, conflictPairs,
    dependencies, arcs, labelDependencies
    ) where

import Data.List
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Parameterised unfoldings could be better, but for now String's are fine
type Label = String
type Id    = String

data Unfolding = Unfolding [Event] (Set Event) (Set Condition) deriving Show
data Event     = Event Id Label [Condition]                    deriving Show
data Condition = Condition Id [Event]                          deriving Show

instance Ord Event where
    compare (Event id1 _ _) (Event id2 _ _) = compare id1 id2

instance Eq Event where
    (Event id1 _ _) == (Event id2 _ _) = id1 == id2

instance Ord Condition where
    compare (Condition id1 _) (Condition id2 _) = compare id1 id2

instance Eq Condition where
    (Condition id1 _) == (Condition id2 _) = id1 == id2

data RawEvent = RawEvent Id Label [Id] [Id]

parseUnfolding :: String -> Unfolding
parseUnfolding = buildUnfolding . parseRawEvents . filter (/="") . lines

events :: Unfolding -> Set Event
events (Unfolding _ es _) = es

conditions :: Unfolding -> Set Condition
conditions (Unfolding _ _ cs) = cs

arcs :: Unfolding -> Integer
arcs (Unfolding _ es cs) = sum $ eArcs ++ cArcs
  where
    eArcs = map (\(Event _ _ postset) -> toInteger $ length postset) $ Set.toList es
    cArcs = map (\(Condition _ postset) -> toInteger $ length postset) $ Set.toList cs

labels :: Unfolding -> Set Label
labels = Set.fromList . map (\(Event _ label _) -> label) . Set.toList . events

conflictPairs :: Unfolding -> Integer
conflictPairs = sum . map combinations . Set.toList . conditions
  where
    combinations (Condition _ es) = toInteger $ length es * (length es - 1) `div` 2

conflicts :: Unfolding -> Integer
conflicts = sum . map count . Set.toList . conditions
  where
    count (Condition _ es) = toInteger $ length es

dependencies :: Unfolding -> Integer
dependencies = sum . map dependants . Set.toList . events
  where
    dependants (Event _ _ cs) = toInteger $ Set.size . Set.unions $ map children cs
    children (Condition _ es) = Set.fromList es

labelDependencies :: Unfolding -> Integer
labelDependencies = toInteger . Set.size . Set.unions . map labelPairs . Set.toList . events
  where
    labelPairs (Event _ label cs) =
        Set.fromList [ (label, child) | Condition _ es <- cs, Event _ child _ <- es ]

parseRawEvents :: [String] -> [RawEvent]
parseRawEvents ss
    | length ss == 4 = [parseRawEvent ss]
    | otherwise      = parseRawEvent four : parseRawEvents rest
  where
    (four, rest) = splitAt 4 ss

-- TODO: switch to using stripPrefix?
parseRawEvent :: [String] -> RawEvent
parseRawEvent [a, b, c, d] =
    RawEvent (                  drop  7 a) -- Event: e
             (                  drop 11 b) -- Operation: some event
             (parseConditions $ drop  8 c) -- preset: ...
             (parseConditions $ drop  9 d) -- postset: ...
parseRawEvent ss = error $ "parseRawEvent: cannot parse " ++ show ss

parseConditions :: String -> [Id]
parseConditions [] = []
parseConditions ss = c : parseConditions (drop 1 rest)
  where
    (c, rest) = break (==',') ss

buildUnfolding :: [RawEvent] -> Unfolding
buildUnfolding es = Unfolding
    (map (\(RawEvent eid _ _ _) -> getE eid) $ filter isTop es)
    (Set.fromList $ Map.elems emap) (Set.fromList $ Map.elems cmap)
  where
    emap = Map.fromList
        [ (eid, Event eid el . map getC $ sort cs) | RawEvent eid el _ cs <- es ]

    cmap = Map.fromList
         . map fromPairs
         . groupBy ((==) `on` fst)
         $ sort [ (cid, eid) | RawEvent eid _ cs _ <- es, cid <- cs ]

    fromPairs ps = (cid, Condition cid $ map (getE . snd) ps)
      where
        cid = fst $ head ps

    getE eid = emap Map.! eid
    getC cid = Map.findWithDefault (Condition cid []) cid cmap

    isTop (RawEvent _ _ preset _) = null preset

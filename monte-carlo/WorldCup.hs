module WorldCup where

import Data.Maybe (fromJust)
import Data.List (sortBy)
import Data.List.Split (splitEvery)
import qualified Data.Map as Map

import System.Random

type Ranking = Double

type League = Map.Map Team Int

data GameResult = Win | Lose | Draw
                  deriving (Show,Eq)

data Team = RSA | MEX | URA | FRA | 
            ARG | NGA | KOR | GRE | 
            ENG | USA | ALG | SVN | 
            GER | AUS | SRB | GHA | 
            NED | DEN | JPN | CMR | 
            ITA | PAR | NZL | SVK | 
            BRA | PRK | CIV | POR | 
            ESP | SUI | HON | CHI
            deriving (Show,Eq,Ord)

data GroupName = A | B | C | D | E | F | G | H
                 deriving (Show,Eq,Enum)

data Group = Group GroupName (Team,Team,Team,Team) deriving (Show)

data WorldCup = WorldCup [Group] deriving (Show)

data KnockoutStage = KnockoutStage [Team] deriving (Show)

class Model a where
    play :: a -> Team -> Team -> GameResult
    winner :: a -> Team -> Team -> Team

data RankingModel = RankingModel {
      ratings :: [(Team,Ranking)]
} deriving (Show)

instance Model RankingModel where
    play = play'
    winner = winner'

play' :: RankingModel -> Team -> Team -> GameResult
play' (RankingModel m) x y = case result of
             GT -> Win
             LT -> Lose
             EQ -> Draw
    where
      r1 = fromJust $ lookup x m
      r2 = fromJust $ lookup y m
      result = compare (truncate r1 `div` 25) (truncate r2 `div` 25)

winner' :: RankingModel -> Team -> Team -> Team
winner' m x y = case result of
                  Win -> x
                  Lose -> y
                  Draw -> x 
    where
      result = play' m x y

-- |Simulate the world cup
rankings28April :: [(Team,Ranking)]
rankings28April = 
    [
     (RSA,369),  (MEX,936), (URA,902), (FRA,1044),
     (ARG,1084), (NGA,883), (KOR,619), (GRE,968),
     (ENG,1068), (USA,950), (ALG,821), (SVN,860),
     (GER,1107), (AUS,883), (SRB,944), (GHA,802),
     (NED,1221), (DEN,767), (JPN,674), (CMR,887),
     (ITA,1184), (PAR,882), (NZL,413), (SVK,742),
     (BRA,1611), (PRK,292), (CIV,846), (POR,1249),
     (ESP,1565), (SUI,854), (HON,727), (CHI,948)
    ]

makeGroup :: GroupName -> (Team,Team,Team,Team) -> Group
makeGroup = Group

groupA :: Group
groupA = makeGroup A (RSA, MEX, URA, FRA)

groupB :: Group
groupB = makeGroup B (ARG, NGA, KOR, GRE)

groupC :: Group
groupC = makeGroup C (ENG, USA, ALG, SVN)

groupD :: Group
groupD = makeGroup D (GER, AUS, SRB, GHA)

groupE :: Group
groupE = makeGroup E (NED, DEN, JPN, CMR)

groupF :: Group
groupF = makeGroup F (ITA, PAR, NZL, SVK)

groupG :: Group
groupG = makeGroup G (BRA, PRK, CIV, POR)

groupH :: Group
groupH = makeGroup H (ESP, SUI, HON, CHI)

wcGroups :: [Group]
wcGroups = [groupA,groupB,groupC,groupD,groupE,groupF,groupG,groupH]

worldCup :: WorldCup
worldCup = WorldCup wcGroups

rules :: [(GroupName,Int)]
rules = [(A,1),(F,1),(B,1),(E,1),(C,1),(H,1),(D,1),(G,1),
         (B,2),(E,2),(A,2),(F,2),(D,2),(G,2),(C,2),(H,2)]

scoreGame :: League -> ((Team,Team),GameResult) -> League
scoreGame r ((x,_),Win) = Map.insertWith (+) x 3 r
scoreGame r ((_,y),Lose) = Map.insertWith (+) y 3 r
scoreGame r ((x,y),Draw) = Map.insertWith (+) y 1 (Map.insertWith (+) x 1 r)

scoreGames :: League -> [((Team,Team),GameResult)] -> League
scoreGames = foldl scoreGame 

fixtures :: (Team,Team,Team,Team) -> [(Team,Team)]
fixtures (a,b,c,d) = [(a,b),(a,c),(a,d),(b,c),(b,d),(c,d)]

initialLeague :: (Team,Team,Team,Team) -> League
initialLeague (a,b,c,d) = Map.fromList [(a,0),(b,0),(c,0),(d,0)]

playGroup :: Model a => a -> Group -> League
playGroup model (Group _ t) = scoreGames (initialLeague t) (zip matches results) 
    where
      matches = fixtures t
      results = map (uncurry (play model)) matches :: [GameResult]    

lookupPosition :: [(GroupName,League)] -> (GroupName,Int) -> Team
lookupPosition s (n,x) | x == 1 = fst $ head sortedList
                       | x == 2 = fst $ head $ tail sortedList
                       | otherwise = error "Invalid rules for looking up groups"
    where
      l = Map.toList $ fromJust (lookup n s)
      sortedList = sortBy (\(_,a) (_,b) -> compare b a) l

advanceToKnockOut :: Model a => WorldCup -> a -> KnockoutStage
advanceToKnockOut (WorldCup groups) model = KnockoutStage teams where
    groupWinners = zip [A .. H] (map (playGroup model) groups) :: [(GroupName,League)]
    teams = map (lookupPosition groupWinners) rules

nextRound :: Model a => a -> KnockoutStage -> KnockoutStage
nextRound _ (KnockoutStage (x:[])) = KnockoutStage [x]
nextRound model (KnockoutStage teams) = KnockoutStage results where
    len = length teams `div` 2
    matchUps = uncurry zip $ splitAt len teams
    results = map (uncurry (winner model)) matchUps

simulate :: Model a => WorldCup -> a -> Team
simulate wc model = head x where
    knockOut = advanceToKnockOut wc model
    rounds = iterate (nextRound model) knockOut
    KnockoutStage x = rounds !! 4

simulations :: Model a => WorldCup -> [a] -> League
simulations wc = foldl (simulateOne wc) Map.empty

simulateOne :: Model a => WorldCup -> League -> a -> League
simulateOne wc league model = Map.insertWith (+) w 1 league
    where 
      w = simulate wc model

createRatings :: [Double] -> [(Team,Ranking)]
createRatings p = map (\(x,(w,r)) -> (w,x*r)) (zip p rankings28April) where

createRankings :: [RankingModel]
createRankings = map (RankingModel . createRatings) weightings
    where
      weightings = splitEvery 32 randomDoubles

seed :: Int
seed = 32158972315                              

generator :: StdGen
generator = mkStdGen seed
  
randomDoubles :: [Double]
randomDoubles = map (\x -> (x*0.6) + 0.70) (randoms generator) 

main :: IO ()
main = do
  let models = (take 100000 createRankings)
      results = simulations worldCup models
  print results
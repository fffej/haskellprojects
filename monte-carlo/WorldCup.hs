module WorldCup where

import Data.Maybe (fromJust)
import Data.List (sortBy)
import qualified Data.Map as Map

type Ranking = Int

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

-- TODO provide a sensible way of winning
play :: Team -> Team -> GameResult
play x y = case result of
             GT -> Win
             LT -> Lose
             EQ -> Lose
    where
      r1 = fromJust $ lookup x rankings28April
      r2 = fromJust $ lookup y rankings28April
      result = compare r1 r2

winner :: Team -> Team -> Team
winner x y = case result of
               Win -> x
               Lose -> y
               Draw -> winner x y
    where
      result = play x y

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

worldCup :: WorldCup
worldCup = WorldCup [
            groupA
           ,groupB
           ,groupC
           ,groupD
           ,groupE
           ,groupF
           ,groupG
           ,groupH
           ]

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

playGroup :: (Team -> Team -> GameResult) -> Group -> League
playGroup resultFunc (Group _ t) = scoreGames (initialLeague t) (zip (fixtures t) results) 
    where
      results = map (uncurry resultFunc) (fixtures t) :: [GameResult]    

lookupPosition :: [(GroupName,League)] -> (GroupName,Int) -> Team
lookupPosition s (n,x) | x == 1 = fst $ head sortedList
                       | x == 2 = fst $ head $ tail sortedList
                       | otherwise = error "Invalid rules for looking up groups"
    where
      l = Map.toList $ fromJust (lookup n s)
      sortedList = sortBy (\(_,a) (_,b) -> compare b a) l

advanceToKnockOut :: WorldCup -> (Team -> Team -> GameResult) -> KnockoutStage
advanceToKnockOut (WorldCup groups) playFunc = KnockoutStage teams where
    groupWinners = zip [A .. H] (map (playGroup playFunc) groups) :: [(GroupName,League)]
    rules = [(A,1),(F,1),(B,1),(E,1),(C,1),(H,1),(D,1),(G,1),
             (B,2),(E,2),(A,2),(F,2),(D,2),(G,2),(C,2),(H,2)]
    teams = map (lookupPosition groupWinners) rules

nextRound :: (Team -> Team -> Team) -> KnockoutStage -> KnockoutStage
nextRound _ (KnockoutStage (x:[])) = KnockoutStage [x]
nextRound winFunc (KnockoutStage teams) = KnockoutStage results where
    len = length teams `div` 2
    matchUps = uncurry zip $ splitAt len teams
    results = map (uncurry winFunc) matchUps

-- simulate worldCup (play) (winner)
simulate :: WorldCup -> (Team -> Team -> GameResult) -> (Team -> Team -> Team) -> Team
simulate wc leagueFunc winFunc = head x where
    knockOut = advanceToKnockOut wc leagueFunc
    rounds = iterate (nextRound winFunc) knockOut
    KnockoutStage x = rounds !! 4
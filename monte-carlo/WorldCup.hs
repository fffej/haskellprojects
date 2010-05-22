module WorldCup where

import Data.Maybe (fromJust)
import Data.List ((!!),sortBy)
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

data Group = Group {
      group :: GroupName
    , teams :: (Team,Team,Team,Team)
} deriving (Show)

data WorldCup = WorldCup [Group] deriving (Show)

data KnockoutStage = KnockoutStage [Team] deriving (Show)

-- TODO provide a sensible way of winning
play :: Team -> Team -> GameResult
play x y | r1 > r2 = Win
         | r2 > r1 = Lose
         | r1 == r2 = Lose
    where
      r1 = fromJust $ lookup x rankings28April
      r2 = fromJust $ lookup y rankings28April

winner :: Team -> Team -> Team
winner x y | play x y == Win = x
winner x y | play x y == Lose = y
winner x y | play x y == Draw = winner x y

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
makeGroup name t@(a,b,c,d) = Group name t

groupA = makeGroup A (RSA, MEX, URA, FRA)
groupB = makeGroup B (ARG, NGA, KOR, GRE)
groupC = makeGroup C (ENG, USA, ALG, SVN)
groupD = makeGroup D (GER, AUS, SRB, GHA)
groupE = makeGroup E (NED, DEN, JPN, CMR)
groupF = makeGroup F (ITA, PAR, NZL, SVK)
groupG = makeGroup G (BRA, PRK, CIV, POR)
groupH = makeGroup H (ESP, SUI, HON, CHI)


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
scoreGame r ((x,y),Win) = Map.insertWith (+) x 3 r
scoreGame r ((x,y),Lose) = Map.insertWith (+) y 3 r
scoreGame r ((x,y),Draw) = Map.insertWith (+) y 1 (Map.insertWith (+) x 1 r)

scoreGames :: League -> [((Team,Team),GameResult)] -> League
scoreGames = foldl scoreGame 

fixtures :: (Team,Team,Team,Team) -> [(Team,Team)]
fixtures (a,b,c,d) = [(a,b),(a,c),(a,d),(b,c),(b,d),(c,d)]

initialLeague :: (Team,Team,Team,Team) -> League
initialLeague (a,b,c,d) = Map.fromList [(a,0),(b,0),(c,0),(d,0)]

playGroup :: Group -> League
playGroup (Group _ t) = scoreGames (initialLeague t) (zip (fixtures t) results) where
    results = map (uncurry play) (fixtures t) :: [GameResult]    

lookupPosition :: [(GroupName,League)] -> (GroupName,Int) -> Team
lookupPosition s (n,x) | x == 1 = fst $ head sortedList
                       | x == 2 = fst $ head $ tail sortedList
    where
      l = Map.toList $ fromJust (lookup n s)
      sortedList = sortBy (\(_,x) (_,y) -> compare y x) l

advanceToKnockOut :: WorldCup -> KnockoutStage
advanceToKnockOut (WorldCup groups) = KnockoutStage teams where
    groupWinners = zip [A .. H] (map playGroup groups) :: [(GroupName,League)]
    rules = [(A,1),(F,1),(B,1),(E,1),(C,1),(H,1),(D,1),(G,1),
             (B,2),(E,2),(A,2),(F,2),(D,2),(G,2),(C,2),(H,2)]
    teams = map (lookupPosition groupWinners) rules

nextRound :: KnockoutStage -> KnockoutStage
nextRound (KnockoutStage (x:[])) = KnockoutStage [x]
nextRound (KnockoutStage teams) = KnockoutStage results where
    len = length teams `div` 2
    matchUps = uncurry zip $ splitAt len teams
    results = map (uncurry winner) matchUps

simulate :: WorldCup -> Team
simulate wc = head x where
    knockOut = advanceToKnockOut wc
    rounds = iterate nextRound knockOut
    KnockoutStage x = rounds !! 4
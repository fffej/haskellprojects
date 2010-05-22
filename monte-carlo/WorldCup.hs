module WorldCup where

import Data.Maybe (fromJust)
import Data.List ((!!))

type Ranking = Int

data LeagueEntry = LeagueEntry Team Int
                   deriving (Show,Eq)

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
            deriving (Show,Eq)

data GroupName = A | B | C | D | E | F | G | H
                 deriving (Show)

data Group = Group {
      group :: GroupName
    , teams :: [Team]
    , league :: [LeagueEntry]
} deriving (Show)

data WorldCup = WorldCup [Group]

data KnockoutStage = KnockoutStage [Team]

-- TODO provide a sensible way of winning
play :: Team -> Team -> GameResult
play _ _ = Win

winner :: Team -> Team -> Team
winner x y | play x y == Win = x
winner x y | play x y == Lose = x
winner x y | play x y == Draw = winner x y

-- | (NameOfWinner, ((Position,Group) vs (Position, Group)))
last16 :: [(Int,((Int,GroupName),(Int,GroupName)))]
last16 = [(2,((1,B),(2,A)))
         ,(7,((1,G),(2,H)))
         ,(3,((1,C),(2,D)))
         ,(4,((1,D),(2,C)))
         ,(8,((1,H),(2,G)))
         ,(5,((1,E),(2,F)))
         ,(1,((1,A),(2,B)))
         ,(6,((1,F),(2,E)))]

last8 :: [(Char,(Int,Int))]
last8 = [('A', (1,3))
        ,('D', (6,8))
        ,('B', (2,4))
        ,('C', (5,7))]

last4 = [('A','C'),('B','D')]

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

makeGroup :: GroupName -> [Team] -> Group
makeGroup name teams = Group name teams leagues where
    leagues = map (\x -> LeagueEntry x 0) teams

groupA = makeGroup A [RSA, MEX, URA, FRA]
groupB = makeGroup B [ARG, NGA, KOR, GRE]
groupC = makeGroup C [ENG, USA, ALG, SVN]
groupD = makeGroup D [GER, AUS, SRB, GHA]
groupE = makeGroup E [NED, DEN, JPN, CMR]
groupF = makeGroup F [ITA, PAR, NZL, SVK]
groupG = makeGroup G [BRA, PRK, CIV, POR]
groupH = makeGroup H [ESP, SUI, HON, CHI]

worldCup = WorldCup [groupA,groupB,groupC,groupD,groupE,groupF,groupG,groupH]

advanceToKnockOut :: WorldCup -> KnockoutStage
advanceToKnockOut (WorldCup groups) = undefined

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
    KnockoutStage x = rounds !! 5
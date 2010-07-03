import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Char
r=replicate
rf=realToFrac
rd=round
cim m k=M.insertWith (+) k 1 m
cw s=take 22 $ sl $ foldl cim M.empty (filter (not.(`elem` (words "the and of to a i it in or is"))) (words s))
sl m=sortBy (\(_,x) (_,y)-> compare y x) (M.toList m)
dw w=' ':h++concatMap (dwi ww) w
    where
      n=(snd.head) w 
      lw=foldl1 max (map (length.fst) w) 
      ww=rf (80-(lw+3))/rf n
      h=r (rd (ww*rf n)) '_' ++ "\n"
dwi ww (w,n)="|"++r(round (rf n*ww))'_'++"| "++w++"\n"
main=interact (dw.cw.map toLower)
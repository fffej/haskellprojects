module Examples where

import StableMarriage

wolfram1 :: [(Int,[Int])]
wolfram1 = [(1,[7,3,8,9,6,4,2,1,5])
           ,(2,[5,4,8,3,1,2,6,7,9])
           ,(3,[4,8,3,9,7,5,6,1,2])
           ,(4,[9,7,4,2,5,8,3,1,6])
           ,(5,[2,6,4,9,8,7,5,1,3])
           ,(6,[2,7,8,6,5,3,4,1,9])
           ,(7,[1,6,2,3,8,5,4,9,7])
           ,(8,[5,6,9,1,2,8,4,3,7])
           ,(9,[6,1,4,7,5,8,3,9,2])]

wolfram2 :: [(Int,[Int])]
wolfram2 = [(1,[3,1,5,2,8,7,6,9,4])
           ,(2,[9,4,8,1,7,6,3,2,5])
           ,(3,[3,1,8,9,5,4,2,6,7])
           ,(4,[8,7,5,3,2,6,4,9,1])
           ,(5,[6,9,2,5,1,4,7,3,8])
           ,(6,[2,4,5,1,6,8,3,9,7])
           ,(7,[9,3,8,2,7,5,4,6,1])
           ,(8,[6,3,2,1,8,4,5,9,7])
           ,(9,[8,2,6,4,9,1,3,7,5])]

set1 :: [(Char,[Char])]
set1 = [('A',"abcd"),
        ('B',"bacd"),
        ('C',"adcb"),
        ('D',"dcab")]

set2 :: [(Char,[Char])]
set2 = [('a',"ABCD"),
        ('b',"DBCA"),
        ('c',"ABCD"),
        ('d',"CDAB")]

setA :: [(String,[String])]
setA = [("abe",["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"]),
        ("bob",[ "cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"]),
        ("col",[ "hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"]),
        ("dan",[ "ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"]),
        ("ed",[ "jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"]),
        ("fred",[ "bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"]),
        ("gav",[ "gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"]),
        ("hal",[ "abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"]),
        ("ian",[ "hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"]),
        ("jon",[ "abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"])]
   
setB :: [(String,[String])]
setB = [("abi",[ "bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"]),
        ("bea",[ "bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"]),
        ("cath",[ "fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"]),
        ("dee",[ "fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"]),
        ("eve",[ "jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"]),
        ("fay",[ "bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"]),
        ("gay",[ "jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"]),
        ("hope",[ "gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"]),
        ("ivy",[ "ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"]),
        ("jan",[ "ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"])]

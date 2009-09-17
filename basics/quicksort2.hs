-- The new Dual-Pivot Quicksort uses *two* pivots elements in this manner:
-- 1. Pick an elements P1, P2, called pivots from the array.
-- 2. Assume that P1 <= P2, otherwise swap it.
-- 3. Reorder the array into three parts: those less than the smaller
--    pivot, those larger than the larger pivot, and in between are
--    those elements between (or equal to) the two pivots.
-- 4. Recursively sort the sub-arrays.

dualPivotQuickSort :: Ord(a) => [a] -> [a]
dualPivotQuickSort = undefined

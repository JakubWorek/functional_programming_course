f :: (Ord a, Num a) => a -> a -> a
f x y = if (x>0) then 42 else x+y

neverEndingStory :: (Integral a, Num t) => t -> a
neverEndingStory x = neverEndingStory (x+1) `mod` 100
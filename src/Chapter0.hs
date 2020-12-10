
module Chapter0 where
import Refined

x :: Either RefineException (Refined Positive Int)
x = refine 23


y :: Either RefineException (Refined NonEmpty [String])
y = refine ["abc"]
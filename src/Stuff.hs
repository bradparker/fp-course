data Smell = String
data LegCount = Int
data Thing = Fruit Smell | Animal LegCount

nickFn :: Thing -> String
nickFn(Fruit smell) = "Smells like " ++ smell
nickFn(Animal count) = "That has some legs: " ++ count

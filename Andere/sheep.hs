data Sheep = Sheep { name::String,
                     mother::Maybe Sheep,
                     father::Maybe Sheep }

paternalGrandFather :: Sheep -> Maybe Sheep
paternalGrandFather s = father s >>= father

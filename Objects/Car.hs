data Car = Car {
    brand :: String,
    model :: String,
    year :: Int
} deriving(Show)

getCarInfo Car {brand, model, year} =
    "Car details: " ++ brand ++ " " ++ model ++ ", produced in " ++ show year


main = do
    let panda = Car{brand="Fiat", model="Panda", year=2008} 

    print $ panda
    print $ getCarInfo panda

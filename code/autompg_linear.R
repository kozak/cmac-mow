## utils for file IO and stats for linear models / prediction
require(utils);require(stats);

## column name for each column in auto-mpg.data file
columnNames <- c("mpg", "cylinders", "displacement", "horsepower","weight", "acceleration", "model_year", "origin", "car name");

## create data frame from the file
carsmpg<- read.table("data/auto-mpg.data", col.names=columnNames);

## create a linear model, using parameters from the auto mpg data
carModel <- lm(mpg~cylinders+displacement+as.numeric(horsepower)+weight+acceleration+model_year, data=carsmpg);

## define a new car model, this one is a 2007 mercedes s500, model assigned to agree with the data in file
newCar <- data.frame("cylinders" = 8, "displacement"=546.1, "horsepower"=388, "weight"=4310, "acceleration" = 5.44, "model_year" = 105);
predict(carModel, newCar);

## same parameters but much older
newCar <- data.frame("cylinders" = 8, "displacement"=546.1, "horsepower"=388, "weight"=4310, "acceleration" = 5.44, "model_year" = 90);
predict(carModel, newCar);

## same age but havier
newCar <- data.frame("cylinders" = 8, "displacement"=546.1, "horsepower"=388, "weight"=5310, "acceleration" = 5.44, "model_year" = 105);
predict(carModel, newCar);

## Analysis. Running the test and changing some other parameters we will notice that weight and age have the biggest influence
## on the result. While the weight is self explanatory, the influence of model year can be explained by the fast development of effective 
## engines between the years 70-90.
## If this progress wouldnt reach a plateau, today we would drive very energy efficient cars.  
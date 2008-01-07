source("cmac.R")

load_data = function() {
    cars = na.omit(read.table("data/auto-mpg.data", header=TRUE, na.strings="?"))
    mpgPredForm = mpg ~ . - name
    formTerms = terms(mpgPredForm, data=cars)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(cars[modelVars], min)
    maxes = sapply(cars[modelVars], max)


    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = 30)
    }

    model = cmac(mpgPredForm, cars, 10, 3, attrDescs)
    predict(model, cars[1,])
}

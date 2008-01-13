source("cmac.R")

load_sin = function() {
    x = seq(-pi/2, pi/2, 0.1)
    sinx = sin(x)
    dataFrame = data.frame(cbind(x,sinx))
    predForm = formula(sinx ~ x)
    
    formTerms = terms(predForm, data=dataFrame)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(dataFrame[modelVars], min)
    maxes = sapply(dataFrame[modelVars], max)
    cat(mins, "\n")
    cat(maxes, "\n")

    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = 20)
    }

    model = create.cmac(predForm, dataFrame, 10, 20, attrDescs)
    cat("created\n")
    model = train.cmac(model, dataFrame, 0.0001, 0.5)
}

load_data = function() {
    cars = na.omit(read.table("data/auto-mpg.data", header=TRUE, na.strings="?"))
    cars = cars[sample(1:nrow(cars)),]
    mpgPredForm = mpg ~ . - name
    formTerms = terms(mpgPredForm, data=cars)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(cars[modelVars], min)
    maxes = sapply(cars[modelVars], max)


    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = 10)
    }

    model = create.cmac(mpgPredForm, cars, 15, 20, attrDescs)
    model = train.cmac(model, cars[1:100,], 0.0001, 0.5)
    model
}

xf = function(li) {
    li[[1]] = 1
}

testGetInterval = function() {
    x = seq(1,6, 0.25)
    attrDescx = list(min = 1, max = 6, nDiv = 5)
    for (i in x) {
        cat("input = ", i, ", interval = ", getInterval(3, attrDescx, 1, i), "\n")
    }
}

source("cmac.R")

load_sin = function() {
    debug_disable()
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
        attrDescs[[varName]] = list(min = min, max = max, nDiv = 10)
    }

    model = create.cmac(predForm, dataFrame, 20, 20, attrDescs)
    cat("created\n")
    model = train.cmac(model, dataFrame, 0.0001, 0.5)
    
    x2 = seq(-pi/2 + 0.05, pi/2, 0.1)
    df2 = data.frame(x = x2, sinx = sin(x2))
    list(comp = cbind(sin(x2), predict(model, df2)), mse = mse(sin(x2), predict(model, df2)))
}

load_data = function() {
    cars = na.omit(read.table("data/auto-mpg.data", header=TRUE, na.strings="?"))
    cars = cars[sample(1:nrow(cars)),]
    mpgPredForm = mpg ~ . - name
    formTerms = terms(mpgPredForm, data=cars)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(cars[modelVars], min)
    maxes = sapply(cars[modelVars], max)

    nDiv = list(
    cylinders = 2,
    displacement = 10,
    horsepower = 10,
    weight = 70,
    acceleration = 2,
    modyear = 2,
    origin = 2)

    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = nDiv[[varName]])
    }

    model = create.cmac(mpgPredForm, cars, 10, 20, attrDescs)
    model = train.cmac(model, cars[1:200,], 0.01, 0.5)
    list(cmac = model, testSet = cars[201:nrow(cars),])
}


load_simple = function() {
    xt = seq(1, 11, 0.25)
    yt = xt * 2

    xts = seq(1.125, 11, 0.25)
    yts = xts * 2

    adx = list(min = 1, max = 11, nDiv = 10)
    attrDescs = list(x = adx)

    dft = data.frame(x = xt, y = yt)
    dfts = data.frame(x = xts, y = yts)
    
    model = create.cmac(y ~ x, df, 20, 20, attrDescs)
    model = train.cmac(model, dft, 0.001, 0.4)

    cbind(yts, predict(model, dfts))
}

xf = function(li) {
    li[[1]] = 1
}


testGetInterval = function() {
    x = seq(1, 6)
    y = seq(-2, 2)
    xy = expand.grid(x = x, y = y)

    adx = list(min = 1, max = 6, nDiv = 5)
    ady = list(min = -2, max = 2, nDiv = 5)
    attrDescs = list(x = adx, y = ady)
    
    nLayers = 4
    df = data.frame(xy)
    for (ind in 1:nrow(df)) {
        i = df[ind,, drop=FALSE]
        cat("input = ", i$x, " ", i$y, " ")
        intervalx = vector("numeric", nLayers)
        intervaly = vector("numeric", nLayers)
        for (nl in 0:(nLayers - 1)) {
            intervalx[nl + 1] = getInterval(nLayers, adx, nl, i$x)
            intervaly[nl + 1] = getInterval(nLayers, ady, nl, i$y)
        }
        cat("\t intervals x = ", intervalx)
        cat("\t intervals y = ", intervaly, "\n")
        cat("weightIndices = \t", getWeightIndices(nLayers, list("x", "y"), attrDescs, i))
        cat("\n\n")
    }
}

load_wine = function() {
    wines = na.omit(read.table("data/vin.data", header=TRUE, na.strings="?"))
    wines = wines[sample(1:nrow(wines)),]
    mpgPredForm = l91 ~ .
    formTerms = terms(mpgPredForm, data=wines)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(wines[modelVars], min)
    maxes = sapply(wines[modelVars], max)

    cat("mins = ", mins, "maxes = ", maxes, "\n")

    nDiv = list(l89 = 5, l90 = 6)

    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = nDiv[[varName]])
    }

    model = create.cmac(mpgPredForm, wines, 7, 20, attrDescs)
    model = train.cmac(model, wines[1:30,], 0.5, 0.35)
    model

}

test_nnet = function() {
    x = seq(-pi/2, pi/2, 0.3)
    sinx = sin(x)
    dataFrame = data.frame(cbind(x,sinx))
    predForm = formula(sinx ~ x)
    net = nnet(predForm, data=dataFrame, size = 5, linout=T)
    x2 = seq(-pi/2 + 0.15, pi/2 - 0.15, 0.3)
    sinx2 = sin(x2)
    dataFrame2 = data.frame(cbind(x = x2, sinx = sinx2))
    cbind(sinx2, predict(net, dataFrame2))
}

load_weather = function() {
    weather = na.omit(read.table("data/weather.data", header=TRUE, na.strings="?"))
    weather = resample(weather)
    mpgPredForm = meantemp ~ .
    formTerms = terms(mpgPredForm, data=weather)
    modelVars = attr(formTerms, "term.labels")
    mins = sapply(weather[modelVars], min)
    maxes = sapply(weather[modelVars], max)

    attrDescs = list()
    for (varName in modelVars) {
        min = mins[[varName]]
        max = maxes[[varName]]
        attrDescs[[varName]] = list(min = min, max = max, nDiv = 8)
    }

    model = create.cmac(mpgPredForm, weather, 8, 20, attrDescs)
    model = train.cmac(model, weather[1:1073,], 1, 0.5)
    model
}


source("hash.R")
source("common.R")

# Tworzy nowy aproksymator CMAC
# formula: Formuła, podobnie jak w lm(), np.: "mpg ~ . - name". Patrz ?formula
# data: data.frame z którego pobierane będą dane, podobnie jak w lm()
# nLayers: liczba warstw CMACa
# nWeightBits: definiuje rozmiar tablicy mieszajacej jako 2^nWeightBits
# attrDesc: lista charakteryzująca atrybuty na podstawie których będzie dokonane
#   przewidywanie. Nazwy elementów tej listy muszą pokrywać się z nazwami argumentów z 
#   data, na których podstawie budowany jest model. Dla każdego z atrybutów tworzony jest 
#   wpis w postaci listy z 3 elementami: min, max oraz nDiv, gdzie min oznacza minimalną
#   wartość atrybutu, max oznacza maksymalną wartość atrybutu a nDiv liczbę podprzedziałów
#   na które zostanie podzielony przedział atrybutu w warstwie 1.
#   Przykładowo dla atrybutów "mpg" i "weight", parametr attrDesc będzie tworzony następująco
#   attrDesc = list()
#   attrDesc$mpg = list(min = 10, max = 20, nDiv = 10)
#   attrDesc$weight = list(min = 100, max = 500, nDiv = 20)
create.cmac = function(formula, data, nLayers, nWeightBits, attrDescs, ...) {
    if (missing(data)) {
        data = environment(formula)
    }
    
    cmac = list()
    class(cmac) = "cmac"
    
    cmac$nLayers = nLayers
    cmac$nWeightBits = nWeightBits
    cmac$weights = rep(0, 2^nWeightBits)

    cmac$targetAttr = all.vars(formula)[[1]]
    cmac$otherAttrs = attr(terms(formula, data=data), "term.labels")
    cmac$attrDescs = attrDescs
    cmac
}

# Ucz CMAC
# cmac - zainicjalizowany obiekt cmac
# data - dane do nauki
# targetMse - zadany błąd średniokwadratowy
# tr - współczynnik uczenia się (training rate)
train.cmac = function(cmac, data, targetMse, tr) {
    cat("cmac$targetAttr = ", cmac$targetAttr, "\n")
    desiredOutput = data[[cmac$targetAttr]]
    cat("desired\n")
    newData = data[cmac$otherAttrs]

    actualOutput = zomg(cmac, newData)
    cat("ac = ", actualOutput, "\n")
    xx = mse(desiredOutput, (actualOutput = zomg(cmac, newData)))
    cat("xx = ", xx, "\n")
    while ((currentMse = mse(desiredOutput, (actualOutput = zomg(cmac, newData)))) > targetMse) {
        cat("Training, mse = ",  "\n")
        for (i in 1:nrows(newData)) {
            example = data[i, cmac$otherAttrs]
            cat("example$weight = ", example$weight, "\n")
            weightIndices = getHmWeightIndices(cmac, example);
            weightUpdate = (desiredOutput[i] - actualOutput) * tr / length(weightIndices)
            cmac$weights[weightIndices] = cmac$weights[weightIndices] + weightUpdate
        }
    }
    cmac
}

zomg = function(cmac, newData) {
    cat("zomg, cmac$nLayers = ", cmac$nLayers, "\n")
    hmWeightIndices = getHmWeightIndices(cmac, newData)
    sum(cmac$weights[hmWeightIndices])
}

getWeightIndices = function(cmac, input) {
    cat("cmac$nLayers = ", cmac$nLayers, "\n");
    weightIndices = vector("numeric", cmac$nLayers)
    for (i in 1:cmac$nLayers) {
        indices = list()
        for (attrName in cmac$otherAttrs) {
            cat("attrName = ", attrName,  "\n")
            cat("input[attrName] = ", input[[attrName]],  "\n")
            indices[[attrName]] = getInterval(cmac, cmac$attrDescs[[attrName]], i, input[[attrName]])
            cat("inidces[attrName] = ", indices[[attrName]], "\n")
        }
        weightIndices[i] = getWeightIndex(cmac, indices)
    }
    weightIndices
}

getHmWeightIndices = function(cmac, data) {
    weightIndices = getWeightIndices(cmac, data)
    hash(weightIndices, i, cmac$nWeightBits) + 1
}

getInterval = function(cmac, attrDesc, iLayer, input) {
    cat("ilayer = ", iLayer, "\n")
    cat("input = ", input, "\n")
    cat("attrDesc$min = ", attrDesc$min, "\n")
    cat("attrDesc$max = ", attrDesc$max, "\n")
    cat("attrDesc$nDiv = ", attrDesc$nDiv, "\n")
    if (iLayer == 0) {
        if (input == attrDesc$max) {
            return (attrDesc$nDiv)
        }
        return (floor((input - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    } else {
        intervalWidth = (attrDesc$max - attrDesc$min) / attrDesc$nDiv
        shift = (intervalWidth / cmac$nLayers) * iLayer;
        cat("shift = ", shift, "\n")
        inputPos = input - shift;
        cat("inputPos = ", inputPos, "\n")
        if (inputPos - attrDesc$min <= 0) {
            return (0)
        }
        return (ceiling((inputPos - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    }
}

getWeightIndex = function(cmac, intervals) {
    index = 0
    multiplier = 1
    for (i in 1:length(intervals)) {
        name = cmac$otherAttrs[i]
        cat("len = ", length(intervals[[i]]), "\n")
        cat("index before = ", index, "\n")
        cat("intervals[[i]] = ", intervals[[i]], "\n")
        cat("multiplier = ", multiplier, "\n")
        cat("intervals[[i]] *  multiplier = ", intervals[[i]] * multiplier, "\n")
        index = index + (intervals[[i]] * multiplier)
        multiplier = multiplier * (cmac$attrDescs)[[i]][["nDiv"]]
    }
    cat("index = ", index, "\n")
    index
}


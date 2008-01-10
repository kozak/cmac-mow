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
    debug_enter("crete.cmac()")
    if (missing(data)) {
        data = environment(formula)
    }
    
    cmac = list()
    class(cmac) = "cmac"
    
    cmac$nLayers = nLayers
    cmac$nWeightBits = nWeightBits
    cmac$weights = vector("numeric", 2^nWeightBits)

    cmac$targetAttr = all.vars(formula)[[1]]
    cmac$otherAttrs = attr(terms(formula, data=data), "term.labels")
    cmac$attrDescs = attrDescs

    debug_ret("create_cmac()")
    cmac
}

# Ucz CMAC
# cmac - zainicjalizowany obiekt cmac
# data - dane do nauki
# targetMse - zadany błąd średniokwadratowy
# tr - współczynnik uczenia się (training rate)
train.cmac = function(cmac, data, targetMse, tr) {
    debug_enter("train.cmac")

    desiredOutput = data[[cmac$targetAttr]]
    newData = data[cmac$otherAttrs]

    while ((currentMse = mse(desiredOutput, predict.cmac(cmac, newData))) > targetMse) {
        info("Training, mse = ", currentMse, "\n")
        flush.console()
        for (i in 1:nrow(newData)) {
            example = data[i, cmac$otherAttrs, drop=FALSE]
            actualOutput = predict.cmac(cmac, example)
            weightIndices = getHmWeightIndices(cmac, example);
            # cat("weights activated by input: ", weightIndices, "\n")
            # cat("desired output = ", desiredOutput[i], ", actual = ", actualOutput,"\n")
            weightUpdate = (desiredOutput[i] - actualOutput) * tr / length(weightIndices)
            #cat("weight update = ", weightUpdate, "\n")
            cmac$weights[weightIndices] = cmac$weights[weightIndices] + weightUpdate
        }
    }
    debug_ret("train.cmac")
    cmac
}

predict.cmac = function(cmac, newData) {
    debug_enter("predict.cmac")
    output = vector("numeric", nrow(newData))
    for (i in 1:nrow(newData)) {
        hmWeightIndices = getHmWeightIndices(cmac, newData[i,,drop=FALSE])
        # cat("predict: hmWeightIndices = ", hmWeightIndices, "\n")
        # cat("cmacweights$hmWeightIndices = ", cmac$weights[hmWeightIndices], "\n")

        output[i] = sum(cmac$weights[hmWeightIndices])
    }
    # cat("prediction output = ", output, "\n")
    debug_ret("predict.cmac")
    output
}

getWeightIndices = function(cmac, input) {
    debug_enter("getWeightIndices")

    weightIndices = vector("numeric", cmac$nLayers)
    for (i in 0:(cmac$nLayers - 1)) {
        indices = list()
        for (attrName in cmac$otherAttrs) {
            indices[[attrName]] = getInterval(cmac$nLayers, cmac$attrDescs[[attrName]], i, input[[attrName]])
        }
        weightIndices[i] = getWeightIndex(cmac, indices)
    }

    debug_ret("getWeightIndices")
    weightIndices
}

getHmWeightIndices = function(cmac, data) {
    debug_enter("getHmWeightIndices")

    weightIndices = getWeightIndices(cmac, data)
    for (i in 0:(cmac$nLayers - 1)) {
        weightIndices[i + 1] = hash(weightIndices[i + 1], i, cmac$nWeightBits) + 1
    }

    debug_ret("getHmWeightIndices")
    weightIndices
}

getInterval = function(nLayers, attrDesc, iLayer, input) {
    debug_enter("getInterval")

    if (iLayer == 0) {
        if (input == attrDesc$max) {
            debug_ret("getInterval")
            return (attrDesc$nDiv - 1)

        }
        debug_ret("getInterval")
        return (floor((input - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    } else {
        intervalWidth = (attrDesc$max - attrDesc$min) / attrDesc$nDiv
        shift = (intervalWidth / nLayers) * iLayer;
        inputPos = input - shift;
        
        if ((inputPos - attrDesc$min) <= 0) {
            debug_ret("getInterval")
            return (0)
        }
        debug_ret("getInterval")
        return (ceiling((inputPos - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    }
}

getWeightIndex = function(cmac, intervals) {
    debug_enter("getWeightIndex")
    index = 0
    multiplier = 1
    for (i in 1:length(intervals)) {
        name = cmac$otherAttrs[i]
        index = index + (intervals[[i]] * multiplier)
        multiplier = multiplier * (cmac$attrDescs)[[i]][["nDiv"]]
    }
    debug_ret("getWeightIndex")
    index
}


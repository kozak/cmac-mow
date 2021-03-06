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
    cmac$refWeights = vector("logical", 2^nWeightBits)

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
train.cmac = function(cmac, data, targetMse, tr, maxIters=10000000) {
    debug_enter("train.cmac")

    cmac$tr = tr

    desiredOutput = data[[cmac$targetAttr]]
    newData = data[cmac$otherAttrs]

    actualOutput = NULL
    currentMse = NULL
    iters = 0
    while ((currentMse = mse(desiredOutput, (actualOutput = predict.cmac(cmac, newData)))) > targetMse) {
        if (maxIters == iters) {
            break
        }
        info("Training, mse = ", currentMse, "\n")
        flush.console()
        for (i in 1:nrow(newData)) {
            example = data[i, cmac$otherAttrs, drop=FALSE]
            #actualOutput = predict.cmac(cmac, example)
            weightIndices = getHmWeightIndices(cmac$nLayers, cmac$nWeightBits, cmac$otherAttrs, 
                cmac$attrDescs, example);
            # cat("weights activated by input: ", weightIndices, "\n")
            # cat("desired output = ", desiredOutput[i], ", actual = ", actualOutput,"\n")
            weightUpdate = (desiredOutput[i] - actualOutput[i]) * tr / length(weightIndices)
            #cat("weight update = ", weightUpdate, "\n")
            cmac$weights[weightIndices] = cmac$weights[weightIndices] + weightUpdate
            cmac$refWeights[weightIndices] = TRUE
        }
        iters = iters + 1
    }
    info("trained CMAC, mse = ", currentMse)
    debug_ret("train.cmac")
    cmac
}

# Przewiduj atrybut docelowy dla przykladu lub zbioru przykladow
predict.cmac = function(cmac, newData) {
    debug_enter("predict.cmac")
    output = vector("numeric", nrow(newData))
    for (i in 1:nrow(newData)) {
        hmWeightIndices = getHmWeightIndices(cmac$nLayers, cmac$nWeightBits, 
            cmac$otherAttrs, cmac$attrDescs, newData[i,,drop=FALSE])
        # cat("predict: hmWeightIndices = ", hmWeightIndices, "\n")
        # cat("cmacweights$hmWeightIndices = ", cmac$weights[hmWeightIndices], "\n")

        output[i] = sum(cmac$weights[hmWeightIndices])
        refW = cmac$refWeights[hmWeightIndices]
        debug("Referenced weights: ", length(refW[refW == TRUE]), "/", length(hmWeightIndices))
    }
    # cat("prediction output = ", output, "\n")
    debug("predicted output = ", output)
    debug_ret("predict.cmac")
    output
}

getWeightIndices = function(nLayers, otherAttrs, attrDescs, input) {
    debug_enter("getWeightIndices")
    debug("nLayers = ", nLayers)
    weightIndices = vector("numeric", nLayers)
    for (i in 0:(nLayers - 1)) {
        indices = list()
        for (attrName in otherAttrs) {
            indices[[attrName]] = getInterval(nLayers, attrDescs[[attrName]], i, input[[attrName]])
        }
        weightIndices[i + 1] = getWeightIndex(otherAttrs, attrDescs, indices)
    }

    debug("Weight indices = ", weightIndices)
    debug_ret("getWeightIndices")
    weightIndices
}

getHmWeightIndices = function(nLayers, nWeightBits, otherAttrs, attrDescs, data) {
    debug_enter("getHmWeightIndices")

    weightIndices = getWeightIndices(nLayers, otherAttrs, attrDescs, data)
    for (i in 0:(nLayers - 1)) {
        weightIndices[i + 1] = hash(weightIndices[i + 1], i, nWeightBits) + 1
    }

    debug("Weight indices in hashmap = ", weightIndices)
    debug_ret("getHmWeightIndices")
    weightIndices
}

getInterval = function(nLayers, attrDesc, iLayer, input) {
    debug_enter("getInterval")
    debug("getInterval layer = ", iLayer, ", input = ", input, "(min = ", attrDesc$min, ", max = ", attrDesc$max, 
        ", ndiv = ", attrDesc$nDiv) 
        

    if (iLayer == 0) {
        if (input == attrDesc$max) {
            interval = attrDesc$nDiv - 1
            debug("interval = ", interval)
            debug_ret("getInterval")
            return (interval)

        }

        interval = floor((input - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv)
        debug("interval = ", interval)
        debug_ret("getInterval")
        return (interval)
    } else {
        intervalWidth = (attrDesc$max - attrDesc$min) / attrDesc$nDiv
        shift = (intervalWidth / nLayers) * iLayer;
        inputPos = input - shift;
        
        if ((inputPos - attrDesc$min) <= 0) {
            debug("interval = ", 0)
            debug_ret("getInterval")
            return (0)
        }
        interval =  ceiling((inputPos - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv)
        debug("interval = ", interval)
        debug_ret("getInterval")
        return (interval)
    }
}

getWeightIndex = function(otherAttrs, attrDescs, intervals) {
    debug_enter("getWeightIndex")
    
    msg = NULL
    if (debugEnable) {
        for (name in names(intervals)) {
            msg = c(msg, name, "= ", intervals[[name]], " ")
        }
        debug("intervals: ", msg)
    }
    
    index = 0
    multiplier = 1
    for (i in 1:length(intervals)) {
        name = otherAttrs[i]
        index = index + (intervals[[i]] * multiplier)
        multiplier = multiplier * (attrDescs)[[i]][["nDiv"]]
    }
    debug("Weight index = ", index)
    debug_ret("getWeightIndex")
    index
}


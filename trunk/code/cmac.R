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
cmac = function(formula, data, nLayers, nWeightBits, attrDescs, ...) {
    if (missing(data)) {
        data = environment(formula)
    }
    
    cmac = list()
    class(cmac) = "cmac"
    
    cmac$nLayers = nLayers
    cmac$nWeightBits = nWeightBits
    cmac$weights = rep(0, 2^nWeightBits)

    cmac$targetAttr = attr(terms(formula, data=data), "variables")[[2]]
    cmac$otherAttrs = attr(terms(formula, data=data), "term.labels")

}

# Ucz CMAC
# cmac - zainicjalizowany obiekt cmac
# data - dane do nauki
# targetMse - zadany błąd średniokwadratowy
# tr - współczynnik uczenia się (training rate)
train.cmac = function(cmac, data, targetMse, tr) {
    desiredOutput = data[cmac$targetAttr]
    newData = data[cmac$otherAttrs]
    while((mse = mse(data[cmac$targetAttr], predict(cmac, newData)) > targetMse) {
        cat("Training, mse = ", mse, "\n")
        for i in length(newData) {
            example = data[i, cmac$otherAttrs]
            weightIndices = getHmWeightIndices(cmac, getWeightIndices(cmac, example));
            actualOutput = predict(cmac, example);
            weightUpdate = (desiredOutput[i] - actualOutput) * tr / weightIndices.length
            cmac$weights[weightIndices] = cmac$weights[weightIndices] + weightUpdate
        }
    }
}

predict.cmac = function(cmac, newData) {
    weightIndices = getWeightIndices(cmac, newData)
    hmWeightIndices = getHmWeightIndices(cmac, newData)
    sum(cmac$weights[hmWeightIndices])
}

getWeightIndices = function(cmac, input) {
    weightIndices = vector("numeric", cmac$nLayers)
    for (int i in 1:cmac$nLayers) {
        indices = list()
        for (attrName in length(cmac$otherAttrs)) {
            indices[attrName] = getInterval(cmac, attrDescs[attrName], i, input[attrName])
        }
        weightIndices[i] = getWeightIndex(cmac, indices, attrDescs)
    }
    weightIndices
}

getHmWeightIndices = function(cmac, data) {
    weightIndices  = getWeightIndices(cmac, data)
    hmWeightIndices = rep(0, length(weightIndices))
    for i in 1:length(weightIndices) {
        hmWeightIndices[i] = hash(weightIndices[i], i, cmac$nWeightBits)
    }
    hmWeightIndices
}

getInterval = function(cmac, attrDesc, iLayer, input) {
    if (iLayer = 0) {
        if (input = attrDesc$max) {
            return (attrDesc$nDiv)
        }
        return (floor((input - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    } else {
        intervalWidth = (attrDesc$max - attrDesc$min) / attrDesc.nDiv
        shift = (intervalWidth / cmac$nLayers) * iLayer;
        inputPos = input - shift;
        return (ceiling((inputPos - attrDesc$min) / (attrDesc$max - attrDesc$min) * attrDesc$nDiv))
    }
}

getWeightIndex = function(cmac, intervals) {
    int index = 0;
    int multiplier = 1;
    for (int i in 1:length(intervals)) {
        index += intervals[i] * multiplier;
        multiplier *= cmac$attrDescs[i]$nDiv
    }
    return index;
}


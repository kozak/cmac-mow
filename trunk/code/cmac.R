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

cmac = function(formula, data, nLayers, nWeightBits, attrDesc, ...) {
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
train.cmac = function(cmac, data, targetMse, tr) {
    while((mse = mse(data[cmac$targetAttr], predict(cmac, data[cmac$otherAttrs]))) > targetMse) {
        cat("Training, mse = ", mse, "\n")
    }
}


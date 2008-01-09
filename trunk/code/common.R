# Liczy błąd średniokwadratowy dla modelu i oczekiwanych wyjść

mse = function(desiredOutput, actualOutput) {
    error = desiredOutput - actualOutput;
    mse = mean(error * error)
    cat("mse = ", mse, "\n")
    mse
}

debug = function() {

}

debug_enter = function() {

}
info

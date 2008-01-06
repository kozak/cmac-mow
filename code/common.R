# Liczy błąd średniokwadratowy dla modelu i oczekiwanych wyjść

mse = function(model, input, desiredOutput) {
    actualOutput = predict(model, input)
    error = desiredOutput - actualOutput;
    mean(error * error)
}

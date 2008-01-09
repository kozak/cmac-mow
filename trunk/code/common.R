# Liczy błąd średniokwadratowy dla modelu i oczekiwanych wyjść



mse = function(desiredOutput, actualOutput) {
    error = desiredOutput - actualOutput;
    mse = mean(error * error)
    cat("mse = ", mse, "\n")
    mse
}


debug_enable = function() {
    debugEnable <<- TRUE
    callStackCnt <<- 0
}

debug = function(...) {
    if (debugEnable) {
        mkindent()
        cat("DEBUG:", ..., "\n")
    }
}

debug_enter = function(fname) {
    if (debugEnable) {
        callStackCnt <<- callStackCnt + 1
        mkindent()
        cat("DEBUG: --->>> entering ", fname, "() \n")
    }
}

debug_ret = function(fname) {
    if (debugEnable) {
        mkindent()
        cat("DEBUG: <<<--- leaving ", fname, "() \n")
        callStackCnt <<- callStackCnt - 1
    }
}

info = function(...) {
    mkindent()
    cat("INFO", ..., "\n")
}

mkindent = function() {
    for (i in 1:callStackCnt) {
        cat("\t")
    }
}

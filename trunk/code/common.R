# Liczy błąd średniokwadratowy dla modelu i oczekiwanych wyjść



mse = function(desiredOutput, actualOutput) {
    error = desiredOutput - actualOutput;
    mse = mean(error * error)
    cat("mse = ", mse, "\n")
    mse
}

resample = function(df) {
    df[sample(1:nrow(df)),]
}

debug_enable = function() {
    debugEnable <<- TRUE
    callStackCnt <<- 0
}

debug_disable = function() {
    debugEnable <<- FALSE
    callStackCnt <<- 0
}

debug = function(...) {
    if (debugEnable) {
        mkindent()
        cat("DEBUG:", c(...), "\n")
        flush.console()
    }
}

debug_enter = function(fname) {
    if (debugEnable) {
        callStackCnt <<- callStackCnt + 1
        mkindent()
        cat("DEBUG: --->>> entering ", fname, "()\n")
        flush.console()
    }
}

debug_ret = function(fname) {
    if (debugEnable) {
        mkindent()
        cat("DEBUG: <<<--- leaving ", fname, "()\n")
        callStackCnt <<- callStackCnt - 1
        flush.console()
    }
}

info = function(...) {
    mkindent()
    cat("INFO", ..., "\n")
}

mkindent = function() {
    for (i in 1:callStackCnt) {
        cat(" ")
    }
}

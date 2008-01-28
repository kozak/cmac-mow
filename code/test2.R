library(e1071)

source("cmac.R")
debug_disable()

tf = function(x, y) {
    10 * cos(sqrt(x^2+y^2))/(1+sqrt(x^2+y^2))
}

# Testuje aproksymatory na sztucznie utworzonej funkcji dwoch zmiennych.
test_artificial  = function () {
    gr <- expand.grid(x = seq(-2, 2, 0.2), y = seq(-2, 2, 0.2))
    gr$z = tf(gr$x, gr$y)
    artif = data.frame(gr)

    # Permutuj losowo
    artif = resample(artif)

    # Utworz zbior treningowy
    training_set = artif[1:147,]

    # Utworz zbior walidacyjny
    validation_set = artif[148:294,]
    
    # Utworz zbior testowy
    test_set = artif[295:441,]

    form = z ~ x + y
    formTerms = terms(form, data=artif)
    cmac_modelVars = attr(formTerms, "term.labels")
    mins = sapply(artif[cmac_modelVars], min)
    maxes = sapply(artif[cmac_modelVars], max)

    # liczba warstw
    nHashBits = 20

    # liczba pozadanych podzialow dziedziny atrybutu
    nDiv = list(x = 90, y = 90)


    
    nLayersList = 8:12
    trainingRates = rev(nLayersList / 75)
    
    maxIters = 20
    targetMse = 0.01

    minMse = 1000000000
    best_cmac = NULL
    

    info("Choosing best CMAC by cross validation")
    for (i in 1:length(nLayersList)) {
        nLayers = nLayersList[i]
        trainingRate = trainingRates[i]
        info("Training for ", nLayers, " layers, tr = ", trainingRate, " maxIters = ", maxIters)

        attrDescs = list()
        for (varName in cmac_modelVars) {
            min = mins[[varName]]
            max = maxes[[varName]]
            attrDescs[[varName]] = list(min = min, max = max, nDiv = floor(nDiv[[varName]] / nLayers))
        }

        cmac_model = create.cmac(form, artif, nLayers, nHashBits, attrDescs)

        cmac_model = train.cmac(cmac_model, training_set, targetMse, trainingRate, maxIters)
        current_mse = mse(predict(cmac_model, validation_set), validation_set$z)
        info("mse on validation set = ", current_mse)
        if (current_mse < minMse) {
            best_cmac = cmac_model
            minMse = current_mse
        }
    }
    info("Best CMAC has ", best_cmac$nLayers, " layers")
    info("Training CMAC on training set + val_set")
    maxIters = 30

    # Bedziemy uczyc na polaczonym zbiorze treningowym i walidacyjnym
    new_tr_set = rbind(training_set, validation_set)
    best_tr = best_cmac$tr
    best_cmac = create.cmac(form, artif, best_cmac$nLayers, nHashBits, attrDescs)

    # Czyscimy pamiec
    gc()
    best_cmac = train.cmac(best_cmac, new_tr_set, targetMse, 
        best_tr, maxIters)

    info("Choosing best SVM")
    best_svm = best.svm(form, data = new_tr_set)
    
    info("Choosing best RandomForest")
    best_rf = best.randomForest(form, data = new_tr_set)
    
    info("Creating linear regression model")
    best_lm = lm(form, data = new_tr_set)

    # Blad sredniokwadratowy na testowym zbiorze
    mse_tst_cmac = mse(predict(best_cmac, test_set), test_set$z)
    mse_tst_svm = mse(predict(best_svm, test_set), test_set$z)
    mse_tst_rf = mse(predict(best_rf, test_set), test_set$z)
    mse_tst_lm = mse(predict(best_lm, test_set), test_set$z)
    
    cat("<<< mse on TEST SET >>>\n")
    cat("for CMAC: ", mse_tst_cmac, "\n")
    cat("for SVM: ", mse_tst_svm, "\n")
    cat("for RandomForest: ", mse_tst_rf, "\n")
    cat("for LM: ", mse_tst_lm, "\n")
}

test_basketball  = function () {
    bb = read.table("data/basketball.data", header=TRUE)

    # Permutuj losowo
    bb = resample(bb)

    # Utworz zbior treningowy
    training_set = bb[1:32,]

    # Utworz zbior walidacyjny
    validation_set = bb[33:64,]
    
    # Utworz zbior testowy
    test_set = bb[64:96,]

    form = points_per_minute ~ .
    formTerms = terms(form, data=bb)
    cmac_modelVars = attr(formTerms, "term.labels")
    mins = sapply(bb[cmac_modelVars], min)
    maxes = sapply(bb[cmac_modelVars], max)

    # liczba warstw
    nHashBits = 20

    # liczba pozadanych podzialow dziedziny atrybutu
    #nDiv = list(age = 25, assists_per_minute = 20, height = 10, time_played = 15)
    nDiv = list(age = 15, assists_per_minute = 10, height = 10, time_played = 10)

    
    #nLayersList = 6:9
    nLayersList = 7:10
    trainingRates = rev(nLayersList / 100)
    
    maxIters = 30
    targetMse = 0.001

    minMse = 1000000000
    best_cmac = NULL
    

    info("Choosing best CMAC by cross validation")
    for (i in 1:length(nLayersList)) {
        nLayers = nLayersList[i]
        trainingRate = trainingRates[i]
        info("Training for ", nLayers, " layers, tr = ", trainingRate, " maxIters = ", maxIters)

        attrDescs = list()
        for (varName in cmac_modelVars) {
            min = mins[[varName]]
            max = maxes[[varName]]
            attrDescs[[varName]] = list(min = min, max = max, nDiv = floor(nDiv[[varName]] / nLayers))
        }

        cmac_model = create.cmac(form, bb, nLayers, nHashBits, attrDescs)

        cmac_model = train.cmac(cmac_model, training_set, targetMse, trainingRate, maxIters)
        current_mse = mse(predict(cmac_model, validation_set), validation_set$points_per_minute)
        info("mse on validation set = ", current_mse)
        if (current_mse < minMse) {
            best_cmac = cmac_model
            minMse = current_mse
        }
    }
    info("Best CMAC has ", best_cmac$nLayers, " layers")
    info("Training CMAC on training set + val_set")
    maxIters = 50

    # Bedziemy uczyc na polaczonym zbiorze treningowym i walidacyjnym
    new_tr_set = rbind(training_set, validation_set)
    best_tr = best_cmac$tr / 2
    best_cmac = create.cmac(form, bb, best_cmac$nLayers, nHashBits, attrDescs)

    # Czyscimy pamiec
    gc()
    best_cmac = train.cmac(best_cmac, new_tr_set, targetMse, 
        best_tr, maxIters)

    info("Choosing best SVM")
    best_svm = best.svm(form, data = new_tr_set)
    
    info("Choosing best RandomForest")
    best_rf = best.randomForest(form, data = new_tr_set)
    
    info("Creating linear regression model")
    best_lm = lm(form, data = new_tr_set)

    # Blad sredniokwadratowy na testowym zbiorze
    mse_tst_cmac = mse(predict(best_cmac, test_set), test_set$points_per_minute)
    mse_tst_svm = mse(predict(best_svm, test_set), test_set$points_per_minute)
    mse_tst_rf = mse(predict(best_rf, test_set), test_set$points_per_minute)
    mse_tst_lm = mse(predict(best_lm, test_set), test_set$points_per_minute)
    
    cat("<<< mse on TEST SET >>>\n")
    cat("for CMAC: ", mse_tst_cmac, "\n")
    cat("for SVM: ", mse_tst_svm, "\n")
    cat("for RandomForest: ", mse_tst_rf, "\n")
    cat("for LM: ", mse_tst_lm, "\n")
}

test_quake  = function () {
    qq = read.table("data/quake.data", header=TRUE)

    # Permutuj losowo
    qq = resample(qq)

    # Utworz zbior treningowy
    training_set = qq[1:726,]

    # Utworz zbior walidacyjny
    validation_set = qq[727:1452,]
    
    # Utworz zbior testowy
    test_set = qq[1453:2178,]

    form = richter ~ .
    formTerms = terms(form, data=qq)
    cmac_modelVars = attr(formTerms, "term.labels")
    mins = sapply(qq[cmac_modelVars], min)
    maxes = sapply(qq[cmac_modelVars], max)

    # liczba warstw
    nHashBits = 20

    # liczba pozadanych podzialow dziedziny atrybutu
    nDiv = list(focaldepth = 400, latitude = 300, longitude = 300)

    
    nLayersList = 15:20
    trainingRates = rev(nLayersList / 200)
    
    maxIters = 30
    targetMse = 0.005

    minMse = 1000000000
    best_cmac = NULL
    

    info("Choosing best CMAC by cross validation")
    for (i in 1:length(nLayersList)) {
        nLayers = nLayersList[i]
        trainingRate = trainingRates[i]
        info("Training for ", nLayers, " layers, tr = ", trainingRate, " maxIters = ", maxIters)

        attrDescs = list()
        for (varName in cmac_modelVars) {
            min = mins[[varName]]
            max = maxes[[varName]]
            attrDescs[[varName]] = list(min = min, max = max, nDiv = floor(nDiv[[varName]] / nLayers))
        }

        cmac_model = create.cmac(form, qq, nLayers, nHashBits, attrDescs)

        cmac_model = train.cmac(cmac_model, training_set, targetMse, trainingRate, maxIters)
        current_mse = mse(predict(cmac_model, validation_set), validation_set$richter)
        info("mse on validation set = ", current_mse)
        if (current_mse < minMse) {
            best_cmac = cmac_model
            minMse = current_mse
        }
    }
    info("Best CMAC has ", best_cmac$nLayers, " layers")
    info("Training CMAC on training set + val_set")
    maxIters = 50

    # Bedziemy uczyc na polaczonym zbiorze treningowym i walidacyjnym
    new_tr_set = rbind(training_set, validation_set)
    best_tr = best_cmac$tr / 2
    best_cmac = create.cmac(form, qq, best_cmac$nLayers, nHashBits, attrDescs)

    # Czyscimy pamiec
    gc()
    best_cmac = train.cmac(best_cmac, new_tr_set, targetMse, 
        best_tr, maxIters)

    info("Choosing best SVM")
    best_svm = best.svm(form, data = new_tr_set)
    
    info("Choosing best RandomForest")
    best_rf = best.randomForest(form, data = new_tr_set)
    
    info("Creating linear regression model")
    best_lm = lm(form, data = new_tr_set)

    # Blad sredniokwadratowy na testowym zbiorze
    mse_tst_cmac = mse(predict(best_cmac, test_set), test_set$richter)
    mse_tst_svm = mse(predict(best_svm, test_set), test_set$richter)
    mse_tst_rf = mse(predict(best_rf, test_set), test_set$richter)
    mse_tst_lm = mse(predict(best_lm, test_set), test_set$richter)
    
    cat("<<< mse on TEST SET >>>\n")
    cat("for CMAC: ", mse_tst_cmac, "\n")
    cat("for SVM: ", mse_tst_svm, "\n")
    cat("for RandomForest: ", mse_tst_rf, "\n")
    cat("for LM: ", mse_tst_lm, "\n")
}

test_stocks  = function () {
    qq = read.table("data/stockprices.data", header=TRUE)

    # Permutuj losowo
    qq = resample(qq)

    # Utworz zbior treningowy
    training_set = qq[1:216,]

    # Utworz zbior walidacyjny
    validation_set = qq[217:434,]
    
    # Utworz zbior testowy
    test_set = qq[435:916,]

    form = company10 ~ .
    formTerms = terms(form, data=qq)
    cmac_modelVars = attr(formTerms, "term.labels")
    mins = sapply(qq[cmac_modelVars], min)
    maxes = sapply(qq[cmac_modelVars], max)

    # liczba warstw
    nHashBits = 20

    # liczba pozadanych podzialow dziedziny atrybutu
    nDiv = list(
        company1 = 60, 
        company2 = 60, 
        company3 = 20, 
        company4 = 35, 
        company5 = 80, 
        company6 = 30, 
        company7 = 50, 
        company8 = 20, 
        company9 = 30)

    
    nLayersList = 8
    trainingRates = rev(nLayersList / 80)
    
    maxIters = 40
    targetMse = 0.5

    minMse = 1000000000
    best_cmac = NULL
    

    info("Choosing best CMAC by cross validation")
    for (i in 1:length(nLayersList)) {
        nLayers = nLayersList[i]
        trainingRate = trainingRates[i]
        info("Training for ", nLayers, " layers, tr = ", trainingRate, " maxIters = ", maxIters)

        attrDescs = list()
        for (varName in cmac_modelVars) {
            min = mins[[varName]]
            max = maxes[[varName]]
            attrDescs[[varName]] = list(min = min, max = max, nDiv = floor(nDiv[[varName]] / nLayers))
        }

        cmac_model = create.cmac(form, qq, nLayers, nHashBits, attrDescs)

        cmac_model = train.cmac(cmac_model, training_set, targetMse, trainingRate, maxIters)
        current_mse = mse(predict(cmac_model, validation_set), validation_set$company10)
        info("mse on validation set = ", current_mse)
        if (current_mse < minMse) {
            best_cmac = cmac_model
            minMse = current_mse
        }
    }
    info("Best CMAC has ", best_cmac$nLayers, " layers")
    info("Training CMAC on training set + val_set")
    maxIters = 50

    # Bedziemy uczyc na polaczonym zbiorze treningowym i walidacyjnym
    new_tr_set = rbind(training_set, validation_set)
    best_tr = best_cmac$tr / 2
    best_cmac = create.cmac(form, qq, best_cmac$nLayers, nHashBits, attrDescs)

    # Czyscimy pamiec
    gc()
    best_cmac = train.cmac(best_cmac, new_tr_set, targetMse, 
        best_tr, maxIters)

    info("Choosing best SVM")
    best_svm = best.svm(form, data = new_tr_set)
    
    info("Choosing best RandomForest")
    best_rf = best.randomForest(form, data = new_tr_set)
    
    info("Creating linear regression model")
    best_lm = lm(form, data = new_tr_set)

    # Blad sredniokwadratowy na testowym zbiorze
    mse_tst_cmac = mse(predict(best_cmac, test_set), test_set$company10)
    mse_tst_svm = mse(predict(best_svm, test_set), test_set$company10)
    mse_tst_rf = mse(predict(best_rf, test_set), test_set$company10)
    mse_tst_lm = mse(predict(best_lm, test_set), test_set$company10)
    
    cat("<<< mse on TEST SET >>>\n")
    cat("for CMAC: ", mse_tst_cmac, "\n")
    cat("for SVM: ", mse_tst_svm, "\n")
    cat("for RandomForest: ", mse_tst_rf, "\n")
    cat("for LM: ", mse_tst_lm, "\n")
}

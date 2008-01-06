test.appr = function(df, trains.size, tests.size) {
    # Permutuj losowo zbior przykladow
    df.perm = df[sample(nrow(df)),]
    trs = df[1:trs.size,]
    tss = df[trs.size + 1:trs.size + 1 + tss.size,]

}

pred.mse = function(model, testdf, targetf) {
    prediction = predict(model, testdf);
    mean((prediction -  testdf$targetf) ^ 2) 
}

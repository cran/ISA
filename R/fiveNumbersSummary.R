`fiveNumbersSummary` <-
function(vx){

minimo <- min(vx);
massimo <- max(vx);
mediana <- as.numeric(quantiB(vx,prob=0.5))
primoQuart <- as.numeric(quantiB(vx, prob=0.25))
terzoQuart <- as.numeric(quantiB(vx, prob=0.75))

c(
minimo =minimo,
quant0.25 = primoQuart,
mediana=mediana,
quant0.75 = terzoQuart,
massimo=massimo
)

}


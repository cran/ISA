`quantiB` <-
function(vettore, prob=0.5){
res <- rep(NA,length(prob));
dime <- length(prob);
vS <- sort(vettore) 
atomi <-  unique(vS) 
frequeCumu <- (1:length(vettore))/length(vettore);
for(aux in 1:dime){
estrattore <- frequeCumu >= prob[aux]
res[aux]  <- min(vS[estrattore])
}
 
names(res )<- paste("q:",prob,sep="")
res
}


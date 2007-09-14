`boxPstd` <-
function(vetto,displace=1.5){

quantili <- quantiB(vetto, prob=c(0.25,0.5,0.75));

baffoInf <- quantili[1]- displace*(quantili[2]-quantili[1]);
baffoSup <- quantili[3]+ displace*(quantili[3]-quantili[2]);
bordo <-     (baffoSup-  baffoInf )/25
estremi <- c(min(c(vetto,baffoInf) )-bordo,max(c(vetto, baffoSup))+bordo);
oltreSup <- vetto > baffoSup; 
sottoInf <- vetto < baffoInf; 

plot(NA, xlab="", ylab="",xlim=c(-2,2),ylim= c(estremi),type="n",
    xaxt="n")
lines(c(-0.5,-0.50), c(quantili[1],quantili[3]),lwd=1.2)
lines(c(0.5,0.5), c(quantili[1],quantili[3]),lwd=1.2)
lines(c(-0.5,0.5), c(quantili[2],quantili[2]),lwd=1.2)
lines(c(-0.5,0.5), c(quantili[1],quantili[1]),lwd=1.2)
lines(c(-0.5,0.5), c(quantili[3],quantili[3]),lwd=1.2)

# baffi
lines(c(0,0), c(quantili[1],baffoInf),lwd=1.2)
lines(c(-0.25,0.25), c(baffoInf,baffoInf),lwd=1.2)
lines(c(0,0), c(quantili[3],baffoSup),lwd=1.2)
lines(c(-0.25,0.25), c(baffoSup,baffoSup),lwd=1.2)

if(sum(sottoInf)>0){
    punti <- vetto[sottoInf]
    points( rep(0,length(punti)),punti ,cex=1.2)
   }

if(sum(oltreSup) > 0){
    punti <- vetto[oltreSup]
    points( rep(0,length(punti)),punti ,cex=1.2)
   }   
}


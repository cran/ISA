`difSempliceMed` <-
function(vetto){ 
dimensione   <-  length(vetto) 
differenze  <-  outer(vetto,vetto,"-") 
valoreassoluto   <-  abs(differenze) 
somma  <-  sum(c(valoreassoluto)) 
denominatore  <-  dimensione^2 - dimensione 
risultato  <-  somma /denominatore 
risultato 
}


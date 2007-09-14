`moda` <-
function(vect){  
    freq  <-  table(vect)   
    flag  <-  max(freq)  
    flag2  <-  (freq == flag[1])  
    table(vect)[flag2]/length(vect)  
}


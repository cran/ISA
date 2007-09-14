`FLinearMod` <-
function(Y,X){

XTX <- t(X) %*% X
if(det(XTX) == 0) stop("Matrice XTX singolare!")

XTY <- t(X)%*% Y

XTXM1 <- solve(XTX)
beta <- XTXM1 %*% XTY
mH <- X %*% XTXM1 %*%t(X)
mIH <- diag(rep(1,length(c(Y)))) - mH

HY <- mH %*% Y
residui <- mIH %*% Y


variaStim <- sum(residui^2)/(nrow(X)-ncol(X))


list(
Y= Y,
X = X,
XTX = XTX,
detXTX = det(XTX),
inversaXTX = XTXM1,
XTY = XTY,
beta=beta,
H = mH,
IH = mIH,
attesa = HY,
residui = residui,
varianzaStimata =variaStim
)

}


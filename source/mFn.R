ddM<-function(wt,par){ 
  par["m1"]%*%(wt%^%par["m2"])}

M1Fn=function(x,y=0.025) FLQuant(y,dimnames=dimnames(m(x))) 
M2Fn=function(x) m(x) - M1Fn(x)

forage=function(x) 
  apply(stock.wt(x)%*%stock.n(x)%*%(m(x)-M1Fn(x))%/%(z(x))%*%(1-exp(-z(x))),c(2,6),sum)


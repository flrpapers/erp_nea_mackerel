eqFn=function(x,s=0.7,sd=0.3,eq=TRUE,nyear=3){
  
  ## With prior for steepness
  sr =as.FLSR(x,model="bevholtSV") 
  sr =FLCandy:::ftmb(sr,s.est=T,s=s,s.logitsd=sd,spr0=mean(spr0Yr(x)))
  params(sr)=FLPar(apply(params(sr),1,mean))
  #scaling=exp(mean(residuals(sr)[,ac(2001:2020)])) 
  #params(sr)["a"]=params(sr)["a"]*scaling
  
  srs=FLSRs("Prior"=sr)
  
  ## Fix steepness
  sr=as.FLSR(x,model="bevholtSV") 
  
  #sr=fmle(sr, fixed=list(s=0.9,spr0=mean(spr0Yr(iter(x,1)))),
  #        control=list(silent=TRUE), method="L-BFGS-B",
  #        upper=1e+20,
  #        lower=1e-12)
  #scaling=exp(mean(residuals(sr)[,ac(2001:2020)])) 
  #sr=ab(sr)
  #params(sr)["a"]=params(sr)["a"]*scaling
  
  sr =as.FLSR(x,model="bevholtSV") 
  sr =FLCandy:::ftmb(sr,s.est=T,s=0.9,s.logitsd=0.01,spr0=mean(spr0Yr(x)))
  params(sr)=FLPar(apply(params(sr),1,mean))
  #scaling=exp(mean(residuals(sr)[,ac(2001:2020)])) 
  #params(sr)["a"]=params(sr)["a"]*scaling
  
  srs["h=0.9"]=sr
  
  sr=srrTMB(as.FLSR(x, model=bevholtDa), spr0=spr0y(x))
  sr=fmle(  as.FLSR(x, model=bevholtDa), fixed=list(d=1.5),control=list(silent=TRUE))
  #print(plot(sr))
  #scaling=exp(mean(residuals(sr)[,ac(2001:2020)])) 
  #params(sr)["a"]=params(sr)["a"]*scaling
  
  #params(sr)[c("a","b")]=params(srs[["Prior"]])[c("a","b")]
  #params(sr)[c("d")]=1.5
  
  srs["Depensation"]=sr
  
  if (eq){
    eqs=FLBRPs("Prior"      =FLBRP(x,nyear=nyear,sr=srs[["Prior"]]),
               "h=0.9"      =FLBRP(x,nyear=nyear,sr=srs[["h=0.9"]]),
               "Depensation"=FLBRP(x,nyear=nyear,sr=srs[["Depensation"]]))
    fbar(eqs[["Depensation"]])=fbar(eqs[["h=0.9"]])
    eqs  
  }else srs}

msyFn<-function(x){
  
  fbar(x)[]=0.1
  stk=as(x,"FLStock")
  
  fn<-function(f,stk,sr){
    ftarget=FLQuant(f,dimnames=dimnames(fbar(stk)[,-(1:2)]))
    rtn    =ffwd(stk,fbar=ftarget,sr=sr)
    catch(rtn)[,dim(rtn)[2]]}
  
  fmsy=optimise(fn,c(0.001,0.3),maximum=TRUE,stk=stk,sr=x)
  
  ftarget=FLQuant(fmsy$maximum,dimnames=dimnames(fbar(stk)[,-seq(5)]))
  rtn    =ffwd(stk,fbar=ftarget,sr=x)
  
  FLPar(model.frame(FLQuants(rtn[,dim(rtn)[2]],
                             Catch  =catch,SSB     =ssb,
                             Biomass=stock,Recruits=rec,
                             F      =fbar, Forage  =forage),drop=T))}

vrgFn<-function(x){
  
  fbar(x)[]=1e-20
  stk      =as(x,"FLStock")
  rtn      =fwd(stk,fbar=fbar(x)[,-(1:5)],sr=x)
  
  FLPar(model.frame(FLQuants(rtn[,dim(rtn)[2]],
                             SSB     =function(x) ssb(x),
                             Biomass =function(x) stock(x),
                             Recruits=function(x) rec(x),
                             Forage  =function(x) forage(x)),drop=T))}

  
prdFn<-function(fmsy,x,nits=dim(stk)[2]){
  
  stk=as(x,"FLStock")
  stk=propagate(stk,nits)
  
  ftarget=FLQuant(rep(seq(0,3,length.out=dim(stk)[2]),each=nits),
                  dimnames=dimnames(fbar(stk)))[,-(1:2)]*fmsy
  
  rtn    =ffwd(stk,fbar=ftarget,sr=x)
  
  model.frame(FLQuants(rtn[,dim(rtn)[2]],Catch=catch,SSB=ssb,Biomass=stock,Recruits=rec,F=fbar,Forage=forage),drop=T)}


abiFn<-function(fmsy,x,p=0.9){
  
  stk  =as(x,"FLStock")
  stk  =ffwd(stk,fbar=fbar(stk)[,-(1:3)]%=%fmsy,sr=x)[,dim(stk)[2]]
  cumN =apply(stock.n(stk),c(2,6),cumsum)%/%quantSums(stock.n(stk))
  ages =ages(stock.n(stk))
  
  ages[cumN<=p]=NA
  A=apply(ages,c(2:6),function(y) min(c(y+1,dims(x)$max),na.rm=T))
  
  ABIstock(stk,A)}




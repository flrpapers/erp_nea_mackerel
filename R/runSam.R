runSam<-function(scenario="nm",dir){
    
  ################################################################################
  ## Get ICES 2022 assessment data inputs ########################################
  ################################################################################
  
  sw<-read.ices(file.path(dir,"sw.dat"))
  pm<-read.ices(file.path(dir,"pm.dat"))
  pf<-read.ices(file.path(dir,"pf.dat"))
  nm<-read.ices(file.path(dir,paste(scenario,"dat",sep=".")))
  mo<-read.ices(file.path(dir,"mo.dat"))
  lw<-read.ices(file.path(dir,"lw.dat"))
  lf<-read.ices(file.path(dir,"lf.dat"))
  dw<-read.ices(file.path(dir,"dw.dat"))
  cw<-read.ices(file.path(dir,"cw.dat"))
  cn<-read.ices(file.path(dir,"cn.dat"))
  surveys<-read.ices(file.path(dir,"survey.dat"))
  
  
  recap<-read.table(file.path(dir,"tag_steel.dat"), header=TRUE)
  recap<-recap[recap$Type==1 & recap$RecaptureY<=2006,]
  recap<-recap[recap[,1]>=min(as.numeric(rownames(sw))), ]
  
  recap2<-read.table(file.path(dir,"tag_RFID.dat"), header=TRUE)
  recap2$r<-round(recap2$r)
  recap2<-recap2[recap2$Nscan>0,]
  age <- recap2$ReleaseY - recap2$Yearclass
  # subset of the data according to changes made in IBP 2019
  recap2 <- recap2[age>4,]
  recap2 <- recap2[(recap2$RecaptureY-recap2$ReleaseY) %in% c(1,2), ]
  recap2 <- recap2[recap2$ReleaseY>=2013, ]
  
  recap<-rbind(recap,recap2)
  
  age<- recap$ReleaseY - recap$Yearclass
  recap  <- recap[age>1 & age<12,]     # possibly test id <13 ok???
  
  #### Remove the tags recapture in the release year
  recap <- recap[recap$ReleaseY!=recap$RecaptureY,]
  
  # remove scans of catch in the current Y
  recap <- recap[recap$RecaptureY!=2022,]     
  
  W<-matrix(NA,nrow=nrow(cn), ncol=ncol(cn))
  W[as.numeric(rownames(cn))<2000]<-10
  attr(cn,"weight")<-W
  
  recap$R <- as.numeric(recap$R)
  
  dat<-setup.sam.data(surveys=surveys,
                      residual.fleet=cn, 
                      prop.mature=mo, 
                      stock.mean.weight=sw, 
                      catch.mean.weight=cw, 
                      dis.mean.weight=dw, 
                      land.mean.weight=lw,
                      prop.f=pf, 
                      prop.m=pm, 
                      natural.mortality=nm, 
                      land.frac=lf,
                      recapture=recap)
  
  # read in model configuration --------------------------------------------------
  conf=defcon(dat)
  conf$keyLogFsta[1,] <- c(0,1,2,3,4,5,6,7,7,7,7,7,7)
  conf$keyVarObs[4,]      <- c(-1,-1,-1, 3,4,4,4,4,4,4,4,4,-1)
  conf$fbarRange <- c(4,8)
  conf$corFlag <- 0
  conf$fixVarToWeight<-1
  conf$obsCorStruct[] <- c("ID", "ID", "ID", "AR", "ID")
  #conf$keyCorObs[4,7:11]<-0
  conf$keyCorObs[4,]<-c(-1 ,-1, -1 , 0  ,0 , 0,  0,  0,  0,   0 ,  0 , -1)
  
  conf$keyVarF[1,]    <- c(0,1,rep(2,6),rep(-1,5))
  
  conf$keyVarObs[1,]   <- c( 0,1,rep(2,11) )
  conf$keyVarObs[2,1]  <- 3
  conf$keyVarObs[3,1]  <- 4
  conf$keyVarObs[4,]   <- c(-1, -1, -1,  5, 6,  6,  6,  6, 6,  6,  6,   6,  -1 )
  
  par<-defpar(dat,conf)
  # model fitting ----------------------------------------------------------------
  #sam.ices <-sam.fit(dat,conf,par, newtonsteps=0, map=list(logitRecapturePhi=factor(c(1,1))))
  # don't forget to remove the newtonsteps = 0
  
  sink(file=tempfile())
  sam.ices <-sam.fit(dat,conf,par,  map=list(logitRecapturePhi=factor(c(1,1))))
  sink(file=NULL)
  
  sam.ices$data$landMeanWeight =sam.ices$data$landMeanWeight[ ,,1,drop=T]
  sam.ices$data$catchMeanWeight=sam.ices$data$catchMeanWeight[,,1,drop=T]
  sam.ices$data$landFrac       =sam.ices$data$landFrac[,,1,drop=T]
  sam.ices$data$disMeanWeight  =sam.ices$data$disMeanWeight[,,1,drop=T]
  sam.ices$data$propF          =sam.ices$data$propF[,,1,drop=T]          
  
  sam=window(SAM2FLStock(sam.ices),end=2020)
  sam=sam+VPA(sam)
  
  list(sam,sam.ices)}
  
#' =============================================================================
#' Name   : BSP Outlier ResearchL Outlier Types plot
#' Author : Joe W. Byers
#' Date: 4/3/17
#' Modifed :
#' Version: 1.0001
#' Mail   : <<<ecjbosu@aol.com>>>
#' =============================================================================
#'
#set up enviroment and libraries
require(xts)
require(lubridate)
library(PerformanceAnalytics)
require(dplyr)
library(tsoutliers)
require(grDevices)
require(plotrix)
require(fBasics)
library(tseries)

setSource <- function (srcPath=getwd(), srcDirs, include.dirs=T ){

  #set sourced directories
  #put in a system cache database

  if (missing(srcDirs)) {
    srcDirs <- list.files(srcPath, pattern = '^[+]', all.files=F, full.names=F,
                          include.dirs=include.dirs);
  }

  for (i in 1:length(srcDirs)) {
    sapply(list.files(file.path(srcPath, srcDirs[i]), pattern='^[^\\+]', all.files = F,
                      full.names = T, include.dirs=F), source, echo=F);
  }


}

setSource(srcPath='', srcDirs=sub('scripts','sources',getwd()));
#setJarClasses()

#####Input section##############################################################
maxcurve<-60;

rdatadir ='/Share/NAS/work/RData/';
outdir  <- "/home/byersjw/data/";  #changed to data from data2 10/17/19
plotdir <- '/home/byersjw/data/simExample/';
First.Date <-as.POSIXlt('2010-01-01', tz='UTC');
Last.Date  <-"2017-03-20"; # this was the last date that this analysis was
#build zoo time series object, easier to do some things like this pivot
# retZoo <- read.zoo(file=out[, c(1,3,5)], split="ContractQuoteName");
# retZoo <- lapply(retZoo, FUN=Return.calculate, method="log");

#If return skip this and load pr file
### Random 100 days
# set.seed(20170321)
# Z0 <- rchisq(101, df = 1, ncp = 1.)
# hist(log(Z0)-lag(log(Z0)))
# View(as.data.frame(log(Z0)-lag(log(Z0))))
# 
# View(as.data.frame(Z0))

pr = data.frame(Clean=rnorm(100, sd=.25/sqrt(252)),
  row.names=seq(First.Date, by='day', length.out=100))
# to get second path run code below and then execute here.
pr$Clean = prr[[8]]
################################################################################
# load file
 pr1 <- read.csv("/home/byersjw/data/simExample/OutlierTypesExampleandPlotData.csv") #see data data2 above
 row.names(pr1) <- pr1$X;
 pr <- pr1[,-1];
################################################################################
#set.seed(20170321);
#n.start= pr$Clean[17:45] + c(rep(0,1,28),.7) #pr$Clean[1:45]+c(rep(0,1,44),.7)
#IOsim=arima.sim(n=20, list(ar=c(.8,-.5,-.2), ma=(-.5), order=c(3,1,1)),mean=.1, sd=.0025)#, 
#                start.innov=n.start, n.start=3)#, innov=pr$Clean[45:65])
IOsim = c(0.7174858018094946,-0.698402899022722,0.4661042463529083,-0.48035879561738454,
0.2186588938669538,-0.25036210268457276,0.09169920205853166,-0.10351667251354572,0.04446668944732056,-0.02279495518402847,0.0015160617396463051,-0.007979351997957412,0.026791732181872273,0.01099641696527596,0.0059798358091518065,-0.0031254257093507767,0.019353944833791995,-0.01465881173863515,0.0004251051487542707,-0.016582621016139273)
pr$AO <- pr$Clean;
pr$AO[33] <- pr$AO[33]+.8; pr$AO[67] <- pr$AO[67]-.8;
pr$TC <- pr$Clean; pr$TC[45] <- pr$TC[45]+.0; pr$TC[45:64] <- pr$TC[45:64]-.7^seq(1:20);
#pr$TC <- pr$Clean; pr$TC[45:65] <- pr$TC[45:65]+.1; pr$TC[65] <- pr$TC[65]-.1;
pr$LS <- pr$Clean; pr$LS[45:100] <- pr$LS[45:100]+.1;
pr$IO <- pr$Clean; pr$IO[46:65]=pr$IO[46:65]+as.numeric(IOsim[1:20])

#pr<-pr[,c(1:2,4:5, 3)]
summary.pr <- table.Stats(pr)
summary.pr[length(rownames(summary.pr))+1,] <- summary.pr[rownames(summary.pr)=='Stdev',]*sqrt(252);
rownames(summary.pr)[length(rownames(summary.pr))] <- 'Annualized.Stdev'
jb.test           <- do.call(rbind.data.frame, lapply(pr, jarque.bera.test));
jb.test           <- jb.test[,c(1,3)];
names(jb.test)[1] <-"Jarque Beta Test Contaminated"

sw.test           <- lapply(pr, shapiroTest);
sw.test           <- do.call(rbind.data.frame,lapply(sw.test,
 FUN=function(x) x@test));
sw.test           <- sw.test[,c(1,2)];
names(sw.test)[1] <-"Shapiro Wilk Statistic Contaminated"

ll = zoo(pr[,c(2:5)],order.by=as.numeric(strftime(rownames(pr), format="%j")));

plot(ll, type="l", col="black", main = 'Outlier Types',
  xlab=paste('AO:Additive', 'IO:Innovative','TC:Temporary', 'LS:Level Shift', sep=' | '));



###model selection parameters
maxit.iloop=30;
maxit.oloop=35;
remove.method="bottom-up";
cval = 3.5;

tmp.pr=list();
ar0=list();
ar1=list();
sigma.pr <- data.frame(Model=NULL, Clean=NULL, AO=NULL, IO=NULL, LS=NULL, TC=NULL);
for (i in seq(1:5)){
  types=c("AO", "LS", "TC","IO");
  if(i==5) types=c("LS", "IO", "TC");
tmp.pr[[i]] <- tso(ts(pr[,i]), tsmethod = c("auto.arima"),
            discard.method=remove.method, cval=3.5,
            maxit.oloop=maxit.oloop, maxit.iloop=maxit.iloop,
            args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE),
            types=types)
ar0[[i]]=forecast::auto.arima(pr[,i], allowmean=F,allowdrift=F, max.p=0, max.q=0,max.d=0)
ar1[[i]]=forecast::auto.arima(pr[,i], allowmean=T,allowdrift=T)
sigma.pr[1,i] <- tmp.pr[[i]]$fit$sigma2;
sigma.pr[2,i] <- sqrt(tmp.pr[[i]]$fit$sigma2);
sigma.pr[3,i] <- tmp.pr[[i]]$fit$loglik;
sigma.pr[4,i] <- ar0[[i]]$sigma2;
sigma.pr[5,i] <- sqrt(ar0[[i]]$sigma2);
sigma.pr[6,i] <- ar0[[i]]$loglik;
sigma.pr[7,i] <- ar1[[i]]$sigma2;
sigma.pr[8,i] <- sqrt(ar1[[i]]$sigma2);
sigma.pr[9,i] <- ar1[[i]]$loglik;
sigma.pr[10,i] <- mean(tmp.pr[[i]]$fit$residuals);
sigma.pr[11,i] <- sqrt(tmp.pr[[i]]$fit$sigma2);
sigma.pr[12,i] <- sqrt(tmp.pr[[i]]$fit$sigma2*252);
}
tmp.pr

#prettyr sigma.pr
colnames(sigma.pr)=names(pr);
sigma.pr$Model=c('TSO', '', '', 'AR0', '', '', 'AR1', '', '','Outlier Corrected','','');
sigma.pr$Stat= c('MSE', 'RMSE', 'Log-L');
sigma.pr$Stat[10]='Arithmetic Mean'
sigma.pr$Stat[11]='Standard Deviation'
sigma.pr$Stat[12]='Annualized Standard Deviation'
sigma.pr <- sigma.pr[,c(6,7,1:5)];

#AIC/BIC
aicdf=data.frame(t(mapply(x=ar0, y=ar1, z=tmp.pr, FUN=function(x,y,z) {
  tmp = t(AIC(x, y, z$fit))
  tmp = c(tmp[1,], tmp[2,])
  return(tmp)
  }
  )));
rownames(aicdf)<-names(pr);
colnames(aicdf)<- c('df.AR0', 'df.Auto.AR','df.TSO', 'AR0', 'Auto.AR', 'TSO');

#aicdf[6,1]=list(c('TSO', ))

bicdf=data.frame(t(mapply(x=ar0, y=ar1, z=tmp.pr, FUN=function(x,y,z) {
  tmp = t(BIC(x, y, z$fit))
  tmp = c(tmp[1,], tmp[2,])
  return(tmp)
}
)));
rownames(bicdf)<-names(pr);
colnames(bicdf)<- c('df.AR0', 'df.Auto.AR','df.TSO', 'AR0', 'Auto.AR', 'TSO');


resids <- do.call(cbind.data.frame, lapply(tmp.pr, FUN=function(x) x$fit$residuals));
names(resids) <- names(pr);

jb1.test           <- do.call(rbind.data.frame, lapply(resids, jarque.bera.test));
jb1.test           <- jb1.test[,c(1,3)];
names(jb.test)[1] <-"Jarque Beta Statistic"

sw1.test           <- lapply(resids, shapiroTest);
sw1.test           <- do.call(rbind.data.frame,lapply(sw1.test,
                                                     FUN=function(x) x@test));
sw1.test           <- sw1.test[,c(1,2)];
names(sw1.test)[1] <-"Shapiro Wilk Statistic"

normtests <- rbind(t(jb.test),t(sw.test),t(jb1.test),t(sw1.test))




arma0 <- do.call(rbind.data.frame, lapply(ar0, FUN=function(x) x$arma));
names(arma0) <- c('AR','MA','SAR','SMA','Period','NSD','SD');
rownames(arma0)<-names(pr);
arma0<- arma0[,c(1:2,5:6)]
arma1 <- do.call(rbind.data.frame, lapply(ar1, FUN=function(x) x$arma));
names(arma1) <- c('AR','MA','SAR','SMA','Period','NSD','SD');
rownames(arma1)<-names(pr);
arma1<- arma1[,c(1:2,5:6)]
armaTSO <- do.call(rbind.data.frame, lapply(tmp.pr, FUN=function(x) x$fit$arma));
names(armaTSO) <- c('AR','MA','SAR','SMA','Period','NSD','SD');
rownames(armaTSO)<-names(pr);
armaTSO<- armaTSO[,c(1:2,5:6)]

library(xtable)
## show sigma2 in sigma.pr
print(xtable(summary.pr, digits=5), type='latex')
print(xtable(data.frame(Statistic=rownames(normtests), normtests, row.names=NULL), digits=5, include.rownames=F), 
      type='latex', include.rownames=F);

print(xtable(sigma.pr , digits=5), type='latex', include.rownames=FALSE)
print(xtable(t(aicdf), type='latex'))
print(xtable(t(bicdf), type='latex'))
print(xtable(data.frame(Model=rownames(t(cbind(arma0,arma1,armaTSO))),t(cbind(arma0,arma1,armaTSO)), row.names=NULL),include.rownames=FALSE , type='latex'),include.rownames=FALSE)

lapply(ar1, FUN=function(x) {
  if(length(x$coef)>0) {
    cat('coef', '\n');
    print(x$coef)
    cat('std.errors', '\n');
    print(sqrt(diag(x$var.coef)))};
   });

################################################################################
#Plot POC to use.
#set image output, display or png file
toimage <- F;
if (!toimage) {
  tline = -2;
  inst  = c(0,-.27);
} else {
  tline = -2;
  inst  = c(0,-.15);
}

for (i in seq(1:length(tmp.pr))) {
  if (dim(tmp.pr[[i]]$outliers)[1]!=0) {
    graph1 <- paste(colnames(pr)[i],"1",'png', sep='.')

    #if (!toimage) dev.new();
    if (toimage) png(file.path(plotdir,graph1),
                     width = 652, height = 478);
    oldPar <-par(xpd = NA, mar = par()$mar + c(3,0,0,0)); #mar = par()$mar + c(0,0,0,7) , oma = c(5,0,0,0)
    plot(tmp.pr[[i]]) ;#,args.x.axis=args.x.axis)
    title(paste('Outliers and effects for ', colnames(pr)[i]), outer=T, line=-2)
    #close device
    if (toimage) ret<-dev.off();
    #reset par
    par(oldPar);
  }
}

library(lmtest)

lapply(tmp.pr[[2:5]], FUN=function(x) coeftest(x$fit))

coeftest(tmp.pr[[2]]$fit)
coeftest(tmp.pr[[3]]$fit)
coeftest(tmp.pr[[4]]$fit)
coeftest(tmp.pr[[5]]$fit)
coeftest(ar1[[2]])
coeftest(ar1[[3]])
coeftest(ar1[[4]])
coeftest(ar1[[5]])
write.csv(pr, "/home/byersjw/data2/simExample/OutlierTypesExampleandPlotData_IO_filter.csv")

#pull sims out of tmp.pr since did not save seed so we can save pr.
pr$Clean <- tmp.pr[[1]]$y
pr$AO <- tmp.pr[[2]]$y
pr$IO <-  tmp.pr[[3]]$y
pr$TC <-  tmp.pr[[4]]$y
pr$LS <- tmp.pr[[5]]$y

#set.seed(12500);
#IOsim=arima.sim(n=20, list(ar=c(.4, .3), ma=(.5), order=c(2,1,1)),mean=0, sd=.15/sqrt(252))#, innov=pr$Clean[45:65])
set.seed(.Random.seed[51]);
n.start= pr$Clean[17:45] + c(rep(0,1,28),.7) #pr$Clean[1:45]+c(rep(0,1,44),.7)
IOsim=arima.sim(n=20, list(ma=c(-.5), order=c(0,0,1)),mean=0, sd=.025)#, 
plot(IOsim)
remove(prio)
prio<-pr
prio=prio[,1]
prio[46:65]=prio[46:65]+as.numeric(rev(IOsim)[1:20])
io.tso=tso(ts(pr[,5]), tsmethod = c("auto.arima"),
        discard.method=remove.method, cval=3.5,
        maxit.oloop=maxit.oloop, maxit.iloop=maxit.iloop,
        args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE),
        types=c( "LS", "TC","IO","AO"))
plot(io.tso)

jarque.bera.test(io.tso$fit$residuals)

tso(ts(prio), tsmethod = c("auto.arima"),
    discard.method=remove.method, cval=3.5,
    maxit.oloop=10, maxit.iloop=16,
    args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE),
    types=c( "LS", "TC","IO","AO"))


#random walk test
set.seed(20170321)
prr = lapply(1:100, FUN=function(x) rnorm(100, sd=.25/sqrt(252)))
prrr = as.numeric(cbind(lapply(prr, FUN=sd)))*sqrt(252)
mean(prrr)



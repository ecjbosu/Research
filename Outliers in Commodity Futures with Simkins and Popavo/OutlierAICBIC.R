#' =============================================================================
#' Name   : BSP Outlier Research build model estimate report
#' Author : Joe W. Byers
#' Date: 4/5/17
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

rdatadir ='/Share/NAS/work/RData/';
outdir  <- "/home/byersjw/data2/";
plotdir <- '/home/byersjw/data2/';
filenm <- 'tsoout2.RData'; # 2 must match above directories
First.Date <-'2000-01-01';
Last.Date  <-"2017-03-20"; # this was the last date that this analysis was
# executed.  For replications
first.yr <- 2007;
#ticker <- paste(c("NG","CL","GC"),collapse="','");
commodity <- "CL"

load(file.path(rdatadir, paste(commodity,filenm,sep='')));

# tmp.pr=list();
# ar0=list();
# ar1=list();
# sigma.pr <- data.frame(Model=NULL, Clean=NULL, AO=NULL, IO=NULL, LS=NULL, TC=NULL);
# for (i in seq(1:5)){
#   types=c("AO", "LS", "TC","IO");
#   if(i==5) types=c("LS", "IO");
# tmp.pr[[i]] <- tso(ts(pr[,i]), tsmethod = c("auto.arima"),
#             discard.method=remove.method, cval=3.5,
#             maxit.oloop=maxit.oloop, maxit.iloop=maxit.iloop,
#             args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE),
#             types=types)
# ar0[[i]]=forecast::auto.arima(pr[,i], allowmean=F,allowdrift=F, max.p=0, max.q=0,max.d=0)
# ar1[[i]]=forecast::auto.arima(pr[,i], allowmean=T,allowdrift=T)
# sigma.pr[1,i] <- tmp.pr[[i]]$fit$sigma2;
# sigma.pr[2,i] <- sqrt(tmp.pr[[i]]$fit$sigma2);
# sigma.pr[3,i] <- tmp.pr[[i]]$fit$loglik;
# sigma.pr[4,i] <- ar0[[i]]$sigma2;
# sigma.pr[5,i] <- sqrt(ar0[[i]]$sigma2);
# sigma.pr[6,i] <- ar0[[i]]$loglik;
# sigma.pr[7,i] <- ar1[[i]]$sigma2;
# sigma.pr[8,i] <- sqrt(ar1[[i]]$sigma2);
# sigma.pr[9,i] <- ar1[[i]]$loglik;
# sigma.pr[10,i] <- mean(tmp.pr[[i]]$fit$residuals);
# sigma.pr[11,i] <- sqrt(tmp.pr[[i]]$fit$sigma2);
# sigma.pr[12,i] <- sqrt(tmp.pr[[i]]$fit$sigma2*252);
# }
# tmp.pr
# 
# #prettyr sigma.pr
# colnames(sigma.pr)=names(pr);
# sigma.pr$Model=c('TSO', '', '', 'AR0', '', '', 'AR1', '', '','Outlier Corrected','','');
# sigma.pr$Stat= c('MSE', 'RMSE', 'Log-L');
# sigma.pr$Stat[10]='Arithmetic Mean'
# sigma.pr$Stat[11]='Standard Deviation'
# sigma.pr$Stat[12]='Annualized Standard Deviation'
# sigma.pr <- sigma.pr[,c(6,7,1:5)];

#AIC/BIC
aicdf=data.frame(t(mapply(x=arf, y=arfit, z=ar1, FUN=function(x,y,z) {
  tmp = t(AIC(x, y, z$fit))
  tmp = c(tmp[1,], tmp[2,])
  return(tmp)
  }
  )));
colnames(aicdf)<- c('df.AR0', 'df.Auto.AR','df.TSO', 'AR0', 'Auto.AR', 'TSO');
aicdf$Contract=rownames(aicdf)
aicdf <- aicdf[,c(7,1:6)]
aicdf$Fit1 <- aicdf$Auto.AR-aicdf$AR0;
aicdf$FitTSO <- aicdf$TSO-aicdf$AR0;
aicdf$Fit1P <- aicdf$Fit1/aicdf$AR0;
aicdf$FitTSOP <- aicdf$FitTSO/aicdf$AR0;
aicdf$Better1 <- aicdf$Fit1<=0;
aicdf$BetterTSO <- aicdf$FitTSO<=0;
a=sapply(FUN=function(x) {format(datefromContractcode(x),'%Y-%m-%d')}, rownames(aicdf), simplify=T)
rownames(aicdf) <- a;
table.Stats(aicdf[,c(8:11)])


bicdf=data.frame(t(mapply(x=arf, y=arfit, z=ar1, FUN=function(x,y,z) {
  tmp = t(BIC(x, y, z$fit))
  tmp = c(tmp[1,], tmp[2,])
  return(tmp)
}
)));
colnames(bicdf)<- c('df.AR0', 'df.Auto.AR','df.TSO', 'AR0', 'Auto.AR', 'TSO');
bicdf$Contract=rownames(bicdf)
bicdf <- bicdf[,c(7,1:6)]
bicdf$Fit1 <- bicdf$Auto.AR-bicdf$AR0;
bicdf$FitTSO <- bicdf$TSO-bicdf$AR0;
bicdf$Fit1P <- bicdf$Fit1/bicdf$AR0;
bicdf$FitTSOP <- bicdf$FitTSO/bicdf$AR0;
bicdf$Fit1P <- bicdf$Fit1/bicdf$AR0;
bicdf$FitTSOP <- bicdf$FitTSO/bicdf$AR0;
bicdf$Better1 <- bicdf$Fit1<=0;
bicdf$BetterTSO <- bicdf$FitTSO<=0;
a=sapply(FUN=function(x) {format(datefromContractcode(x),'%Y-%m-%d')}, rownames(bicdf), simplify=T)
rownames(bicdf) <- a;
table.Stats(bicdf[,c(2:13)])


library(xtable)
## show sigma2 in sigma.pr
#print(xtable(summary.pr, digits=5), type='latex')
#print(xtable(data.frame(Statistic=rownames(normtests), normtests, row.names=NULL), digits=5, include.rownames=F), 
#      type='latex', include.rownames=F);

#print(xtable(sigma.pr , digits=5), type='latex', include.rownames=FALSE)
print(xtable(table.Stats(aicdf[,c(8:11)],digits=5), type='latex',digits=5))
print(xtable(table.Stats(bicdf[,c(8:11)],digits=5), type='latex',digits=5))
n=198
c(sum(aicdf$Better1), sum(aicdf$BetterTSO))
c(sum(aicdf$Better1), sum(aicdf$BetterTSO))/n
c(sum(bicdf$Better1), sum(bicdf$BetterTSO))
c(sum(bicdf$Better1), sum(bicdf$BetterTSO))/n

lapply(ar1, FUN=function(x) {
  if(length(x$coef)>0) {
    cat('coef', '\n');
    print(x$coef)
    cat('std.errors', '\n');
    print(sqrt(diag(x$var.coef)))};
   });

################################################################################

write.csv(aicdf, paste(outdir, commodity,"Outlieraic.csv", sep=''))
write.csv(bicdf, paste(outdir, commodity,"Outlierbic.csv", sep=''))


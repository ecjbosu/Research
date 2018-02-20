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
commodity <- "NG"

load(file.path(rdatadir, paste(commodity,filenm,sep='')));
     
#load ar1 if RDaa file in needed

coef  <- lapply(ar1, FUN= function(x) data.frame(x$fit$coef));
coef  <- lapply(coef, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
coef  <- do.call(rbind.data.frame, coef);
coef$Contract <- substr(rownames(coef),1,8);
serr  <- lapply(ar1, FUN= function(x) data.frame(sqrt(diag(x$fit$var.coef))));
serr  <- lapply(serr, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
serr <- do.call(rbind.data.frame, serr);
serr$Contract <- substr(rownames(serr),1,8);
sig  <- lapply(ar1, FUN= function(x) data.frame(x$fit$sigma2));
#sig  <- lapply(sig, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
sig <- do.call(rbind.data.frame, sig);
sig$Parameter <- 'SSE';
sig$Contract <- rownames(sig);
sig <- sig[,c(3,2,1)];
names(sig)[3] <- 'Estimate';
sig$StdErr <- NA;

parms <- plyr::join(coef, serr)
parms <- parms[,c(3,2,1,4)];
names(parms)[3] <- 'Estimate';
names(parms)[4] <- 'StdErr';
parms <- rbind(parms,sig);
parms <- parms[with(parms, order(parms[,1],parms[,2])),]

## save summary results
write.csv(parms, paste(outdir, commodity,
  "ModelParmEstimates.csv", sep=''))

###Initial coef
initcoef  <- lapply(arfit, FUN= function(x) data.frame(x$coef));
initcoef  <- lapply(initcoef, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
initcoef  <- do.call(rbind.data.frame, initcoef);
initcoef$Contract <- substr(rownames(initcoef),1,8);
initserr  <- lapply(arfit, FUN= function(x) data.frame(sqrt(diag(x$var.coef))));
initserr  <- lapply(initserr, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
initserr <- do.call(rbind.data.frame, initserr);
initserr$Contract <- substr(rownames(initserr),1,8);
initsig  <- lapply(arfit, FUN= function(x) data.frame(x$sigma2));
#initsig  <- lapply(initsig, FUN=function(x) cbind(x,"Parameter"=rownames(x)));
initsig <- do.call(rbind.data.frame, initsig);
initsig$Parameter <- 'SSE';
initsig$Contract <- rownames(initsig);
initsig <- initsig[,c(3,2,1)];
names(initsig)[3] <- 'Estimate';
initsig$StdErr <- NA;

initparms <- plyr::join(initcoef, initserr)
initparms <- initparms[,c(3,2,1,4)];
names(initparms)[3] <- 'Estimate';
names(initparms)[4] <- 'StdErr';
initparms <- rbind(initparms,initsig);
initparms <- initparms[with(initparms, order(initparms[,1],initparms[,2])),]

## save summary results
write.csv(initparms, paste(outdir, commodity,
                       "InitialModelParmEstimates.csv", sep=''))


###AIC/BIC checks


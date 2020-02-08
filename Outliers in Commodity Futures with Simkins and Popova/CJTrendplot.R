#' =============================================================================
#' Name   : BSP Outlier ResearchL NG Trend plot
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
setJarClasses()

datefromContractcode <- function(code) {
  #Uses parses contract code as YY.XXF where YY is the year and XX is the ticker
  yr <- as.numeric(do.call(rbind,strsplit(code, split='[.]'))[,1]);
  tcode <- substring(code, nchar(code), nchar(code));
  #  for (i in (tcode)) {
  out <- switch(tcode,
                F = as.POSIXlt(ymd(yr*10000+100+01, tz='UTC')),
                G = as.POSIXlt(ymd(yr*10000+200+01, tz='UTC')),
                H = as.POSIXlt(ymd(yr*10000+300+01, tz='UTC')),
                J = as.POSIXlt(ymd(yr*10000+400+01, tz='UTC')),
                K = as.POSIXlt(ymd(yr*10000+500+01, tz='UTC')),
                M = as.POSIXlt(ymd(yr*10000+600+01, tz='UTC')),
                N = as.POSIXlt(ymd(yr*10000+700+01, tz='UTC')),
                Q = as.POSIXlt(ymd(yr*10000+800+01, tz='UTC')),
                U = as.POSIXlt(ymd(yr*10000+900+01, tz='UTC')),
                V = as.POSIXlt(ymd(yr*10000+1000+01, tz='UTC')),
                X = as.POSIXlt(ymd(yr*10000+1100+01, tz='UTC')),
                Z = as.POSIXlt(ymd(yr*10000+1200+01, tz='UTC'))
  )#}
  return(out);
}

termStructurePromptShift <- function(obj, maxtenor) {
  if (!is.data.frame(obj)) stop('Error, obj must be a data.frame');
  out = lapply(1:nrow(obj), function(x) obj[x,][is.na(obj[x,])==F])
  out = lapply(obj,function(x) x[1:maxcurve])
  out = as.data.frame(do.call(rbind,obj));
}

#####Input section##############################################################
maxcurve<-60;

user='ecjbosu';
passwd='cHJvZmVzc29y';
host='fsealshare';
#host ='73.150.229.65';
rdatadir ='/Share/NAS/work/RData/';
outdir  <- "/home/byersjw/data2/";
plotdir <- '/home/byersjw/data2/';
First.Date <-'2000-01-01';
Last.Date  <-"2017-03-20"; # this was the last date that this analysis was
# executed.  For replications
first.yr <- 2007;
#ticker <- paste(c("NG","CL","GC"),collapse="','");
commodity <- "CJ"
driverClass <- "com.mysql.jdbc.Driver";
schema <- 'CommodityPrices';

################################################################################

#####Generate extended Inputs###################################################
ticker <- paste(c(commodity),collapse="','");

sqlstr <-paste("select TradeDate, CONTRACT, Settle as Close, ContractDate,
               ContractQuoteName, Prompt, Month, Year
               from CommodityPrices.Futures where TradeDate> '", First.Date,
               "' and TradeDate <= '",Last.Date, "' and CONTRACT in ('",
               ticker,"') and Year>=",first.yr, sep="");

################################################################################

#####Get Data, massage, and calculate returns###################################
out <- JDBC.Pull(sqlstr, host, port=3306, schema, user,
                 rsecurity()$base64decode(passwd),
                 driverClass, dbInfoFlag=F);
#rebuild ContractQuoteName to sort correctly
out$ContractQuoteName <- paste(as.numeric(
  substr(out$ContractQuoteName,4,5))+2000,
  substr(out$ContractQuoteName,1,3),sep='.');
#for some commodity need to check for 0 and Na
out <- out[!is.na(out$Settle),];
out <- out[out$Settle!=0,];
#build zoo time series object, easier to do some things like this pivot
retZoo <- read.zoo(file=out[, c(1,3,5)], split="ContractQuoteName");
retZoo <- lapply(retZoo, FUN=Return.calculate, method="log");

n= 36
pr =read.zoo(file=out[, c(1,3,5)], split="ContractQuoteName");

pt = data.frame(levels=na.omit(pr[,n])[2:length(na.omit(pr[,n]))],
      logs=log(na.omit(pr[,n])[2:length(na.omit(pr[,n]))]),
               returns=na.omit(retZoo[[n]]))


ll = na.omit(zoo(pt,order.by=as.Date(rownames(pt))));
plot(ll, type="l", col="black", main = names(retZoo)[n],
             xlab='Date');

#' =============================================================================
#' Name   : BSP Outlier Research
#' Author : Joe W. Byers
#' Date: 3/10/17
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
outdir  <- "/home/byersjw/data/";
plotdir <- '/home/byersjw/data/';
First.Date <-'2000-01-01';
Last.Date  <-"2017-03-20"; # this was the last date that this analysis was 
                           # executed.  For replications
first.yr <- 2007;
#ticker <- paste(c("NG","CL","GC"),collapse="','");
commodity <- "CL"
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

################################################################################

#####Generate Summary Statistics################################################
###Univariate Statistics
summaryStats <- do.call(rbind.data.frame,lapply(retZoo, 
          FUN= function(x) data.frame(t(table.Stats(xts(x[!is.na(x)]),digits=10)))));
summaryStats$Contract <- substr(rownames(summaryStats),1,8);

###Note: we must handle Na's since the zoo object pads Na's to normalize the 
### length of all zoo series.
#Normality test
#Jarque Bera Test
jb.test           <- do.call(rbind.data.frame, lapply(retZoo, 
  FUN = function(x) jarque.bera.test(x[!is.na(x)])));
jb.test           <- jb.test[,c(1,2,3)];
names(jb.test)    <- gsub(' ' ,'.',paste(names(jb.test), 
  "Jarque Bera Test", sep="."));
jb.test$Contract  <- rownames(jb.test);
#Shapiro-Wilk normality test
sw.test           <- lapply(retZoo, FUN = function(x) shapiroTest(x[!is.na(x)]));
sw.test           <- do.call(rbind.data.frame,lapply(sw.test, 
  FUN=function(x) x@test));
sw.test           <- sw.test[,c(1,2)];
names(sw.test)    <- gsub(' ' ,'.', 
  paste(names(sw.test), "Shapiro-Wilk normality test", sep="."));
sw.test$Contract  <- rownames(sw.test);
#Lilliefors Test
lks.test           <- lapply(retZoo, 
  FUN = function(x) lillieTest(x[!is.na(x)]));
lks.test           <- do.call(rbind.data.frame,lapply(lks.test, 
  FUN=function(x) x@test));
lks.test           <- lks.test[,c(1,2)];
names(lks.test)    <- gsub(' ' ,'.', 
  paste(names(lks.test), "Lilliefors Test", sep="."));
lks.test$Contract  <- rownames(lks.test);
#Normality results
summaryStats <- plyr::join(summaryStats,jb.test);
summaryStats <- plyr::join(summaryStats,sw.test);
summaryStats <- plyr::join(summaryStats,lks.test);
summaryStats$AnnualizedSTD <- summaryStats$Stdev * sqrt(252);
rm(jb.test, sw.test, lks.test);
## save summary results
write.csv(summaryStats, paste(outdir, commodity, 
  "Summary_with_NormTests.csv", sep=''))
################################################################################

#####Outlier Test section ######################################################
## create ts data series object for arima modelling
ar=ts(data.frame(retZoo, check.names=F))

### Get initial outliers assuming ARIMA(1,0,0) or AR(1) model, since AR(1) is 
### the underlying model for Browniam motion.
#     I should test using auto.arima to generate a data driven model instead of
#     assuming the AR(1) model.  I did with arfit variable below but do not do
#     anything with in for now.
#NOTE: handle Na's again

arf <- lapply(ar, FUN=function(x) arima(x[!is.na(x)], order=c(1,0,0), 
  include.mean=TRUE));
# arfit=arima(ar[,2],order=c(1,0,0),include.mean=TRUE)
arfit <- lapply(ar, FUN=function(x) forecast::auto.arima(x[!is.na(x)], allowdrift=TRUE, 
  allowmean=TRUE))
#arfit=forecast::auto.arima(ar[,1])
# arfts=locate.outliers(residuals(arfit),coefs2poly(arfit))
arfts <- lapply(arf, 
  FUN= function(x) locate.outliers(residuals(x),coefs2poly(x)));
arfitts <- lapply(arfit, 
  FUN= function(x) locate.outliers(residuals(x),coefs2poly(x)));
#Get the date of each outlier
odates  <-  mapply(retZoo, arfts, FUN=function(x,y) index(na.omit(x))[y$ind]);
arfts   <-  mapply(cbind, arfts , 'OutlierDate' = odates,SIMPLIFY=F);
odates  <-  mapply(retZoo, arfitts, FUN=function(x,y) index(na.omit(x))[y$ind]);
arfitts <-  mapply(cbind, arfitts , 'OutlierDate' = odates,SIMPLIFY=F);
#build initial result outliers dataframe
initoutlrs <- do.call(rbind.data.frame,arfts)
initoutlrs$Contract <- substr(rownames(initoutlrs),1,8);
## save initial outlier results
write.csv(initoutlrs, paste(outdir, commodity, 
  "InitialOutliers.csv", sep=''))

###Get final outlier set using tso on each contract
arnames <- dimnames(ar)[[2]];
ar1 <- list();
reqarima <- unlist(list());
j=1; #seq(1,length(arnames))
warns <- list()
for (i in seq(length(arnames))){ #125,126)) {
    #idx <- aryears==i;
    tmp <- ar[,i];
    tryCatch( {
      tmp1 <- tso(ts(tmp[!is.na(tmp)]), tsmethod = c("auto.arima"), 
                  remove.method="bottom-up", maxit.iloop=25, 
                  args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE), 
                  types=c("AO", "LS", "TC","IO"))
      # changed to above 3/30/17 since lapply is not neede
      # tmp1 <- lapply(tmp, FUN=function(x) tso(ts(x[!is.na(x)]), 
      #   tsmethod = c("auto.arima"), remove.method="bottom-up", maxit.iloop=20, 
      #   args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE), 
      #   types=c("AO", "LS", "TC","IO")));
      ar1[[i]] <- tmp1;} # remove [1] 3/30/17 due to removal of lapply above
      , error = function(cond){
        print(cond);
        cat('Moving on, but will attempt to fix using arima later!!\n');
        ar1[i] <- NULL;
      }
    )
    #warns[i] <- warnings();
    cat(i, sep='\n')
}
###clear warnings
assign("last.warning", NULL, envir = baseenv())
## fix the errors
tsoissues=which(as.logical(lapply(ar1, FUN=is.null)));
reqarima<-arnames[tsoissues];
for (i in tsoissues) {
  tmp  <- ar[,i];  #added 3/30/17 missed in orginal code
  tryCatch({
    tmp1 <- tso(ts(tmp[!is.na(tmp)]), tsmethod = c("arima"), 
                remove.method="bottom-up", maxit.iloop=20, 
                args.tsmethod=list(include.mean=TRUE), 
                types=c("AO", "LS", "TC","IO"));
    # changed to above 3/30/17 since lapply is not neede
    # tmp1 <- lapply(tmp, FUN=function(x) tso(ts(x[!is.na(x)]), 
    #   tsmethod = c("arima"), remove.method="bottom-up", maxit.iloop=20, 
    #   args.tsmethod=list(include.mean=TRUE), 
    #   types=c("AO", "LS", "TC","IO")));
    ar1[[i]] <- tmp1; # remove [1] 3/30/17 due to removal of lapply above
    j=j+1;
    cat('Using arima successfull!!!!!!!!!!!!!!!!!!!!!!!!!\n');
  }
  , error = function(cond){
    print(cond);
    cat('Moving on\n');
    ar1[i] <- NULL;
    reqarima[length(reqarima)+1] <- paste('Error:',arnames[i]);
  })
  #warns[i] <- print(warnings());
  cat(i, sep='\n')
}
##fix list names of outlier results.
names(ar1)<-arnames;
## save outlier final to RData file
save(ar1, file=paste(file.path(rdatadir, commodity), "tsoar1.RData", sep=''));
################################################################################

###Compile Outlier Results######################################################
#build result outliers dataframe
odates  <-  mapply(retZoo, ar1, FUN=function(x,y) index(na.omit(x))[y$times]);
outlrs  <- lapply(ar1, FUN= function(x) x$outliers);
outlrs   <-  mapply(cbind, outlrs , 'OutlierDate' = odates, SIMPLIFY=F);
outlrs <- do.call(rbind.data.frame, outlrs);
outlrs$Contract <- substr(rownames(outlrs),1,8);
## save initial outlier results
write.csv(outlrs, paste(outdir, commodity, 
  "FinalOutliers.csv", sep=''))

################################################################################
#build yadj and calc summary statas
zooyadj        <- mapply(x=ar1, y=retZoo, FUN=function(x,y) 
  zoo(x$yadj, order.by=index(na.omit(y)))); # changed to na.omit 3/30/17
names(zooyadj) <- arnames;
#####Generate Summary Statistics################################################
###Univariate Statistics on Adjusted
summaryStatsAdj <- do.call(rbind.data.frame,lapply(zooyadj, 
  FUN= function(x) data.frame(t(table.Stats(xts(x),digits=10)))));
summaryStatsAdj$Contract <- substr(rownames(summaryStatsAdj),1,8);

###Note: we must don't need to handle Na's 
#Normality test
#Jarque Bera Test
jb.test           <- do.call(rbind.data.frame, lapply(ar1, 
  FUN = function(x) jarque.bera.test(x$yadj)));
jb.test           <- jb.test[,c(1,2,3)];
names(jb.test)    <- gsub(' ' ,'.',paste(names(jb.test), 
  "Jarque Bera Test", sep="."));
jb.test$Contract  <- rownames(jb.test);
#Shapiro-Wilk normality test
sw.test           <- lapply(ar1, FUN = function(x) shapiroTest(x$yadj));
sw.test           <- do.call(rbind.data.frame,lapply(sw.test, 
  FUN=function(x) x@test));
sw.test           <- sw.test[,c(1,2)];
names(sw.test)    <- gsub(' ' ,'.', 
  paste(names(sw.test), "Shapiro-Wilk normality test", sep="."));
sw.test$Contract  <- rownames(sw.test);
#Lilliefors Test
lks.test           <- lapply(ar1, FUN = function(x) lillieTest(x$yadj));
lks.test           <- do.call(rbind.data.frame,lapply(lks.test, 
  FUN=function(x) x@test));
lks.test           <- lks.test[,c(1,2)];
names(lks.test)    <- gsub(' ' ,'.', 
  paste(names(lks.test), "Lilliefors Test", sep="."));
lks.test$Contract  <- rownames(lks.test);
#Normality results
summaryStatsAdj <- plyr::join(summaryStatsAdj,jb.test);
summaryStatsAdj <- plyr::join(summaryStatsAdj,sw.test);
summaryStatsAdj <- plyr::join(summaryStatsAdj,lks.test);
summaryStatsAdj$AnnualizedSTD <- summaryStatsAdj$Stdev * sqrt(252);
rm(jb.test, sw.test, lks.test);
## save summary results
write.csv(summaryStatsAdj, paste(outdir, commodity, 
  "Adj_Summary_with_NormTests.csv", sep=''))
################################################################################
################################################################################
#build outlier residuals and calc summary statas
zooyRes        <- mapply(x=ar1, y=retZoo, FUN=function(x,y) zoo(x$fit$residuals, 
  order.by=index(na.omit(y)))); # changed to na.omit 3/30/17
names(zooyRes) <- arnames;
#####Generate Summary Statistics################################################
###Univariate Statistics on Outlier Residuals
summaryStatsRes <- do.call(rbind.data.frame,lapply(zooyRes, 
  FUN= function(x) data.frame(t(table.Stats(xts(x),digits=10)))));
summaryStatsRes$Contract <- substr(rownames(summaryStatsRes),1,8);

###Note: we must don't need to handle Na's 
#Normality test
#Jarque Bera Test
jb.test           <- do.call(rbind.data.frame, lapply(ar1, 
  FUN = function(x) jarque.bera.test(x$fit$residuals)));
jb.test           <- jb.test[,c(1,2,3)];
names(jb.test)    <- gsub(' ' ,'.',paste(names(jb.test), 
  "Jarque Bera Test", sep="."));
jb.test$Contract  <- rownames(jb.test);
#Shapiro-Wilk normality test
sw.test           <- lapply(ar1, FUN = function(x) shapiroTest(x$fit$residuals));
sw.test           <- do.call(rbind.data.frame,lapply(sw.test, 
  FUN=function(x) x@test));
sw.test           <- sw.test[,c(1,2)];
names(sw.test)    <- gsub(' ' ,'.', 
  paste(names(sw.test), "Shapiro-Wilk normality test", sep="."));
sw.test$Contract  <- rownames(sw.test);
#Lilliefors Test
lks.test           <- lapply(ar1, FUN = function(x) lillieTest(x$fit$residuals));
lks.test           <- do.call(rbind.data.frame,lapply(lks.test, 
  FUN=function(x) x@test));
lks.test           <- lks.test[,c(1,2)];
names(lks.test)    <- gsub(' ' ,'.', 
    paste(names(lks.test), "Lilliefors Test", sep="."));
lks.test$Contract  <- rownames(lks.test);
#Normality results
summaryStatsRes <- plyr::join(summaryStatsRes,jb.test);
summaryStatsRes <- plyr::join(summaryStatsRes,sw.test);
summaryStatsRes <- plyr::join(summaryStatsRes,lks.test);
summaryStatsRes$AnnualizedSTD <- summaryStatsRes$Stdev * sqrt(252);
rm(jb.test, sw.test, lks.test);
## save summary results
write.csv(summaryStatsRes, paste(outdir, commodity, 
  "Residual_Summary_with_NormTests.csv", sep=''))
################################################################################

## save outlier final to RData file
save(ar1,outlrs,retZoo,ar,arf,summaryStats,arfit,arfts, arfitts, initoutlrs,
  zooyadj, summaryStatsAdj, summaryStatsRes, tsoissues, reqarima,
  file=paste(file.path(rdatadir, commodity), 
  "tsoout.RData", sep=''));

write.csv(out, paste(outdir, commodity, "Prices.csv", sep=''))
## save required arima fix ; added 3/30/17
write.csv(data.frame(Contact=reqarima), paste(outdir, commodity, 
  "RequiredArimaFix.csv", sep=''));

################################################################################


################################################################################
#Plot POC to use.
#set image output, display or png file
toimage <- T;
if (!toimage) {
  tline = -2;
  inst  = c(0,-.27);
} else {
  tline = -2;
  inst  = c(0,-.15);
}
###clear warnings
assign("last.warning", NULL, envir = baseenv())
for (i in seq(1,length(arnames))) {
  idx <- is.na(retZoo[[i]]);
  plotdata=retZoo[[i]][!idx];
  ##AO Init
  aoinit <- arfts[[i]]$ind[arfts[[i]]$type=='AO'];
  aofin  <- ar1[[i]]$times[ar1[[i]]$outliers$type=='AO'];
  aoinit <- aoinit[!aoinit%in%aofin];
  ##IO
  ioinit <- arfts[[i]]$ind[arfts[[i]]$type=='IO'];
  iofin  <- ar1[[i]]$times[ar1[[i]]$outliers$type=='IO'];
  ioinit <- ioinit[!ioinit%in%iofin];
  ##TC
  tcinit <- arfts[[i]]$ind[arfts[[i]]$type=='TC'];
  tcfin  <- ar1[[i]]$times[ar1[[i]]$outliers$type=='TC'];
  tcinit <- tcinit[!tcinit%in%tcfin];
  ##LS
  lsinit <- arfts[[i]]$ind[arfts[[i]]$type=='LS'];
  lsfin  <- ar1[[i]]$times[ar1[[i]]$outliers$type=='LS'];
  lsinit <- lsinit[!lsinit%in%lsfin];
  
  graph1 <- paste(arnames[i],'png', sep='.')
  if (!toimage) dev.new();
  if (toimage) png(file.path(plotdir,paste(commodity,'Plots',sep=''),'FittedPlots',graph1), 
    width = 700, height = 750);
  oldPar <-par(xpd = NA, mar = par()$mar + c(3,0,0,0)); #mar = par()$mar + c(0,0,0,7) , oma = c(5,0,0,0)
  #par(mfrow=c(2,1));
  plot(plotdata, ylab='Returns', xlab='Date');
  ##IO
  points(plotdata[ioinit],col='blue', pch=24)
  points(plotdata[iofin],col='blue', bg='blue', pch=24)
  ##TC
  points(plotdata[tcinit],col='red', pch=25)
  points(plotdata[tcfin],col='red', bg='red', pch=25)
  ##AO
  points(plotdata[aoinit],col='green', pch=21)
  points(plotdata[aofin],col='green', bg='green', pch=21)
  ##LS
  points(plotdata[lsinit],col='purple', pch=22)
  points(plotdata[lsfin],col='purple', bg='purple', pch=22)
  
  title(names(ar1)[i])
  leg.txt = c('Innovative', 'Temporary', 'Additive', 'Level Shift') ;
  legend(x="bottom", leg.txt, pch=c(24,25,21,22), horiz=T, 
         col=c('blue','red','green', 'purple'), 
        pt.bg=c('blue','red','green', 'purple'), inset=inst); #-.55
  mtext('Empty Outlier symbols identify removed outliers.',cex = .75, outer=T, 
        side=1, line=tline)
  #close device
  if (toimage) ret<-dev.off();
  #reset par
  par(oldPar);
  
  ##Fitted Plot
  if (dim(ar1[[i]]$outliers)[1]!=0) {
    if (!toimage) dev.new();
    if (toimage) png(file.path(plotdir,paste(commodity,'Plots',sep=''),'Series_with_outliersPlots',graph1), 
        width = 700, height = 750);
    oldPar <-par(xpd = NA, mar = par()$mar + c(3,0,0,0)); #mar = par()$mar + c(0,0,0,7) , oma = c(5,0,0,0)
    plot(ar1[[i]]) ;#,args.x.axis=args.x.axis)
    title(paste('Outliers and effects for ', arnames[i]), outer=T, line=-2)
    #close device
    if (toimage) ret<-dev.off();
    #reset par
    par(oldPar);
  }
  cat(i,'\n');
  print(warnings())
}
##End of Plot###################################################################



#####Test Code for POC during writing###########################################

#write.zoo(sdZoo, "/home/byersjw/work/repo/fseal/dataWorking/RData/NGsdZoo.csv", sep=',')
#a=read.csv.zoo("/home/byersjw/work/repo/fseal/dataWorking/RData/NGsdZoo.csv",
#              sep=',',tz='UTC',format = "%Y-%m-%d")

#make sure we align 
# a=retZoo[[60]][!idx]
# b=ar[,1][!is.na(ar[,1])]
# length(a)==length(b)
# sum(a-b)
# all

# data.frame(Contract=names(retZoo)[60],t(table.Stats(xts(plotdata))))
# t(table.Stats(xts(retZoo[[60]][!idx])));

#retZoo = lapply(retZoo, FUN=na.omit)

# xlabs = index(retZoo[[60]])[!idx]
# args.x.axis = list(at = pretty(time(ar1[[1]]$y),n=5), labels=year(pretty(xlabs, 
#     n=length(pretty(time(ar1[[1]]$y))))), tcl = -0.5, lwd = 0, lwd.ticks = 1)
#points(retZoo[[60]][!idx][ar1[[1]]$times],col='green', bg='green',pch=19)

#abline(v=index(retZoo[[60]][!idx])[ar1[[1]]$times], col='red')
# arfts[[1]]$ind[!arfts[[1]]$ind%in%ar1[[1]]$times]
# ar1[[1]]$ind[!ar1[[1]]$ind%in%arfts[[1]]$times]

# abline(v=index(retZoo[[60]][!idx])[173], col='red')

# legendg("bottom", leg.txt, horiz=T, pch=list(24:24,25:25,21:21,22:22), 
#         col=list(rep('red',2),rep('blue',2),rep('green',2), rep('purple',2)), 
#         pt.bg=list(c('','red'),c('','blue'),c('','green'), c('','purple')), 
#         inset=c(0,-2.65),xpd=T); #-.65

#plot(retZoo[[60]][!idx], ylab='Returns', xlab='Date', col='red')
#plot(ar1[[1]]$effects, ylab='Outlier Impact', xlab='Date', col='red')
#lines(x=index(retZoo[[60]][!idx]),y=ar1[[1]]$yadj,col='black')
#mtext('Outlier effects ', side=3)#E, lines=-1)
#title(paste('Outlier effects ', names(ar1)[1]))
#leg.txt = c('Return Series', 'Outlier Adjusted Seris');
#legend("bottom", leg.txt, fill=c('red','black'),inset=c(0,-.65))


# ar=ts(data.frame(retZoo[names(retZoo)[60:71]], check.names=F))
# tmp=ar[,231]; #60:71];
# ar2 <- lapply(tmp, FUN=function(x) tso(ts(x[!is.na(x)]), 
#                                        tsmethod = c("auto.arima"), remove.method="bottom-up", maxit.iloop=20, 
#                                        args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE, method='ML'), 
#                                        types=c("AO", "LS", "TC","IO")))
# ar2 <- lapply(tmp, FUN=function(x) tso(ts(x[!is.na(x)]), 
#                                        tsmethod = c("arima"), remove.method="bottom-up", maxit.iloop=20, 
#                                        args.tsmethod=list(include.mean=TRUE,order=c(1,0,1)),
#                                        types=c("AO", "LS", "TC","IO"))); #method='ML',
# ar2 <- lapply(tmp, FUN=function(x) tso(ts(x[!is.na(x)]), 
#                                        tsmethod = c("auto.arima"), remove.method="bottom-up", maxit.iloop=20, 
#                                        args.tsmethod=list(allowdrift=TRUE,allowmean=TRUE, method='ML', 
#                                                           seasonal=T,stationary=T),
#                                        types=c("AO", "LS", "TC","IO"))); #method='ML',stepwise=F,, parallell=T,max.d=2, max.P=2, max.D=1, max.Q=2
# forecast::auto.arima(ar[!is.na(ar[,37]),37], allowdrift=T)


# tmp1 <- tso(ts(tmp[!is.na(tmp)]), tsmethod = c("auto.arima"), 
#             remove.method="bottom-up", maxit.iloop=20, 
#             args.tsmethod=list(allowdrift=TRUE, allowmean=TRUE), 
#             types=c("AO", "LS", "TC","IO"))
# 
# tmp1 <- tso(ts(tmp[!is.na(tmp)]), tsmethod = c("arima"), 
#             remove.method="bottom-up", maxit.iloop=20, 
#             args.tsmethod=list(include.mean=TRUE), 
#             types=c("AO", "LS", "TC","IO"));
# 
# mapply(x=ar1, y=retZoo, FUN=function(x,y) 
#   length(index(na.omit(y[[1]])))==length(x[[1]]$fit$residuals))

#' =============================================================================
#' Name   : VaR estimations
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

source('~/work/repo/fseal/projects/research/outliers/MultivariateMoments.R')


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
alpha <- .95
load(file.path(rdatadir, paste(commodity,filenm,sep='')));

w <- 1000000;

#set prices again
out <- read.csv(paste(outdir, commodity, "Prices.csv", sep=''));
pr <- plyr::ddply(out, "ContractQuoteName", transform, max = max(TradeDate));
pr <- pr[pr$max==pr$TradeDate,c('ContractQuoteName','Settle','TradeDate')];
names(pr)[1] <- 'Contract';

summaryStats <- plyr::join(summaryStats, pr);
summaryStats$w <- summaryStats$Settle * w;
summaryStatsRes <- plyr::join(summaryStatsRes, pr);
summaryStatsRes$w <- summaryStatsRes$Settle * w;

#NOte: add 3 to excess kurtosis to get kurtosis
vars <- data.frame(Contract = summaryStats$Contract,
  VaRG.Contaminated = apply(summaryStats, 1, FUN=function(x) GVaR.MM(as.numeric(x[['w']]), 
  as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, alpha)),
  VaRG.Clean = apply(summaryStatsRes, 1, FUN=function(x) GVaR.MM(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, alpha)),
  ESG.Contaminated = apply(summaryStats, 1, FUN=function(x) GES.MM(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, alpha)),
  ESG.Clean = apply(summaryStatsRes, 1, FUN=function(x) GES.MM(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, alpha)),
  VaRM.Contaminated = apply(summaryStats, 1, FUN=function(x) mVaR.MM1(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, 
    as.numeric(x[['Skewness']])^3, (as.numeric(x[['Kurtosis']])+3)*as.numeric(x[['Stdev']])^4, alpha)),
  VaRM.Clean = apply(summaryStatsRes, 1, FUN=function(x) mVaR.MM1(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, 
    as.numeric(x[['Skewness']])^3, (as.numeric(x[['Kurtosis']])+3)*as.numeric(x[['Stdev']])^4, alpha)),
  ESM.Contaminated = apply(summaryStats, 1, FUN=function(x) mES.MM1(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, 
    as.numeric(x[['Skewness']])^3, (as.numeric(x[['Kurtosis']])+3)*as.numeric(x[['Stdev']])^4, alpha)),
  ESM.Clean = apply(summaryStatsRes, 1, FUN=function(x) mES.MM1(as.numeric(x[['w']]), 
    as.numeric(x[['Arithmetic.Mean']]), as.numeric(x[['Stdev']])^2, 
    as.numeric(x[['Skewness']])^3, (as.numeric(x[['Kurtosis']])+3)*as.numeric(x[['Stdev']])^4, alpha))
  );
  
  vars$VaRG_Percent_Change <- vars$VaRG.Clean/vars$VaRG.Contaminated -1;
  vars$ESG_Percent_Change <- vars$ESG.Clean/vars$ESG.Contaminated -1;
  vars$VaRM_Percent_Change <- vars$VaRM.Clean/vars$VaRM.Contaminated -1;
  vars$ESM_Percent_Change <- vars$ESM.Clean/vars$ESM.Contaminated -1;

  vars$VaRG_Cost <- (vars$VaRG.Clean-vars$VaRG.Contaminated)/1;
  vars$ESG_Cost <- (vars$ESG.Clean-vars$ESG.Contaminated)/1;
  vars$VaRM_Cost <- (vars$VaRM.Clean-vars$VaRM.Contaminated)/1;
  vars$ESM_Cost <- (vars$ESM.Clean-vars$ESM.Contaminated)/1;

  vars <- plyr::join(vars,summaryStats[,c('Contract','Stdev')]);
  names(vars)[18]<- 'Stdev1'; #contaminated clean will be stdev
  vars <- plyr::join(vars,summaryStatsRes[,c('Contract','Stdev')]);

  vars$VaRG_Elasticity <- vars$VaRG_Percent_Change / (vars$Stdev/vars$Stdev1 -1);
  vars$ESG_Elasticity <- vars$ESG_Percent_Change / (vars$Stdev/vars$Stdev1 -1);
  vars$VaRM_Elasticity <- vars$VaRM_Percent_Change / (vars$Stdev/vars$Stdev1 -1);
  vars$ESM_Elasticity <- vars$ESM_Percent_Change / (vars$Stdev/vars$Stdev1 -1);
  vars<-  plyr::join(vars,summaryStats[,c('Contract','w')]);

## save summary results
write.csv(vars, paste(outdir, commodity,
                       "VaRandCVaREstimates.csv", sep=''))



mVaR.MM(w * 
             as.numeric(summaryStats[['Arithmetic.Mean']])[1], 0,  as.numeric(summaryStats[['Stdev']])[1], 
             as.numeric(summaryStats[['Skewness']])[1]*as.numeric(summaryStats[['Stdev']])[1]^3, as.numeric(summaryStats[['Kurtosis']])[1]+3*as.numeric(summaryStats[['Stdev']])[1]^4, alpha)
mVaR.MM(w,
               as.numeric(summaryStats[['Arithmetic.Mean']])[1],  as.numeric(summaryStats[['Stdev']])[1], 
             0, 3*as.numeric(summaryStats[['Stdev']])[1]^4, alpha)

mVaR.MM(w , 
          as.numeric(summaryStats[['Arithmetic.Mean']])[1], as.numeric(summaryStats[['Stdev']])[1], 
        0, 0, alpha)


GVaR.MM(w, summaryStatsRes$Arithmetic.Mean[1], summaryStatsRes$Stdev[1], alpha)


#test for sigfinance
w = 1000000;
Mean = 0.0001898251;
Stdev = 0.01612464;
ExKurtosis = 3.946156;
Skewness = -0.1373454;

GVaR = GVaR.MM(w,Mean,Stdev^2, .95)
GVaR
MVaR = mVaR.MM(w,Mean,Stdev^2, 0,0, .95);
mVaR.MM(w, Mean, Stdev^2, 0, 3 * Stdev^4, .95)
#shoud be equal to GVaR
GVaR==MVaR
MVaR

#corrected exkurt calc 
mVaR.MM1 = function(w, mu, sigma, M3, M4, p ){
  skew = M3;#skewness.MM(w,sigma,M3);
  exkurt = M4;#kurtosis.MM(w,sigma,M4);
  z = qnorm(1-p);
  zc = z + (1/6)*(z^2 -1)*skew
  Zcf = zc + (1/24)*(z^3 - 3*z)*exkurt - (1/36)*(2*z^3 - 5*z)*skew^2;
  return ( -multivariate_mean(w,mu) - Zcf*StdDev.MM(w,sigma)  )
}
skewness.MM=function(w,sigma,M3){
  return( ( t(w)%*%M3%*%(w%x%w) )/ (StdDev.MM(w,sigma))^3         )
}

kurtosis.MM=function(w,sigma,M4){
  return(  ( t(w)%*%M4%*%(w%x%w%x%w) )/ (StdDev.MM(w,sigma))^4    )
}

#call revised mVAR.MM with m3 and m4 equal 0
MVaR1 = mVaR.MM1(w,Mean,Stdev, 0,0, .95);
#shoud be equal to GVaR
GVaR==MVaR1
MVaR1

#with m3 and m4 not zero
MVaR1 = mVaR.MM1(w, Mean, Stdev, Skewness, ExKurtosis, .95);
MVaR1

mapply(c(0,.0001,.001,.01,.1), c(0,.0001,.001,.01,.1), FUN=function(x,y) mVaR.MM1(1000000,0,Stdev,x,y,.95))
mapply(c(0,.0001,.001,.01,.1), c(0,0,0,0,0), FUN=function(x,y) mVaR.MM1(1000000,0,Stdev,x,y,.95))


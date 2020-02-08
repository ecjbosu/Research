#' =============================================================================
#' Name   : Build Outlier timeseries report
#' Author : Joe W. Byers
#' Date: 4/9/19
#' Modifed :
#' Version: 1.0001
#' Mail   : <<<ecjbosu@aol.com>>>
#' =============================================================================
#'


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


#set up enviroment and libraries
require(tidyquant)
#require(xts)
require(lubridate)
#library(PerformanceAnalytics)
require(dplyr)
require(RMariaDB)
require(rJava)
require(tibble)

setSource(srcPath='', srcDirs=sub('scripts','sources',"/home/byersjw/work/repo/fseal/projects/scripts"));
rootDBPath 	= '/Share/metamarts';
workEnv 	= 'production';
dbSubPath       = '';
setJarClasses()

rdatadir ='//Share/NAS/work/Outliers/RData';
outdir  <- "/home/byersjw/data2/";
plotdir <- '/home/byersjw/data2/';
filenm <- 'tsoout2.RData'; # 2 must match above directories
First.Date <-'2000-01-01';
Last.Date  <-"2017-03-20"; # this was the last date that this analysis was
# executed.  For replications
first.yr <- 2007;
#ticker <- paste(c("NG","CL","GC"),collapse="','");
commodity <- "NG"
commodity <- "CL"
load(file.path(rdatadir, paste(commodity,filenm,sep='')));


user='ecjbosu';
passwd='cHJvZmVzc29y';
host='fsealshare';
#host ='73.150.229.65';
#ticker <- paste(c("NG","CL","GC"),collapse="','");
driverClass <- "com.mysql.cj.jdbc.Driver";#"com.mysql.jdbc.Driver";
schema <- 'CommodityPrices';

################################################################################

#####Generate extended Inputs###################################################

ticker <- paste(substr(rownames(outlrs),6,8),substr(outlrs$Year,3,6),sep="");
#ticker <- paste(ticker,collapse="','");

#dates <- paste(c(as.character(unique(outlrs$OutlierDate))),collapse="','");

dates <- outlrs$OutlierDate;

sqlstr=NULL;
  
for (i in 1:length(ticker)) {
  sqlstr[i] <-paste("select * from CommodityPrices.Futures where TradeDate ='", 
                 dates[i],"' and ContractQuoteName = '",ticker[i],"'", sep="");
}
  
 # sqlstr <-paste("select * from CommodityPrices.Futures where TradeDate in ('", 
#              dates,"') and ContractQuoteName in ('",
#               ticker,"')", sep="");
################################################################################
fix = "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC"
#####Get Data, massage, and calculate returns###################################
#con = dbConnect(RMariaDB::MariaDB(), host=host,dbname=schema, 
#                user=user,password=rsecurity()$base64decode(passwd))
#outvol <- JDBC.Pull(sqlstr, host, port=3306, paste(schema,fix,sep=''), user,
#                         rsecurity()$base64decode(passwd),
#                         driverClass, dbInfoFlag=F);
  
outvol=NULL;
for (i in 1:length(ticker)) {
  
    t1 <- JDBC.Pull(sqlstr[i], host, port=3306, paste(schema,fix,sep=''), user,
                 rsecurity()$base64decode(passwd),
                 driverClass, dbInfoFlag=F);
    if(i==1) {      outvol=t1} else
    {      outvol=rbind(outvol,t1);
    }
}
  
unique(outvol$TradeDate)

unique(outvol$TradeDate[outvol$Volume>0])
unique(outvol$TradeDate[outvol$Volume==0])

View(outvol[outvol$TradeDate%in%unique(outvol$TradeDate[outvol$Volume==0]),])

volumesum <- outvol %>% group_by(TradeDate) %>% 
  summarize(sum_Volume=sum(Volume), TotalContracts=n()) #, Cnt_VolumeNot0= sum(Volume[Volume>0]));

volumesum0 <- outvol[outvol$Volume==0,] %>% group_by(TradeDate) %>% 
  summarize(TotalContracts0=n());

volumesum <- merge(volumesum,volumesum0, all=T)
#NG 297 days, 83 with zero volumes only, 103 days with some zero volumes but some with volume
# meaning 20 days have zero and some volumes.
#CL 145 days, 14 with zero volumes only, 61 days with some zero volumes but some with volume
# meaning 47 days have zero and some volumes.


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

pfile <- 'NGCL0818curves.csv';
#pfile <- 'NGCLPromptMonth.csv';
dpath   <- '/home/byersjw/Dropbox/0000000-Natural Gas and EVT/Results-Prelim/RawPrices';

#read 
out   <- read.csv(file.path(dpath,pfile));
out$TradeDate      <- as.POSIXct(out$TradeDate,'%Y-%m-%d',tz='UTC')
out$ContractDate           <- as.POSIXct(out$ContractDate,'%Y-%m-%d',tz='UTC')
#for some commodity need to check for 0 and Na
out <- out[!is.na(out$Settle),];
out <- out[out$Settle!=0,];

ggplot(data=out, aes(x=Prompt, y=Settle, fill=TradeDate)) +
  #geom_bar(stat="identity", position=position_dodge()) + theme(legend.position="bottom") +
  #geom_point(aes(x=Year, y=Average_Price), data=c) +
  geom_line(stat="identity") +
  theme(legend.position='bottom') 
+
  scale_color_manual(name="", values=c("Average Strip Price"="black"))# + 
out %>% 
  mutate(CONTRACT = factor(CONTRACT, levels = levels(CONTRACT))) %>%
  #mutate(STRIP=as.character(STRIP)) %>% 
  group_by(CONTRACT) %>%
  ggplot(aes(x=Prompt, y=Settle, color=TradeDate)) +
  geom_point( size=.75)  +
  facet_grid(CONTRACT ~.) +
  theme(legend.position='bottom') 


#build zoo time series object, easier to do some things like this pivot
retZoo <- read.zoo(file=out[, c(1,3,5)], split="ContractQuoteName");
retZoo <- lapply(retZoo, FUN=Return.calculate, method="log");

n=37
pr =read.zoo(file=out[, c(1,3,5)], split="ContractQuoteName");

pt = data.frame(levels=na.omit(pr[,n])[2:length(na.omit(pr[,n]))],
    logs=log(na.omit(pr[,n])[2:length(na.omit(pr[,n]))]),
    returns=na.omit(retZoo[[n]]))


ll = na.omit(zoo(pt,order.by=as.Date(rownames(pt))));
plot(ll, type="l", col="black", main = names(retZoo)[n],
             xlab='Date');


save(out,retZoo,ll,pt,pr,n,file='/home/byersjw/data/ngforward20190302.RData')

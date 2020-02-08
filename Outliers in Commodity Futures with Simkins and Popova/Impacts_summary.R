#' =============================================================================
#' Name   : Build Outlier impacts summary tables and figures
#' Author : Joe W. Byers
#' Date: 10/14/19
#' Modifed :
#' Version: 1.0001
#' Mail   : <<<ecjbosu@aol.com>>>
#' =============================================================================
#'
#set up enviroment and libraries
require(tidyquant)
#require(xts)
require(lubridate)
#library(PerformanceAnalytics)
require(dplyr)
library(tsoutliers)
require(grDevices)
require(plotrix)
require(fBasics)
library(tseries)
require(ggplot2)
require(tibble)

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

load(file.path(rdatadir, paste(commodity,filenm,sep='')));

#see model
ar1[[1]] #CL 20
#see xreg 
View(ar1[[1]]$fit$xreg) #CL 20

outlrs <- outlrs  %>% mutate(Type=Type, type=NULL)
outlrs$Type<-factor(outlrs$Type) # to get sort order for BW plots
outlrs$Type<-factor(outlrs$Type, levels=c('TC','AO','LS','IO'))

a <- as.tibble(outlrs) %>% arrange(OutlierDate) %>% group_by(OutlierDate,Type) %>% 
 count(OutlierDate);
b <- as.tibble(outlrs) %>% arrange(OutlierDate) %>% group_by(OutlierDate) %>% 
 count(OutlierDate);
b <- b[b$n>=5,]

#View(a[a$OutlierDate%in%b$OutlierDate,])

tpout <- outlrs[outlrs$OutlierDate%in%b$OutlierDate,];
tpout$OutlierDate <- as.character(tpout$OutlierDate);

# ggplot(data=tpout, aes(y=coefhat, x=OutlierDate)) + #, colour=Type, fill=Type)) +
#   geom_boxplot(aes(group=OutlierDate))  + #stat="count", position="stack")
#   theme(legend.position="bottom")
# 
#   scale_fill_grey() + scale_colour_grey() +theme_bw() +

ggplot(data=tpout ,aes(y=coefhat, x=OutlierDate)) + #, colour=Type, fill=Type)) +
    geom_boxplot(aes(group=OutlierDate),outlier.shape = " ") + #, colour=Type))  + #stat="count", position="stack")
    theme(legend.position="bottom",axis.text.x = element_text(size=8,angle = 90, hjust = 1))

 
# #####################3CL
commodity <- "CL"

load(file.path(rdatadir, paste(commodity,filenm,sep='')));

#see model
ar1[[1]]
#see xreg 
View(ar1[[1]]$fit$xreg)

outlrs <- outlrs  %>% mutate(Type=type, type=NULL)
outlrs$Type<-factor(outlrs$Type) # to get sort order for BW plots
outlrs$Type<-factor(outlrs$Type, levels=c('TC','AO','LS','IO'))

a <- as.tibble(outlrs) %>% arrange(OutlierDate) %>% group_by(OutlierDate,Type) %>% 
    count(OutlierDate);
b <- as.tibble(outlrs) %>% arrange(OutlierDate) %>% group_by(OutlierDate) %>% 
    count(OutlierDate);
b <- b[b$n>=5,]

#View(a[a$OutlierDate%in%b$OutlierDate,])

tpout <- outlrs[outlrs$OutlierDate%in%b$OutlierDate,];
tpout$OutlierDate <- as.character(tpout$OutlierDate);

# ggplot(data=tpout, aes(y=coefhat, x=OutlierDate)) + #, colour=Type, fill=Type)) +
#   geom_boxplot(aes(group=OutlierDate))  + #stat="count", position="stack")
#   theme(legend.position="bottom")
# 
#   scale_fill_grey() + scale_colour_grey() +theme_bw() +

ggplot(data=tpout ,aes(y=coefhat, x=OutlierDate)) + #, colour=Type, fill=Type)) +
    geom_boxplot(aes(group=OutlierDate),outlier.shape = " ") + #, colour=Type))  + #stat="count", position="stack")
    theme(legend.position="bottom",axis.text.x = element_text(size=8,angle = 90, hjust = 1))

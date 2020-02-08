
require(tidyr)

### reload data from db
ticker <- paste(c(commodity),collapse="','");
sqlstr <-paste("select TradeDate, CONTRACT, Settle as Close, ContractDate,
               ContractQuoteName, Prompt, Month, Year
               from CommodityPrices.Futures where TradeDate> '", First.Date,
               "' and TradeDate <= '",Last.Date, "' and CONTRACT in ('",
               ticker,"') and Year>=",first.yr, sep="");

################################################################################
fix = "?useUnicode=true&useJDBCCompliantTimezoneShift=true&useLegacyDatetimeCode=false&serverTimezone=UTC"
#####Get Data, massage, and calculate returns###################################
out <- JDBC.Pull(sqlstr, host, port=3306, paste(schema,fix,sep=''), user,
                 rsecurity()$base64decode(passwd),
                 driverClass, dbInfoFlag=F);
#rebuild ContractQuoteName to sort correctly
out$ContractQuoteName <- paste(as.numeric(
  substr(out$ContractQuoteName,4,5))+2000,
  substr(out$ContractQuoteName,1,3),sep='.');
#for some commodity need to check for 0 and Na
out <- out[!is.na(out$Close),];
out <- out[out$Close!=0,];

###load RData file 
load(paste("/Share/NAS/work/RData/",commodity,"tsoout2.RData", sep=''));


###initial outliers merge prompt
#copy initoutlrs
initoutlrs_pc <- initoutlrs;
initoutlrs_pc <- rename(initoutlrs_pc, c('OutlierDate'="TradeDate", "Contract"="ContractQuoteName"));
#out[initoutlrs_pc$OutlierDate==out$TradeDate&initoutlrs_pc$Contract==out$ContractQuoteName,c('Prompt','Close')]

initoutlrs_pc <- merge(initoutlrs_pc,out,by=c('TradeDate','ContractQuoteName'));
initoutlrs_pc <- select(initoutlrs_pc, -CONTRACT);
initoutlrs_pc <- rename(initoutlrs_pc, c("TradeDate"="OutlierDate", "ContractQuoteName"="Contract"));
## complete merging on prompt and close price

### merge return from retZoo
ret_pc <- retZoo %>% as.data.frame() %>% 
  tibble::rownames_to_column(var='OutlierDate') %>% 
  gather(Contract,Ret,-OutlierDate) %>% na.omit(.) %>%
  mutate(OutlierDate=as.Date(OutlierDate), Contract=substr(Contract,2,9))

initoutlrs_pc <- initoutlrs_pc %>% merge(ret_pc,by=c('OutlierDate','Contract')) %>% 
  arrange(OutlierDate,Prompt);
## complete merging on returns

###final outliers merge prompt
#copy initoutlrs
outlrs_pc <- outlrs %>% rename(c('OutlierDate'="TradeDate", "Contract"="ContractQuoteName"));
#out[initoutlrs_pc$OutlierDate==out$TradeDate&initoutlrs_pc$Contract==out$ContractQuoteName,c('Prompt','Close')]

outlrs_pc <- outlrs_pc %>% merge(out,by=c('TradeDate','ContractQuoteName')) %>%
 select(-CONTRACT) %>% rename(c("TradeDate"="OutlierDate", "ContractQuoteName"="Contract"));
## complete merging on prompt and close price

outlrs_pc <- outlrs_pc %>% merge(ret_pc,by=c('OutlierDate','Contract')) %>% 
  arrange(OutlierDate,Prompt);
## complete merging on returns

## save initial outlier results
write.csv(initoutlrs_pc, paste(outdir, commodity,
                            "InitialOutliers_pcr.csv", sep=''))
## save outlier results
write.csv(outlrs_pc, paste(outdir, commodity,
                            "FinalOutliers_pcr.csv", sep=''))


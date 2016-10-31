#This is the filename in Windows  filename <- "C:\\Users\\sandovaj.AUTH\\Desktop\\db_holdings.csv"
#This is the filename in Mac 
 filename <- "/Users/jesussandoval/Desktop/holdings_analysis/db_holdings.csv" 

holdings <- read.csv(filename,header=TRUE,sep=",")

#I want to fix the columns so that I can use SQl to drive the analysis colnames(holdings) <-
gsub('[.]','',colnames(holdings)) holdings$AsofDate <- as.Date(holdings$AsofDate,format="%m/%d/%Y")

#What's the market value of Indonesian bonds and equities?

total_exposure <- sqldf("select AsofDate, CountryofDomicileName, sum(MarketValue) tot_mkt_val,
	sum(Units) tot_units from holdings where CountryofDomicileName = 'Indonesia' and
	AssetTypeCategory='EQUITY'  or CountryofDomicileName='Indonesia' and AssetTypeCategory='FIXED INCOME
	SECURITIES' group by AsofDate order by AsofDate")

write.csv(total_exposure,"~/Desktop/holdings_analysis/total_exposure.c sv")

#How has the sector exposure and country exposure evolved over the year?

sector_exposure <- sqldf("select AsofDate, GICSSectorName, sum(MarketValue) tot_mkt_val, sum(Units)
	tot_units from holdings where GICSSectorName='INFORMATION TECHNOLOGY' group by AsofDate order by
	AsofDate")

country_exposure <- sqldf("select AsofDate, AssetTypeCategory,
	CountryofDomicileName,sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where
	CountryofDomicileName = 'Indonesia' group by AsofDate, AssetTypeCategory order by AsofDate")

#How has equity exposure to that country changed over the year?

country_equity_exposure <- sqldf("select AsofDate, CountryofDomicileName,AssetTypeCategory,
	sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where CountryofDomicileName =
	'Indonesia' and AssetTypeCategory ='EQUITY' group by AsofDate order by AsofDate")

write.csv(country_equity_exposure,"~/Desktop/holdings_analysis/country _equity_ exposure.csv")

#How has Fixed Income exposure to that country changed over the year?

country_fi_exposure<- sqldf("select AsofDate, CountryofDomicileName, AssetTypeCategory,
	sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where CountryofDomicileName =
	'Indonesia' and AssetTypeCategory ='FIXED INCOME SECURITIES' group by AsofDate order by AsofDate")

write.csv(country_fi_exposure,"~/Desktop/holdings_analysis/country_fi_ exposure .csv")

#Which public equity strategies possess investments to that country and how much?

manager_country_equity_exposure <- sqldf("select AsofDate, SourceAccountName, CountryofDomicileName,
	AssetTypeCategory, sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where
	CountryofDomicileName = 'Indonesia' and AssetTypeCategory ='EQUITY' group by
	AsofDate,SourceAccountName order by SourceAccountName,AsofDate")

write.csv(manager_country_equity_exposure,"~/Desktop/holdings_analysis /manager
_country_equity_exposure.csv")

#Which fixed income strategies possess investments to that country and how much?

country_exposure_fi_manager <- sqldf("select AsofDate, SourceAccountName, CountryofDomicileName,
AssetTypeCategory, sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where
CountryofDomicileName = 'Indonesia' and AssetTypeCategory ='FIXED INCOME SECURITIES' group by
AsofDate,SourceAccountName order by SourceAccountName,AsofDate")

write.csv(country_exposure_fi_manager,
"~/Desktop/holdings_analysis/manager_country_fi_exposure.csv")

#If there is public equity exposure to this country, how much is attributed to particular sectors?

equity_sector_exposure <- sqldf("select AsofDate, CountryofDomicileName, GICSSectorName,
sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where CountryofDomicileName =
'Indonesia' and AssetTypeCategory ='EQUITY' group by AsofDate, GICSSectorName order by
GICSSectorName,AsofDate")

write.csv(equity_sector_exposure,"~/Desktop/holdings_analysis/equity_s ector_ex posure.csv")

#If there is fixed income exposure to this country, how much is attributed to particular industries?

fi_sector_exposure <- sqldf("select AsofDate, CountryofDomicileName, BloombergIndustrySector,
sum(MarketValue) tot_mkt_val, sum(Units) tot_units from holdings where CountryofDomicileName =
'Indonesia' and AssetTypeCategory ='FIXED INCOME SECURITIES' group by AsofDate,
BloombergIndustrySector order by BloombergIndustrySector,AsofDate")

write.csv(fi_sector_exposure,"~/Desktop/holdings_analysis/fi_sector_ex posure.c sv")

#Helper function to get percents percent <- function(x, digits = 2, format = "f", ...) {
paste0(formatC(100 * x, format = format, digits = digits, ...), "%") }

#High level overview of what's happening to a particular sector or country, Info and Tech in this
#case

analyze_changes <- function(x) {          
	count <- 1 
	x['chg_mkt_val'] <- 0 
	x['perc_chg_mkt_val'] <- 0      
	x['chg_units'] <- 0 
	x['unit_perc_chg'] <- 0      
	x['pos_chg'] <- "None" 
	x['val_per_unit'] <-x$tot_mkt_val/x$tot_units          
	for(i in x$tot_mkt_val) {
		if(is.na(x$tot_mkt_val[count+1])) break 
		x$chg_mkt_val[count+1] <- x$tot_mkt_val[count + 1] - x$tot_mkt_val[count]         
		x$perc_chg_mkt_val[count+1] <- percent((x$tot_mkt_val[count +1]/x$tot_mkt_val[count]) - 1) 
		x$chg_units[count+1] <- x$tot_units[count + 1] - x$tot_units[count]
		x$unit_perc_chg[count+1] <- percent((x$tot_units[count+1]/x$tot_units[count]) - 1)
		if(x$chg_units[count+1] > 0) x$pos_chg[count+1] <- "Added" 
		if(x$chg_units[count+1] < 0) x$pos_chg[count+1] <- "Trimmed" 
		count <- count + 1 
	}          
	return(x)  
}




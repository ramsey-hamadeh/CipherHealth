##### Library setup, excel sheet imports
library(readr)
library(readxl)

setwd("/Users/ramseyhamadeh/Documents/ROI pipeline/VBP")
col_filter <- rep('text',12)
col_filter[2] <- 'numeric'
col_filter[9:12] <- 'numeric'
data <- read_excel("Medicare_Provider_Charge_Inpatient_DRGALL_FY2014_R.xlsx", col_types = col_filter)
data_TPS <- read_excel("Hospital_Value-Based_Purchasing__HVBP____Total_Performance_Score_R.xlsx")

##### Inputs
penalty_cap <- 0.02

##### Data cleaning
colnames(data_TPS)[1] <- "Provider_ID"
colnames(data)[2] <- "Provider_ID"

##### Sumproducts to get total annual base operating DRG payment amounts
unique_providers <- sort(unique(data$'Provider_ID'))
DRG_values <- c()
for (i in unique_providers){
  data_1 <- data[data$`Provider_ID`==i,]
  DRG_values <- c(DRG_values,sum(data_1$`Total Discharges` * data_1$`Average Medicare Payments`))
}
df <- data.frame(unique_providers,DRG_values)
names(df) <- c('Provider_ID','Total_base_DRG_payment')

##### Creating output dataframe
df_2 <- data_TPS[,c("Provider_ID","Total Performance Score")]
final_data <- merge(df,df_2,by="Provider_ID")

##### Getting the linear exchange function slope
slope <- sum(final_data$Total_base_DRG_payment)/sum(final_data$Total_base_DRG_payment * final_data$`Total Performance Score` /100)
final_data$Multiplier <- final_data$`Total Performance Score`/100*slope*penalty_cap
final_data$Net_change_DRG <- final_data$Multiplier-penalty_cap
final_data$VBP_impact <- final_data$Total_base_DRG_payment * final_data$Net_change_DRG

write.csv(final_data, file="final_DRG_TPS_data.csv")

## Density plots
den <- density(scale(final_data$`Total Performance Score`, center = FALSE, scale = FALSE))
plot(den, main="VBP impact distribution")
polygon(den, col="red", border="blue")


##### Improvement model
final_improvement <- final_data
CP <- 390267
old_TPS <- final_data$`Total Performance Score`[final_data$Provider_ID == CP] 
trend_a <- 44.673
trend_b <- -0.03
new_TPS <- trend_a*exp(trend_b*old_TPS)+old_TPS
final_improvement$`Total Performance Score`[final_improvement$Provider_ID == CP] <- new_TPS


slope_new <- sum(final_improvement$Total_base_DRG_payment)/sum(final_improvement$Total_base_DRG_payment * final_improvement$`Total Performance Score` /100)
final_improvement$Multiplier <- final_improvement$`Total Performance Score`/100*slope_new*penalty_cap
final_improvement$Net_change_DRG <- final_improvement$Multiplier-penalty_cap
final_improvement$VBP_impact <- final_improvement$Total_base_DRG_payment * final_improvement$Net_change_DRG

net_cipher_impact <- final_improvement$VBP_impact[final_improvement$Provider_ID == CP]-final_data$VBP_impact[final_data$Provider_ID == CP]

setwd("C:/Users/akoira/Desktop/Arogya Projects/USAID_Assessment_Analysis")
install.packages("reshape")
install.packages("corrplot")
install.packages("prettydoc")\
library(prettydoc)
library(corrplot)
library(reshape)
library(tidyr)
library(dplyr)

# Constr_status
df <- read.csv("input_table.csv")
df <- df [-(which(df$surveyed_popn == 0)),]
df_soc <- df[,c("constr_completed", "constr_in_progress", "constr_not_started", "category")]
df_soc_long <- gather(df_soc, constr_status, value, constr_completed:constr_not_started, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, constr_status) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, constr_status, total)

chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

corrplot(contrib, is.cor = FALSE)
# Building Codes

# Construction_completed
colnames(df)
df_soc <- df[,c("bcf_comp_yes", "bcf_comp_no", "category")]
df_soc_long <- gather(df_soc, bcf_comp, value, bcf_comp_yes:bcf_comp_no, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, bcf_comp) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, bcf_comp, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Construction_in progress
colnames(df)
df_soc <- df[,c("bcf_ip_yes", "bcf_ip_no", "category")]
df_soc_long <- gather(df_soc, bcf_ip, value, bcf_ip_yes:bcf_ip_no, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, bcf_ip) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, bcf_ip, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Construction_ns
colnames(df)
df_soc <- df[,c("bcf_ns_yes", "bcf_ns_no", "category")]
df_soc_long <- gather(df_soc, bcf_ns, value, bcf_ns_yes:bcf_ns_no, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, bcf_ns) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, bcf_ns, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)


# Why weren't building codes followed?
# Construction_completed
colnames(df)
df_soc <- df[,c("wnf_comp_dk","wnf_comp_te","wnf_comp_ltm","wnf_comp_dc","wnf_comp_other", "category")]
df_soc_long <- gather(df_soc, wnf_comp, value, wnf_comp_dk:wnf_comp_other, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, wnf_comp) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, wnf_comp, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Construction_in_progress
colnames(df)
df_soc <- df[,c("wnf_ip_dk","wnf_ip_te","wnf_ip_ltm","wnf_ip_dc","wnf_ip_other", "category")]
df_soc_long <- gather(df_soc, wnf_ip, value, wnf_ip_dk:wnf_ip_other, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, wnf_ip) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, wnf_ip, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Construction_not_started
colnames(df)
df_soc <- df[,c("wnf_ns_dk","wnf_ns_te","wnf_ns_ltm","wnf_ns_dc","wnf_ns_other", "category")]
df_soc_long <- gather(df_soc, wnf_ns, value, wnf_ns_dk:wnf_ns_other, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, wnf_ns) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, wnf_ns, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

# Why hasn't construction started?
colnames(df)
df_soc <- df[,c("wns_ns_other","wns_ns_dbe","wns_ns_drg","wns_ns_ltk","wns_ns_lcm","wns_ns_lhr","wns_ns_lop","wns_ns_ahoc", "category")]
df_soc_long <- gather(df_soc, wnns_ns, value, wns_ns_other:wns_ns_ahoc, factor_key=TRUE)
df_soc_long$value <- as.numeric(as.character(df_soc_long$value))
aggdata<- df_soc_long %>% group_by(category, wnns_ns) %>% summarise(total = sum(value))
aggData_wide <- spread(aggdata, wnns_ns, total)
chisq <- chisq.test(as.matrix(aggData_wide[,-1]))
corrplot(chisq$residuals, is.cor = FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)
round(chisq$stdres, 3)


# Why havent you applied for second installments


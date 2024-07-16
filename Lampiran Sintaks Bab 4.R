#---- Run Data Skripsi ----

#---- Import Data -----
library(readxl)
# Data awal sebelum difilter (12098 obs, 535 variables)
pisa18_IDN <- read_excel("E:/Documents/UNJ/SKRIPSI/SPS/PISA/Data/
                         Questionnare Data/Indo/PISA2018_StuQue_IDN.xlsx")

# list variabel pisa18 (dari fungsi pisa.var.label(), ngebaca file sav
library(intsvy)
pisa18_varlist
datarerun <- pisa18_IDN # 12098 obs
PVREAD <- pisa18_IDN[,464:473] #10 Plausible Value for Read Score 
readscore <- rowMeans(PVREAD)
library(tibble)
datarerun <- add_column(datarerun, readscore)
summary(datarerun$readscore)

#---- Variable Filter ----
# missing values
datarerun # 12098 obs
which(is.na(datarerun))
misRow <- apply(datarerun,1,function(X) any(is.na(X))) #missing values by row
sum(misRow==TRUE) # 1462 obs
#na.omit Remove Any Row with NA’s
datarerun <- na.omit(datarerun) #deleting missing values -> 10636 obs 

# adjust X
varlist_rerun <- names(a)
varlist_rerun <- varlist_rerun[-309]
varlist_rerun <- append(varlist_rerun, c("AGE","readscore"))
varlist_rerun <- append(varlist_rerun, "CNTSTUID",after = 0)
str(varlist_rerun)

library(dplyr)
datarerun <- datarerun %>% select(all_of(varlist_rerun))

#find labeled missing data
# 99/9 No Response
# ST005Q01TA (99 = 134 obs)
datarerun %>% filter(!ST005Q01TA == 99)
datarerun <- datarerun %>% filter(!ST005Q01TA == 99)# 10502 obs
# ST011Q01TA (9 = 79 obs)
datarerun %>% filter(!ST011Q01TA == 9) 
datarerun <- datarerun %>% filter(!ST011Q01TA == 9)# 10423 obs
# ST011Q02TA (9 = 32 obs)
datarerun %>% filter(!ST011Q02TA == 9)
datarerun <- datarerun %>% filter(!ST011Q02TA == 9)# 10391 obs
# ST011Q03TA (9 = 53 obs)
datarerun %>% filter(!ST011Q03TA == 9)
datarerun <- datarerun %>% filter(!ST011Q03TA == 9)# 10338 obs
# ST011Q04TA (9 = 19 obs)
datarerun %>% filter(!ST011Q04TA == 9)
datarerun <- datarerun %>% filter(!ST011Q04TA == 9)# 10319 obs
# ST011Q05TA (9 = 28 obs)
datarerun %>% filter(!ST011Q05TA == 9)
datarerun <- datarerun %>% filter(!ST011Q05TA == 9)# 10291 obs
# ST011Q06TA (9 = 27 obs)
datarerun %>% filter(!ST011Q06TA == 9)
datarerun <- datarerun %>% filter(!ST011Q06TA == 9)# 10264 obs
# ST011Q07TA (9 = 20 obs)
datarerun %>% filter(!ST011Q07TA == 9)
datarerun <- datarerun %>% filter(!ST011Q07TA == 9)# 10244 obs
# ST011Q08TA (9 = 28 obs)
datarerun %>% filter(!ST011Q08TA == 9)
datarerun <- datarerun %>% filter(!ST011Q08TA == 9)# 10216 obs
# ST011Q09TA (9 = 36 obs)
datarerun %>% filter(!ST011Q09TA == 9)
datarerun <- datarerun %>% filter(!ST011Q09TA == 9)# 10180 obs
# ST011Q10TA (9 = 15 obs)
datarerun %>% filter(!ST011Q10TA == 9)
datarerun <- datarerun %>% filter(!ST011Q10TA == 9)# 10165 obs
# ST011Q11TA (9 = 57 obs)
datarerun %>% filter(!ST011Q11TA == 9)
datarerun <- datarerun %>% filter(!ST011Q11TA == 9)# 10108 obs
# ST011Q12TA (9 = 38 obs)
datarerun %>% filter(!ST011Q12TA == 9)
datarerun <- datarerun %>% filter(!ST011Q12TA == 9)# 10070 obs
# ST011Q16NA (9 = 48 obs)
datarerun %>% filter(!ST011Q16NA == 9)
datarerun <- datarerun %>% filter(!ST011Q16NA == 9)# 10022 obs
# ST012Q01TA (9 = 103 obs)
datarerun %>% filter(!ST012Q01TA == 9)
datarerun <- datarerun %>% filter(!ST012Q01TA == 9)# 9919 obs
# ST012Q03TA (9 = 147 obs)
datarerun %>% filter(!ST012Q03TA == 9)
datarerun <- datarerun %>% filter(!ST012Q03TA == 9)# 9772 obs
# ST012Q05NA (9 = 60 obs)
datarerun %>% filter(!ST012Q05NA == 9)
datarerun <- datarerun %>% filter(!ST012Q05NA == 9)# 9712 obs
# ST012Q06NA (9 = 40 obs)
datarerun %>% filter(!ST012Q06NA == 9)
datarerun <- datarerun %>% filter(!ST012Q06NA == 9)# 9672 obs
# ST012Q07NA (9 = 47 obs)
datarerun %>% filter(!ST012Q07NA == 9)
datarerun <- datarerun %>% filter(!ST012Q07NA == 9)# 9625 obs
# ST012Q08NA (9 = 52 obs)
datarerun %>% filter(!ST012Q08NA == 9)
datarerun <- datarerun %>% filter(!ST012Q08NA == 9)# 9573 obs
# ST012Q09NA (9 = 60 obs)
datarerun %>% filter(!ST012Q09NA == 9)
datarerun <- datarerun %>% filter(!ST012Q09NA == 9)# 9513 obs
# ST013Q01TA (99 = 18 obs)
datarerun %>% filter(!ST013Q01TA == 99)
datarerun <- datarerun %>% filter(!ST013Q01TA == 99)# 9495 obs
# drop variable c(ST019AQ01T, ST019AQ01T, ST019CQ01T)
datarerun <- datarerun %>% select(-ST019AQ01T, -ST019BQ01T, -ST019CQ01T) #308 variabel
# ST125Q01NA (8 = 862, 99 = 52) drop?
# ST126Q01TA (98 = 71, 99 = 34)
datarerun %>% filter(!ST126Q01TA == 99 & !ST126Q01TA == 98)
datarerun <- datarerun %>% filter(!ST126Q01TA == 99 & !ST126Q01TA == 98)# 9390 obs
# ST127Q01TA (9 = 64)
datarerun %>% filter(!ST127Q01TA == 9)
datarerun <- datarerun %>% filter(!ST127Q01TA == 9) # 9326 obs
# ST127Q02TA (9 = 643)
datarerun %>% filter(!ST127Q02TA == 9)
datarerun <- datarerun %>% filter(!ST127Q02TA == 9) # 8683 obs
# ST127Q03TA (9 = 1452) drop?
# ST022Q01TA (99 = 2)
datarerun %>% filter(!ST022Q01TA == 99)
datarerun <- datarerun %>% filter(!ST022Q01TA == 99) # 8681 obs
# ST023Q01TA (4 = 70, 9 = 13)
datarerun %>% filter(ST023Q01TA == 9 | ST023Q01TA == 4 )
datarerun <- datarerun %>% filter(!ST023Q01TA == 4 & !ST023Q01TA == 9) # 8598 obs
# ST023Q02TA (4 = 42, 9 = 60)
datarerun %>% filter(ST023Q02TA == 9 | ST023Q02TA == 4 )
datarerun <- datarerun %>% filter(!ST023Q02TA == 4 & !ST023Q02TA == 9) # 8496 obs
# ST023Q03TA (4 = 119, 9 = 92)
datarerun %>% filter(ST023Q03TA == 9 | ST023Q03TA == 4 )
datarerun <- datarerun %>% filter(!ST023Q03TA == 4 & !ST023Q03TA == 9) # 8285 obs
# ST023Q04TA (4 = 40, 9 = 13)
datarerun %>% filter(ST023Q04TA == 9 | ST023Q04TA == 4 )
datarerun <- datarerun %>% filter(!ST023Q04TA == 4 & !ST023Q04TA == 9) # 8232 obs
# ST023Q05TA
datarerun %>% filter(ST023Q05TA == 9 | ST023Q05TA == 4 )
datarerun <- datarerun %>% filter(!ST023Q05TA == 4 & !ST023Q05TA == 9) # 8201 obs
# ST097Q01TA
datarerun %>% filter(ST097Q01TA == 9)
datarerun <- datarerun %>% filter(!ST097Q01TA == 9) # 8191 obs
# ST097Q02TA   
datarerun %>% filter(ST097Q02TA == 9)
datarerun <- datarerun %>% filter(!ST097Q02TA == 9) # 8174 obs
# ST097Q03TA
datarerun %>% filter(ST097Q03TA == 9)
datarerun <- datarerun %>% filter(!ST097Q03TA == 9) # 8138 obs
# ST097Q04TA
datarerun %>% filter(ST097Q04TA == 9)
datarerun <- datarerun %>% filter(!ST097Q04TA == 9) # 8108 obs
# ST097Q05TA
datarerun %>% filter(ST097Q05TA == 9)
datarerun <- datarerun %>% filter(!ST097Q05TA == 9) # 8091 obs
# ST100Q01TA
datarerun %>% filter(ST100Q01TA == 9)
datarerun <- datarerun %>% filter(!ST100Q01TA == 9) # 8079 obs
# ST100Q02TA
datarerun %>% filter(ST100Q02TA == 9)
datarerun <- datarerun %>% filter(!ST100Q02TA == 9) # 8073 obs
# ST100Q03TA
datarerun %>% filter(ST100Q03TA == 9)
datarerun <- datarerun %>% filter(!ST100Q03TA == 9) # 8060 obs
# ST100Q04TA
datarerun %>% filter(ST100Q04TA == 9)
datarerun <- datarerun %>% filter(!ST100Q04TA == 9) # 8048 obs
# ST102Q01TA
datarerun %>% filter(ST102Q01TA == 9)
datarerun <- datarerun %>% filter(!ST102Q01TA == 9) # 8030 obs
# ST102Q02TA
datarerun %>% filter(ST102Q02TA == 9)
datarerun <- datarerun %>% filter(!ST102Q02TA == 9) # 8015 obs
# ST102Q03TA
datarerun %>% filter(ST102Q03TA == 9)
datarerun <- datarerun %>% filter(!ST102Q03TA == 9) # 8003 obs  
# ST102Q04TA
datarerun %>% filter(ST102Q04TA == 9)
datarerun <- datarerun %>% filter(!ST102Q04TA == 9) # 7988 obs
# ST211Q01HA
datarerun %>% filter(ST211Q01HA == 9)
datarerun <- datarerun %>% filter(!ST211Q01HA == 9) # 7981 obs
# ST211Q02HA
datarerun %>% filter(ST211Q02HA == 9)
datarerun <- datarerun %>% filter(!ST211Q02HA == 9) # 7976 obs  
# ST211Q03HA
datarerun %>% filter(ST211Q03HA == 9)
datarerun <- datarerun %>% filter(!ST211Q03HA == 9) # 7966 obs
# ST211Q04HA
datarerun %>% filter(ST211Q03HA == 9)
datarerun <- datarerun %>% filter(!ST211Q03HA == 9) # 7966 obs
# ST212Q01HA
datarerun %>% filter(ST212Q01HA == 9)
datarerun <- datarerun %>% filter(!ST212Q01HA == 9) # 7952 obs
# ST212Q02HA
datarerun %>% filter(ST212Q02HA == 9)
datarerun <- datarerun %>% filter(!ST212Q02HA == 9) # 7940 obs
# ST212Q03HA
datarerun %>% filter(ST212Q03HA == 9)
datarerun <- datarerun %>% filter(!ST212Q03HA == 9) # 7921 obs
# ST104Q02NA
datarerun %>% filter(ST104Q02NA == 9)
datarerun <- datarerun %>% filter(!ST104Q02NA == 9) # 7907 obs
# ST104Q03NA
datarerun %>% filter(ST104Q03NA == 9)
datarerun <- datarerun %>% filter(!ST104Q03NA == 9) # 7897 obs
# ST104Q04NA
datarerun %>% filter(ST104Q04NA == 9)
datarerun <- datarerun %>% filter(!ST104Q04NA == 9) # 7881 obs
# ST213Q01HA
datarerun %>% filter(ST213Q01HA == 9)
datarerun <- datarerun %>% filter(!ST213Q01HA == 9) # 7869 obs
# ST213Q02HA
datarerun %>% filter(ST213Q02HA == 9)
datarerun <- datarerun %>% filter(!ST213Q02HA == 9) # 7849 obs
# ST213Q03HA
datarerun %>% filter(ST213Q03HA == 9)
datarerun <- datarerun %>% filter(!ST213Q03HA == 9) # 7841 obs
# ST213Q04HA
datarerun %>% filter(ST213Q04HA == 9)
datarerun <- datarerun %>% filter(!ST213Q04HA == 9) # 7825 obs
# ST150Q01IA
datarerun %>% filter(ST150Q01IA == 9)
datarerun <- datarerun %>% filter(!ST150Q01IA == 9) # 7812 obs
# ST150Q02IA
datarerun %>% filter(ST150Q02IA == 9)
datarerun <- datarerun %>% filter(!ST150Q02IA == 9) # 7803 obs
# ST150Q03IA
datarerun %>% filter(ST150Q03IA == 9)
datarerun <- datarerun %>% filter(!ST150Q03IA == 9) # 7782 obs
# ST150Q04HA
datarerun %>% filter(ST150Q04HA == 9)
datarerun <- datarerun %>% filter(!ST150Q04HA == 9) # 7763 obs
# ST152Q05IA
datarerun %>% filter(ST152Q05IA == 9)
datarerun <- datarerun %>% filter(!ST152Q05IA == 9) # 7752 obs
# ST152Q06IA
datarerun %>% filter(ST152Q06IA == 9)
datarerun <- datarerun %>% filter(!ST152Q06IA == 9) # 7741 obs
# ST152Q07IA
datarerun %>% filter(ST152Q07IA == 9)
datarerun <- datarerun %>% filter(!ST152Q07IA == 9) # 7715 obs
# ST152Q08IA
datarerun %>% filter(ST152Q08IA == 9)
datarerun <- datarerun %>% filter(!ST152Q08IA == 9) # 7699 obs
# ST154Q01HA
datarerun %>% filter(ST154Q01HA == 99)
datarerun <- datarerun %>% filter(!ST154Q01HA == 99) # 7688 obs
# ST153Q01HA
datarerun %>% filter(ST153Q01HA == 9)
datarerun <- datarerun %>% filter(!ST153Q01HA == 9) # 7679 obs
# ST153Q02HA
datarerun %>% filter(ST153Q02HA == 9)
datarerun <- datarerun %>% filter(!ST153Q02HA == 9) # 7654 obs
# ST153Q03HA
datarerun %>% filter(ST153Q03HA == 9)
datarerun <- datarerun %>% filter(!ST153Q03HA == 9) # 7638 obs
# ST153Q04HA
datarerun %>% filter(ST153Q04HA == 9)
datarerun <- datarerun %>% filter(!ST153Q04HA == 9) # 7629 obs
# ST153Q05HA
datarerun %>% filter(ST153Q05HA == 9)
datarerun <- datarerun %>% filter(!ST153Q05HA == 9) # 7618 obs
# ST153Q06HA
datarerun %>% filter(ST153Q06HA == 9)
datarerun <- datarerun %>% filter(!ST153Q06HA == 9) # 7602 obs
# ST153Q08HA
datarerun %>% filter(ST153Q08HA == 9)
datarerun <- datarerun %>% filter(!ST153Q08HA == 9) # 7578 obs 
# ST153Q09HA
datarerun %>% filter(ST153Q09HA == 9)
datarerun <- datarerun %>% filter(!ST153Q09HA == 9) # 7560 obs
# ST153Q10HA
datarerun %>% filter(ST153Q10HA == 9)
datarerun <- datarerun %>% filter(!ST153Q10HA == 9) # 7545 obs
# ST158Q01HA
datarerun %>% filter(ST158Q01HA == 9)
datarerun <- datarerun %>% filter(!ST158Q01HA == 9) # 7531 obs
# ST158Q02HA
datarerun %>% filter(ST158Q02HA == 9)
datarerun <- datarerun %>% filter(!ST158Q02HA == 9) # 7504 obs
# ST158Q03HA
datarerun %>% filter(ST158Q03HA == 9)
datarerun <- datarerun %>% filter(!ST158Q03HA == 9) # 7479 obs
# ST158Q04HA
datarerun %>% filter(ST158Q04HA == 9)
datarerun <- datarerun %>% filter(!ST158Q04HA == 9) # 7461 obs
# ST158Q05HA
datarerun %>% filter(ST158Q05HA == 9)
datarerun <- datarerun %>% filter(!ST158Q05HA == 9) # 7436 obs
# ST158Q06HA
datarerun %>% filter(ST158Q06HA == 9)
datarerun <- datarerun %>% filter(!ST158Q06HA == 9) # 7412 obs
# ST158Q07HA
datarerun %>% filter(ST158Q07HA == 9)
datarerun <- datarerun %>% filter(!ST158Q07HA == 9) # 7393 obs
# ST160Q01IA
datarerun %>% filter(ST160Q01IA == 9)
datarerun <- datarerun %>% filter(!ST160Q01IA == 9) # 7385 obs
# ST160Q02IA
datarerun %>% filter(ST160Q02IA == 9)
datarerun <- datarerun %>% filter(!ST160Q02IA == 9) # 7355 obs
# ST160Q03IA
summary(as.factor(datarerun$ST160Q03IA))
datarerun <- datarerun %>% filter(!ST160Q03IA == 9) # 7310 obs
# ST160Q04IA
summary(as.factor(datarerun$ST160Q04IA))
datarerun <- datarerun %>% filter(!ST160Q04IA == 9) # 7284 obs
# ST160Q05IA
summary(as.factor(datarerun$ST160Q05IA))
datarerun <- datarerun %>% filter(!ST160Q05IA == 9) # 7274 obs
# ST167Q01IA
summary(as.factor(datarerun$ST167Q01IA))
datarerun <- datarerun %>% filter(!ST167Q01IA == 99) # 7254 obs
# ST167Q02IA
summary(as.factor(datarerun$ST167Q02IA))
datarerun <- datarerun %>% filter(!ST167Q02IA == 99) # 7228 obs
# ST167Q03IA
summary(as.factor(datarerun$ST167Q03IA))
datarerun <- datarerun %>% filter(!ST167Q03IA == 99) # 7207 obs
# ST167Q04IA
summary(as.factor(datarerun$ST167Q04IA))
datarerun <- datarerun %>% filter(!ST167Q04IA == 99) # 7186 obs
# ST167Q05IA
summary(as.factor(datarerun$ST167Q05IA))
datarerun <- datarerun %>% filter(!ST167Q05IA == 99) # 7162 obs
# ST168Q01HA
summary(as.factor(datarerun$ST168Q01HA))
datarerun <- datarerun %>% filter(!ST168Q01HA == 9) # 7149 obs
# ST175Q01IA
summary(as.factor(datarerun$ST175Q01IA))
datarerun <- datarerun %>% filter(!ST175Q01IA == 99) # 7138 obs
# ST176Q01IA
summary(as.factor(datarerun$ST176Q01IA))
datarerun <- datarerun %>% filter(!ST176Q01IA == 99) # 7086 obs
# ST176Q02IA
summary(as.factor(datarerun$ST176Q02IA))
datarerun <- datarerun %>% filter(!ST176Q02IA == 99) # 7056 obs
# ST176Q05IA
summary(as.factor(datarerun$ST176Q05IA))
datarerun <- datarerun %>% filter(!ST176Q05IA == 99) # 7019 obs
# ST176Q03IA
summary(as.factor(datarerun$ST176Q03IA))
datarerun <- datarerun %>% filter(!ST176Q03IA == 99) # 6985 obs
# ST176Q06IA
summary(as.factor(datarerun$ST176Q06IA))
datarerun <- datarerun %>% filter(!ST176Q06IA == 99) # 6948 obs
# ST176Q07IA
summary(as.factor(datarerun$ST176Q07IA))
datarerun <- datarerun %>% filter(!ST176Q07IA == 99) # 6934 obs
# ST161Q01HA
summary(as.factor(datarerun$ST161Q01HA))
datarerun <- datarerun %>% filter(!ST161Q01HA == 9) # 6922 obs
# ST161Q02HA
summary(as.factor(datarerun$ST161Q02HA))
datarerun <- datarerun %>% filter(!ST161Q02HA == 9) # 6906 obs
# ST161Q03HA
summary(as.factor(datarerun$ST161Q03HA))
datarerun <- datarerun %>% filter(!ST161Q03HA == 9) # 6853 obs
# ST161Q06HA
summary(as.factor(datarerun$ST161Q06HA))
datarerun <- datarerun %>% filter(!ST161Q06HA == 9) # 6838 obs
# ST161Q07HA
summary(as.factor(datarerun$ST161Q07HA))
datarerun <- datarerun %>% filter(!ST161Q07HA == 9) # 6818 obs
# ST161Q08HA
summary(as.factor(datarerun$ST161Q08HA))
datarerun <- datarerun %>% filter(!ST161Q08HA == 9) # 6804 obs
# ST163Q02HA
summary(as.factor(datarerun$ST163Q02HA))
datarerun <- datarerun %>% filter(!ST163Q02HA == 9) # 6796 obs
# ST163Q03HA
summary(as.factor(datarerun$ST163Q03HA))
datarerun <- datarerun %>% filter(!ST163Q03HA == 9) # 6778 obs
# ST163Q04HA
summary(as.factor(datarerun$ST163Q04HA))
datarerun <- datarerun %>% filter(!ST163Q04HA == 9) # 6754 obs
# ST164Q01IA
summary(as.factor(datarerun$ST164Q01IA)) #gak ada no response
# ST164Q02IA
summary(as.factor(datarerun$ST164Q02IA)) #gak ada no response
# ST164Q03IA
summary(as.factor(datarerun$ST164Q03IA)) #gak ada no response
# ST164Q04IA
summary(as.factor(datarerun$ST164Q04IA)) #gak ada no response
# ST164Q05IA
summary(as.factor(datarerun$ST164Q05IA)) #gak ada no response
# ST164Q06IA
summary(as.factor(datarerun$ST164Q06IA)) #gak ada no response
# ST165Q01IA
summary(as.factor(datarerun$ST165Q01IA)) #gak ada no response
# ST165Q02IA
summary(as.factor(datarerun$ST165Q02IA)) #gak ada no response
# ST165Q03IA
summary(as.factor(datarerun$ST165Q03IA)) #gak ada no response
# ST165Q04IA
summary(as.factor(datarerun$ST165Q04IA)) #gak ada no response
# ST165Q05IA
summary(as.factor(datarerun$ST165Q05IA)) #gak ada no response
# ST166Q01HA 
summary(as.factor(datarerun$ST166Q01HA)) #gak ada no response
# ST166Q02HA 
summary(as.factor(datarerun$ST166Q02HA)) #gak ada no response
# ST166Q03HA 
summary(as.factor(datarerun$ST166Q03HA)) #gak ada no response
# ST166Q04HA
summary(as.factor(datarerun$ST166Q04HA)) #gak ada no response
# ST166Q05HA 
summary(as.factor(datarerun$ST166Q05HA)) #gak ada no response 
# ST036Q05TA
summary(as.factor(datarerun$ST036Q05TA))
datarerun <- datarerun %>% filter(!ST036Q05TA == 9) # 6749 obs
# ST036Q06TA
summary(as.factor(datarerun$ST036Q06TA))
datarerun <- datarerun %>% filter(!ST036Q06TA == 9) # 6737 obs
# ST036Q08TA
summary(as.factor(datarerun$ST036Q08TA))
datarerun <- datarerun %>% filter(!ST036Q08TA == 9) # 6723 obs
# ST225Q01HA
summary(as.factor(datarerun$ST225Q01HA)) #gaada No Response
# ST225Q02HA
summary(as.factor(datarerun$ST225Q02HA)) #gaada No Response
# ST225Q03HA
summary(as.factor(datarerun$ST225Q03HA)) #gaada No Response
# ST225Q04HA
summary(as.factor(datarerun$ST225Q04HA)) #gaada No Response
# ST225Q05HA
summary(as.factor(datarerun$ST225Q05HA)) #gaada No Response  
# ST225Q06HA
summary(as.factor(datarerun$ST225Q06HA)) #gaada No Response
# ST181Q02HA
summary(as.factor(datarerun$ST181Q02HA))
datarerun <- datarerun %>% filter(!ST181Q02HA == 9) # 6713 obs
# ST181Q03HA
summary(as.factor(datarerun$ST181Q03HA))
datarerun <- datarerun %>% filter(!ST181Q03HA == 9) # 6705 obs
# ST181Q04HA
summary(as.factor(datarerun$ST181Q04HA))
datarerun <- datarerun %>% filter(!ST181Q04HA == 9) # 6687 obs
# ST182Q03HA
summary(as.factor(datarerun$ST182Q03HA))
datarerun <- datarerun %>% filter(!ST182Q03HA == 9) # 6678 obs
# ST182Q04HA
summary(as.factor(datarerun$ST182Q04HA))
datarerun <- datarerun %>% filter(!ST182Q04HA == 9) # 6664 obs
# ST182Q05HA
summary(as.factor(datarerun$ST182Q05HA))
datarerun <- datarerun %>% filter(!ST182Q05HA == 9) # 6648 obs
# ST182Q06HA
summary(as.factor(datarerun$ST182Q06HA))
datarerun <- datarerun %>% filter(!ST182Q06HA == 9) # 6633 obs
# ST183Q01HA
summary(as.factor(datarerun$ST183Q01HA))
datarerun <- datarerun %>% filter(!ST183Q01HA == 9) # 6621 obs
# ST183Q02HA
summary(as.factor(datarerun$ST183Q02HA))
datarerun <- datarerun %>% filter(!ST183Q02HA == 9) # 6612 obs
# ST183Q03HA
summary(as.factor(datarerun$ST183Q03HA))
datarerun <- datarerun %>% filter(!ST183Q03HA == 9) # 6604 obs
# ST184Q01HA
summary(as.factor(datarerun$ST184Q01HA))
datarerun <- datarerun %>% filter(!ST184Q01HA == 9) # 6595 obs
# ST185Q01HA
summary(as.factor(datarerun$ST185Q01HA))
datarerun <- datarerun %>% filter(!ST185Q01HA == 9) # 6594 obs
# ST185Q02HA
summary(as.factor(datarerun$ST185Q02HA))
datarerun <- datarerun %>% filter(!ST185Q02HA == 9) # 6586 obs
# ST185Q03HA
summary(as.factor(datarerun$ST185Q03HA))
datarerun <- datarerun %>% filter(!ST185Q03HA == 9) # 6574 obs
# ST186Q05HA
summary(as.factor(datarerun$ST186Q05HA))
datarerun <- datarerun %>% filter(!ST186Q05HA == 9) # 6568 obs
# ST186Q05HA
summary(as.factor(datarerun$ST186Q05HA))
datarerun <- datarerun %>% filter(!ST186Q05HA == 9) # 6568 obs
# ST186Q06HA
summary(as.factor(datarerun$ST186Q06HA))
datarerun <- datarerun %>% filter(!ST186Q06HA == 9) # 6549 obs
# ST186Q07HA
summary(as.factor(datarerun$ST186Q07HA))
datarerun <- datarerun %>% filter(!ST186Q07HA == 9) # 6531 obs
# ST186Q10HA
summary(as.factor(datarerun$ST186Q10HA))
datarerun <- datarerun %>% filter(!ST186Q10HA == 9) # 6524 obs
# ST186Q09HA
summary(as.factor(datarerun$ST186Q09HA))
datarerun <- datarerun %>% filter(!ST186Q09HA == 9) # 6503 obs
# ST186Q02HA
summary(as.factor(datarerun$ST186Q02HA))
datarerun <- datarerun %>% filter(!ST186Q02HA == 9) # 6492 obs
# ST186Q01HA
summary(as.factor(datarerun$ST186Q01HA))
datarerun <- datarerun %>% filter(!ST186Q01HA == 9) # 6474 obs
# ST186Q08HA
summary(as.factor(datarerun$ST186Q08HA))
datarerun <- datarerun %>% filter(!ST186Q08HA == 9) # 6458 obs
# ST186Q03HA
summary(as.factor(datarerun$ST186Q03HA))
datarerun <- datarerun %>% filter(!ST186Q03HA == 9) # 6438 obs
# ST208Q01HA
summary(as.factor(datarerun$ST208Q01HA))
datarerun <- datarerun %>% filter(!ST208Q01HA == 99) # 6434 obs
# ST208Q02HA
summary(as.factor(datarerun$ST208Q02HA))
datarerun <- datarerun %>% filter(!ST208Q02HA == 99) # 6427 obs
# ST208Q04HA
summary(as.factor(datarerun$ST208Q04HA))
datarerun <- datarerun %>% filter(!ST208Q04HA == 99) # 6421 obs
# ST188Q01HA
summary(as.factor(datarerun$ST188Q01HA))
datarerun <- datarerun %>% filter(!ST188Q01HA == 9) # 6413 obs
# ST188Q02HA
summary(as.factor(datarerun$ST188Q02HA))
datarerun <- datarerun %>% filter(!ST188Q02HA == 9) # 6393 obs
# ST188Q03HA
summary(as.factor(datarerun$ST188Q03HA))
datarerun <- datarerun %>% filter(!ST188Q03HA == 9) # 6372 obs
# ST188Q06HA
summary(as.factor(datarerun$ST188Q06HA))
datarerun <- datarerun %>% filter(!ST188Q06HA == 9) # 6350 obs
# ST188Q07HA
summary(as.factor(datarerun$ST188Q07HA))
datarerun <- datarerun %>% filter(!ST188Q07HA == 9) # 6329 obs
# ST034Q01TA
summary(as.factor(datarerun$ST034Q01TA))
datarerun <- datarerun %>% filter(!ST034Q01TA == 9) # 6316 obs
# ST034Q02TA
summary(as.factor(datarerun$ST034Q02TA))
datarerun <- datarerun %>% filter(!ST034Q02TA == 9) # 6303 obs
# ST034Q03TA
summary(as.factor(datarerun$ST034Q03TA))
datarerun <- datarerun %>% filter(!ST034Q03TA == 9) # 6277 obs
# ST034Q04TA
summary(as.factor(datarerun$ST034Q04TA))
datarerun <- datarerun %>% filter(!ST034Q04TA == 9) # 6257 obs
# ST034Q05TA
summary(as.factor(datarerun$ST034Q05TA))
datarerun <- datarerun %>% filter(!ST034Q05TA == 9) # 6245 obs
# ST034Q06TA
summary(as.factor(datarerun$ST034Q06TA))
datarerun <- datarerun %>% filter(!ST034Q06TA == 9) # 6226 obs
# ST034Q06TA
summary(as.factor(datarerun$ST034Q06TA))
datarerun <- datarerun %>% filter(!ST034Q06TA == 9) # 6226 obs
# ST196Q02HA
summary(as.factor(datarerun$ST196Q02HA))
datarerun <- datarerun %>% filter(!ST196Q02HA == 9) # 6217 obs
# ST196Q03HA
summary(as.factor(datarerun$ST196Q03HA))
datarerun <- datarerun %>% filter(!ST196Q03HA == 9) # 6190 obs
# ST196Q04HA
summary(as.factor(datarerun$ST196Q04HA))
datarerun <- datarerun %>% filter(!ST196Q04HA == 9) # 6133 obs
# ST196Q05HA
summary(as.factor(datarerun$ST196Q05HA))
datarerun <- datarerun %>% filter(!ST196Q05HA == 9) # 6089 obs
# ST196Q06HA
summary(as.factor(datarerun$ST196Q06HA))
datarerun <- datarerun %>% filter(!ST196Q06HA == 9) # 6069 obs
# ST196Q07HA
summary(as.factor(datarerun$ST196Q07HA))
datarerun <- datarerun %>% filter(!ST196Q07HA == 9) # 6049 obs
# ST197Q01HA
summary(as.factor(datarerun$ST197Q01HA))
datarerun <- datarerun %>% filter(!ST197Q01HA == 9) # 6035 obs
# ST197Q02HA
summary(as.factor(datarerun$ST197Q02HA))
datarerun <- datarerun %>% filter(!ST197Q02HA == 9) # 6018 obs
# ST197Q04HA
summary(as.factor(datarerun$ST197Q04HA))
datarerun <- datarerun %>% filter(!ST197Q04HA == 9) # 5988 obs
# ST197Q07HA
summary(as.factor(datarerun$ST197Q07HA))
datarerun <- datarerun %>% filter(!ST197Q07HA == 9) # 5968 obs
# ST197Q08HA
summary(as.factor(datarerun$ST197Q08HA))
datarerun <- datarerun %>% filter(!ST197Q08HA == 9) # 5944 obs
# ST197Q09HA
summary(as.factor(datarerun$ST197Q09HA))
datarerun <- datarerun %>% filter(!ST197Q09HA == 9) # 5918 obs
# ST197Q12HA
summary(as.factor(datarerun$ST197Q12HA))
datarerun <- datarerun %>% filter(!ST197Q12HA == 9) # 5909 obs
# ST215Q01HA
summary(as.factor(datarerun$ST215Q01HA))
datarerun <- datarerun %>% filter(!ST215Q01HA == 99) # 5899 obs
# ST215Q02HA
summary(as.factor(datarerun$ST215Q02HA))
datarerun <- datarerun %>% filter(!ST215Q02HA == 99) # 5884 obs
# ST215Q03HA
summary(as.factor(datarerun$ST215Q03HA))
datarerun <- datarerun %>% filter(!ST215Q03HA == 99) # 5872 obs
# ST215Q04HA
summary(as.factor(datarerun$ST215Q04HA))
datarerun <- datarerun %>% filter(!ST215Q04HA == 99) # 5855 obs
# ST215Q05HA
summary(as.factor(datarerun$ST215Q05HA))
datarerun <- datarerun %>% filter(!ST215Q05HA == 99) # 5842 obs
# ST216Q01HA
summary(as.factor(datarerun$ST216Q01HA))
datarerun <- datarerun %>% filter(!ST216Q01HA == 99) # 5831 obs
# ST216Q02HA
summary(as.factor(datarerun$ST216Q02HA))
datarerun <- datarerun %>% filter(!ST216Q02HA == 99) # 5808 obs
# ST216Q03HA
summary(as.factor(datarerun$ST216Q03HA))
datarerun <- datarerun %>% filter(!ST216Q03HA == 99) # 5785 obs
# ST216Q04HA
summary(as.factor(datarerun$ST216Q04HA))
datarerun <- datarerun %>% filter(!ST216Q04HA == 99) # 5753 obs
# ST216Q05HA
summary(as.factor(datarerun$ST216Q05HA))
datarerun <- datarerun %>% filter(!ST216Q05HA == 99) # 5740 obs
# ST216Q06HA
summary(as.factor(datarerun$ST216Q06HA))
datarerun <- datarerun %>% filter(!ST216Q06HA == 99) # 5720 obs
# ST218Q01HA
summary(as.factor(datarerun$ST218Q01HA))
datarerun <- datarerun %>% filter(!ST218Q01HA == 9) # 5705 obs
# ST218Q02HA
summary(as.factor(datarerun$ST218Q02HA))
datarerun <- datarerun %>% filter(!ST218Q02HA == 9) # 5686 obs
# ST218Q03HA
summary(as.factor(datarerun$ST218Q03HA))
datarerun <- datarerun %>% filter(!ST218Q03HA == 9) # 5663 obs
# ST218Q04HA
summary(as.factor(datarerun$ST218Q04HA))
datarerun <- datarerun %>% filter(!ST218Q04HA == 9) # 5640 obs
# ST218Q05HA
summary(as.factor(datarerun$ST218Q05HA))
datarerun <- datarerun %>% filter(!ST218Q05HA == 9) # 5623 obs
# ST218Q06HA
summary(as.factor(datarerun$ST218Q06HA))
datarerun <- datarerun %>% filter(!ST218Q06HA == 9) # 5599 obs
# ST218Q07HA
summary(as.factor(datarerun$ST218Q07HA))
datarerun <- datarerun %>% filter(!ST218Q07HA == 9) # 5584 obs
# ST222Q01HA
summary(as.factor(datarerun$ST222Q01HA))
datarerun <- datarerun %>% filter(!ST222Q01HA == 9) # 5569 obs
#ST222Q03HA
summary(as.factor(datarerun$ST222Q03HA))
datarerun <- datarerun %>% filter(!ST222Q03HA == 9) # 5558 obs
#ST222Q04HA
summary(as.factor(datarerun$ST222Q04HA))
datarerun <- datarerun %>% filter(!ST222Q04HA == 9) # 5542 obs
#ST222Q05HA
summary(as.factor(datarerun$ST222Q05HA))
datarerun <- datarerun %>% filter(!ST222Q05HA == 9) # 5520 obs
#ST222Q06HA
summary(as.factor(datarerun$ST222Q06HA))
datarerun <- datarerun %>% filter(!ST222Q06HA == 9) # 5508 obs
#ST222Q08HA
summary(as.factor(datarerun$ST222Q08HA))
datarerun <- datarerun %>% filter(!ST222Q08HA == 9) # 5487 obs
#ST222Q09HA
summary(as.factor(datarerun$ST222Q09HA))
datarerun <- datarerun %>% filter(!ST222Q09HA == 9) # 5463 obs
#ST222Q10HA
summary(as.factor(datarerun$ST222Q10HA))
datarerun <- datarerun %>% filter(!ST222Q10HA == 9) # 5444 obs
# ST214Q01HA
summary(as.factor(datarerun$ST214Q01HA))
datarerun <- datarerun %>% filter(!ST214Q01HA == 99) # 5436 obs
# ST214Q02HA
summary(as.factor(datarerun$ST214Q02HA))
datarerun <- datarerun %>% filter(!ST214Q02HA == 99) # 5426 obs
# ST214Q03HA
summary(as.factor(datarerun$ST214Q03HA))
datarerun <- datarerun %>% filter(!ST214Q03HA == 99) # 5419 obs
# ST214Q06HA
summary(as.factor(datarerun$ST214Q06HA))
datarerun <- datarerun %>% filter(!ST214Q06HA == 99) # 5408 obs
# ST220Q01HA
summary(as.factor(datarerun$ST220Q01HA))
datarerun <- datarerun %>% filter(!ST220Q01HA == 9) # 5302 obs
# ST220Q02HA
summary(as.factor(datarerun$ST220Q02HA))
datarerun <- datarerun %>% filter(!ST220Q02HA == 9) # 5259 obs
# ST220Q03HA
summary(as.factor(datarerun$ST220Q03HA))
datarerun <- datarerun %>% filter(!ST220Q03HA == 9) # 5239 obs
# ST220Q04HA
summary(as.factor(datarerun$ST220Q04HA))
datarerun <- datarerun %>% filter(!ST220Q04HA == 9) # 5228 obs
# ST217Q01HA
summary(as.factor(datarerun$ST217Q01HA))
datarerun <- datarerun %>% filter(!ST217Q01HA == 99) # 5213 obs
# ST217Q02HA
summary(as.factor(datarerun$ST217Q02HA))
datarerun <- datarerun %>% filter(!ST217Q02HA == 99) # 5211 obs
# ST217Q03HA
summary(as.factor(datarerun$ST217Q03HA))
datarerun <- datarerun %>% filter(!ST217Q03HA == 99) # 5197 obs
# ST217Q04HA
summary(as.factor(datarerun$ST217Q04HA))
datarerun <- datarerun %>% filter(!ST217Q04HA == 99) # 5180 obs
# ST217Q05HA
summary(as.factor(datarerun$ST217Q05HA))
datarerun <- datarerun %>% filter(!ST217Q05HA == 99) # 5161 obs
# ST219Q01HA
summary(as.factor(datarerun$ST219Q01HA))
datarerun <- datarerun %>% filter(!ST219Q01HA == 9) # 5151 obss
# ST219Q02HA
summary(as.factor(datarerun$ST219Q02HA))
datarerun <- datarerun %>% filter(!ST219Q02HA == 9) # 5129 obs
# ST219Q03HA
summary(as.factor(datarerun$ST219Q03HA))
datarerun <- datarerun %>% filter(!ST219Q03HA == 9) # 5116 obs
# ST219Q04HA
summary(as.factor(datarerun$ST219Q04HA))
datarerun <- datarerun %>% filter(!ST219Q04HA == 9) # 5096 obs
# ST219Q05HA
summary(as.factor(datarerun$ST219Q05HA))
datarerun <- datarerun %>% filter(!ST219Q05HA == 9) # 5077 obs
# ST219Q06HA
summary(as.factor(datarerun$ST219Q06HA))
datarerun <- datarerun %>% filter(!ST219Q06HA == 9) # 5049 obs
# ST204Q02HA
summary(as.factor(datarerun$ST204Q02HA))
datarerun <- datarerun %>% filter(!ST204Q02HA == 9) # 5032 obs
# ST204Q03HA
summary(as.factor(datarerun$ST204Q03HA))
datarerun <- datarerun %>% filter(!ST204Q03HA == 9) # 5023 obs
# ST204Q04HA
summary(as.factor(datarerun$ST204Q04HA))
datarerun <- datarerun %>% filter(!ST204Q04HA == 9) # 5017 obs
# ST204Q05HA
summary(as.factor(datarerun$ST204Q05HA))
datarerun <- datarerun %>% filter(!ST204Q05HA == 9) # 5003 obs
# ST177Q01HA
summary(as.factor(datarerun$ST177Q01HA))
datarerun <- datarerun %>% filter(!ST177Q01HA == 9) # 4990 obs
# ST177Q02HA
summary(as.factor(datarerun$ST177Q02HA))
datarerun <- datarerun %>% filter(!ST177Q02HA == 9) # 4974  obs
# ST177Q03HA
summary(as.factor(datarerun$ST177Q03HA))
datarerun <- datarerun %>% filter(!ST177Q03HA == 9) # 4961  obs
# ST221Q01HA
summary(as.factor(datarerun$ST221Q01HA))
datarerun <- datarerun %>% filter(!ST221Q01HA == 9) # 4943  obs
# ST221Q02HA
summary(as.factor(datarerun$ST221Q02HA))
datarerun <- datarerun %>% filter(!ST221Q02HA == 9) # 4927  obs
# ST221Q03HA
summary(as.factor(datarerun$ST221Q03HA))
datarerun <- datarerun %>% filter(!ST221Q03HA == 9) # 4906 obs
# ST221Q04HA
summary(as.factor(datarerun$ST221Q04HA))
datarerun <- datarerun %>% filter(!ST221Q04HA == 9) # 4899 obs
# ST221Q05HA
summary(as.factor(datarerun$ST221Q05HA))
datarerun <- datarerun %>% filter(!ST221Q05HA == 9) # 4893 obs
# ST221Q06HA
summary(as.factor(datarerun$ST221Q06HA))
datarerun <- datarerun %>% filter(!ST221Q06HA == 9) # 4885 obs
# ST221Q07HA
summary(as.factor(datarerun$ST221Q07HA))
datarerun <- datarerun %>% filter(!ST221Q07HA == 9) # 4870 obs
# ST221Q08HA
summary(as.factor(datarerun$ST221Q08HA))
datarerun <- datarerun %>% filter(!ST221Q08HA == 9) # 4854 obs
# ST221Q09HA
summary(as.factor(datarerun$ST221Q09HA))
datarerun <- datarerun %>% filter(!ST221Q09HA == 9) # 4837 obs
# ST221Q11HA
summary(as.factor(datarerun$ST221Q11HA))
datarerun <- datarerun %>% filter(!ST221Q11HA == 9) # 4812 obs
# ST223Q02HA
summary(as.factor(datarerun$ST223Q02HA))
datarerun <- datarerun %>% filter(!ST223Q02HA == 9) # 4803 obs
# ST223Q04HA
summary(as.factor(datarerun$ST223Q04HA))
datarerun <- datarerun %>% filter(!ST223Q04HA == 9) # 4794 obs
# ST223Q05HA
summary(as.factor(datarerun$ST223Q05HA))
datarerun <- datarerun %>% filter(!ST223Q05HA == 9) # 4786 obs
# ST223Q08HA
summary(as.factor(datarerun$ST223Q08HA))
datarerun <- datarerun %>% filter(!ST223Q08HA == 9) # 4765 obs
# ST123Q02NA
summary(as.factor(datarerun$ST123Q02NA))
datarerun <- datarerun %>% filter(!ST123Q02NA == 9) # 4758 obs
# ST123Q03NA
summary(as.factor(datarerun$ST123Q03NA))
datarerun <- datarerun %>% filter(!ST123Q03NA == 9) # 4754 obs
# ST123Q04NA
summary(as.factor(datarerun$ST123Q04NA))
datarerun <- datarerun %>% filter(!ST123Q04NA == 9) # 4748 obs
# ST205Q01HA
summary(as.factor(datarerun$ST205Q01HA))
datarerun <- datarerun %>% filter(!ST205Q01HA == 9) # 4740 obs
# ST205Q02HA
summary(as.factor(datarerun$ST205Q02HA))
datarerun <- datarerun %>% filter(!ST205Q02HA == 9) # 4731 obs
# ST205Q03HA
summary(as.factor(datarerun$ST205Q03HA))
datarerun <- datarerun %>% filter(!ST205Q03HA == 9) # 4722 obs
# ST205Q04HA
summary(as.factor(datarerun$ST205Q04HA))
datarerun <- datarerun %>% filter(!ST205Q04HA == 9) # 4713 obs
# ST062Q01TA
summary(as.factor(datarerun$ST062Q01TA))
datarerun <- datarerun %>% filter(!ST062Q01TA == 9) # 4706 obs
# ST062Q02TA
summary(as.factor(datarerun$ST062Q02TA))
datarerun <- datarerun %>% filter(!ST062Q02TA == 9) # 4697 obs
# ST062Q03TA
summary(as.factor(datarerun$ST062Q03TA))
datarerun <- datarerun %>% filter(!ST062Q03TA == 9) # 4687 obs
# ST038Q03NA
summary(as.factor(datarerun$ST038Q03NA))
datarerun <- datarerun %>% filter(!ST038Q03NA == 9) # 4680 obs
# ST038Q04NA
summary(as.factor(datarerun$ST038Q04NA))
datarerun <- datarerun %>% filter(!ST038Q04NA == 9) # 4664 obs
# ST038Q05NA
summary(as.factor(datarerun$ST038Q05NA))
datarerun <- datarerun %>% filter(!ST038Q05NA == 9) # 4641 obs
# ST038Q06NA
summary(as.factor(datarerun$ST038Q06NA))
datarerun <- datarerun %>% filter(!ST038Q06NA == 9) # 4622 obs
# ST038Q07NA
summary(as.factor(datarerun$ST038Q07NA))
datarerun <- datarerun %>% filter(!ST038Q07NA == 9) # 4613 obs
# ST038Q08NA
summary(as.factor(datarerun$ST038Q08NA))
datarerun <- datarerun %>% filter(!ST038Q08NA == 9) # 4601 obs
# ST207Q01HA
summary(as.factor(datarerun$ST207Q01HA))
datarerun <- datarerun %>% filter(!ST207Q01HA == 9) # 4589 obs
# ST207Q02HA
summary(as.factor(datarerun$ST207Q02HA))
datarerun <- datarerun %>% filter(!ST207Q02HA == 9) # 4581 obs
# ST207Q03HA
summary(as.factor(datarerun$ST207Q03HA))
datarerun <- datarerun %>% filter(!ST207Q03HA == 9) # 4568 obs
# ST207Q04HA
summary(as.factor(datarerun$ST207Q04HA))
datarerun <- datarerun %>% filter(!ST207Q04HA == 9) # 4544 obs
# ST206Q01HA
summary(as.factor(datarerun$ST206Q01HA))
datarerun <- datarerun %>% filter(!ST206Q01HA == 9) # 4539 obs
# ST206Q02HA
summary(as.factor(datarerun$ST206Q02HA))
datarerun <- datarerun %>% filter(!ST206Q02HA == 9) # 4539 obs
# ST206Q03HA
summary(as.factor(datarerun$ST206Q03HA))
datarerun <- datarerun %>% filter(!ST206Q03HA == 9) # 4528 obs
# ST206Q04HA
summary(as.factor(datarerun$ST206Q04HA))
datarerun <- datarerun %>% filter(!ST206Q04HA == 9) # 4510 obs
# ISCEDL
summary(as.factor(datarerun$ISCEDL)) #gak ada data no response
# ISCEDD
summary(as.factor(datarerun$ISCEDL)) #gak ada data no response
# ISCEDO
summary(as.factor(datarerun$ISCEDO)) #gak ada data no response
# MISCED
summary(as.factor(datarerun$MISCED)) #gak ada data no response
# MISCED_D
summary(as.factor(datarerun$MISCED_D)) #gak ada data no response
# FISCED
summary(as.factor(datarerun$FISCED))
datarerun <- datarerun %>% filter(!FISCED == 99) # 4506 obs
# FISCED_D
summary(as.factor(datarerun$FISCED_D))
datarerun <- datarerun %>% filter(!FISCED_D == 99) # 4492 obs
# HISCED
summary(as.factor(datarerun$HISCED))
# HISCED_D
summary(as.factor(datarerun$HISCED_D))
# LANGMOTHER
summary(as.factor(datarerun$LANGMOTHER)) # 7 (Not Aplicable) - 2287 // drop
# LANGFATHER
summary(as.factor(datarerun$LANGFATHER)) # 7 (Not Aplicable) - 2287 // drop
# LANGSIBLINGS
summary(as.factor(datarerun$LANGSIBLINGS)) # 7 (Not Aplicable) - 2287 // drop
# LANGFRIEND
summary(as.factor(datarerun$LANGFRIEND)) # 7 (Not Aplicable) - 2287 // drop
# LANGSCHMATES
summary(as.factor(datarerun$LANGSCHMATES)) # 7 (Not Aplicable) - 2287 // drop
# IMMIG
summary(as.factor(datarerun$IMMIG))
datarerun <- datarerun %>% filter(!IMMIG == 9) # 4455 obs
# DURECEC
summary(as.factor(datarerun$DURECEC)) # 99 (No Repsonse) - 883 obs
datarerun <- datarerun %>% filter(!DURECEC == 99) # 3580 obs
# REPEAT
summary(as.factor(datarerun$REPEAT))
# AGE (Cont)
summary(datarerun$AGE)
#KETINGGALAN
# ST006Q01TA
summary(as.factor(datarerun$ST006Q01TA))
datarerun <- datarerun %>% filter(!ST006Q01TA == 9) # 3302 obs
# ST006Q02TA
summary(as.factor(datarerun$ST006Q02TA))
datarerun <- datarerun %>% filter(!ST006Q02TA == 9) # 3289 obs
# ST006Q03TA
summary(as.factor(datarerun$ST006Q03TA))
datarerun <- datarerun %>% filter(!ST006Q03TA == 9) # 3286 obs
# ST006Q04TA
summary(as.factor(datarerun$ST006Q04TA))
datarerun <- datarerun %>% filter(!ST006Q04TA == 9) # 3280 obs
# ST007Q01TA
summary(as.factor(datarerun$ST007Q01TA))
# ST008Q01TA
summary(as.factor(datarerun$ST008Q01TA))
datarerun <- datarerun %>% filter(!ST008Q01TA == 9) # 3205 obs
# ST008Q02TA
summary(as.factor(datarerun$ST008Q02TA))
datarerun <- datarerun %>% filter(!ST008Q02TA == 9) # 3200 obs
# ST008Q03TA
summary(as.factor(datarerun$ST008Q03TA))
# ST008Q04TA
summary(as.factor(datarerun$ST008Q04TA))
datarerun <- datarerun %>% filter(!ST008Q04TA == 9) # 3195 obs
# ST127Q03TA
summary(as.factor(datarerun$ST127Q03TA))
datarerun <- datarerun %>% filter(!ST127Q03TA == 8) # 3192 obs
datarerun <- datarerun %>% filter(!ST127Q03TA == 9) # 2771 obs
# ST207Q05HA
summary(as.factor(datarerun$ST207Q05HA))
datarerun <- datarerun %>% filter(!ST207Q05HA == 9) # 2768 obs
# ST206Q02HA
summary(as.factor(datarerun$ST206Q02HA))
datarerun <- datarerun %>% filter(!ST206Q02HA == 9) # 2759 obs
write.csv(datarerun, file="datarerun_filtered.csv")

# drop LANG variable
# LANGMOTHER, LANGFATHER, LANGSIBLINGS, LANGFRIEND, LANGSCHMATES
datarerun_wo_LANG <- datarerun%>% select(! c(LANGMOTHER, LANGFATHER, 
                                             LANGSIBLINGS, LANGFRIEND, 
                                             LANGSCHMATES))
#---- Multicollinearity Check ----
datarerunfix_mc <- as.data.frame(datarerunfix[,-1])
library(polycor)
mc_hetcor <- hetcor(datarerunfix_mc)
write.csv(mc_hetcor$correlations,file = "mc_hector.csv")
which(mc_hetcor$correlations>=0.7)

mc_cor <- mc_hetcor$correlation

datarerun %>% filter(!ST220Q02HA == 9)

#---- Making dummy variable -----
datarerunfix <- datarerun_wo_LANG
datarerunfix[,-(302:303)] <- lapply(datarerunfix[,-(302:303)], as.factor)
datarerunfix <- datarerunfix[,-299]

write.csv(datarerunfix, file="datarerunfix2.csv")


# mengecek data dengan level = 2, biar gk usah didummy
# bikin fungsi buat levels
hitung_levels <- function(x){
  length(levels(x))
}
jumlah_faktor <- sapply(datarerunfix, hitung_levels)
index_faktor_2<- which(jumlah_faktor==2)
View(datarerunfix[,index_faktor_2])

datarerunfix_2 <- datarerunfix

install.packages("fastDummies")
library(fastDummies)
dummyfix <- dummy_cols(datarerunfix[,- c(1,302,303)],
                       remove_selected_columns = TRUE)
dummyfix <- add_column(dummyfix, AGE = datarerunfix$AGE)
write.csv(dummyfix, file="datadummy.csv")

datarerundummy <- dummy_cols(datarerunfix[,- c(1,301,302,index_faktor_2)],
                             remove_selected_columns = TRUE) #10004
datarerundummy <- add_column(datarerundummy, 
                             datarerunfix[,c(index_faktor_2,301)]) #1076
datarerundummy <- as_tibble(lapply(datarerundummy, as.numeric))
datarerundummy <- as_tibble(datarerundummy)

#---- Descriptive----
summary(datarerunfix$readscore)
prop.table(datarerunfix$ST004D01T, datarerunfix$readscore)
table(datarerunfix$ST004D01T,datarerunfix$ST001D01T)
#---- Pemodelan SCAD ----
library(ncvreg)
X <- datarerunfix[,-303]
Y <- datarerunfix$readscore
# mencari lambda optimum
cvfit_scad <- cv.ncvreg(datarerundummy, datarerunfix$readscore,
                        family = "gaussian", penalty = "SCAD")
coef_ncvreg <- coef(cvfit_scad)
length(which(coef_ncvreg!=0))
lambdaopt <- cvfit_scad$lambda.min

model_scad <- ncvreg(datarerundummy, datarerunfix$readscore, 
                     family = "gaussian", penalty = "SCAD") #drop CNTSTUDENT

summary(model_scad, lambda = lambdaopt)

fit_scad <- ncvfit(datarerunfix[,-c(1,303)], datarerunfix$readscore, 
                   penalty = "SCAD", lambda = lambdaopt, max.iter = 10000)
library(ncpen)
# mencari lambda optimum, dilihat dari RMSE yang paling kecil
cvfit_ncpen <- cv.ncpen(datarerunfix$readscore, datarerundummy,
                        family = "gaussian", penalty = "scad")
coef_cvfit_ncpen <- coef(cvfit_ncpen)
write.csv(coef_cvfit_ncpen$beta, file ="coef_fit_ncpen.csv")

#cv ncpen reg

#compute error (manually)
#---- CV & RMSE ----
cvfit_ncpen$lambda
cvfit_ncpen$rmse
coef_cvfit_ncpen$lambda
vindex_lambdaopt<- which(cvfit_ncpen$lambda==coef_cvfit_ncpen$lambda)

lambdaopt <- cvfit_ncpen$lambda[vindex_lambdaopt]
RMSE <- cvfit_ncpen$rmse[vindex_lambdaopt]
minRMSE <- min(cvfit_ncpen$rmse)

#---- Kelayakan Model ----
# manual
MSE <- minRMSE^2
df <-nrow(datarerundummy)-ncol(datarerundummy)
SSE <- MSE*df
SST <- sum(datarerunfix$readscore^2)
SSR <- SST-SSE
Rsq <- SSR/SST

# predict ncpen
x.mat <- as.matrix(datarerundummy)
y.vec <- datarerunfix$readscore
predict_ncpen_reg <- predict.ncpen(cvfit_ncpen$ncpen.fit, type = "reg",
                                   new.x.mat = x.mat)
write.csv(predict_ncpen_reg, file = "Output/02192024_predict_ncpen_reg.csv")
predict_ncpen_y <- predict.ncpen(cvfit_ncpen$ncpen.fit, type = "y",
                                 new.x.mat = x.mat)
write.csv(predict_ncpen_y, file = "Output/02272024_predict_ncpen_y.csv")
predict_ncpen_rmse <- predict.ncpen(cvfit_ncpen$ncpen.fit, type = "rmse",
                                    new.x.mat = x.mat)

y.new <- predict_ncpen_y[,15]
residuals()
error <- y.new - y.vec
error2 <- y.vec - y.new
#---- Asumsi Klasik ----
#normalitas
ks.test(error, "pnorm", mean(error), sd(error)) #jika p-value > 0.05 maka data normal

#heterokedasitas
library(lmtest)
#gabungin y.new sama variable dummy (x)
dataxy_new <- data.frame(y.new,x.mat)
dataxy_new2 <- data.frame(y.new,cvfit_ncpen$ncpen.fit$x.mat)
dataxy_new2 <- dataxy_new2 [,-2]
dataxy_new[,2:1076] <- lapply(dataxy_new[,2:1076],factor)
#dataxy_new
bptest(y.new~.,data=dataxy_new) #P-value = 0.01168
hmctest(y.new~.,data=dataxy_new, plot = T) #pvalue = 1
#dataxy_new2
bptest(y.new~.,data=dataxy_new2)
hmctest(y.new~.,data=dataxy_new2,plot = T)

#Gunakan model yang hanya berisi variabel X yang signifikan saja
#white test
Ru2<- summary(lm(error^2 ~ y.new + I(y.new^2)))$r.squared
LM <- nrow(dataxy_new)*Ru2
whitetest_p.value <- 1-pchisq(LM, 2)
whitetest_p.value

#---- Plot/Grafik ----
# Histogram
hist(datarerunfix$readscore, 
     main = "Histogram of Indonesian Student 
       PISA 2018 Readscore",
     xlab = "Readscore PISA")

# Histogram with Lines
hist(datarerunfix$readscore, prob = T,
     main = "Histogram of Indonesian Student 
       PISA 2018 Readscore",
     xlab = "Readscore PISA", 
     col = "#98d0df", border = "#3592ab")
lines(density(datarerunfix$readscore), col = 2, lwd = 3)

# Boxplot
boxplot(datarerunfix$readscore,
        main = "Boxplot of Indonesian Student
          PISA 2018 Readscore",
        xlab = "Readscore PISA",
        horizontal = T,
        col = "#98d0df",
        outpch = 20, outcex = 2)

# Cross-Bloxpot 
boxplot(datarerunfix$readscore)

#plotting lambda (CV)
plot.cv.ncpen(cvfit_ncpen, type = "rmse", log.scale=F)

#plotting coef (Trace of Coeff)
plot.ncpen(cvfit_ncpen$ncpen.fit, log.scale = F)

#q-q plot (Asumsi Klasik - Normalitas)
qqnorm(error, frame = F)
qqline(error, col="#3592ab", lwd=3)
library(car)
qqPlot(error)

#Residual Scatter Plot (Asumsi Klasik - Heterokedastisitas)
#Horizontal Index
plot(x=error, y=c(1:2759),
     xlab="Residual", ylab="Index")
#Vertical Index
plot(error, ylab="Residual")

#Residual vs Fit
plot(x=error, y=y.new,
     xlab="Residuals", ylab="Fitted Values",
     col="#3592ab")


library(forcats)
library(ipumsr)
library(tidyverse)
library(dplyr)
library(stargazer)

#read in the data file and the codebook 
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)

#filter out observations where incwage=0 so you get the people who are making money
data <- filter(data, data$INCWAGE>0)
#filter so that just employed people show up
data <- filter(data, data$EMPSTAT==1)
#filter so that everyone in the sample speaks english (at any level)
data <- filter(data, data$SPEAKENG %in% c(2,3,4,5,6))


#create a transformation by making a logwage variable from incwage which will be our dependent variable
data <- mutate(data,LOGWAGE=log(data$INCWAGE) )

#make things into factors
data <- data%>%mutate(LANGUAGE=(as.factor(lbl_clean(LANGUAGE))))
data <- data%>%mutate(RACE=as.factor(lbl_clean(RACE)))
data <- data%>%mutate(LANGUAGED=as.factor(lbl_clean(LANGUAGED)))
data <- data%>%mutate(SEX=as.factor(lbl_clean(SEX)))
data <- data%>%mutate(SPEAKENG=as.factor(lbl_clean(SPEAKENG)))
data <- data%>%mutate(EMPSTAT=as.factor(lbl_clean(EMPSTAT)))
data <- data%>%mutate(OCC=as.factor(lbl_clean(OCC)))
data <- data%>%mutate(IND=as.character(lbl_clean(IND)))
data <- data%>%mutate(PWSTATE2=as.factor(lbl_clean(PWSTATE2)))
data$AGE <- as.numeric(data$AGE)




#try to make a dummy variable for anything besides English
#make a duplicate of the language factor column
#do an if else to turn it into a binary
data$LANGUAGE_AT_ALL=data$LANGUAGE
data$LANGUAGE_AT_ALL <- ifelse(data$LANGUAGE==1, 0, 1)


#condense the industries into broad sectors to reduce the # of regressors using IPUMS' categories
data%>%mutate(data$IND2 <- fct_collapse(data$IND, 
                                        AG_OIL=c(data$IND0171, data$IND0180, data$IND0190, data$IND0270, data$IND0280, data$IND0290, data$IND0370, 
                                                 data$IND0380, data$ind0390, data$IND0470, data$IND0480, data$IND0490), 
                                        CONSTRUCTION=c(0770), 
                                        MANUFACTURING=c(data$IND1070, data$IND1080, data$IND1090, data$IND1170, data$IND1180, data$IND1190, 
                                                        data$IND1270, data$IND1280, data$IND1290, data$IND1370, data$IND1380, data$IND1390, 
                                                        data$IND1470, data$IND1480, data$IND1490, data$IND1570, data$IND1590, data$IND1670, 
                                                        data$IND1691, data$IND1770, data$IND1790, data$IND1870, data$IND1880, data$IND1890, 
                                                        data$IND1990, data$IND2070, data$IND2090, data$IND2170, data$IND2180, data$IND2190,
                                                        data$IND2270, data$IND2280, data$IND2290, data$IND2370, data$IND2380, data$IND2390, 
                                                        data$IND2470, data$IND2480, data$IND2490, data$IND2570, data$IND2590, data$IND2670, 
                                                        data$IND2680, data$IND2690, data$IND2770, data$IND2780, data$IND2790, data$IND2870, 
                                                        data$IND2880, data$IND2890, data$IND2970, data$IND2980, data$IND2990, data$IND3070, 
                                                        data$IND3080, data$IND3095, data$IND3170, data$IND3180, data$IND3291, data$IND3365, 
                                                        data$IND3370, data$IND3380, data$IND3390, data$IND3470, data$IND3490, data$IND3570, 
                                                        data$IND3580, data$IND3590, data$IND3670, data$IND3680, data$IND3690, data$IND3770, 
                                                        data$IND3780, data$IND3790, data$IND3875, data$IND3895, data$IND3960, data$IND3970, 
                                                        data$IND3980, data$IND3990),
                                        WHOLESALETRADE=c(data$IND4070, data$IND4080, data$IND4090, data$IND4170, data$IND4180, data$IND4195,
                                                         data$IND4265, data$IND4270, data$IND4280, data$IND4290, data$IND4370, data$IND4380, 
                                                         data$IND4390, data$IND4470, data$IND4480, data$IND4490, data$IND4560, data$IND4570, 
                                                         data$IND4580, data$IND4585, data$IND4590), 
                                        RETAILTRADE=c(data$IND4670, data$IND4680, data$IND4690, data$IND4770, data$IND4780, data$IND4795, 
                                                      data$IND4870, data$IND4880, data$IND4890, data$IND4971, data$IND4972, data$IND4980, 
                                                      data$IND4990, data$IND5070, data$IND5080, data$IND5090, data$IND5170, data$IND5180, 
                                                      data$IND5190, data$IND5275, data$IND5280, data$IND5295, data$IND5370, data$IND5381, 
                                                      data$IND5391, data$IND5470, data$IND5480, data$IND5490, data$IND5570, data$IND5580, 
                                                      data$IND5593, data$IND5670, data$IND5680, data$IND5690, data$IND5790), 
                                        TRANSPO_UTILITIES=c(data$IND6070, data$IND6080, data$IND6090, data$IND6170, data$IND6180, data$IND6190, 
                                                            data$IND6270, data$IND6280, data$IND6290, data$IND6370, data$IND6380, data$IND6390, 
                                                            data$IND0570, data$IND0580, data$IND0590, data$IND0670, data$IND0680, data$IND0690), 
                                        INFORMATION=c(data$IND6470, data$IND6480, data$IND6490, data$IND6470, data$IND6590, data$IND6670, data$IND6672,
                                                      data$IND6680, data$IND6690, data$IND6695, data$IND6770, data$IND6780), 
                                        FINANCE_INSURANCE_REALESTATE=c(data$IND6870, data$IND6880, data$IND6890, data$IND6970, data$IND6991, 
                                                                       data$IND6992, data$IND7071, data$IND7072, data$IND7080, data$IND7181, 
                                                                       data$IND7190), 
                                        PROFESSIONAL_MGMT_ADMIN=c(data$IND7270, data$IND7280, data$IND7290, data$IND7370, data$IND7380, data$IND7390, 
                                                                  data$IND7460, data$IND7470, data$IND7480, data$IND7490, data$IND7570, data$IND7580, 
                                                                  data$IND7590, data$IND7670, data$IND7680, data$IND7690, data$IND7770, data$IND7780, 
                                                                  data$IND7790),
                                        EDCU_HEALTH_SOCIAL=c(data$IND7860, data$IND7870, data$IND7880, data$IND7890, data$IND7970, data$IND7980, 
                                                             data$IND7990, data$IND8070, data$IND8080, data$IND8090, data$IND8170, data$IND8180, 
                                                             data$IND8191, data$IND8192, data$IND8270, data$IND8290, data$IND8370, data$IND8380, 
                                                             data$IND8390, data$IND8470),
                                        ENTERTAIN_HOSPITALITY=c(data$IND8561, data$IND8562, data$IND8563, data$IND8564, data$IND8570, data$IND8580, 
                                                                data$IND8590, data$IND8660, data$IND8670, data$IND8680, data$IND8690),
                                        SERVICES=c(data$IND8770, data$IND8780, data$IND8790, data$IND8870, data$IND8891, data$IND8970, data$IND8980,
                                                   data$IND8990, data$IND9070, data$IND9080, data$IND9090, data$IND9160, data$IND9170, data$IND9180, 
                                                   data$IND9190, data$IND9290),
                                        PUBLICADMIN=c(data$IND9370, data$IND9380, data$IND9390, data$IND9470, data$IND9480, data$IND9490, data$IND9570, 
                                                      data$IND9590),
                                        MILITARY=c(data$IND9670, data$IND9680, data$IND9690, data$IND9770, data$IND9780, data$IND9790, data$IND9870)
))
#that took over an hour to write out
#and it doesn't work which is super frustrating 

#run a model with a yes/no for the second language, while using all of the control variables
est1 <- lm(data$LOGWAGE~data$LANGUAGE_AT_ALL+data$IND+data$PWSTATE2+data$AGE+data$SEX+data$RACE)
summary(est1)

#run a model with a vector of unique languages 
est2 <- lm(data$LOGWAGE~data$LANGUAGE+data$IND+data$PWSTATE2+data$AGE+data$SEX+data$RACE)
summary(est2)

#this is the third model which makes my computed explode because I couldn't get the industries condensed
est3 <- lm(data$LOGWAGE~data$LANGUAGE*data$IND+data$PWSTATE2+data$AGE+data$SEX+data$RACE)
summary(est3)

#make some LateX tables
#except it doesn't work
writeLines(capture.output(stargazer(est1, title = "Model 1", align = TRUE, summary = FALSE)), model1.tex)

#make some LateX tables
#except it doesn't work
stargazer(est2, title = "Model 2", align = TRUE, summary = FALSE)

#make some LateX tables
#except it doesn't work
stargazer(est3, title = "Model 3", align = TRUE, summary = FALSE)
            
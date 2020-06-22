# Cleaning  for Self Injections Model
options(java.parameters = "-Xmx8000m")
library(reshape2)
library(readr)
library(tidyr)
library(dplyr)
library(xlsx)
library(ggplot2)

setwd("C:/Users/KristinBietsch/Files/Track20/Win Requests/Self Injections")
oldpop <- read.xlsx2("Default Data Self Injections Model.xlsx",sheetName="Population",startRow=1,header=TRUE,
                     colClasses = c( "character",
                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

iso <- oldpop %>% group_by(Country) %>% summarise(ISO=mean(iso)) %>% select(-Country)
iso$FP2020 <- 1

newpop <- read.csv("WPP Female 15-49 2019.csv", sep=",")

data <- full_join(iso, newpop, by="ISO")
pop <- data %>% filter(!is.na(FP2020)) %>% filter(Date>=2019 & Date<=2030) %>% 
  rename(iso=ISO,	Year=Date,	pop15.19=y1519,	pop20.24=y2024,	pop25.29=y2529,	pop30.34=y3034,	pop35.39=y3539,	pop40.44=y4044,	pop45.49=y4549) %>% 
  select(Country,	iso,	Year,	pop15.19,	pop20.24,	pop25.29,	pop30.34,	pop35.39,	pop40.44,	pop45.49)

###################################################################################

# updated KB052020
users <- read.xlsx2("Michelle Projected Users by Method Annual 052020.xlsx",sheetName="All Years",startRow=1,header=TRUE,
                    colClasses = c("numeric", "character",
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))

# from the track20 2019 global run, not updating KB052020
mcpr <- read.xlsx2("All Women mCPR 20192030.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                   colClasses = c( "character",
                                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric"))

# updated with new surveys KB052020
reason <- read.xlsx2("ReasonNotUsing062220.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                     colClasses = c( "character",
                                     "numeric", "character" , "numeric"))


policy <- read.csv("Policy Limitations Data061120.csv", sep=",")

# discontinuation 052920

discontinuation <- read.xlsx2("Discontinuation Rates.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                   colClasses = c( "character",
                                   "numeric", "character", "numeric", "numeric", "numeric"))


# Things that don't vary by year: reason, policy, discontinuation, high injectable use

# Default parameters
method <- read.csv("MethodMix.csv")
inconv <- read.csv("DiscontinuedInconvient061820.csv")
regions <- read.csv("Regions.csv")
#################################################################

# need to interpolate for 2026, 2027, 2028, 2029
# going to to exponential interpolations

#p2=p1*exp(rt)
#p2/p1=exp(rt)
#log(p2/p1)/5=r

users <- users %>% mutate(Sterilization.r=log(Sterilization_2030/Sterilization_2025)/5,
                          Implant.r=log(Implant_2030/Implant_2025)/5,
                          IUD.r=log(IUD_2030/IUD_2025)/5,
                          Injectable.r=log(Injectable_2030/Injectable_2025)/5,
                          Pill.r=log(Pill_2030/Pill_2025)/5,
                          Condom.r=log(Condom_2030/Condom_2025)/5,
                          Other.r=log(Other_2030/Other_2025)/5)
users <- users %>% mutate(Sterilization_2026=Sterilization_2025*exp(Sterilization.r), Sterilization_2027=Sterilization_2025*exp(Sterilization.r*2), Sterilization_2028=Sterilization_2025*exp(Sterilization.r*3), Sterilization_2029=Sterilization_2025*exp(Sterilization.r*4),
                          Implant_2026=Implant_2025*exp(Implant.r), Implant_2027=Implant_2025*exp(Implant.r*2), Implant_2028=Implant_2025*exp(Implant.r*3), Implant_2029=Implant_2025*exp(Implant.r*4),
                          IUD_2026=IUD_2025*exp(IUD.r), IUD_2027=IUD_2025*exp(IUD.r*2), IUD_2028=IUD_2025*exp(IUD.r*3), IUD_2029=IUD_2025*exp(IUD.r*4),
                          Injectable_2026= Injectable_2025*exp(Injectable.r), Injectable_2027= Injectable_2025*exp(Injectable.r *2), Injectable_2028= Injectable_2025*exp(Injectable.r *3), Injectable_2029= Injectable_2025*exp(Injectable.r*4),
                          Pill_2026= Pill_2025*exp(Pill.r), Pill_2027= Pill_2025*exp(Pill.r *2), Pill_2028= Pill_2025*exp(Pill.r *3), Pill_2029= Pill_2025*exp(Pill.r*4),
                          Condom_2026= Condom_2025*exp(Condom.r), Condom_2027= Condom_2025*exp(Condom.r *2), Condom_2028= Condom_2025*exp(Condom.r *3), Condom_2029= Condom_2025*exp(Condom.r*4),
                          Other_2026= Other_2025*exp(Other.r), Other_2027= Other_2025*exp(Other.r *2), Other_2028= Other_2025*exp(Other.r *3), Other_2029= Other_2025*exp(Other.r*4))


names(users)
users.sm <- users %>% 
  mutate(total_2018=Sterilization_2018+ Implant_2018+IUD_2018+Injectable_2018+Pill_2018+Condom_2018+Other_2018,
         ltm_2018=Sterilization_2018+ Implant_2018+IUD_2018,
         stm_2018=Pill_2018+Condom_2018+ Other_2018,
         total_2019=Sterilization_2019+ Implant_2019+IUD_2019+Injectable_2019+Pill_2019+Condom_2019+ Other_2019,
         ltm_2019=Sterilization_2019+ Implant_2019+IUD_2019,
         stm_2019=Pill_2019+Condom_2019+ Other_2019,
         total_2020=Sterilization_2020+ Implant_2020+IUD_2020+Injectable_2020+Pill_2020+Condom_2020+ Other_2020,
         ltm_2020=Sterilization_2020+ Implant_2020+IUD_2020,
         stm_2020=Pill_2020+Condom_2020+ Other_2020,
         total_2021=Sterilization_2021+ Implant_2021+IUD_2021+Injectable_2021+Pill_2021+Condom_2021+ Other_2021,
         ltm_2021=Sterilization_2021+ Implant_2021+IUD_2021,
         stm_2021=Pill_2021+Condom_2021+ Other_2021,
         total_2022=Sterilization_2022+ Implant_2022+IUD_2022+Injectable_2022+Pill_2022+Condom_2022+Other_2022,
         ltm_2022=Sterilization_2022+ Implant_2022+IUD_2022,
         stm_2022=Pill_2022+Condom_2022+ Other_2022,
         total_2023=Sterilization_2023+ Implant_2023+IUD_2023+Injectable_2023+Pill_2023+Condom_2023+ Other_2023,
         ltm_2023=Sterilization_2023+ Implant_2023+IUD_2023,
         stm_2023=Pill_2023+Condom_2023+ Other_2023,
         total_2024=Sterilization_2024+ Implant_2024+IUD_2024+Injectable_2024+Pill_2024+Condom_2024+ Other_2024,
         ltm_2024=Sterilization_2024+ Implant_2024+IUD_2024,
         stm_2024=Pill_2024+Condom_2024+ Other_2024,
         total_2025=Sterilization_2025+ Implant_2025+IUD_2025+Injectable_2025+Pill_2025+Condom_2025+Other_2025,
         ltm_2025=Sterilization_2025+ Implant_2025+IUD_2025,
         stm_2025=Pill_2025+Condom_2025+ Other_2025,
         total_2026=Sterilization_2026+ Implant_2026+IUD_2026+Injectable_2026+Pill_2026+Condom_2026+ Other_2026,
         ltm_2026=Sterilization_2026+ Implant_2026+IUD_2026,
         stm_2026=Pill_2026+Condom_2026+ Other_2026,
         total_2027=Sterilization_2027+ Implant_2027+IUD_2027+Injectable_2027+Pill_2027+Condom_2027+ Other_2027,
         ltm_2027=Sterilization_2027+ Implant_2027+IUD_2027,
         stm_2027=Pill_2027+Condom_2027+ Other_2027,
         total_2028=Sterilization_2028+ Implant_2028+IUD_2028+Injectable_2028+Pill_2028+Condom_2028+Other_2028,
         ltm_2028=Sterilization_2028+ Implant_2028+IUD_2028,
         stm_2028=Pill_2028+Condom_2028+ Other_2028,
         total_2029=Sterilization_2029+ Implant_2029+IUD_2029+Injectable_2029+Pill_2029+Condom_2029+ Other_2029,
         ltm_2029=Sterilization_2029+ Implant_2029+IUD_2029,
         stm_2029=Pill_2029+Condom_2029+ Other_2029,
         total_2030=Sterilization_2030+ Implant_2030+IUD_2030+Injectable_2030+Pill_2030+Condom_2030+ Other_2030,
         ltm_2030=Sterilization_2030+ Implant_2030+IUD_2030,
         stm_2030=Pill_2030+Condom_2030+ Other_2030) %>% 
  mutate(prop_ltm_2018= ltm_2018/total_2018, 
         prop_stm_2018= stm_2018/total_2018,
         prop_Injection_2018= Injectable_2018/total_2018,
         prop_ltm_2019= ltm_2019/total_2019, 
         prop_stm_2019= stm_2019/total_2019,
         prop_Injection_2019= Injectable_2019/total_2019,
         prop_ltm_2020= ltm_2020/total_2020, 
         prop_stm_2020= stm_2020/total_2020,
         prop_Injection_2020= Injectable_2020/total_2020,
         prop_ltm_2021= ltm_2021/total_2021, 
         prop_stm_2021= stm_2021/total_2021,
         prop_Injection_2021= Injectable_2021/total_2021,
         prop_ltm_2022= ltm_2022/total_2022, 
         prop_stm_2022= stm_2022/total_2022,
         prop_Injection_2022= Injectable_2022/total_2022,
         prop_ltm_2023= ltm_2023/total_2023, 
         prop_stm_2023= stm_2023/total_2023,
         prop_Injection_2023= Injectable_2023/total_2023,
         prop_ltm_2024= ltm_2024/total_2024, 
         prop_stm_2024= stm_2024/total_2024,
         prop_Injection_2024= Injectable_2024/total_2024,
         prop_ltm_2025= ltm_2025/total_2025, 
         prop_stm_2025= stm_2025/total_2025,
         prop_Injection_2025= Injectable_2025/total_2025,
         prop_ltm_2026= ltm_2026/total_2026, 
         prop_stm_2026= stm_2026/total_2026,
         prop_Injection_2026= Injectable_2026/total_2026,
         prop_ltm_2027= ltm_2027/total_2027, 
         prop_stm_2027= stm_2027/total_2027,
         prop_Injection_2027= Injectable_2027/total_2027,
         prop_ltm_2028= ltm_2028/total_2028, 
         prop_stm_2028= stm_2028/total_2028,
         prop_Injection_2028= Injectable_2028/total_2028,
         prop_ltm_2029= ltm_2029/total_2029, 
         prop_stm_2029= stm_2029/total_2029,
         prop_Injection_2029= Injectable_2029/total_2029,
         prop_ltm_2030= ltm_2030/total_2030, 
         prop_stm_2030= stm_2030/total_2030,
         prop_Injection_2030= Injectable_2030/total_2030 ) %>%
  select(Country, iso,
         prop_ltm_2019, 
         prop_stm_2019,
         prop_Injection_2019,
         prop_ltm_2020, 
         prop_stm_2020,
         prop_Injection_2020,
         prop_ltm_2021, 
         prop_stm_2021,
         prop_Injection_2021,
         prop_ltm_2022, 
         prop_stm_2022,
         prop_Injection_2022,
         prop_ltm_2023, 
         prop_stm_2023,
         prop_Injection_2023,
         prop_ltm_2024, 
         prop_stm_2024,
         prop_Injection_2024,
         prop_ltm_2025, 
         prop_stm_2025,
         prop_Injection_2025,
         prop_ltm_2026, 
         prop_stm_2026,
         prop_Injection_2026,
         prop_ltm_2027, 
         prop_stm_2027,
         prop_Injection_2027,
         prop_ltm_2028, 
         prop_stm_2028,
         prop_Injection_2028,
         prop_ltm_2029, 
         prop_stm_2029,
         prop_Injection_2029,
         prop_ltm_2030, 
         prop_stm_2030,
         prop_Injection_2030) %>%
  filter( !is.na(iso))



####################################################################
names(pop)
pop.sm <- pop %>% mutate(WRA=pop15.19 + pop20.24 +
                           pop25.29 + pop30.34 +
                           pop35.39 + pop40.44 + 
                           pop45.49 ) %>%
  select(Country, iso, Year, WRA) %>%
  spread(Year, WRA) %>%
  rename(Pop2019="2019",
         Pop2020="2020",
         Pop2021="2021",
         Pop2022="2022",
         Pop2023="2023",
         Pop2024="2024",
         Pop2025="2025",
         Pop2026="2026",
         Pop2027="2027",
         Pop2028="2028",
         Pop2029="2029",
         Pop2030="2030")






####################################################################




# bring in mcpr from global run
# bring in popualtion numbers

data <- full_join(mcpr, users.sm, by=c("Country", "iso"))
data <- full_join(data, pop.sm, by="iso")

names(data)
data <- data %>% select(-Country.y) %>% rename(Country=Country.x)

total_users <- data %>% mutate(ltm_users_2019=mcpr2019*prop_ltm_2019*(Pop2019*1000),
                               stm_users_2019=mcpr2019*prop_stm_2019*(Pop2019*1000),
                               Injection_users_2019=mcpr2019*prop_Injection_2019*(Pop2019*1000),
                               ltm_users_2020=mcpr2020*prop_ltm_2020*(Pop2020*1000),
                               stm_users_2020=mcpr2020*prop_stm_2020*(Pop2020*1000),
                               Injection_users_2020=mcpr2020*prop_Injection_2020*(Pop2020*1000),
                               ltm_users_2021=mcpr2021*prop_ltm_2021*(Pop2021*1000),
                               stm_users_2021=mcpr2021*prop_stm_2021*(Pop2021*1000),
                               Injection_users_2021=mcpr2021*prop_Injection_2021*(Pop2021*1000),
                               ltm_users_2022=mcpr2022*prop_ltm_2022*(Pop2022*1000),
                               stm_users_2022=mcpr2022*prop_stm_2022*(Pop2022*1000),
                               Injection_users_2022=mcpr2022*prop_Injection_2022*(Pop2022*1000),
                               ltm_users_2023=mcpr2023*prop_ltm_2023*(Pop2023*1000),
                               stm_users_2023=mcpr2023*prop_stm_2023*(Pop2023*1000),
                               Injection_users_2023=mcpr2023*prop_Injection_2023*(Pop2023*1000),
                               ltm_users_2024=mcpr2024*prop_ltm_2024*(Pop2024*1000),
                               stm_users_2024=mcpr2024*prop_stm_2024*(Pop2024*1000),
                               Injection_users_2024=mcpr2024*prop_Injection_2024*(Pop2024*1000),
                               ltm_users_2025=mcpr2025*prop_ltm_2025*(Pop2025*1000),
                               stm_users_2025=mcpr2025*prop_stm_2025*(Pop2025*1000),
                               Injection_users_2025=mcpr2025*prop_Injection_2025*(Pop2025*1000),
                               ltm_users_2026=mcpr2026*prop_ltm_2026*(Pop2026*1000),
                               stm_users_2026=mcpr2026*prop_stm_2026*(Pop2026*1000),
                               Injection_users_2026=mcpr2026*prop_Injection_2026*(Pop2026*1000),
                               ltm_users_2027=mcpr2027*prop_ltm_2027*(Pop2027*1000),
                               stm_users_2027=mcpr2027*prop_stm_2027*(Pop2027*1000),
                               Injection_users_2027=mcpr2027*prop_Injection_2027*(Pop2027*1000),
                               ltm_users_2028=mcpr2028*prop_ltm_2028*(Pop2028*1000),
                               stm_users_2028=mcpr2028*prop_stm_2028*(Pop2028*1000),
                               Injection_users_2028=mcpr2028*prop_Injection_2028*(Pop2028*1000),
                               ltm_users_2029=mcpr2029*prop_ltm_2029*(Pop2029*1000),
                               stm_users_2029=mcpr2029*prop_stm_2029*(Pop2029*1000),
                               Injection_users_2029=mcpr2029*prop_Injection_2029*(Pop2029*1000),
                               ltm_users_2030=mcpr2030*prop_ltm_2030*(Pop2030*1000),
                               stm_users_2030=mcpr2030*prop_stm_2030*(Pop2030*1000),
                               Injection_users_2030=mcpr2030*prop_Injection_2030*(Pop2030*1000),
                               non_users_2019=(1-mcpr2019)*(Pop2019*1000),
                               non_users_2020=(1-mcpr2020)*(Pop2020*1000),
                               non_users_2021=(1-mcpr2021)*(Pop2021*1000),
                               non_users_2022=(1-mcpr2022)*(Pop2022*1000),
                               non_users_2023=(1-mcpr2023)*(Pop2023*1000),
                               non_users_2024=(1-mcpr2024)*(Pop2024*1000),
                               non_users_2025=(1-mcpr2025)*(Pop2025*1000),
                               non_users_2026=(1-mcpr2026)*(Pop2026*1000),
                               non_users_2027=(1-mcpr2027)*(Pop2027*1000),
                               non_users_2028=(1-mcpr2028)*(Pop2028*1000),
                               non_users_2029=(1-mcpr2029)*(Pop2029*1000),
                               non_users_2030=(1-mcpr2030)*(Pop2030*1000)) %>%
  select(Country           	,	iso	,				
         Injection_users_2019	,	ltm_users_2019	,	stm_users_2019	,	non_users_2019	,
         Injection_users_2020	,	ltm_users_2020	,	stm_users_2020	,	non_users_2020	,
         Injection_users_2021	,	ltm_users_2021	,	stm_users_2021	,	non_users_2021	,
         Injection_users_2022	,	ltm_users_2022	,	stm_users_2022	,	non_users_2022	,
         Injection_users_2023	,	ltm_users_2023	,	stm_users_2023	,	non_users_2023	,
         Injection_users_2024	,	ltm_users_2024	,	stm_users_2024	,	non_users_2024	,
         Injection_users_2025	,	ltm_users_2025	,	stm_users_2025	,	non_users_2025	,
         Injection_users_2026	,	ltm_users_2026	,	stm_users_2026	,	non_users_2026	,
         Injection_users_2027	,	ltm_users_2027	,	stm_users_2027	,	non_users_2027	,
         Injection_users_2028	,	ltm_users_2028	,	stm_users_2028	,	non_users_2028	,
         Injection_users_2029	,	ltm_users_2029	,	stm_users_2029	,	non_users_2029	,
         Injection_users_2030	,	ltm_users_2030	,	stm_users_2030	,	non_users_2030	) %>%
  gather(method, pop, Injection_users_2019:non_users_2030) %>% 
  separate(method, c("Method", "User" , "Year"), "_") %>%
  select(-User) %>%
  filter(!is.na(Country)) %>%
  filter(iso!=710)

total_users_wide <- total_users %>% spread(Method, pop) %>% mutate(Total=Injection+ltm+stm+non)

total_users$Method<- ifelse(total_users$Method == "stm", "Short Term Method",
                            ifelse(total_users$Method == "ltm", "Long Term Method",
                                   ifelse(total_users$Method == "non", "Non-User", "Injectable User")))

total_users$Method <- factor(total_users$Method, levels=c("Injectable User", "Short Term Method", "Long Term Method", "Non-User"))

ggplot(total_users, aes(x=Year, y=pop, fill=Method))+
  geom_bar( stat = "identity",
            position = "stack") +
  ggtitle("Total Women of Reproductive Age, FP2020 Countries (Excluding Western Sahara)") +
  theme_bw()

categories <- total_users %>% group_by(Method, Year) %>% summarise(total=sum(pop))


##############################################################3
prop.inject.2019 <- select(users.sm, Country, iso, prop_Injection_2019)
prop.inject.2019$high <- ifelse(prop.inject.2019$prop_Injection_2019>.33,1,0)


####################################################################
# Creating Baseline Data Tab
#total_users_wide
#prop.inject.2019
#reason
#policy
# empty columns: discon_inj discon_stm

total_users_wide$CountryYear <- paste(total_users_wide$Country, total_users_wide$Year, sep="")
total_users_wide <- total_users_wide %>% rename(LTM=ltm, STM=stm, NonUser=non) %>% select(Country, iso, Year, CountryYear, Total, LTM, STM, Injection, NonUser)


BaselineData <- total_users_wide %>% filter(iso!=710)




# no year variation
prop.inject.2019 <- prop.inject.2019 %>% select(iso, high)

regions <- regions %>% select(-Country)
reason <- full_join(reason, regions, by="iso")

reg_reason <- reason %>% filter(!is.na(ReasonNotUsingSI)) %>% group_by(Region) %>% summarise(ReasonNotUsingSI_reg=mean(ReasonNotUsingSI))
reason <- full_join(reason, reg_reason, by="Region")
reason <- reason %>% mutate(reason_note=case_when(!is.na(ReasonNotUsingSI) ~ "National", is.na(ReasonNotUsingSI) ~ "Regional") , 
                            ReasonNotUsingSI=case_when(!is.na(ReasonNotUsingSI) ~ ReasonNotUsingSI, is.na(ReasonNotUsingSI) ~ ReasonNotUsingSI_reg)) %>% select(iso, ReasonNotUsingSI, reason_note) 

policy <- policy %>% rename(scale=policy_pr) %>% select(iso, scale, notes, Public, HC_CBR_clean)

# if missing, using global average of 40%
discontinuation <- discontinuation %>% mutate(discon_inj=Injectables/100,
                                              discon_stm=(Pill+Condom)/200) %>%
                                      mutate(discon_inj_note=case_when(is.na(discon_inj) ~ "No data, global average" , !is.na(discon_inj) ~ ""),
                                             discon_stm_note=case_when(is.na(discon_stm) ~ "No data, global average" , !is.na(discon_stm) ~ ""),
                                             discon_inj=case_when(is.na(discon_inj) ~ .4 , !is.na(discon_inj) ~ discon_inj),
                                             discon_stm=case_when(is.na(discon_stm) ~ .4 , !is.na(discon_stm) ~ discon_stm) )

discontinuation <- discontinuation %>% select(iso, discon_inj, discon_stm, discon_inj_note,  discon_stm_note)


# country names
names <- BaselineData %>% group_by(Country) %>% summarise(iso=mean(iso))

default <- full_join(names, prop.inject.2019, by="iso")
default <- full_join(default, reason, by="iso")
default <- full_join(default, policy, by="iso")
default <- full_join(default, discontinuation, by="iso")
default <- default %>% filter(iso!=710)

default <- default %>% mutate(Country_note1 = case_when(!is.na(Public)~ paste(Public, "% of Injectables are from the Public Sector and ", HC_CBR_clean, "% of Public Sector Injectables are from Health Centers and Field Workers.", sep=""),
                                                        is.na(Public) ~ "Data on public sector distribution is not available, using regional average."),
                              Country_note= Country_note1) %>% select(-Country_note1,  -Public, -HC_CBR_clean)

#write.csv(default, "DefaultData062220.csv", row.names = F, na="")



########################## Parameters #######################

method <- method %>% left_join( regions)

method <- method %>% mutate(share= Injections/mCPR)
africa <- method %>% filter(Region=="Africa") %>% filter(Country!= "Egypt" & Country!= "Zimbabwe")

fit <- lm(share ~ mCPR , data=africa)
africa$distance <- residuals(fit)
other <- method %>% filter(Region=="Asia" | Region=="LAC" | Country== "Egypt" | Country== "Zimbabwe")
fit1 <- lm(share ~ mCPR , data=other)
other$distance <- residuals(fit1)

combined <- bind_rows(africa, other)
combined <- combined %>% select( iso, distance)

# SI bonus prep
inconv <- inconv %>% left_join( regions)
reg_incon <- inconv %>% filter(!is.na(Disc_Inconv)) %>% group_by(Region) %>% summarise(Disc_Inconv_reg=mean(Disc_Inconv))
inconv <- full_join(inconv, reg_incon, by="Region")
inconv <- inconv %>% mutate(Disc_Inconv_note=case_when(!is.na(Disc_Inconv) ~ "National", is.na(Disc_Inconv) ~ "Regional") , 
                            Disc_Inconv=case_when(!is.na(Disc_Inconv) ~ Disc_Inconv, is.na(Disc_Inconv) ~ Disc_Inconv_reg)) %>%
                  select(iso, Disc_Inconv, Disc_Inconv_note)

combined <- full_join(combined, inconv, by="iso")

# From PATH on 6/17
# 854 Burkina 2019, 2022
# 404 Kenya 2025, not official- kb 2027
# 566 Nigeria 2024 2024
# 686 senegal (was 2018) 2019, 2020
# 800 Uganda 2019, 2022

# KB other default will be 2022 and 2025
combined <- combined %>% mutate(year_sc=case_when(iso==854 ~ 2019,
                                                  iso==404 ~ 2025,
                                                  iso==566 ~ 2024,
                                                  iso==686 ~ 2019,
                                                  iso==800 ~ 2019,
                                                  iso!=854 & iso!=404 & iso!=566 & iso!=686 & iso!=800 ~ 2022 ),
                                year_si=case_when(iso==854 ~ 2022,
                                                  iso==404 ~ 2027,
                                                  iso==566 ~ 2024,
                                                  iso==686 ~ 2020,
                                                  iso==800 ~ 2022,
                                                  iso!=854 & iso!=404 & iso!=566 & iso!=686 & iso!=800 ~ 2025 ))
combined <- combined %>% mutate(nu_to_si=case_when(distance< -.15 ~ 0.05,
                                                   distance >= -.15 & distance < -.05 ~ 0.04,
                                                   distance >= -.05 & distance < .05 ~ 0.03,
                                                   distance >= .05 & distance < .15 ~ 0.02,
                                                   distance>= .15 ~ 0.01))

combined <- combined %>% mutate(si_bonus_base=case_when(Disc_Inconv< 2 ~ 0.01,
                                                   Disc_Inconv>= 2 &  Disc_Inconv< 4 ~ 0.02,
                                                   Disc_Inconv>=4 ~ 0.03)) %>%
                          mutate(si_bonus=case_when(nu_to_si==0.01 ~ si_bonus_base + 0.01 ,
                                                    nu_to_si==0.02 ~ si_bonus_base + 0.005 ,
                                                    nu_to_si>=0.03 ~ si_bonus_base))


# Non-Varying
combined <- combined %>%  mutate(max_siofsc=.4,
                                 inj_to_si=.16,	
                                 stm_to_si=.08)
combined <- combined %>% select(iso, inj_to_si, stm_to_si, nu_to_si, si_bonus, max_siofsc, year_sc, year_si)
combined <- combined %>% filter(iso!=710)
#write.csv(combined, "ParameterData061820.csv", row.names = F, na="")


#################################
default <- default %>% select(-Country)
BaselineData <- full_join(BaselineData, default, by="iso")
BaselineData <- full_join(BaselineData, combined, by="iso")

write.csv(BaselineData, "ModelData062220.csv", row.names = F, na="")
#################################


#Old data

#mean(prop.inject.2019$high)

#ggplot(prop.inject.2019, aes(x=prop_Injection_2019)) + 
#  geom_histogram(color="black", fill="white")


##########################################################
# Saving datasets


# pop_clean <- filter(pop.sm, iso!=710 & iso!=732)
# names <- select(pop_clean, Country, iso)
# 
# mcpr_clean <- mcpr %>% filter(iso!=710 & iso!=732) %>% select(-Country)
# mcpr_clean <- full_join(names, mcpr_clean, by="iso")
# 
# users_clean <- users.sm %>% filter(iso!=710 & iso!=732) %>% select(-Country)
# users_clean <- full_join(names, users_clean, by="iso")
# 
# high_injectables <- prop.inject.2019 %>% filter(iso!=710 & iso!=732) %>% select(-Country, -prop_Injection_2019)
# high_injectables <- full_join(names, high_injectables, by="iso")
# 
# 
# 
# 
# write.xlsx2(as.data.frame(pop_clean), "SIModelDefaultData053119.xlsx", sheetName="Population",
#             col.names=TRUE, row.names=FALSE, append=FALSE)
# 
# write.xlsx2(as.data.frame(mcpr_clean), "SIModelDefaultData053119.xlsx", sheetName="mCPR",
#             col.names=TRUE, row.names=FALSE, append=TRUE)
# 
# write.xlsx2(as.data.frame(users_clean), "SIModelDefaultData053119.xlsx", sheetName="MethodMix",
#             col.names=TRUE, row.names=FALSE, append=TRUE)
# 
# write.xlsx2(as.data.frame(high_injectables), "SIModelDefaultData053119.xlsx", sheetName="HighInjectable",
#             col.names=TRUE, row.names=FALSE, append=TRUE)
# 
# #################################################################
# # Data Long
# # going to create starting pop distribution
# 
# total_users_wide_clean <- total_users_wide %>% filter(iso!=710 & iso!=732) %>% select(-Country)
# total_users_wide_clean <- full_join(names, total_users_wide_clean, by="iso")
# 
# total_users_wide_clean$CountryYear <- paste(total_users_wide_clean$Country, total_users_wide_clean$Year, sep="")
# total_users_wide_clean <- total_users_wide_clean %>% rename(LTM=ltm, STM=stm, NonUser=non) %>% 
#   select(Country, iso, Year, CountryYear, Total, LTM, STM, Injection, NonUser)
# 
# total_users_wide_clean <- full_join(total_users_wide_clean, high_injectables, by=c("Country" , "iso"))
# 
# reason_clean <- select(reason, -Country)
# total_users_wide_clean <- full_join(total_users_wide_clean, reason_clean, by="iso")
# 
# 
# 
# # add in 2 discontinuation columns
# total_users_wide_clean$discon_inj <- 0.05
# total_users_wide_clean$discon_stm <- 0.05
# 
# # add in Emily's 2 assumptions
# total_users_wide_clean$public <- NA
# total_users_wide_clean$cbd <- NA
# 
# policy_clean <- select(policy, iso, policy_pr)
# total_users_wide_clean <- full_join(total_users_wide_clean, policy_clean, by="iso")
# 
# 
# CountryYear<- select(total_users_wide_clean, Country, iso, Year, CountryYear)
# 
# policyyear<- select(total_users_wide_clean, Country, iso, Year, policy_pr)
# 
# 
# 
# write.xlsx2(as.data.frame(total_users_wide_clean), "SIModelDefaultDataLong.xlsx", sheetName="TotalUsers",
#             col.names=TRUE, row.names=FALSE, append=FALSE)
# 
# write.xlsx2(as.data.frame(CountryYear), "SIModelDefaultDataLong.xlsx", sheetName="CountryID",
#             col.names=TRUE, row.names=FALSE, append=TRUE)
# 
# write.xlsx2(as.data.frame(policyyear), "SIModelDefaultDataLong.xlsx", sheetName="policyyear",
#             col.names=TRUE, row.names=FALSE, append=TRUE)

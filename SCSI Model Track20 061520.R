# Track20, Avenir Health
# Kristin Bietsch, PhD and Emily Sonneveldt, PhD

library(dplyr)
library(tidyr)
library(ggplot2)

# Baseline Data
baseline <- read.csv("BaselineData052920.csv")
default <- read.csv("DefaultData061120.csv")
default <- default%>% select(-Country)
# combine
baseline <- full_join(baseline, default, by="iso")


# Adaptable Parameters
#Proportion of Injectable users that will switch to self-injectable contraceptives
inj_to_si <- .16
#Proportion of short-term method users that will switch to self-injectable contraceptives
stm_to_si <- .08
#Proportion of Non-users, whose reasons for not using would be alleviated by benefits of self-injection (decreased side-effects, increased availability, ease of use) that will uptake self-injectable contraceptives                               
nu_to_si <- .04

# SI bonus- increase in each parameter whem SI of SC becomes fully available (partial bonus awarded during scale up)
si_bonus <- .01

# New Adjustable Parament
# Year SC full scale
year_sc <- 2020
# Year SI is reaches maximum proportion of SC use
# if the two years are differect, assume 0% in first year and max% in last year and linearly interpolate
year_si <- 2021

max_siofsc <- .4


##############################################################
# Share of SI that are provider provided versus SI
Year <- c(seq(2019, 2030, 1))
yearsdf <- as.data.frame(Year)
yearsdf$SC <- year_sc
yearsdf$SI <- year_si
yearsdf$max_siofsc <- max_siofsc
yearsdf <- yearsdf %>% mutate(time_sc_si = SI-SC,
                              years_after_sc=Year-SC,
                              share_between_scsi = (years_after_sc/time_sc_si)*max_siofsc,
                              per_si =case_when(Year>=SI ~ max_siofsc,
                                                Year<=SC ~ 0,
                                                Year>SC & Year<SI ~ share_between_scsi),
                              share_bonus=per_si/max_siofsc) %>% 
                      select(Year, per_si, share_bonus) %>%
                      mutate(id=1)



si_firstsc <- baseline %>% filter(Year==2019) %>% select(iso, Injection, STM, NonUser, ReasonNotUsingSI, scale, high)  %>%
  mutate(id=1) %>% mutate(high_scale=case_when(high==1 ~ 1, high==0 ~ .9))
si_firstsc <- full_join(si_firstsc, yearsdf, by="id") %>% select(-id)




siannual <- si_firstsc %>% mutate(inj_switch_si_a=(((inj_to_si+(si_bonus*share_bonus))*scale*high_scale)*Injection)/11,
                              stm_switch_si_a=(((stm_to_si+(si_bonus*share_bonus))*scale*high_scale)*STM)/11,
                              nu_switch_si_a=(((nu_to_si+(si_bonus*share_bonus))*scale*high_scale)*NonUser*ReasonNotUsingSI)/11) %>%
  select(iso, Year, inj_switch_si_a, stm_switch_si_a, nu_switch_si_a)

equations <- full_join(baseline, siannual, by=c("iso", "Year"))

# Here is where the years since full scale matter- making annual numbers before it 0
equations <- equations %>% mutate(year_fullscale= case_when(Year-year_sc<0 ~ 0, Year-year_sc>=0 ~ Year-year_sc+1 )) %>%
  mutate(binary_full_scale=case_when(year_fullscale>0 ~ 1, year_fullscale==0 ~ 0)) %>% 
  mutate(inj_switch_si_a=inj_switch_si_a*binary_full_scale,
         stm_switch_si_a=stm_switch_si_a*binary_full_scale,
         nu_switch_si_a=nu_switch_si_a*binary_full_scale) %>%
  group_by(iso) %>%
  mutate(inj_switch_si = cumsum(inj_switch_si_a),
         stm_switch_si = cumsum(stm_switch_si_a),
         inj_stay= Injection-inj_switch_si,
         stm_stay=STM-stm_switch_si)
  



#SI users who would have been non-users because of discontinuation from IM
equations <- equations %>% mutate(si_non_disc_im = case_when(year_fullscale==0 ~ 0, year_fullscale!=0 ~ inj_switch_si*(discon_inj*.26)))

# SI users who would have been non-users because of discontinuation from STM
equations <- equations %>% mutate(si_non_disc_stm = case_when(year_fullscale==0 ~ 0, year_fullscale!=0 ~ stm_switch_si*(discon_stm*.26)))

#Nonusers Not SI Relevant
equations <- equations %>% mutate(nu_not_sirelevant=(NonUser-si_non_disc_im-si_non_disc_stm)*(1-ReasonNotUsingSI),
                                  nu_sirelevant=(NonUser-si_non_disc_im-si_non_disc_stm)*(ReasonNotUsingSI))

# Nonusers  SI Relevant Uptake
equations <- equations %>% mutate( nu_sirelevant_uptake= cumsum(nu_switch_si_a))
equations <- equations %>% mutate(nu_sirelevant_notuptake=nu_sirelevant-nu_sirelevant_uptake)

names(equations)
equations <- equations %>% mutate(si_users=inj_switch_si+stm_switch_si+si_non_disc_im+si_non_disc_stm+nu_sirelevant_uptake,
                                  im_users=inj_stay,
                                  stm_users=stm_stay,
                                  ltm_users=LTM,
                                  non_users=nu_not_sirelevant+nu_sirelevant_notuptake,
                                  per_users_si=si_users/(si_users+im_users+stm_users+ltm_users))





# Proportion of SI Users from Various sources
equations <- equations %>% mutate(prop_si_im= case_when(si_users==0 ~ 0, si_users!=0 ~ inj_switch_si/si_users),
                                  prop_si_stm= case_when(si_users==0 ~ 0, si_users!=0 ~ stm_switch_si/si_users),
                                  prop_si_disc= case_when(si_users==0 ~ 0, si_users!=0 ~ (si_non_disc_im+si_non_disc_stm)/si_users),
                                  prop_si_uptake= case_when(si_users==0 ~ 0, si_users!=0 ~ nu_sirelevant_uptake/si_users))

equations <- equations %>% mutate(baseline_users=LTM+STM+Injection,
                                  total_user_w_si=si_users+im_users+stm_users+ltm_users)

# Injectable Users
equations <- equations %>% mutate(injec_user_w_si=si_users+im_users)

# IM Users, SI Users, and SC Users
equations <- full_join(equations, yearsdf, by="Year")
equations <- equations %>% mutate(IM_injec_user_w_si=round(im_users),
                                  SCP_injec_user_w_si=round(si_users*(1-per_si)),
                                  SI_injec_user_w_si= round(si_users*(per_si)),
                                  non_users <- Total-total_user_w_si,
                                  mcpr_w_si=round((total_user_w_si/Total)*100,1),
                                  inject_share=injec_user_w_si/total_user_w_si,
                                  baseline_mcpr=round((baseline_users/Total)*100,1))


graph <- equations %>% select(iso, Year, IM_injec_user_w_si, SCP_injec_user_w_si, SI_injec_user_w_si, stm_users , ltm_users, non_users ) %>% 
  gather(Type, Number, IM_injec_user_w_si:non_users ) %>%
  mutate(Type=case_when(Type=="stm_users" ~ "STM",
                        Type=="ltm_users" ~ "LTM",
                        Type=="IM_injec_user_w_si" ~ "Injectable: IM",
                        Type=="SCP_injec_user_w_si" ~ "Injectable: SC (Provider)",
                        Type=="SI_injec_user_w_si" ~ "Injectable: SC (Self)",
                        Type=="non_users" ~ "Nonuser"))

graph$Type <- factor(graph$Type, levels = c( "Injectable: SC (Self)", "Injectable: SC (Provider)", "Injectable: IM", "LTM", "STM", "Nonuser" ))

graph <- graph %>% filter(iso==4)




ggplot(graph,aes(x=Year,y=Number, fill=Type))+ 
  geom_bar(stat="identity")





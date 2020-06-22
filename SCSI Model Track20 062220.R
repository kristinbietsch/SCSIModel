# Track20, Avenir Health
# Kristin Bietsch, PhD and Emily Sonneveldt, PhD

library(dplyr)
library(tidyr)
library(ggplot2)
setwd("C:/Users/KristinBietsch/files/Track20/Win Requests/Self Injections")

# Baseline Data
baseline <- read.csv("ModelData062220.csv")


# Adaptable Parameters
#Proportion of Injectable users that will switch to self-injectable contraceptives
#inj_to_si <- .16
#Proportion of short-term method users that will switch to self-injectable contraceptives
#stm_to_si <- .08
#Proportion of Non-users, whose reasons for not using would be alleviated by benefits of self-injection (decreased side-effects, increased availability, ease of use) that will uptake self-injectable contraceptives                               
#nu_to_si <- .04

# SI bonus- increase in each parameter whem SI of SC becomes fully available (partial bonus awarded during scale up)
#si_bonus <- .01

# New Adjustable Parament
# Year SC full scale
#year_sc <- 2020
# Year SI is reaches maximum proportion of SC use
# if the two years are differect, assume 0% in first year and max% in last year and linearly interpolate
#year_si <- 2021

#max_siofsc <- .4


##############################################################
# Share of SI that are provider provided versus SI


baseline$SC <- baseline$year_sc
baseline$SI <- baseline$year_si
baseline <- baseline %>% mutate(time_sc_si = SI-SC,
                              years_after_sc=Year-SC,
                              share_between_scsi = (years_after_sc/time_sc_si)*max_siofsc,
                              per_si =case_when(Year>=SI ~ max_siofsc,
                                                Year<=SC ~ 0,
                                                Year>SC & Year<SI ~ share_between_scsi),
                              share_bonus=per_si/max_siofsc) 


baseline <- baseline %>% mutate(high_scale=case_when(high==1 ~ 1, high==0 ~ .9))

# take the 2019 numbers, and put them as their own variable, then apply the bonsus
base2019 <- baseline %>% filter(Year==2019) %>% select(iso, Injection, STM, NonUser) %>% 
        rename(Injection_2019=Injection,
               STM_2019=STM,
               NonUser_2019=NonUser)


baseline <- full_join(baseline, base2019, by="iso")

baseline <- baseline %>% mutate(inj_switch_si_a=(((inj_to_si+(si_bonus*share_bonus)))*Injection_2019)/11,
                              stm_switch_si_a=(((stm_to_si+(si_bonus*share_bonus))*scale)*STM_2019)/11,
                              nu_switch_si_a=(((nu_to_si+(si_bonus*share_bonus))*scale)*NonUser_2019*ReasonNotUsingSI)/11) 

equations <- baseline

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
equations <- equations %>% mutate(IM_injec_user_w_si=round(im_users),
                                  SCP_injec_user_w_si=round(si_users*(1-per_si)),
                                  SI_injec_user_w_si= round(si_users*(per_si)),
                                  non_users <- Total-total_user_w_si,
                                  mcpr_w_si=round((total_user_w_si/Total)*100,1),
                                  inject_share=injec_user_w_si/total_user_w_si,
                                  baseline_mcpr=round((baseline_users/Total)*100,1),
                                  additional_users=round(total_user_w_si-baseline_users))



# Gates Priority Coutries
gates <- equations %>% filter(iso== 566 | iso==404 | iso==854 | iso==586 | iso==800 | iso==686 | iso==454 ) %>% filter(Year==2030) %>% select(Country, iso,  additional_users)

write.csv(gates, "Gates Priority Countries SI 062220 With Change in NonUsers.csv", row.names = F)


# Results 2030
results <- equations %>% select(iso, Year, baseline_mcpr, mcpr_w_si, additional_users, total_user_w_si, baseline_users, Total, SI_injec_user_w_si, SCP_injec_user_w_si, IM_injec_user_w_si, non_users) %>%
    filter(Year==2030)




# Overall change in mcpr/number of users
sum2030 <- results %>% ungroup() %>% summarise(total_user_w_si=sum(total_user_w_si), baseline_users=sum(baseline_users), Total=sum(Total)) %>% mutate(addition=total_user_w_si-baseline_users, mcpr_si=total_user_w_si/Total, mcpr_nosi=baseline_users/Total)
############################################################
data2030 <- equations %>% ungroup() %>% 
            filter(Year==2030) %>%
            mutate(additional_users=total_user_w_si-baseline_users) %>% filter(iso==4) %>% select(additional_users) 
add <- data2030$additional_users[1]

############################################################


############################################################
sc_users2030 <-  equations %>% ungroup() %>% 
  filter(Year==2030) %>% filter(iso==4) %>% select(prop_si_im,  prop_si_stm,  prop_si_disc, prop_si_uptake) %>%
  gather(Type, Number, prop_si_im:prop_si_uptake ) %>% mutate(Number=100*Number, x=1) %>% 
  mutate(Type=case_when(Type=="prop_si_im" ~ "Former IM",
                        Type=="prop_si_stm" ~ "Former STM",
                        Type=="prop_si_disc" ~ "Reduced Discontinuation",
                        Type=="prop_si_uptake" ~ "Uptake"))

ggplot(sc_users2030,aes(x=x,y=Number, fill=Type))+ 
  geom_bar(stat="identity") +
  labs(title="Where SC Users Come From?", x="", y="", fill="")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


############################################################
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
  geom_bar(stat="identity") +
  labs(title="Number of Women", x="", y="", fill="")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=12))

############################################################

mcpr <-  equations %>% select(iso, Year, mcpr_w_si, baseline_mcpr ) %>%    gather(Type, Number, mcpr_w_si:baseline_mcpr )  %>%
  mutate(Type= case_when(Type=="mcpr_w_si" ~ "mCPR with SC Introduction",
                         Type=="baseline_mcpr" ~ "mCPR without SC Introduction"))


mcpr <- mcpr %>% filter(iso==4)

ggplot(mcpr,aes(x=Year,y=Number, color=Type))+ 
  geom_line(size=1.5)+
  labs(title="Effect for Self-Injectable Introduction on mCPR", x="", y="mCPR (AW)", color="")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=12))


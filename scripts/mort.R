#Data with mortality back in

#TPA constant ----
#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots
#4 subplots for DESIGNCD 423, 424, 425; 4 microplots
#5 subplots for DESIGNCD 410; 4 microplots 
dens_con_mort <- dens_mort %>%
  group_by(DESIGNCD) %>%
  mutate(SUBP_N = ifelse(TPA_UNADJ %in% c(75,60), 4, max(SUBP_t))) 
#DIA timber < 5 and woodland < 3 inches measured on microplots; TPA is 75

dens_con_mort <- dens_con_mort %>%
  group_by(PLT_CN,SUBP_t,Year) %>%
  mutate(PCCF = sum(CCF_t * (TPA_UNADJ * SUBP_N), na.rm = TRUE))

length(unique(dens_con_mort$PLT_CN)) #475

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
dens_con_mort <- dens_con_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE)) #%>%
#mutate(CCF = ifelse(CCF == 0, NA, CCF))

#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa
dens_con_mort <- dens_con_mort %>%
  mutate(BA_pa = ((DIA_C^2) * 0.005454) * TPA_UNADJ)


#rank trees per year per plot
#BAL
#sum BApa of trees larger on the same plot in same year

dens_con_mort <- dens_con_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(rank_pltyr = rank(DIA_C, na.last = TRUE, ties.method = "min")) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA_C,~sum(BA_pa[DIA_C>.x],na.rm = TRUE)))

dens_con_mort <- dens_con_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(SDI = sum(TPA_UNADJ*(DIA_C/10)^1.6)) 

#number of trees on plot for a given year
dens_con_mort <- dens_con_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(num_t = length(unique(TRE_CN)))

incr_cov_mort <- dens_con_mort %>%
  filter(TRE_CN %in% incr_tre)

incr_cov_mort <- crw_bound(data = incr_cov_mort, CR_WEIB_df)

save(incr_cov_mort,file = "./data/formatted/incr_cov_mort.Rdata")

incr_cov_red <- incr_cov_mort %>%
  dplyr::select(TRE_CN,Year,CCF,PCCF,BAL,SDI,CR_fvs)

load('./data/formatted/glmm_data_df.Rdata')
load('./data/formatted/glmm_data_pp.Rdata')
load('./data/formatted/glmm_data_es.Rdata')

glmm_data_df <- glmm_data_df %>%
 dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
glmm_data_pp <- glmm_data_pp %>%
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
glmm_data_es <- glmm_data_es %>%
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)

conmt_df_z <- stdize(as.data.frame(glmm_data_df),append=TRUE)
conmt_pp_z <- stdize(as.data.frame(glmm_data_pp),append=TRUE)
conmt_es_z <- stdize(as.data.frame(glmm_data_es),append=TRUE)

save(conmt_df_z, file = './data/formatted/conmt_df_z.Rdata')
save(conmt_pp_z, file = './data/formatted/conmt_pp_z.Rdata')
save(conmt_es_z, file = './data/formatted/conmt_es_z.Rdata')

#Variable plot ----

dens_mort <- dens_mort %>%
  mutate(TPA_C = ifelse(DESIGNCD %in% c(423,424,425),
                        TPA_UNADJ, #constant on fixed radius
                        ifelse(SPCD %in% c(475,322,814,749,321,65,66,106) & DIA_C >= 3,
                               (40/(0.005454*(DIA_C^2)))/5,
                               ifelse(SPCD %in% c(202,122,93,15,108,19,96,133,113,102,746) & DIA_C >= 5,
                                      (40/(0.005454*(DIA_C^2)))/5, 
                                      ifelse(TPA_UNADJ == 75, 75,60)))))



#Crown Competition Factor
#measure of stand density
#equations taken from Utah variant guide
##If DBH greater than or equal to 1” CCFt= R1 + (R2 * DBH) + (R3 * DBH2)
##If DBH less than 1” but greater than 0.1” CCFt = R4 * DBHR5
##If DBH less than 0.1” CCFt = 0.001


#dataframe of species specific coefficients for ccf calculation
ccf_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321,66,475,113,746,814,102),
                     #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,66=RM,96=BS,106=PI,108=LP,113=LM,133=PM,321=OH,475=MC,746=AS,814=GO,102=bristlecone pine
                     r1 = c(0.03,0.11,0.03,0.04,0.03,0.01925,0.03,0.01925,0.01925,0.01925,0.03,0.01925,0.0204,0.01925,0.03,0.03,0.01925),
                     r2 = c(0.0173,0.0333,0.0180,0.0270,0.0216,0.01676,0.0173,0.01676,0.01676,0.01676,0.0215,0.01676,0.0246,0.01676,0.0238,0.0215,0.01676),
                     r3 = c(0.00259,0.00259,0.00281,0.00405,0.00405,0.00365,0.00259,0.00365,0.00365,0.00365,0.00363,0.00365,0.0074,0.00365,0.00490,0.00363,0.00365),
                     r4 = c(0.007875,0.017299,0.007813,0.015248,0.011402,0.009187,0.007875,0.009187,0.009187,0.009187,0.011109,0.009187,0,0.009187,0.008915,0.011109,0.009187),
                     r5 = c(1.7360,1.5571,1.7680,1.7333,1.7560,1.76,1.736,1.76,1.76,1.76,1.7250,1.76,0,1.76,1.78,1.725,1.76),
                     dbrk = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10)) #can add more species later 

#ccf_t is for the tree and will be used to calculate plot and subplot level ccf
for(i in 1:nrow(dens_mort)){
  Species <- dens_mort$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(dens_mort$DIA_C[i] < dbrk,
           CCF_t <- dens_mort$DIA_C[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * dens_mort$DIA_C[i]) + (r3 * dens_mort$DIA_C[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(dens_mort$DIA_C[i]), 
           CCF_t <- NA,
           ifelse(dens_mort$DIA_C[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(dens_mort$DIA_C[i] < dbrk, 
                         CCF_t <- r4 * (dens_mort$DIA_C[i]^r5),
                         CCF_t <- r1 + (r2 * dens_mort$DIA_C[i]) + (r3 * dens_mort$DIA_C[i]^2))))
  }
  dens_mort$CCF_t[i] <- CCF_t
}

#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots
#4 subplots for DESIGNCD 423, 424, 425; 4 microplots
#5 subplots for DESIGNCD 410; 4 microplots 
dens_mort <- dens_mort %>%
  group_by(DESIGNCD) %>%
  mutate(SUBP_N = ifelse(TPA_C %in% c(75,60), 4, max(SUBP_t))) 
#DIA timber < 5 and woodland < 3 inches measured on microplots; TPA is 75

dens_mort <- dens_mort %>%
  group_by(PLT_CN,SUBP_t,Year) %>%
  mutate(PCCF = sum(CCF_t * (TPA_C * SUBP_N), na.rm = TRUE))

length(unique(dens_mort$PLT_CN)) #475

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
dens_mort <- dens_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(CCF = sum(CCF_t * TPA_C,na.rm = TRUE)) #%>%
#mutate(CCF = ifelse(CCF == 0, NA, CCF))

#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa
dens_mort <- dens_mort %>%
  mutate(BA_pa = ((DIA_C^2) * 0.005454) * TPA_C)


#rank trees per year per plot
#BAL
#sum BApa of trees larger on the same plot in same year

dens_mort <- dens_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(rank_pltyr = rank(DIA_C, na.last = TRUE, ties.method = "min")) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA_C,~sum(BA_pa[DIA_C>.x],na.rm = TRUE)))

dens_mort <- dens_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(SDI = sum(TPA_C*(DIA_C/10)^1.6)) 

#number of trees on plot for a given year
dens_mort <- dens_mort %>%
  group_by(PLT_CN,Year) %>%
  mutate(num_t = length(unique(TRE_CN)))

incr_mort <- dens_mort %>%
  filter(TRE_CN %in% incr_tre)

incr_cov_mort <- crw_bound(data = incr_mort, CR_WEIB_df)

incr_cov_red <- incr_cov_mort %>%
  dplyr::select(TRE_CN,Year,CCF,PCCF,BAL,SDI,CR_fvs)

load('./data/formatted/glmm_data_df.Rdata')
load('./data/formatted/glmm_data_pp.Rdata')
load('./data/formatted/glmm_data_es.Rdata')

glmm_data_df <- glmm_data_df %>%
  filter(!(PLT_CN %in% plt_dist)) %>% #see missDBH
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
length(unique(glmm_data_df$TRE_CN)) #111
glmm_data_pp <- glmm_data_pp %>%
  filter(!(PLT_CN %in% plt_dist)) %>% #see missDBH
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
length(unique(glmm_data_pp$TRE_CN)) #69
glmm_data_es <- glmm_data_es %>%
  filter(!(PLT_CN %in% plt_dist)) %>% #see missDBH
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
length(unique(glmm_data_es$TRE_CN)) #85

varmt_df_z <- stdize(as.data.frame(glmm_data_df),append=TRUE)
varmt_pp_z <- stdize(as.data.frame(glmm_data_pp),append=TRUE)
varmt_es_z <- stdize(as.data.frame(glmm_data_es),append=TRUE)

save(varmt_df_z, file = './data/formatted/varmt_df_z.Rdata')
save(varmt_pp_z, file = './data/formatted/varmt_pp_z.Rdata')
save(varmt_es_z, file = './data/formatted/varmt_es_z.Rdata')

#variable

load('./data/formatted/incr_calcov.Rdata')

incr_cov_red <- incr_calcov %>%
  dplyr::select(TRE_CN,Year,CCF,PCCF,BAL,SDI,CR_fvs)

var_df_z <- stdize(as.data.frame(glmm_data_df),append=TRUE)
var_pp_z <- stdize(as.data.frame(glmm_data_pp),append=TRUE)
var_es_z <- stdize(as.data.frame(glmm_data_es),append=TRUE)

var_es_z <- var_es_z %>%
  filter(!(PLT_CN %in% plt_dist)) %>% #see missDBH
  dplyr::select(-c(CCF,PCCF,BAL,SDI,CR_fvs)) %>%
  left_join(.,incr_cov_red)
length(unique(test_es_z$TRE_CN)) #85

save(var_df_z, file = './data/formatted/var_df_z.Rdata')
save(var_pp_z, file = './data/formatted/var_pp_z.Rdata')
save(var_es_z, file = './data/formatted/var_es_z.Rdata')


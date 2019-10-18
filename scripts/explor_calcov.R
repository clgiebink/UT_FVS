#Exploration
#August 2019
#Courtney Giebink: clgiebink@gmail.com

#DIA
hist((per_cov$DIA - per_cov$DIA_t),breaks=50,
     main = "Histogram of core DIA - FIADB DIA") #remove outliers?
DIA_diff <- per_cov %>%
  select(TRE_CN,DIA,DIA_t) %>%
  mutate(diff = DIA - DIA_t) %>%
  arrange(desc(diff))
write.csv(DIA_diff,"./data/explore/DIA_diff.csv")

#what is the average diameter of each tree?
ave_dia <- aggregate(incr_percov$DIA_t, by = list(incr_percov$TRE_CN), mean)
colnames(ave_dia) <- c("TRE_CN","ave_dia")
hist(ave_dia$ave_dia,breaks=50,
     main = "Histogram of Average DBH")

#Last ring width year - measure year

lastrw_measyr <- aggregate(incr_percov$Year, by = list(incr_percov$TRE_CN), max)
colnames(lastrw_measyr) <- c("TRE_CN","Year")
lastrw_measyr$MEASYEAR <- UT_per$MEASYEAR[match(lastrw_measyr$TRE_CN, UT_per$TRE_CN)]
lastrw_measyr$diff <- lastrw_measyr$Year - lastrw_measyr$MEASYEAR
hist(lastrw_measyr$diff,breaks=50,
     main = "Histogram of Last RW Year - MEASYEAR")
sum(lastrw_measyr$diff < -1) #96; what???
write.csv(lastrw_measyr,"./data/explore/lastrw_measyr.csv")

#CCF

#new dataframe
plot_des2 <- unique(incr_imputed[,c('PLT_CN','DESIGNCD','TPA_UNADJ', 'SUBP','SUBP_EXAM')])
unique(plot_des2$DESIGNCD[plot_des2$SUBP == 5])
write.csv(plot_des2,"./data/explore/plot_design.csv")

tree_subp5 <- incr_imputed %>%
  filter(SUBP == 5) %>%
  select(TRE_CN, SUBP,DESIGNCD) %>%
  distinct(TRE_CN,SUBP,DESIGNCD)
write.csv(tree_subp5,"./data/explore/tree_subp5.csv")
unique(incr_imputed$SUBP_EXAM)

#old dataframes

plot_design <- unique(data_all_df[,c('PLT_CN','SUBP')])

plot_design2 <- unique(density_data[,c('PLT_CN','DESIGNCD','TPA_UNADJ', 'SUBP')])
#plot_design2 is length 2201
#how many unique plots?
length(unique(plot_design2$PLT_CN))
#237

plot_design3 <- unique(density_data[,c('DESIGNCD','TPA_UNADJ')])
#2196
#how many unique plots?
length(unique(plot_design3$PLT_CN))
#237
tpa_check <- density_data %>%
  filter(DESIGNCD == 410)  %>%
  filter(DIA_C <= 5) %>%
  filter(MEASYEAR == Year) %>%
  dplyr::select(SPCD,DIA_C,TPA_UNADJ,TPA_C)

miss_tpa_check <- miss_data %>%
  dplyr::select(PLT_CN,TRE_CN,Year,SPCD,SUBP_t,MEASYEAR,DIA_t,DESIGNCD,TPA_UNADJ) %>%
  filter(MEASYEAR == Year) %>%
  filter(DESIGNCD == 423) %>%
  arrange(TPA_UNADJ)

tree1 <- miss_tpa_check[3733,2]
diac_check <- miss_data_imputed %>%
  dplyr::select(PLT_CN,TRE_CN,Year,SPCD,DESIGNCD,TPA_UNADJ,DIA_C) %>%
  filter(TRE_CN == tree1[1,1])

plot1 <- miss_tpa_check[3733,1]
subp_check <- miss_data_imputed %>%
  dplyr::select(PLT_CN,TRE_CN,Year,SPCD,SUBP_t,DESIGNCD,TPA_UNADJ,DIA_C) %>%
  filter(PLT_CN == plot1[1,1]) %>%
  filter(Year == 1988)

subp_check2 <- density_data %>%
  ungroup() %>%
  filter(DESIGNCD != 410) %>%
  dplyr::select(PLT_CN,SUBP_t,TPA_UNADJ) %>%
  distinct()

#unique subplots per plot?
#does subplot equal plot and plot equal stand?

plot_design4 <- unique(incr_calcov[,c('DESIGNCD','TPA_UNADJ', 'SUBP')])

#CR

#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)

density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(SDI = sum((DIA_C/10)^1.6)) 

#mutate(SDI = ifelse(SDI == 0, NA, SDI))
#ungroup?

#also try Reineke's method
density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(num_t = length(unique(TRE_CN)),
         QMD = sqrt(sum(DIA_C^2)/num_t),
         RSDI = ((QMD/10)^1.6)*TPA_C)

#check SDI calculation with one plot, one year

plot1 <- incr_calcov[410,32] #15 trees
sdi_check <- incr_calcov %>%
  dplyr::select(TRE_CN,PLT_CN,Year,SPCD,DIA_C,rank_pltyr,DESIGNCD,TPA_C,CCF,SDI,CR_weib) %>%
  filter(PLT_CN == plot1) %>%
  filter(Year == 1988)
save(sdi_check,file = "./data/explore/sdi_check.csv")                        
write.csv(sdi_check,file = "./data/explore/sdi_check.csv")

#calculations are right
#go with first SDI calculation

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

#tpa_unadj check
#do I have to do any transforming in ccf.R?
tpa_check <- density_data %>%
  ungroup() %>%
  dplyr::select(PLT_CN,TRE_CN,DIA_t,DESIGNCD,TPA_UNADJ) %>%
  distinct()
#check (40/(0.005454*(DIA_C^2)))/5
#can just use tpa_unadj

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
  mutate(num_t = length(unique(TRE_CN)))#,
         #QMD = sqrt(sum(DIA_C^2)/num_t),
         #RSDI = ((QMD/10)^1.6)*TPA_C)

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

#BAL
load("./data/formatted/density_data.Rdata")

bal_check <- density_data %>%
  select(PLT_CN,TRE_CN,Year,DIA_C,BALIVE,TPA_C,BA_pa,BAL,DESIGNCD) %>%
  filter(PLT_CN == plot1) %>%
  filter(Year == 1988)

#disturbance
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
cond <- tbl(UT_FIA, sql("SELECT PLT_CN, CONDID, COND_STATUS_CD, SLOPE, ASPECT,SDI_RMRS, SDIMAX_RMRS, SICOND, BALIVE, DSTRBCD1, SIBASE, SISP FROM COND")) %>%
  collect()

load("./data/formatted/data_all.Rdata")
data_all_sp <- data_all %>%
  filter(SPCD %in% c(93,122,202))
data_all_sp$DSTRBCD1 <- cond$DSTRBCD1[match(data_all_sp$PLT_CN,cond$PLT_CN)]
cal_dist <- data_all_sp %>%
  ungroup() %>%
  select(TRE_CN,DSTRBCD1,SPCD) %>%
  group_by(DSTRBCD1,SPCD) %>%
  summarise(n_tre = length(unique(TRE_CN)))


#validation ----
val_red %>%
  filter(SPCD == SISP) %>%
  ungroup() %>%
  dplyr::select(TRE_CN,SIBASE) %>%
  group_by(SIBASE) %>%
  summarise(n_tre = length(unique(TRE_CN)))


#look at a single tree over time
bal_yr_plots <- pred_clim%>%
  group_by(TRE_CN) %>%
  do(plots=ggplot(data=.) +
       aes(x=DIA, y=BAL) + geom_point() + ggtitle(unique(.$TRE_CN)))

par(mfrow=c(2,2))
bal_yr_plots$plots[[10]]
bal_yr_plots$plots[[20]]
bal_yr_plots$plots[[40]]
bal_yr_plots$plots[[70]]


#cr
pred_clim_2 <- pred_clim %>%
  group_by(TRE_CN) %>%
  filter(Year == MEASYEAR | Year == MEASYEAR + 1) %>%
  mutate(cr_change = lag(CR_fvs) - CR_fvs)
hist(pred_clim_2$cr_change, breaks = 50,
     xlab = "CR(t) - CR(t+1)")

pred_clim_2 <- pred_clim %>%
  group_by(TRE_CN) %>%
  filter(Year == MEASYEAR | Year == fMEASYEAR) %>%
  mutate(cr_change = lag(CR_fvs) - CR_fvs)
hist(pred_clim_2$cr_change, breaks = 50,
     xlab = "CR(start) - CR(end)", main = "Weibull Distribution")

hist((val_dset$CR_fvs - val_dset$fCR), breaks = 50,
     xlab = "CR(start) - CR(end)", main = "Observed")

##fvs
hist((fvs_check_red$PctCr - fvs_check_red$CR2), breaks = 50,
     xlab = "CR(start) - CR(end)", main = "FVS")

val_dset$PctCR <- fvs_check_red$PctCr[match(val_dset$TRE_CN,fvs_check_red$TRE_CN)]
plot(val_dset$CR_fvs,val_dset$PctCR,
     xlab = "Observed CR", ylab = "FVS CR")

#bal
val_dset$PtBAL <- fvs_check_red$PtBAL[match(val_dset$TRE_CN,fvs_check_red$TRE_CN)]

val_dset$BAL <- density_val$BAL[match(val_dset$TRE_CN,density_val$TRE_CN)]
plot(val_dset$BAL,val_dset$PtBAL,
     xlab = "My BAL", ylab = "FVS BAL",
     main = "Plot-level")

val_dset$BAL_s <- density_val$BAL_s[match(val_dset$TRE_CN,density_val$TRE_CN)]
plot(val_dset$BAL_s,val_dset$PtBAL,
     xlab = "My BAL", ylab = "FVS BAL",
     main = "Subplot/point-level")

val_dset$BAL_sc <- density_val$BAL_sc[match(val_dset$TRE_CN,density_val$TRE_CN)]
plot(val_dset$BAL_sc,val_dset$PtBAL,
     xlab = "My BAL", ylab = "FVS BAL",
     main = "Subplot/point-level & Scaled")


#stand density through time -----
plt_df <- unique(glmm_df_z$PLT_CN)
plt_pp <- unique(glmm_pp_z$PLT_CN)
plt_es <- unique(glmm_es_z$PLT_CN)
plt_foc <- c(plt_df,plt_pp,plt_es)
load("./data/formatted/data_all.Rdata")
plt_sdi_cal <- data_all %>%
  dplyr::select(PLT_CN,DESIGNCD,SPCD,Year,SDI) %>%
  filter(Year >= 1958 &
           SPCD %in% c(93,122,202)) %>%
  distinct() %>%
  drop_na()
length(unique(plt_sdi_cal$PLT_CN)) #224
#are there distinct species on each plot
dim(plt_sdi_cal %>%
      ungroup()%>%
      dplyr::select(PLT_CN,SPCD) %>%
      distinct())
#228 - 4 plots with multiple

load("./data/formatted/pred_clim_cr2.Rdata")
plt_sdi_val <- pred_clim_cr2 %>%
  dplyr::select(PLT_CN,SPCD,Year,SDI) %>%
  distinct()

#projection dataset
plt_sdi_proj <-  nonfoc_proj_exp %>%
  group_by(PLT_CN,Year) %>%
  mutate(SDI = sum(TPA_UNADJ*(DIA_int/10)^1.6)) %>%
  ungroup() %>%
  dplyr::select(PLT_CN,SPCD,Year,SDI) %>%
  filter(SPCD %in% c(93,122,202)) %>%
  distinct()

#stand age
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
cond_red <- tbl(UT_FIA, sql("SELECT PLT_CN, STDAGE FROM COND")) %>%
  collect()

plt_sdi_val <- left_join(plt_sdi_val, cond_red, by = "PLT_CN") #PLT is character
plt_sdi_proj <- left_join(plt_sdi_proj, cond_red, by = "PLT_CN") #PLT is character
cond_red$PLT_CN <- as.numeric(cond_red$PLT_CN)
plt_sdi_cal <- left_join(plt_sdi_cal, cond_red, by = "PLT_CN") #PLT is numeric
#plt_sdi_proj <- left_join(plt_sdi_proj, cond_red, by = "PLT_CN")

#for each plot
# new data frame arranged by
## max year for calibration
## min year for validation

#cal
#number of years for each plot
plt_sdi_cal <- plt_sdi_cal %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(n = n()) %>%
  full_join(.,plt_sdi_cal)
#stand age back in time
plt_sdi_cal <- plt_sdi_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN,SPCD) %>%
  mutate(age = (STDAGE-n+1):STDAGE)



#validation
#number of years for each plot
plt_sdi_val <- plt_sdi_val %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(n = n()) %>%
  full_join(.,plt_sdi_val)
#stand age forward in time
plt_sdi_val <- plt_sdi_val %>%
  arrange(Year) %>%
  group_by(PLT_CN,SPCD) %>%
  mutate(age = STDAGE:(STDAGE+n-1))

#proj
#number of years for each plot and species
plt_sdi_proj <- plt_sdi_proj %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(n = n()) %>%
  full_join(.,plt_sdi_proj)
#stand age back in time
plt_sdi_proj <- plt_sdi_proj %>%
  arrange(Year) %>%
  group_by(PLT_CN,SPCD) %>%
  mutate(age = (STDAGE-n+1):STDAGE)

#plot
ggplot(plt_sdi_cal, aes(x = age, y = SDI, group = PLT_CN)) +
  geom_line() + 
  geom_text(data=plt_sdi_cal ,
            aes(x = age + 0.03, label=PLT_CN), hjust=0)
#%>% group_by(ID) %>% 
#  arrange(desc(Year)) %>% 
#  slice(1) %>% 
#  filter(Y >= 50)
#add max sdi?
#add point for starting sdi?
#some going up back in time?
ggplot(plt_sdi_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()
ggplot(plt_sdi_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(DESIGNCD))) +
  labs(color = "Design code") +
  theme_bw()

#val
ggplot(plt_sdi_val) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of validation plots") +
  labs(color = "Species", caption = "Note: data from best validation run (clim_cr2)") +
  theme_bw()

sdi_hist_val <- plt_sdi_val%>%
  group_by(PLT_CN) %>%
  slice(which.min(Year), which.max(Year)) %>%
  mutate(when = c("start", "end")) %>%
  dplyr::select(PLT_CN, when, SDI) %>%
  pivot_wider(names_from = when, values_from = SDI) %>%
  mutate(diff = end-start)
hist(sdi_hist_val$diff,breaks = 50, xlab = "SDI2 - SDI1", main = "Difference in SDI")

#proj
ggplot(plt_sdi_proj) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  theme_bw() +
  ggtitle("Density of projection plots with BAR method") +
  labs(color = "Species", caption = "Note: only includes nonfocal trees")


plt_cal_ex <- plt_sdi_cal[1142, "PLT_CN"]
View(density_data %>%
       filter(PLT_CN %in% plt_cal_ex &
                Year >= 1958) %>%
       dplyr::select(PLT_CN,TRE_CN,DIA_C,TPA_C,Year,SDI))
View(density_data %>%
       filter(Year >= 1958) %>%
       dplyr::select(PLT_CN,TRE_CN,Year) %>%
       group_by(PLT_CN,Year) %>%
       summarise(n_tre = n()))

#ccf
plt_comp_cal <- data_all %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF) %>%
  distinct() %>%
  right_join(.,plt_sdi_cal)
ggplot(plt_comp_cal) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  ggtitle("CCF on calibration plots") +
  labs(color = "Species") +
  theme_bw()

#bal
plt_bal_cal <- data_all %>%
  dplyr::select(PLT_CN,TRE_CN,SPCD,Year,BAL) %>%
  filter(Year >= 1958 &
           SPCD %in% c(93,122,202)) %>%
  distinct() %>%
  drop_na()
plt_bal_cal <- plt_sdi_cal %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,age) %>%
  full_join(.,plt_bal_cal)
ggplot(plt_bal_cal) +
  geom_line(aes(age,BAL, group = TRE_CN, color = factor(SPCD))) +
  ggtitle("BAL on calibration plots") +
  labs(color = "Species") +
  theme_bw()

#number of species per plot
load("./data/formatted/density_data.Rdata")
plt_tre <- density_data %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,TRE_CN,SPCD,SDI) %>%
  filter(Year >= 1958 & 
           PLT_CN %in% plt_foc) %>%
  group_by(PLT_CN) %>%
  summarise(n_tre = length(unique(TRE_CN)),
            n_sp = length(unique(SPCD)),
            mn_sdi = mean(SDI))
ggplot(plt_tre) +
  geom_point(aes(n_tre,n_sp,color=mn_sdi,alpha = 0.5)) +
  ggtitle("Calibration plots") +
  xlab("Number of trees") + ylab("Unique Species") + labs(color = "Mean SDI") +
  theme_bw()

#site index by species
load("./data/formatted/data_all.Rdata")
length(unique(data_all$TRE_CN)) #568
si_df <- data_all %>%
  ungroup() %>%
  filter(SPCD %in% c(93,122,202)) %>%
  dplyr::select(TRE_CN,SPCD,SICOND) %>%
  distinct() %>%
  mutate(Species = factor(ifelse(SPCD==93,"Engelmann spruce",
                                 ifelse(SPCD==122,"Ponderosa pine",
                                        "Douglas fir"))))
ggplot(si_df, aes(x=SICOND, fill=Species)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  theme_bw()

ggplot(si_df, aes(x=SICOND, fill=Species)) +
  geom_density(alpha=.5) +
  theme_bw()

#with variable radius plots
#df
var_df_cal <- var_df_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
var_df_cal <- left_join(var_df_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
var_df_cal <- var_df_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 202)
ggplot(var_df_cal) +
  geom_line(aes(age,SDI, group = PLT_CN)) +
  geom_hline(yintercept=570, linetype="dashed") +
  ggtitle("Douglas fir") +
  theme_bw()

#pp
var_pp_cal <- var_pp_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
var_pp_cal <- left_join(var_pp_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
var_pp_cal <- var_pp_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 122)

#es
var_es_cal <- test_es_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
var_es_cal <- left_join(var_es_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
var_es_cal <- var_es_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 93)

var_cal <- bind_rows(var_df_cal,var_pp_cal) %>%
  bind_rows(.,var_es_cal)
ggplot(var_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()
ggplot(var_cal) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()

#df
varmt_df_cal <- varmt_df_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
varmt_df_cal <- left_join(varmt_df_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
varmt_df_cal <- varmt_df_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 202)

#pp
varmt_pp_cal <- varmt_pp_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
varmt_pp_cal <- left_join(varmt_pp_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
varmt_pp_cal <- varmt_pp_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 122)

#es
varmt_es_cal <- varmt_es_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
varmt_es_cal <- left_join(varmt_es_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
varmt_es_cal <- varmt_es_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 93)

varmt_cal <- bind_rows(varmt_df_cal,varmt_pp_cal) %>%
  bind_rows(.,varmt_es_cal)
ggplot(varmt_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()
ggplot(varmt_cal) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()



#df
con_df_cal <- con_df_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
con_df_cal <- left_join(con_df_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
con_df_cal <- con_df_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 202)

#pp
con_pp_cal <- con_pp_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
con_pp_cal <- left_join(con_pp_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
con_pp_cal <- con_pp_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 122)

#es
con_es_cal <- con_es_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
con_es_cal <- left_join(con_es_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
con_es_cal <- con_es_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 93)

con_cal <- bind_rows(con_df_cal,con_pp_cal) %>%
  bind_rows(.,con_es_cal)
ggplot(con_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()
ggplot(con_cal) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()

#df
conmt_df_cal <- conmt_df_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
conmt_df_cal <- left_join(conmt_df_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
conmt_df_cal <- conmt_df_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 202)

#pp
conmt_pp_cal <- conmt_pp_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
conmt_pp_cal <- left_join(conmt_pp_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
conmt_pp_cal <- conmt_pp_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 122)

#es
conmt_es_cal <- conmt_es_z %>%
  ungroup() %>%
  dplyr::select(PLT_CN,Year,CCF,SDI) %>%
  distinct()
conmt_es_cal <- left_join(conmt_es_cal, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
conmt_es_cal <- conmt_es_cal %>%
  arrange(Year) %>%
  group_by(PLT_CN) %>%
  mutate(age = (STDAGE-length(Year)+1):STDAGE,
         SPCD = 93)

conmt_cal <- bind_rows(conmt_df_cal,conmt_pp_cal) %>%
  bind_rows(.,conmt_es_cal)
ggplot(conmt_cal) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()
ggplot(conmt_cal) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()

#explore the correlation between the different methods
load('./data/formatted/con_cal.Rdata')
load('./data/formatted/conmt_cal.Rdata')
load('./data/formatted/var_cal.Rdata')
load('./data/formatted/varmt_cal.Rdata')

colnames(var_cal)[colnames(var_cal)=="CCF"] <- "CCF_var"
colnames(var_cal)[colnames(var_cal)=="SDI"] <- "SDI_var"

colnames(varmt_cal)[colnames(varmt_cal)=="CCF"] <- "CCF_varmt"
colnames(varmt_cal)[colnames(varmt_cal)=="SDI"] <- "SDI_varmt"

colnames(conmt_cal)[colnames(conmt_cal)=="CCF"] <- "CCF_conmt"
colnames(conmt_cal)[colnames(conmt_cal)=="SDI"] <- "SDI_conmt"

colnames(con_cal)[colnames(con_cal)=="CCF"] <- "CCF_con"
colnames(con_cal)[colnames(con_cal)=="SDI"] <- "SDI_con"

comp_cor <- left_join(var_cal, varmt_cal) %>% left_join(., con_cal) %>% left_join(., conmt_cal)

#vis correlation
library(PerformanceAnalytics)

ccf_cor <- comp_cor %>% ungroup() %>% dplyr::select(starts_with("CCF"))
chart.Correlation(ccf_cor, histogram=TRUE, pch=19)

sdi_cor <- comp_cor %>% ungroup() %>% dplyr::select(starts_with("SDI"))
chart.Correlation(sdi_cor, histogram=TRUE, pch=19)

#check projection outputs
load('./data/formatted/projection/red_bcc85.Rdata')

bcc85_red_ex <- red_bcc85 %>%
  ungroup() %>%
  dplyr::select(PLT_CN,SPCD,Year,CCF,SDI) %>%
  distinct()
bcc85_red_ex <- left_join(bcc85_red_ex, cond_red, by = "PLT_CN") #PLT is numeric
#stand age back in time
bcc85_red_ex <- bcc85_red_ex %>%
  arrange(Year) %>%
  group_by(PLT_CN,SPCD) %>%
  mutate(age = STDAGE:(STDAGE+length(Year)-1))

ggplot(bcc85_red_ex) +
  geom_line(aes(age,CCF, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()

ggplot(bcc85_red_ex) +
  geom_line(aes(age,SDI, group = PLT_CN, color = factor(SPCD))) +
  geom_hline(yintercept=620, linetype="dashed", color = "red") +
  geom_hline(yintercept=446, linetype="dashed", color = "green") +
  geom_hline(yintercept=570, linetype="dashed", color = "blue") +
  #ggtitle("Density of calibration plots") +
  labs(color = "Species") +
  theme_bw()



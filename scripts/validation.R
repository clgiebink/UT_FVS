#Validation
#grow trees forward with model object
#Courtney Giebink
#clgiebink@gmail.com
#February 2020


# FIA ----

#connect to database
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
tree <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, PLOT, COUNTYCD, SUBP, TREE, SPCD, STATUSCD, PREV_TRE_CN, DIA, CR, TPA_UNADJ, INVYR FROM TREE")) %>%
  collect()
colnames(tree)[colnames(tree)=="CN"] <- "TRE_CN"
plot <- tbl(UT_FIA, sql("SELECT CN, MEASYEAR, LAT, LON, ELEV, DESIGNCD, SUBP_EXAMINE_CD, PREV_PLT_CN FROM PLOT")) %>%
  collect()
colnames(plot)[colnames(plot)=="CN"] <- "PLT_CN"
cond <- tbl(UT_FIA, sql("SELECT PLT_CN, CONDID, COND_STATUS_CD, SLOPE, ASPECT,SDI_RMRS, SDIMAX_RMRS, SICOND, BALIVE, DSTRBCD1, SIBASE, SISP FROM COND")) %>%
  collect()

#merge tables to make dataset
#each row is a tree
plt_tre <- left_join(tree,plot, by = "PLT_CN")
val_full <- left_join(plt_tre,cond, by = "PLT_CN")

#get future data for each tree
#get DIA at remeasurement
val_full$fDIA <- tree$DIA[match(val_full$TRE_CN,tree$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
val_full$fSTATUSCD <- tree$STATUSCD[match(val_full$TRE_CN,tree$PREV_TRE_CN)]
#get future crown ratio to interpolate later
val_full$fCR <- tree$CR[match(val_full$TRE_CN,tree$PREV_TRE_CN)]

val_full$fMEASYEAR <- plot$MEASYEAR[match(val_full$PLT_CN,plot$PREV_PLT_CN)]

#filter for trees to be used for validation

#filter for trees that were remeasured
#trees can have a PREV_TRE_CN of NA if first measurement
val_red <- val_full %>%
  filter(TRE_CN %in% PREV_TRE_CN &
           SPCD %in% c(93,122,202)) %>% #filter for focal species
  #filter for alive trees at both remeasurement
  filter(STATUSCD == 1 & fSTATUSCD == 1) %>%
  #filter for trees that are greater than 3 inch (large tree growth model threshold)
  filter(DIA >= 3) %>%
  #filter for trees that that are larger at remeasurement/grew
  filter(DIA <= fDIA)
#5689 rows
length(unique(val_red$TRE_CN))
#4614 trees
#why are there duplicate trees? - more than one condition?


#summarize dataset
#find NAs, etc
summary(val_red)
#site index has NA

#filter for SICOND
#Species associated with SICOND (aka SISP) doesn't always match SPCD
SITREE <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, SPCD, SITREE FROM SITETREE")) %>%
  collect()
#filter trees for SI
val_si_check <- val_red %>%
  select(PLT_CN,SUBP,TRE_CN,SPCD,SICOND,SISP,SIBASE) %>%
  distinct() #for some reason there are duplicate trees - more than one condition?
#join with SITREE table
val_si_check <- left_join(val_si_check, SITREE, by = c("PLT_CN","SPCD")) %>%
  distinct()
#calculate average site index per species per plot
val_si_plt <- val_si_check %>%
  dplyr::select(PLT_CN,SPCD,SITREE) %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(mean = mean(SITREE,na.rm=TRUE))
#replace in validation dataset
val_red <- val_red %>%
  mutate(SICOND_c = ifelse(SPCD == SISP,
                           SICOND,
                           val_si_plt$mean[val_si_plt$PLT_CN == PLT_CN &
                                               val_si_plt$SPCD == SPCD])) %>%
  filter(!is.na(SICOND_c))
#3457 rows
length(unique(val_red$TRE_CN))
#reduced by 1246

#Condition
#CONDID is the unique number assigned to each condition on a plot (1,2,etc)
#CON_STATUS_CD is the number associated with the type of land
##1 = forest land
##2 = nonforested land
##3 = noncensus water
##4 = census water
##5 = non sampled forest land

#find plots with multiple conditions
plt_val <- val_red$PLT_CN
cond_red <- tbl(UT_FIA, sql("SELECT PLT_CN, CONDID, COND_STATUS_CD FROM COND")) %>%
  collect() %>%
  filter(PLT_CN %in% plt_val)
#summarize to find how many unique conditions on each plot
val_cond_plt <- cond_red %>%
  group_by(PLT_CN) %>%
  summarise(n_cond = length(unique(CONDID)), #number of conditions
            n_stat = length(unique(COND_STATUS_CD)), 
            max_stat = max(COND_STATUS_CD)) #maximum condition status code on the plot
#don't keep plots with greater than 2 condition status (don't keep 3 - 5)
val_cond_plt <- val_cond_plt %>%
  filter(max_stat <= 2)
#filter
length(unique(val_red$TRE_CN)) #3368
val_red <- val_red %>%
  filter(PLT_CN %in% val_cond_plt$PLT_CN)
length(unique(val_red$TRE_CN)) #3042
#no duplicates

#how many more trees on plots with multiple conditions?
val_cond_tre <- val_red %>%
  select(PLT_CN,TRE_CN) %>%
  filter(PLT_CN %in% val_cond_plt$PLT_CN[val_cond_plt$n_cond == 2])
#506 trees

#try filter with only 1 condition on plot, which is forest land
val_cond_plt <- val_cond_plt %>%
  filter(n_cond == 1 & max_stat ==1)
val_red <- val_red %>%
  filter(PLT_CN %in% val_cond_plt$PLT_CN)
length(unique(val_red$TRE_CN)) #total reduction is 832

#disturbance
val_dist <- val_red %>%
  select(TRE_CN,DSTRBCD1) %>%
  group_by(DSTRBCD1) %>%
  summarise(n_tre = length(unique(TRE_CN)))
#reduce by 326

#keep disturbance
#calibration data set has disturbance codes:
# 10, 20, 30, 50, 52, 60, 80

val_dset <- val_red
val_dset %>%
  dplyr::select(TRE_CN,SPCD) %>%
  group_by(SPCD) %>%
  summarise(n_tre = length(unique(TRE_CN)))

PLOTGEOM <- tbl(UT_FIA, sql("SELECT CN, FVS_LOC_CD FROM PLOTGEOM")) %>%
  collect()
val_dset$FVS_LOC_CD <- PLOTGEOM$FVS_LOC_CD[match(val_dset$PLT_CN, PLOTGEOM$CN)]

# Disconnect from the database
dbDisconnect(UT_FIA)

#all available trees
save(val_dset,file = "./data/formatted/val_dset.Rdata")

# Density ----
#for current year
#fetch trees from same plot in FIADB
#with associated covariate data (above)
#will include other species

#covariates
#density
#ccf - calculate
#bal - 
#cr - given

#get trees from same plot
plot_val <- unique(val_dset$PLT_CN)
tree_val <- unique(val_dset$TRE_CN)
density_val <- tree[(tree$PLT_CN %in% plot_val),]
#make sure trees are on the same plot b/c calculating stand variables

#remove trees that are dead
density_val <- density_val %>%
  filter(STATUSCD == 1)

#get DIA at remeasurement
density_val$fDIA <- tree$DIA[match(density_val$TRE_CN,tree$PREV_TRE_CN)]
#get future status - dead or alive
density_val$fSTATUSCD <- tree$STATUSCD[match(density_val$TRE_CN,tree$PREV_TRE_CN)]

#plot information
density_val <- left_join(density_val,plot)

density_val$fMEASYEAR <- plot$MEASYEAR[match(density_val$PLT_CN, plot$PREV_PLT_CN)]

#check
length(plot_val)
length(unique(density_val$PLT_CN))
#also check - no duplicate trees - can happen with multiple CONDID

#calculate density metrics
#ccf
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

#empty vector to hold calculated crown competition factor
density_val$CCF_t <- vector(mode="numeric", length=nrow(density_val))
for(i in 1:nrow(density_val)){
  Species <- density_val$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(density_val$DIA[i] < dbrk,
           CCF_t <- density_val$DIA[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * density_val$DIA[i]) + (r3 * density_val$DIA[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(density_val$DIA[i]), 
           CCF_t <- NA,
           ifelse(density_val$DIA[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(density_val$DIA[i] < dbrk, 
                         CCF_t <- r4 * (density_val$DIA[i]^r5),
                         CCF_t <- r1 + (r2 * density_val$DIA[i]) + (r3 * density_val$DIA[i]^2))))
  }
  density_val$CCF_t[i] <- CCF_t
}

#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots

density_val <- density_val %>%
  group_by(PLT_CN,SUBP) %>%
  mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
density_val <- density_val %>%
  group_by(PLT_CN) %>%
  mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))

#bal
#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa
density_val <- density_val %>%
  mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)

#BAL
#sum BApa of trees larger on the same plot in same year
density_val <- density_val %>%
  group_by(PLT_CN) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))

#CR
#see CR.R
#filter for focal trees
val_dset <- density_val %>%
  select(TRE_CN,CCF,PCCF,BAL) %>%
  right_join(.,val_dset)

#First need to calcuate SDI
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)
#then number of trees on a plot
#rank in the diameter distribution
# val_dset <- val_dset %>%
#   group_by(PLT_CN) %>%
#   mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6),
#          num_t = length(unique(TRE_CN)),
#          rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min"))

#will only need cr function for crown ratio change
crw_bound <- function(data,CR_WEIB_df){
  #by tree
  data$CR_fvs <- NA
  data_empty <- data[F,]
  for(t in unique(data$TRE_CN)){
    #make dataframe
    tre_df <- data %>%
      filter(TRE_CN == t) %>%
      arrange(Year)
    Species <- tre_df$SPCD[1]
    
    #only focal species
    if(Species %in% CR_WEIB_df$species){
      
      #SDI max values for each species were pulled from UT Variant overview
      ifelse(is.na(tre_df$SDIMAX_RMRS[1]),
             SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
             SDIMAX <- tre_df$SDIMAX_RMRS[1])
      #parameters true for same species
      d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
      d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
      #Parameters of Weibull distribution: A,B,C
      a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
      b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
      b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
      c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
      c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
      
      N <- which(tre_df$Year == tre_df$MEASYEAR[1]) #next step is to allow N to be ring width year -1
      if(length(N) == 0){
        N <- which(tre_df$Year + 1 == tre_df$MEASYEAR[1])
      }
      if(length(N) > 0){
        Curr_row <- N-1 #each time through subtract 1 and move down one row
        tre_df$CR_fvs[N] <- tre_df$CR[N] #dbh when year of ring width and measure year are equal
        while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
          #Calculate relative density
          #SDI - is SDI of stand (Stage 1968)
          RD <- tre_df$SDI[Curr_row]/SDIMAX
          #Calculate average stand crown ratio (ACR) for each species in the stand
          ACR <- d0 + d1 * RD * 100
          #A parameter
          WEIBA <-a0
          #B parameter
          WEIBB <- b0 + b1*ACR
          #C parameter
          WEIBC <- c0 + c1*ACR
          
          #Function arguments:
          
          #CCF - crown competition factor of stand
          #rank_pltyr - tree's rank in diameter distribution by plot by year
          #N  - number of records in the stand by year
          
          #Calculate scale parameter
          SCALE = (1.0 - .00167 * (tre_df$CCF[Curr_row]-100.0))
          if(SCALE < 0.3){SCALE = 0.3}
          if(SCALE > 1.0){SCALE = 1.0}
          
          N <- tre_df$num_t[Curr_row]
          #X is tree's rank in diameter distribution
          #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
          Y <- tre_df$rank_pltyr[Curr_row]/N * SCALE
          if(Y < 0.05){Y = 0.05}
          if(Y > 0.95){Y = 0.95}
          #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
          
          #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
          X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
          #X = a tree’s crown ratio expressed as a percent / 10
          CR_weib <- X * 10
          
          CR_1 <- tre_df$CR_fvs[Curr_row+1] #or CR_fvs[N] for the first round
          #bound to 1% change per year
          cr_bound1 <- tre_df$CR_fvs[Curr_row+1] * .01
          tre_df$CR_fvs[Curr_row] <- ifelse(CR_1 > CR_weib, 
                                            CR_1 - cr_bound1,
                                            CR_1 + cr_bound1)
          
          #loop will stop when it gets to the end of data for that tree
          #continue loop for next row until curr_row>0
          Curr_row = Curr_row - 1 
        }
      }
      data_empty <- bind_rows(data_empty,tre_df)
    }
    else {
      data_empty <- bind_rows(data_empty,tre_df)
    }
  }
  return(data_empty)
}

#what other species are on the plot?
density_val %>%
  ungroup() %>%
  dplyr::select(TRE_CN,SPCD) %>%
  group_by(SPCD) %>%
  summarise(n_tre = length(unique(TRE_CN)))

save(density_val, file = "./data/formatted/density_val.Rdata")
save(val_dset,file = "./data/formatted/val_dset.Rdata")

# Current FVS ----
#grow focal trees
#coefficients

#b1 based on location
#location codes found in FIADB
#(FVS_LOC_CD) in PLOTGEOM table

View(val_dset %>% 
  ungroup() %>% 
  dplyr::select(TRE_CN,MEASYEAR,fMEASYEAR) %>% 
  group_by(MEASYEAR,fMEASYEAR) %>% 
  summarise(n_tre = length(unique(TRE_CN))))

#FVS ready data
plt_measyr <- val_dset %>%
  select(PLT_CN,MEASYEAR,fMEASYEAR) %>%
  distinct()

#original code Mark castle
#Read in CSV containing validation plots and years
valPlots<-plt_measyr

#Remove row names column
valPlots$X<-NULL;head(valPlots)

#Open DB connection
con <- dbConnect(SQLite(), "./data/raw/FIADB.db")

#Extract FVS_GroupAddFilesAndKeywords table
fvsAddKey<-dbReadTable(con, 'FVS_GROUPADDFILESANDKEYWORDS')

#Extract FVS_PlotInit_Plot table
fvsPlotInitPlot<-dbReadTable(con, 'FVS_PLOTINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsStandInitPlot<-dbReadTable(con, 'FVS_STANDINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsStandInitCond<-dbReadTable(con, 'FVS_STANDINIT_COND')

#Extract FVS_StandInit_Cond table
fvsStandInitPlot<-dbReadTable(con, 'FVS_STANDINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsTreeInitPlot<-dbReadTable(con, 'FVS_TREEINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsTreeInitCond<-dbReadTable(con, 'FVS_TREEINIT_COND')

#Disconnect from input database
dbDisconnect(con)

#Rename  PLT_CN header in valPlots
names(valPlots)[names(valPlots)=="PLT_CN"]<-"STAND_CN";head(valPlots)

#Subset FIA utah data based on the plots in valPlots
fvsPlotInitPlot<-fvsPlotInitPlot[fvsPlotInitPlot$STAND_CN %in% valPlots$STAND_CN,]
fvsStandInitPlot<-fvsStandInitPlot[fvsStandInitPlot$STAND_CN %in% valPlots$STAND_CN,]
fvsTreeInitPlot<-fvsTreeInitPlot[fvsTreeInitPlot$STAND_CN %in% valPlots$STAND_CN,]

#Merge inventory years to FVSPlotInitPlot and FVSStandInitPlot
fvsPlotInitPlot<-merge(fvsPlotInitPlot, valPlots, by="STAND_CN", all.x = T);head(fvsPlotInitPlot)
fvsStandInitPlot<-merge(fvsStandInitPlot, valPlots, by="STAND_CN", all.x = T);head(fvsStandInitPlot)

#Create group label based on inventory years
fvsPlotInitPlot$NewGroup<-paste(fvsPlotInitPlot$MEASYEAR, fvsPlotInitPlot$fMEASYEAR, sep = "_");head(fvsPlotInitPlot)
fvsStandInitPlot$NewGroup<-paste(fvsStandInitPlot$MEASYEAR, fvsStandInitPlot$fMEASYEAR, sep = "_");head(fvsStandInitPlot)

#Add NewGroup to GROUPS column
fvsPlotInitPlot$GROUPS<-paste(fvsPlotInitPlot$GROUPS, fvsPlotInitPlot$NewGroup, sep = " ");head(fvsPlotInitPlot$GROUPS)
fvsStandInitPlot$GROUPS<-paste(fvsStandInitPlot$GROUPS, fvsStandInitPlot$NewGroup, sep = " ");head(fvsStandInitPlot$GROUPS)

#Create new UT DB
conn <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_val.db")

#Write each of the neccesary FVS tables to DB
dbWriteTable(conn, "FVS_GROUPADDFILESANDKEYWORDS", fvsAddKey)
dbWriteTable(conn, "FVS_PLOTINIT_PLOT", fvsPlotInitPlot)
dbWriteTable(conn, "FVS_STANDINIT_COND", fvsStandInitCond)
dbWriteTable(conn, "FVS_STANDINIT_PLOT", fvsStandInitPlot)
dbWriteTable(conn, "FVS_TREEINIT_COND", fvsTreeInitCond)
dbWriteTable(conn, "FVS_TREEINIT_PLOT", fvsTreeInitPlot)


#from FVS online
#https://forest.moscowfsl.wsu.edu/FVSOnline/

#get output from fvs runs
#tree list table
fvs_treelist <- read_csv(file = "./data/raw/FVS/fvs_val.csv")
length(unique(fvs_treelist$StandID))
#mostly to convert 093 -> 93
fvs_treelist$SpeciesFIA <- as.numeric(fvs_treelist$SpeciesFIA)
fvs_red <- fvs_treelist %>%
  dplyr::select(StandID,Year,PrdLen,TreeId,TreeIndex,SpeciesFIA,DBH,DG,PctCr,ActPt) %>%
  filter(SpeciesFIA %in% c(93,122,202)) %>%
  filter(DBH >= 3) #growth model threshold
length(unique(fvs_red$StandID)) #should match above

#connect to fvs ready database for validation data
library(dbplyr)
library(RSQLite)
fvs_val_db <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_val.db")
#Extract FVS_StandInit_Plot table
#has the link between stand_cn (aka plt_cn) and stand_id
fvsStandInitPlot<-dbReadTable(fvs_val_db, 'FVS_STANDINIT_PLOT');head(fvsStandInitPlot)
# Disconnect from the database
dbDisconnect(fvs_val_db)
std2plt <- fvsStandInitPlot %>%
  dplyr::select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID))
#stand_cn = plt_cn
fvs_red$PLT_CN <- std2plt$STAND_CN[match(fvs_red$StandID,std2plt$STAND_ID)]
length(unique(fvs_red$PLT_CN))

#separate observations from predictions
fvs_obs <- fvs_red %>%
  filter(DG == 0) %>%
  select(-DG)
dim(fvs_obs) 
length(unique(fvs_obs$PLT_CN))

fvs_pred <- fvs_red %>%
  filter(DG != 0) %>%
  select(-c("PrdLen"))
dim(fvs_pred) 
length(unique(fvs_pred$PLT_CN))

#rename columns to be able to merge
colnames(fvs_obs)[colnames(fvs_obs)=="Year"] <- "MEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="Year"] <- "fMEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="DBH"] <- "eDBH"
colnames(fvs_pred)[colnames(fvs_pred)=="PctCr"] <- "CR2"

#merge 
fvs_obs_pred <- left_join(fvs_obs,fvs_pred)
#tree index > 2000 means trees died in projection?

#validation dataset for future observations
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE <- dbReadTable(UT_FIA, 'TREE')
TREE_red <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, SPCD, SUBP, TREE, DIA, PREV_TRE_CN, STATUSCD FROM TREE")) %>%
  collect()
TREE_red$fDIA <- TREE$DIA[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
TREE_red$fSTATUSCD <- TREE$STATUSCD[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#prep for merge
colnames(TREE_red)[colnames(TREE_red)=="TREE"] <- "TreeId"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="SpeciesFIA"] <- "SPCD"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="ActPt"] <- "SUBP"
# Disconnect from the database
dbDisconnect(UT_FIA)

#merge fvs runs and fia observations
fvs_check <- left_join(fvs_obs_pred,TREE_red)
length(unique(fvs_check$CN))
#filter for trees that are alive at remeasurement
fvs_check <- fvs_check %>%
  filter(fSTATUSCD == 1)
length(unique(fvs_check$CN))
fvs_check_red <- fvs_check %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% val_dset$TRE_CN)
length(unique(fvs_check_red$CN))

save(fvs_check_red, file = "./data/formatted/fvs_check_red.Rdata")


# Climate-FVS ----

clim_fvs <- val_dset %>%
  mutate(Ele = ELEV/3.28) %>%
  dplyr::select(PLT_CN,LON,LAT,Ele) %>%
  distinct()
write.table(clim_fvs, file = "./data/formatted/clim_fvs.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE)

#climate-ready fvs data
#http://charcoal.cnre.vt.edu/climate/customData/fvs_data.php

#ensemble of 17 AR5 model predictions, rcp85
#get output from fvs runs
#tree list table
fvs_treelist <- read_csv(file = "./data/raw/FVS/fvs_85.csv")
length(unique(fvs_treelist$StandID))
#mostly to convert 093 -> 93
fvs_treelist$SpeciesFIA <- as.numeric(fvs_treelist$SpeciesFIA)
fvs_red <- fvs_treelist %>%
  dplyr::select(StandID,Year,PrdLen,TreeId,TreeIndex,SpeciesFIA,DBH,DG,PctCr,ActPt) %>%
  filter(SpeciesFIA %in% c(93,122,202)) %>%
  filter(DBH >= 3) #growth model threshold
length(unique(fvs_red$StandID)) #should match above

#connect to fvs ready database for validation data
library(dbplyr)
library(RSQLite)
fvs_val_db <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_val.db")
#Extract FVS_StandInit_Plot table
#has the link between stand_cn (aka plt_cn) and stand_id
fvsStandInitPlot<-dbReadTable(fvs_val_db, 'FVS_STANDINIT_PLOT')
# Disconnect from the database
dbDisconnect(fvs_val_db)
std2plt <- fvsStandInitPlot %>%
  dplyr::select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID))
#stand_cn = plt_cn
fvs_red$PLT_CN <- std2plt$STAND_CN[match(fvs_red$StandID,std2plt$STAND_ID)]
length(unique(fvs_red$PLT_CN))

#separate observations from predictions
fvs_obs <- fvs_red %>%
  filter(DG == 0) %>%
  select(-DG)
dim(fvs_obs) 
length(unique(fvs_obs$PLT_CN))

fvs_pred <- fvs_red %>%
  filter(DG != 0) %>%
  select(-c("PrdLen"))
dim(fvs_pred) 
length(unique(fvs_pred$PLT_CN))

#rename columns to be able to merge
colnames(fvs_obs)[colnames(fvs_obs)=="Year"] <- "MEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="Year"] <- "fMEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="DBH"] <- "eDBH"
colnames(fvs_pred)[colnames(fvs_pred)=="PctCr"] <- "CR2"

#merge 
fvs_obs_pred <- left_join(fvs_obs,fvs_pred)
#tree index > 2000 means trees died in projection?

#validation dataset for future observations
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE <- dbReadTable(UT_FIA, 'TREE')
TREE_red <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, SPCD, SUBP, TREE, DIA, PREV_TRE_CN, STATUSCD FROM TREE")) %>%
  collect()
TREE_red$fDIA <- TREE$DIA[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
TREE_red$fSTATUSCD <- TREE$STATUSCD[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#prep for merge
colnames(TREE_red)[colnames(TREE_red)=="TREE"] <- "TreeId"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="SpeciesFIA"] <- "SPCD"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="ActPt"] <- "SUBP"
# Disconnect from the database
dbDisconnect(UT_FIA)

#merge fvs runs and fia observations
fvs_check <- left_join(fvs_obs_pred,TREE_red)
length(unique(fvs_check$CN))
#filter for trees that are alive at remeasurement
fvs_check <- fvs_check %>%
  filter(fSTATUSCD == 1)
length(unique(fvs_check$CN))
fvs_clim_red <- fvs_check %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% val_dset$TRE_CN)
length(unique(fvs_check_red$CN))

#are climate projections different from regular projections?
fvs_clim_check <- fvs_check_red %>%
  dplyr::select(TRE_CN,DIA,fDIA,eDBH)
fvs_clim_check$eDBH_clim <- fvs_clim_red$eDBH[match(fvs_clim_check$TRE_CN,fvs_clim_red$TRE_CN)]
fvs_clim_check <- fvs_clim_check %>%
  mutate(diff_clim = eDBH - eDBH_clim) #0 no difference


# Climate ----
#Precipitation
#Tmax
#for only large trees

#new plots w/ LAT & LON
val_plt <- val_dset %>%
  ungroup() %>%
  dplyr::select(PLT_CN,LON,LAT) %>%
  distinct()

library(raster)
# Make lat, lon data spatial
val_plt_spat <- SpatialPointsDataFrame(coords = cbind(val_plt$LON, val_plt$LAT), 
                                        data = val_plt, 
                                        proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
#tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, val_plt_spat) # this step takes about 8 minutes each (laptop)
tmax.extr <- raster::extract(tmax, val_plt_spat)
#tmin.extr <- raster::extract(tmin, val_tree_spat)

# Jan 1999 - Dec 2019
ppt.extr <- ppt.extr[, 1249:1488] 
tmax.extr <- tmax.extr[, 1249:1488]
#tmin.extr <- tmin.extr[, 1249:1476]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmax.extr <- as.data.frame(tmax.extr)
#tmin.extr <- as.data.frame(tmin.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1249:1488] 
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmax.extr) <- paste0("tmax_", colNames)
#colnames(tmin.extr) <- paste0("tmin_", colNames)


# Export climate data
processed.path <- "./data/formatted/"
write.csv(ppt.extr, paste0(processed.path,"ppt_val_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_val_extr.csv"), row.names = F)
#write.csv(tmin.extr, paste0(processed.path,"tmin_val_extr.csv"), row.names = F)

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr <- read.csv(paste(processed.path,"ppt_val_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_val_extr.csv",sep=''), header = T)
#tmin.extr <- read.csv(paste(processed.path,"tmin_val_extr.csv",sep=''), header = T)

#for all plots
#same dataset LAT/LONG used to extract climate
val_plots <- unique(val_plt$PLT_CN)

clim_col <- function(PRISM,clim_var,PLT_CN){
  #make a list
  #each item is a year
  #each item has 1-13 columns (TRE_CN + 12 months)
  
  #ppt
  #empty list
  climate_list <- list()
  start_col <- 1 #Jan
  end_col <- 12 #Dec
  n <- ncol(PRISM)/12
  for(i in 1:n){ #number of years (1895:2000)
    climate_list[[i]] <- PRISM[,start_col:end_col]
    start_col <- start_col + 12
    end_col <- end_col + 12
  }
  
  prism_id <- colnames(ppt.extr)
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  #clim_var <- str_sub(prism_id,1,4) #1:5 for tmin_ and tmax_
  climate_list <- lapply(climate_list, setNames, nm = paste0(clim_var,months))
  
  #years <- seq(from=start_yr,to=end_yr,by=1)
  prism_yr <- unique(sub('\\D*(\\d{4}).*', '\\1', prism_id))
  names(climate_list) <- as.integer(prism_yr)
  
  climate_stack <- bind_rows(climate_list, .id = "Year")
  climate_stack$PLT_CN <- rep(PLT_CN,times=n)
  return(climate_stack)
}
#could make function for all climate variables
#find way to extract start_yr and end_yr
#lubridate?

val_ppt <- clim_col(ppt.extr,clim_var = "ppt_",PLT_CN = val_plots)
val_tmax <- clim_col(tmax.extr,clim_var = "tmax_",PLT_CN = val_plots)
#val_tmin <- clim_col(tmin.extr,clim_var = "tmin_",PLT_CN = val_plots)

val_clim <- full_join(val_ppt,val_tmax, by = c("PLT_CN","Year"))

val_clim$Year <- as.integer(val_clim$Year)

#seasonal calculations
val_clim <- val_clim %>%
  group_by(PLT_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6, #temp parameter for DF
         tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3, #temp parameter for PP
         tmax_pAug = lag(tmax_Aug)) #temp parameter for ES
save(val_clim,file = "./data/formatted/val_clim.Rdata")


# Forecast ----
library(tidyverse)

#first function for nonfocal trees
tree_val <- unique(val_dset$TRE_CN)
non_focal <- density_val %>%
  dplyr::select(PLT_CN,TRE_CN,SUBP,SPCD,DIA,TPA_UNADJ,DESIGNCD,MEASYEAR,fMEASYEAR,fDIA,STATUSCD,fSTATUSCD) %>%
  filter(!(TRE_CN %in% tree_val)) %>%
  filter(fSTATUSCD != 0) %>% # get rid of trees that are not included in remeasurement sample
  ungroup()
  
#add mortality year (estimated)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE <- dbReadTable(UT_FIA, 'TREE')
non_focal$fMORTYR <- TREE$MORTYR[match(non_focal$TRE_CN,TREE$PREV_TRE_CN)]
dbDisconnect(UT_FIA)

#explore
non_focal_stat <- non_focal %>%
  dplyr::select(STATUSCD,fSTATUSCD) %>%
  group_by(STATUSCD,fSTATUSCD) %>%
  tally()
# STATUSCD fSTATUSCD
#       1         0   #0 = no status; tree is not presently in sample
#       1         1   # keep; alive
#       1         2   # = dead
#       1         3   # = cut

# what about plots that have a lot of die off?
#change in BAL
non_foc_bal <- non_focal %>%
  group_by(PLT_CN) %>%
  mutate(BA1 = sum(((DIA^2) * 0.005454) * TPA_UNADJ),
         BA2 = sum(((fDIA[fSTATUSCD == 1]^2) * 0.005454) * TPA_UNADJ))

non_foc_hist <- non_foc_bal %>%
  select(PLT_CN, BA1, BA2) %>%
  distinct() %>%
  mutate(BA_diff = BA2 - BA1,
         BA_ratio = BA2/BA1)

hist(non_foc_hist$BA_diff,breaks = 50, xlab = "BA2 - BA1", main = "Difference in basal area")
hist(non_foc_hist$BA_ratio,breaks = 50, xlab = "BA2 / BA1", main = "Ratio of basal area")


#interpolate dbh
int_tre <- function(data){
  growth_df <- data %>%
    filter(!is.na(av_growth))
  for(i in 1:nrow(data)){
    Species <- data$SPCD[i]
    data$growth_adj[i] <- ifelse(is.na(data$av_growth[i]),
                                 round(mean(growth_df$av_growth[growth_df$SPCD == Species]),digits = 5),
                                 data$av_growth[i])
    tre_df <- data[i,] %>%
      slice(rep(1:n(), each = (census_int +1))) %>%
      mutate(Year =  ifelse(census_int == 0,
                            MEASYEAR,
                            ifelse(is.na(fMORTYR),
                                   c(MEASYEAR:fMEASYEAR),
                                   c(MEASYEAR:fMORTYR))),
             DIA_int = ifelse(DIA == fDIA,
                              DIA,
                              ifelse(census_int == 0,
                                     DIA,
                                     NA)))
    if(is.na(tre_df$DIA_int[1])){
      N <- which(tre_df$Year == tre_df$MEASYEAR[1])
      tre_df$DIA_int[N] <- tre_df$DIA[N] #dbh when year and measure year are equal
      Curr_row <- N+1 #each time through subtract 1 and move down one row
      while(Curr_row <= (data$census_int[i] +1)){
        DIA_1 <- tre_df$DIA_int[Curr_row-1]
        tre_df$DIA_int[Curr_row] <- DIA_1 + tre_df$growth_adj[Curr_row]
        #continue loop for next row until curr_row>0
        Curr_row = Curr_row + 1 
      }
    }
    data <- full_join(data,tre_df)
  }
  data <- data %>%
    filter(!is.na(Year))
  return(data)
}

non_focal <- non_focal %>%
  mutate(census_int = ifelse(is.na(fMORTYR),
                             fMEASYEAR - MEASYEAR,
                             fMORTYR - MEASYEAR),
         av_growth = ifelse(census_int == 0,
                            0,
                            #change in DBH per year
                            (fDIA-DIA)/census_int)) # some will be NA where fDIA is NA and some will be negative where fDIA<DIA

non_focal_exp <- int_tre(data = non_focal)
save(non_focal_exp, file =  "./data/formatted/non_focal_exp.Rdata")
  
#check
summary(non_focal_exp) #NA, Inf?

#function
#insert model object, focal trees, nonfocal trees, climate (default null)
#N is the row where measure year and ring width year are the same

#standardize new data variables
# get mean and standard deviation from training set (tree ring data set) for each variable
#variables: SICOND, tASPECT, SLOPE, BAL, SDI, CR_weib, PCCF, CCF, cos, sin, solrad_MayAug
# precip: ppt_pJunSep
# temp: different for each species
get_stats <- function(x) c(mean=mean(x), sd=sd(x))
df_stats <- glmm_df_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_FebJul) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
pp_stats <- glmm_pp_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_JunAug) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
es_stats <- glmm_es_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_pAug) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
sp_stats <- list(sp_202 = df_stats, sp_122 = pp_stats,sp_93 = es_stats)
#use to standardize newdata covariates


val_an <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_fvs_df) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    mutate(census_int = fMEASYEAR - MEASYEAR) %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_fvs = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_fvs, where year = measyear
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
      newdata_rep$CR_fvs[i] <- newdata$CR[newdata$TRE_CN == TRE_CN]
    }
  }
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_fvs_df - dataframe of crown ratio constants for equation
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #cr
      for(i in 1:nrow(plt_yr2_df)){
        #Function arguments:
        #SPCD - is number code of species of tree record
        #SDI - is SDI of stand (Stage 1968)
        Species <- plt_yr2_df$SPCD[i]
        #SDI max values for each species were pulled from UT Variant overview
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDIMAX_RMRS[i]),
                         CR_fvs_df$SDIMAX[CR_fvs_df$species == Species],
                         plt_yr2_df$SDIMAX_RMRS[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
        #Calculate average stand crown ratio (ACR) for each species in the stand
        d0 <- CR_fvs_df$d0[CR_fvs_df$species == Species]
        d1 <- CR_fvs_df$d1[CR_fvs_df$species == Species]
        ACR <- d0 + d1 * RD * 100
        
        #Parameters of Weibull distribution: A,B,C
        a0 <- CR_fvs_df$a0[CR_fvs_df$species == Species]
        b0 <- CR_fvs_df$b0[CR_fvs_df$species == Species]
        b1 <- CR_fvs_df$b1[CR_fvs_df$species == Species]
        c0 <- CR_fvs_df$c0[CR_fvs_df$species == Species]
        c1 <- CR_fvs_df$c1[CR_fvs_df$species == Species]
        
        #A parameter
        WEIBA <-a0
        #B parameter
        WEIBB <- b0 + b1*ACR
        #C parameter
        WEIBC <- c0 + c1*ACR
        
        #Function arguments:
        
        #CCF - crown competition factor of stand
        #rank_pltyr - tree's rank in diameter distribution by plot by year
        #N  - number of records in the stand by year
        
        #Calculate scale parameter
        SCALE = (1.0 - .00167 * (plt_yr2_df$CCF[i]-100.0))
        if(SCALE < 0.3){SCALE = 0.3}
        if(SCALE > 1.0){SCALE = 1.0}
        
        N <- plt_yr2_df$num_t[i]
        #X is tree's rank in diameter distribution
        #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
        Y <- plt_yr2_df$rank_pltyr[i]/N * SCALE
        if(Y < 0.05){Y = 0.05}
        if(Y > 0.95){Y = 0.95}
        #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
        
        #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
        X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
        #X = a tree’s crown ratio expressed as a percent / 10
        CR_fvs <- X * 10
        
        #get CR the year before
        TRE_CN <- plt_yr2_df$TRE_CN[i]
        #growthyr is still previous year
        CR_1 <- plt_df$CR_fvs[plt_df$TRE_CN == TRE_CN & 
                                plt_df$Year == growthyr] #or CR_fvs[N] for the first round
        #bound to 1% change per year
        cr_bound1 <- CR_1 * .01
        plt_yr2_df$CR_fvs[i] <- ifelse(CR_1 > CR_fvs, 
                                       CR_1 - cr_bound1,
                                       CR_1 + cr_bound1)
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CR_fvs[i] <- plt_yr2_df$CR_fvs[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}

val_clim <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_fvs_df,climate) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    mutate(census_int = fMEASYEAR - MEASYEAR) %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_fvs = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_fvs, where year = measyear
  #fill climate
  climate <- climate %>%
    ungroup()
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
      newdata_rep$CR_fvs[i] <- newdata$CR[newdata$TRE_CN == TRE_CN]
    }
    #add climate
    #match over PLT and year
    PLT_CN <- newdata_rep$PLT_CN[i]
    newdata_rep$ppt_pJunSep[i] <- climate$ppt_pJunSep[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_JunAug[i] <- climate$tmax_JunAug[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_FebJul[i] <- climate$tmax_FebJul[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_pAug[i] <- climate$tmax_pAug[climate$PLT_CN == PLT_CN &
                                                    climate$Year == newdata_rep$Year[i]]
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_fvs_df - dataframe of crown ratio constants for equation
  #climate - dataframe of climate variables (ppt & tmax) for each tree
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        plt_yr_df$z.ppt_pJunSep[i] = unlist((plt_yr_df[i,"ppt_pJunSep"] - 
                                               para_std[1,"ppt_pJunSep"]) / para_std[2,"ppt_pJunSep"])
        #temperature different for each species
        sp_202 <- sp_stats$sp_202
        sp_122 <- sp_stats$sp_122
        sp_93 <- sp_stats$sp_93
        plt_yr_df$z.tmax_FebJul[i] = unlist((plt_yr_df[i,"tmax_FebJul"] -
                                               sp_202[1,"tmax_FebJul"]) /
                                              sp_202[2,"tmax_FebJul"])
        plt_yr_df$z.tmax_JunAug[i] = unlist((plt_yr_df[i,"tmax_JunAug"] -
                                               sp_122[1,"tmax_JunAug"]) /
                                              sp_122[2,"tmax_JunAug"])
        plt_yr_df$z.tmax_pAug[i] = unlist((plt_yr_df[i,"tmax_pAug"] -
                                             sp_93[1,"tmax_pAug"]) / 
                                            sp_93[2,"tmax_pAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #cr
      for(i in 1:nrow(plt_yr2_df)){
        #Function arguments:
        #SPCD - is number code of species of tree record
        #SDI - is SDI of stand (Stage 1968)
        Species <- plt_yr2_df$SPCD[i]
        #SDI max values for each species were pulled from UT Variant overview
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDIMAX_RMRS[i]),
                         CR_fvs_df$SDIMAX[CR_fvs_df$species == Species],
                         plt_yr2_df$SDIMAX_RMRS[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
        #Calculate average stand crown ratio (ACR) for each species in the stand
        d0 <- CR_fvs_df$d0[CR_fvs_df$species == Species]
        d1 <- CR_fvs_df$d1[CR_fvs_df$species == Species]
        ACR <- d0 + d1 * RD * 100
        
        #Parameters of Weibull distribution: A,B,C
        a0 <- CR_fvs_df$a0[CR_fvs_df$species == Species]
        b0 <- CR_fvs_df$b0[CR_fvs_df$species == Species]
        b1 <- CR_fvs_df$b1[CR_fvs_df$species == Species]
        c0 <- CR_fvs_df$c0[CR_fvs_df$species == Species]
        c1 <- CR_fvs_df$c1[CR_fvs_df$species == Species]
        
        #A parameter
        WEIBA <-a0
        #B parameter
        WEIBB <- b0 + b1*ACR
        #C parameter
        WEIBC <- c0 + c1*ACR
        
        #Function arguments:
        
        #CCF - crown competition factor of stand
        #rank_pltyr - tree's rank in diameter distribution by plot by year
        #N  - number of records in the stand by year
        
        #Calculate scale parameter
        SCALE = (1.0 - .00167 * (plt_yr2_df$CCF[i]-100.0))
        if(SCALE < 0.3){SCALE = 0.3}
        if(SCALE > 1.0){SCALE = 1.0}
        
        N <- plt_yr2_df$num_t[i]
        #X is tree's rank in diameter distribution
        #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
        Y <- plt_yr2_df$rank_pltyr[i]/N * SCALE
        if(Y < 0.05){Y = 0.05}
        if(Y > 0.95){Y = 0.95}
        #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
        
        #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
        X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
        #X = a tree’s crown ratio expressed as a percent / 10
        CR_fvs <- X * 10
        
        #get CR the year before
        TRE_CN <- plt_yr2_df$TRE_CN[i]
        #growthyr is still previous year
        CR_1 <- plt_df$CR_fvs[plt_df$TRE_CN == TRE_CN & 
                                plt_df$Year == growthyr] #or CR_fvs[N] for the first round
        #bound to 1% change per year
        cr_bound1 <- CR_1 * .01
        plt_yr2_df$CR_fvs[i] <- ifelse(CR_1 > CR_fvs, 
                                       CR_1 - cr_bound1,
                                       CR_1 + cr_bound1)
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CR_fvs[i] <- plt_yr2_df$CR_fvs[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}


## CR constant

val_an_cr <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_fvs_df) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, tCONDID, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA,
                  CR_fvs) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_fvs, where year = measyear
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
    }
  }
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_fvs_df - dataframe of crown ratio constants for equation
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}


val_clim_cr <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,climate) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    mutate(census_int = fMEASYEAR - MEASYEAR) %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA,
                  CR) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_fvs, where year = measyear
  #fill climate
  climate <- climate %>%
    ungroup()
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
    }
    #add climate
    PLT_CN <- newdata_rep$PLT_CN[i]
    #match over tree and year
    newdata_rep$ppt_pJunSep[i] <- climate$ppt_pJunSep[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_JunAug[i] <- climate$tmax_JunAug[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_FebJul[i] <- climate$tmax_FebJul[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_pAug[i] <- climate$tmax_pAug[climate$PLT_CN == PLT_CN &
                                                    climate$Year == newdata_rep$Year[i]]
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_fvs_df - dataframe of crown ratio constants for equation
  #climate - dataframe of climate variables (ppt & tmax) for each tree
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR[i] = unlist((plt_yr_df[i,"CR"] - para_std[1,"CR"]) / para_std[2,"CR"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        plt_yr_df$z.ppt_pJunSep[i] = unlist((plt_yr_df[i,"ppt_pJunSep"] - 
                                               para_std[1,"ppt_pJunSep"]) / para_std[2,"ppt_pJunSep"])
        #temperature different for each species
        sp_202 <- sp_stats$sp_202
        sp_122 <- sp_stats$sp_122
        sp_93 <- sp_stats$sp_93
        plt_yr_df$z.tmax_FebJul[i] = unlist((plt_yr_df[i,"tmax_FebJul"] -
                                               sp_202[1,"tmax_FebJul"]) /
                                              sp_202[2,"tmax_FebJul"])
        plt_yr_df$z.tmax_JunAug[i] = unlist((plt_yr_df[i,"tmax_JunAug"] -
                                               sp_122[1,"tmax_JunAug"]) /
                                              sp_122[2,"tmax_JunAug"])
        plt_yr_df$z.tmax_pAug[i] = unlist((plt_yr_df[i,"tmax_pAug"] -
                                             sp_93[1,"tmax_pAug"]) / 
                                            sp_93[2,"tmax_pAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}


## Forest code
val_clim_fc <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,climate) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    mutate(census_int = fMEASYEAR - MEASYEAR) %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA,
                  CR) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_fvs, where year = measyear
  #fill climate
  climate <- climate %>%
    ungroup()
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
    }
    #add climate
    PLT_CN <- newdata_rep$PLT_CN[i]
    #match over tree and year
    newdata_rep$ppt_pJunSep[i] <- climate$ppt_pJunSep[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_JunAug[i] <- climate$tmax_JunAug[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_FebJul[i] <- climate$tmax_FebJul[climate$PLT_CN == PLT_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_pAug[i] <- climate$tmax_pAug[climate$PLT_CN == PLT_CN &
                                                    climate$Year == newdata_rep$Year[i]]
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_fvs_df - dataframe of crown ratio constants for equation
  #climate - dataframe of climate variables (ppt & tmax) for each tree
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR[i] = unlist((plt_yr_df[i,"CR"] - para_std[1,"CR"]) / para_std[2,"CR"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        plt_yr_df$z.ppt_pJunSep[i] = unlist((plt_yr_df[i,"ppt_pJunSep"] - 
                                               para_std[1,"ppt_pJunSep"]) / para_std[2,"ppt_pJunSep"])
        #temperature different for each species
        sp_202 <- sp_stats$sp_202
        sp_122 <- sp_stats$sp_122
        sp_93 <- sp_stats$sp_93
        plt_yr_df$z.tmax_FebJul[i] = unlist((plt_yr_df[i,"tmax_FebJul"] -
                                               sp_202[1,"tmax_FebJul"]) /
                                              sp_202[2,"tmax_FebJul"])
        plt_yr_df$z.tmax_JunAug[i] = unlist((plt_yr_df[i,"tmax_JunAug"] -
                                               sp_122[1,"tmax_JunAug"]) /
                                              sp_122[2,"tmax_JunAug"])
        plt_yr_df$z.tmax_pAug[i] = unlist((plt_yr_df[i,"tmax_pAug"] -
                                             sp_93[1,"tmax_pAug"]) / 
                                            sp_93[2,"tmax_pAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = ~(1+z.|FVS_LOC_CD), type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}


# Predict -----

#load validation trees
#load(file = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/data/formatted/val_dset.Rdata")
#load other trees on plot
#load(file = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/data/formatted/non_focal_exp.Rdata")

#Predict growth using annualized with tree rings only models for each species.
pred_an_full <- val_an(newdata = val_dset,mod_df = an_al_df, mod_pp = an_al_pp, 
                       mod_es = an_al_es,sp_stats = sp_stats,nonfocal = non_focal_exp,
                       bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df)

pred_an <- val_an(newdata = val_dset,mod_df = an_red_df, mod_pp = an_red_pp, 
                  mod_es = an_red_es,sp_stats = sp_stats,nonfocal = non_focal,
                  bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df)


pred_clim_full <- val_an_clim(newdata = val_dset,mod_df = clim_al_df, mod_pp = clim_al_pp, 
                              mod_es = clim_al_es,sp_stats = sp_stats,nonfocal = non_focal_exp,
                              bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, climate = clim_val)

#reduced climate models
pred_clim <- val_clim(newdata = val_dset,mod_df = clim_red_df, mod_pp = clim_red_pp, 
                      mod_es = clim_red_es,sp_stats = sp_stats,nonfocal = non_focal_exp,
                      bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, climate = clim_val)

pred_clim2 <- val_clim(newdata = val_dset,mod_df = clim_red2_df, mod_pp = clim_red2_pp, 
                       mod_es = clim_red_es,sp_stats = sp_stats,nonfocal = non_focal_exp,
                       bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, climate = clim_val)


## Predict CR

pred_fan_cr <- val_an_cr(newdata = val_dset,mod_df = an_al_df, mod_pp = an_al_pp, 
                         mod_es = an_al_es,sp_stats = sp_stats,nonfocal = non_focal,
                         bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df)

pred_an_cr <- val_an_cr(newdata = val_dset,mod_df = an_red_df, mod_pp = an_red_pp, 
                        mod_es = an_red_es,sp_stats = sp_stats,nonfocal = non_focal,
                        bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df)

pred_fclim_cr <- val_clim_cr(newdata = val_dset,mod_df = clim_al_df, mod_pp = clim_al_pp, 
                             mod_es = clim_al_es,sp_stats = sp_stats,nonfocal = non_focal,
                             bratio = bratio_df ,ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, climate = val_clim)

pred_clim_cr <- val_clim_cr(newdata = val_dset,mod_df = clim_mod_df, mod_pp = clim_mod_pp, 
                            mod_es = clim_mod_es,sp_stats = sp_stats,nonfocal = non_focal_exp,
                            bratio = bratio_df ,ccf_df = ccf_df, climate = clim_val)

#Calculate Crown Ratio from annualized DBH
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-25-19

#Calculations modified from Mark Castle
#email: markcastle@fs.fed.us

#load
load('./data/formatted/density_data')

#First need to calcuate SDI
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)

density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(SDI = sum(TPA_C*(DIA_C/10)^1.6)) 

#number of trees on plot for a given year
density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(num_t = length(unique(TRE_CN)))

save(density_data, file = "./data/formatted/density_data.Rdata")

#done with nonfocal trees on plot
#CR computationally expensive so fitler out trees I don't need (miss_data)
length(unique(incr_imputed$TRE_CN))
#[1] 568
incr_tre <- unique(incr_imputed$TRE_CN)
incr_calcov <- density_data %>%
  filter(TRE_CN %in% incr_tre)
length(unique(incr_calcov$TRE_CN))
#568

save(incr_calcov,file = "./data/formatted/incr_calcov.Rdata")

##UT variant guide instructions below:

#dbh>1 (excludes common pinyon)
#average stand crown ratio estimated from stand density
#ACR = do + d1 * RELSDI * 100 , where RELSDI = SDIstand/SDImax

#weibull parameters estimated from ACR
#A = ao
#B = bo + b1 * ACR (B>=1)
#C = co + c1 * ACR (C>=2)

#Weibull distribution
#Y = 1-exp(-((X-A)/B))^C

#Scale = 1 - 0.00167 * (CCF - 100)

#Original code by Mark Castle
#markcastle@fs.fed.us
#Refer to UT variant overview as you go through the steps in this script

#Read in data
load('./data/formatted/incr_calcov')

#####################################
#Step 1:
#Calculate stand density index (RSDI)
#and crown competition factor (CCF)
#####################################

#Instead of Reineke SDI (for stand) use
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)

#Crown Competition Factor (CCF - for stand)
#Refer to variant overview for how to calculate CCF
#calculated in CCF.R script

#####################################
#Step 2:
#Calculate relative density (RD)
#Step 3:
#Calculate average stand crown ratio
#by species (ACR)
#Step 4:
#Determine Weibull parameters to use
#for crown ratio calculation
#Step 5:
#Calculate crown ratio for tree
#records
#####################################

#Function for calculating relative density for each species in a stand
#Maximum stand density index at stand level is weighted average,
#by basal area proportion of individual species SDI max
##for now use individual SDI max
##from UT variant guide

CR_WEIB_df <- data.frame(species=c(93,202,122),#,15,19,96,108,113,475,746
                         #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,66=RM,96=BS,106=PI,108=LP,113=LM,133=PM,321=OH,475=MC,746=AS,814=GO,102=bristlecone pine
                         SDIMAX = c(620,570,446),#,560,625,625,540,400,100,450
                         a0 = c(1,1,1),#,1,1,1,0,1,0,0
                         b0 = c(-0.90648,-0.24217,-0.82631),#,-0.89553,-0.89553,-0.90648,0.17162,-0.82631,-0.23830,-0.08414
                         b1 = c(1.08122,0.96529,1.06217),#,1.07728,1.07728,1.08122,1.07338,1.06217,1.18016,1.14765
                         c0 = c(3.48889,-7.94832,-1.02873),#,1.74621,1.74621,3.48889,3.15000,3.31429,3.04000,2.7750
                         c1 = c(0,1.93832,0.80143),#,0.29052,0.29052,0,0,0,0,0
                         d0 = c(6.81087,7.46296,6.19911),#,7.65751,7.65751,6.81087,6.00567,6.19911,4.62512,4.01678
                         d1 = c(-0.01037,-0.02944,-0.02216))#,-0.03513,-0.03513,-0.01037,-0.03520,-0.02216,-0.01604,-0.01516
#can add other species if needed

#some species use height
#PI,WJ,GO,PM,UJ,OH
#CL = lm(HT)
#CR = CL/HT

CR_weib <- vector(mode="numeric", length=nrow(incr_calcov))
for(i in 1:nrow(incr_calcov)){
  #SPCD - is number code of species of tree record
  #SDI - is SDI of stand (Stage 1968)
  Species <- incr_calcov$SPCD[i]
  if(Species %in% CR_WEIB_df$species){
    #SDI max values for each species were pulled from UT Variant overview
    SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species]
    #Calculate relative density
    RD <- incr_calcov$SDI[i]/SDIMAX
    
    #Calculate average stand crown ratio (ACR) for each species in the stand
    d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
    d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
    ACR <- d0 + d1 * RD * 100
    
    #Parameters of Weibull distribution: A,B,C
    a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
    b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
    b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
    c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
    c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
    
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
    SCALE = (1.0 - .00167 * (incr_calcov$CCF[i]-100.0))
    if(SCALE < 0.3){SCALE = 0.3}
    if(SCALE > 1.0){SCALE = 1.0}
    
    N <- incr_calcov$num_t[i]
    #X is tree's rank in diameter distribution
    #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
    Y <- incr_calcov$rank_pltyr[i]/N * SCALE
    if(Y < 0.05){Y = 0.05}
    if(Y > 0.95){Y = 0.95}
    #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
    
    #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
    X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
    #X = a tree’s crown ratio expressed as a percent / 10
    incr_calcov$CR_weib[i] <- X * 10
  }
  if(!(Species %in% CR_WEIB_df$species)){
    incr_calcov$CR_weib[i] <- NA
  }
}

#fine but I don't trust it - a lot of 0.05 and .95 values
hist(incr_calcov$CR_weib)

save(incr_calcov,file = "./data/formatted/incr_calcov.Rdata")


#Subset out live trees with DBH >= 1 to check crown ratio values predicted by weibull function
cr_check<- incr_calcov %>%
  select(TRE_CN,Year,MEASYEAR,PLT_CN,SPCD,CCF,CR,CR_weib) %>%
  filter(MEASYEAR == Year)

#Also FVS does have logic for adjusting a tree's crown ratio for top-kill 
#not included

#Validation ----
#use validation trees (2000 and after)
#which have measured uncompacted crown ratio (UNCRCD)

load(file = './data/formatted/val_dset.Rdata')

#get trees on same plot
plot_crtest <- unique(val_dset$PLT_CN)
tree_crtest <- unique(val_dset$TRE_CN)
plot_data_cr <- tree[(tree$PLT_CN %in% plot_crtest),c("CN","PLT_CN","SUBP","SPCD","DIA","TPA_UNADJ","UNCRCD")]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including validation trees
colnames(plot_data_cr)[colnames(plot_data_cr)=="CN"] <- "TRE_CN"

plot_data_cr$MEASYEAR <- plot$MEASYEAR[match(plot_data_cr$PLT_CN, plot$CN)]
plot_data_cr$DESIGNCD <- plot$DESIGNCD[match(plot_data_cr$PLT_CN, plot$CN)]
plot_data_cr$CONDID <- cond$CONDID[match(plot_data_cr$PLT_CN, cond$PLT_CN)]
plot_data_cr$SDI <- cond$SDI_RMRS[match(plot_data_cr$PLT_CN, cond$PLT_CN)]
plot_data_cr$SDImax <- cond$SDIMAX_RMRS[match(plot_data_cr$PLT_CN, cond$PLT_CN)]

#check
length(plot_crtest) #85
length(unique(plot_data_cr$PLT_CN)) #85

#First need to calcuate SDI
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)
#then number of trees on a plot
#rank in the diameter distribution
plot_data_cr <- plot_data_cr %>%
  group_by(PLT_CN) %>%
  mutate(stage_sum = TPA_UNADJ*(DIA/10)^1.6,
         SDI_stage = sum(stage_sum),
         num_t = length(unique(TRE_CN)),
         rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min"))

#CCF
#see CCF.R

for(i in 1:nrow(plot_data_cr)){
  Species <- plot_data_cr$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(plot_data_cr$DIA[i] < dbrk,
           CCF_t <- plot_data_cr$DIA[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * plot_data_cr$DIA[i]) + (r3 * plot_data_cr$DIA[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(plot_data_cr$DIA[i]), 
           CCF_t <- NA,
           ifelse(plot_data_cr$DIA[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(plot_data_cr$DIA[i] < dbrk, 
                         CCF_t <- r4 * (plot_data_cr$DIA[i]^r5),
                         CCF_t <- r1 + (r2 * plot_data_cr$DIA[i]) + (r3 * plot_data_cr$DIA[i]^2))))
  }
  plot_data_cr$CCF_t[i] <- CCF_t
}

#crown competition factor on a stand
plot_data_cr <- plot_data_cr %>%
  group_by(PLT_CN) %>%
  mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))

save(plot_data_cr,file = './data/formatted/plot_data_cr.Rdata')
write.csv(plot_data_cr, file = './data/formatted/plot_data_cr.csv')

  #CR
#first filter for focal species
length(tree_crtest) #905
valcr_check <- plot_data_cr %>%
  filter(TRE_CN %in% tree_crtest)

for(i in 1:nrow(valcr_check)){
  #Function arguments:
  #SPCD - is number code of species of tree record
  #SDI - is SDI of stand (Stage 1968)
  Species <- valcr_check$SPCD[i]
  if(Species %in% CR_WEIB_df$species){
    #SDI max values for each species were pulled from UT Variant overview
    ifelse(is.na(valcr_check$SDImax[i]),
           SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
           SDIMAX <- valcr_check$SDImax[i])
    #Calculate relative density
    ifelse(is.na(valcr_check$SDI[i]),
           SDI <- valcr_check$SDI_stage[i],
           SDI <- valcr_check$SDI[i])
    RD <- SDI/SDIMAX
    
    #Calculate average stand crown ratio (ACR) for each species in the stand
    d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
    d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
    ACR <- d0 + d1 * RD * 100
    
    #Parameters of Weibull distribution: A,B,C
    a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
    b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
    b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
    c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
    c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
    
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
    SCALE = (1.0 - .00167 * (valcr_check$CCF[i]-100.0))
    if(SCALE < 0.3){SCALE = 0.3}
    if(SCALE > 1.0){SCALE = 1.0}
    
    N <- valcr_check$num_t[i]
    #X is tree's rank in diameter distribution
    #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
    Y <- valcr_check$rank_pltyr[i]/N * SCALE
    if(Y < 0.05){Y = 0.05}
    if(Y > 0.95){Y = 0.95}
    #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
    
    #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
    X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
    #X = a tree’s crown ratio expressed as a percent / 10
    valcr_check$CR_weib[i] <- X * 10
  }
  if(!(Species %in% CR_WEIB_df$species)){
    valcr_check$CR_weib[i] <- NA
  }
}

save(valcr_check,file = './data/formatted/valcr_check.Rdata')
write.csv(valcr_check, file = './data/formatted/valcr_check.csv')

#
cr_reg <- lm(CR_weib~UNCRCD,data = valcr_check)
summary(cr_reg)
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 55.77157    2.12068  26.299  < 2e-16 ***
#UNCRCD       0.07914    0.02568   3.081  0.00212 **  
plot(valcr_check$UNCRCD,valcr_check$CR_weib)


#FVS check ----

#original code Mark Castle
library(RSQLite)
library(tidyverse)
#create FVS-ready database for calibration data set

#Read in CSV containing validation plots and years
load("./data/formatted/data_all.Rdata")
cal_plt <- data_all %>%
  ungroup() %>%
  filter(SPCD %in% c(93,122,202)) %>%
  dplyr::select(PLT_CN,MEASYEAR) %>%
  distinct()


#Remove row names column
cal_plt$X<-NULL;head(cal_plt)

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

#Rename  PLT_CN header in cal_plt
names(cal_plt)[names(cal_plt)=="PLT_CN"]<-"STAND_CN";head(cal_plt)

#Subset FIA utah data based on the plots in cal_plt
fvsPlotInitPlot<-fvsPlotInitPlot[fvsPlotInitPlot$STAND_CN %in% cal_plt$STAND_CN,]
fvsStandInitPlot<-fvsStandInitPlot[fvsStandInitPlot$STAND_CN %in% cal_plt$STAND_CN,]
fvsTreeInitPlot<-fvsTreeInitPlot[fvsTreeInitPlot$STAND_CN %in% cal_plt$STAND_CN,]

#Merge inventory years to FVSPlotInitPlot and FVSStandInitPlot
fvsPlotInitPlot<-merge(fvsPlotInitPlot, cal_plt, by="STAND_CN", all.x = T);head(fvsPlotInitPlot)
fvsStandInitPlot<-merge(fvsStandInitPlot, cal_plt, by="STAND_CN", all.x = T);head(fvsStandInitPlot)

#Create group label based on inventory years
fvsPlotInitPlot$NewGroup<-fvsPlotInitPlot$MEASYEAR;head(fvsPlotInitPlot)
fvsStandInitPlot$NewGroup<-fvsStandInitPlot$MEASYEAR;head(fvsStandInitPlot)

#Add NewGroup to GROUPS column
fvsPlotInitPlot$GROUPS<-paste(fvsPlotInitPlot$GROUPS, fvsPlotInitPlot$NewGroup, sep = " ");head(fvsPlotInitPlot$GROUPS)
fvsStandInitPlot$GROUPS<-paste(fvsStandInitPlot$GROUPS, fvsStandInitPlot$NewGroup, sep = " ");head(fvsStandInitPlot$GROUPS)

#Create new UT DB
conn <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_cal.db")

#Write each of the neccesary FVS tables to DB
dbWriteTable(conn, "FVS_GROUPADDFILESANDKEYWORDS", fvsAddKey)
dbWriteTable(conn, "FVS_PLOTINIT_PLOT", fvsPlotInitPlot)
dbWriteTable(conn, "FVS_STANDINIT_COND", fvsStandInitCond)
dbWriteTable(conn, "FVS_STANDINIT_PLOT", fvsStandInitPlot)
dbWriteTable(conn, "FVS_TREEINIT_COND", fvsTreeInitCond)
dbWriteTable(conn, "FVS_TREEINIT_PLOT", fvsTreeInitPlot)

cal_treelist <- read_csv(file = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/data/raw/FVS/fvs_cal.csv")
length(unique(cal_treelist$StandID)) #250
cal_treelist$SpeciesFIA <- as.numeric(cal_treelist$SpeciesFIA)
cal_red <- cal_treelist %>%
  select(StandID,Year,PrdLen,TreeId,TreeIndex,SpeciesFIA,DBH,DG,PctCr,PtBAL,ActPt) %>%
  filter(SpeciesFIA %in% c(93,122,202)) %>%
  filter(DBH >= 1)
length(unique(cal_red$StandID)) #248

library(dbplyr)
library(RSQLite)
cal_fvsred_db <- dbConnect(RSQLite::SQLite(), "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/data/raw/FVS/FVS_cal.db")
#Extract FVS_StandInit_Plot table
fvsStandInitPlot<-dbReadTable(cal_fvsred_db, 'FVS_STANDINIT_PLOT');head(fvsStandInitPlot)
std2plt <- fvsStandInitPlot %>%
  select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID)) #63

cal_red$STAND_CN <- std2plt$STAND_CN[match(cal_red$StandID,std2plt$STAND_ID)]
length(unique(cal_red$STAND_CN)) #248

cal_measyr <- cal_red %>%
  filter(DG == 0) %>%
  select(-DG)
length(unique(cal_measyr$STAND_CN)) #247
colnames(cal_measyr)[colnames(cal_measyr)=="Year"] <- "MEASYEAR"
colnames(cal_measyr)[colnames(cal_measyr)=="SpeciesFIA"] <- "SPCD"
colnames(cal_measyr)[colnames(cal_measyr)=="ActPt"] <- "SUBP"
cal_measyr$PLT_CN <- as.numeric(cal_measyr$STAND_CN)

UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/data/raw/FIADB.db")
TREE <- dbReadTable(UT_FIA, 'TREE')
TREE_red <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, SPCD, SUBP, TREE, DIA, CR FROM TREE")) %>%
  collect()
colnames(TREE_red)[colnames(TREE_red)=="TREE"] <- "TreeId"
colnames(TREE_red)[colnames(TREE_red)=="PLT_CN"] <- "STAND_CN"
TREE_red$TRE_CN <- as.numeric(TREE_red$CN)

cal_measyr <- left_join(cal_measyr,TREE_red)

load("./data/formatted/data_all.Rdata")
data_sp <- data_all %>%
  ungroup() %>%
  filter(SPCD %in% c(93,122,202)) %>%
  filter(Year == MEASYEAR) %>%
  select(PLT_CN,TRE_CN,SPCD,SUBP_t,MEASYEAR,DIA_C,CR_weib)

colnames(data_sp)[colnames(data_sp)=="SUBP_t"] <- "SUBP"
colnames(data_sp)[colnames(data_sp)=="DIA_C"] <- "DBH"

cr_check <- left_join(data_sp,cal_measyr, by = c("PLT_CN","TRE_CN", "DBH"))
length(unique(fvs_check$CN)) #999
#filter for trees that are alive at remeasurement
fvs_check <- fvs_check %>%
  filter(fSTATUSCD == 1)
length(unique(fvs_check$CN)) #785
fvs_check_red <- fvs_check %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% val_dset$TRE_CN)
length(unique(fvs_check_red$CN))
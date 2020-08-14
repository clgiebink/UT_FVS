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
#SDI = sum(TPA * (DIA_t/10)^1.6)
#FVS uses stage method

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

#Weibull bound ----

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

#Step 1:
#Calculate stand density index (RSDI)
#and crown competition factor (CCF)

#Instead of Reineke SDI (for stand) use
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum(TPA*(DIA_t/10)^1.6)

#Crown Competition Factor (CCF - for stand)
#Refer to variant overview for how to calculate CCF
#calculated in CCF.R script

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
      
      #SDI - is SDI of stand (Stage 1968)
      #SDI max values for each species were pulled from UT Variant overview
      SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species]
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

incr_calcov <- crw_bound(data = incr_calcov, CR_WEIB_df)
#Also FVS does have logic for adjusting a tree's crown ratio for top-kill 
#not included
save(incr_calcov,file = "./data/formatted/incr_calcov.Rdata")


#Calculate CCF from annualized DBH
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-24-19

#load data
load(file = "./data/formatted/density_data")

#trees w/o increments on plots have same DESIGNCD
unique(miss_data$DESIGNCD)
#410 424 423 425

subp_exam <- density_data %>%
  group_by(PLT_CN,DESIGNCD) %>%
  summarise(subp_des <- length(unique(SUBP_t)))

#expansion factor (TPA) is based on DIA in variale radius plots
#410 is 40 BAF variable radius
#TPA = (BAF/0.005454*DIA^2)/N
#if DIA_C timber < 5 and woodland < 3 inches measured on microplots; TPA is 60
#constant if fixed radius plot (423,424,425)

density_data <- density_data %>%
  mutate(TPA_C = ifelse(DESIGNCD %in% c(423,424,425),
                        TPA_UNADJ,
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

#empty vector to hold calculated crown competition factor
CCF_t <- vector(mode="numeric", length=nrow(density_data))
for(i in 1:nrow(density_data)){
  Species <- density_data$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(density_data$DIA_C[i] < dbrk,
           CCF_t <- density_data$DIA_C[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * density_data$DIA_C[i]) + (r3 * density_data$DIA_C[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(density_data$DIA_C[i]), 
           CCF_t <- NA,
           ifelse(density_data$DIA_C[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(density_data$DIA_C[i] < dbrk, 
                         CCF_t <- r4 * (density_data$DIA_C[i]^r5),
                         CCF_t <- r1 + (r2 * density_data$DIA_C[i]) + (r3 * density_data$DIA_C[i]^2))))
  }
  density_data$CCF_t[i] <- CCF_t
}

#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots
#4 subplots for DESIGNCD 423, 424, 425; 4 microplots
#5 subplots for DESIGNCD 410; 4 microplots 
density_data <- density_data %>%
  group_by(DESIGNCD) %>%
  mutate(SUBP_N = ifelse(TPA_C %in% c(75,60), 4, max(SUBP_t))) 
#DIA timber < 5 and woodland < 3 inches measured on microplots; TPA is 75

density_data <- density_data %>%
  group_by(PLT_CN,SUBP_t,Year) %>%
  mutate(PCCF = sum(CCF_t * (TPA_C * SUBP_N), na.rm = TRUE))

length(unique(density_data$PLT_CN)) #435

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
density_data <- density_data %>%
  group_by(PLT_CN,Year) %>%
  mutate(CCF = sum(CCF_t * TPA_C,na.rm = TRUE)) #%>%
  #mutate(CCF = ifelse(CCF == 0, NA, CCF))


save(density_data,file = "./data/formatted/density_data")

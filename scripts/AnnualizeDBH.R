#Annualization of DBH
#Courtney Giebink: clgiebink@email.arizona.edu
#4-17-19
#with Jeff Oliver: jcoliver@email.arizona.edu 

#load data
load("./data/formatted/incr_comb.Rdata")

#dataframe of coefficients for bark ratio calculation
#from Utah variant guide
bratio_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                        #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                        b1 = c(0.9502,0.867,0.8967,0.890,0.890,0.9002,0.9502,0.9002,0.9625,0.9002,0.93789),
                        b2 = c(-0.2528, 0, -0.4448,0,0,-0.3089,-0.2528,-0.3089,-0.1141,-0.3089,-0.24096),
                        exp = c(1,0,1,0,0,1,1,1,1,1,1)) #can add more species later 

#annualized DBH
#DBH0 = DBH - k * DG , where k = 1/BRATIO and DG = 2 * RW  
#DG = periodic increment in inside bark diameter 
#function to annualize, or back calculate dbh using diameter increment data (2*RW)
library(tidyverse)
calculateDIA <- function(TRE_CN,DIA_t,MEASYEAR,Year,RW,SPCD){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR,Year,RW,SPCD,DIA_C = NA)
  #N is the row where measure year and ring width year are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR[1]) #next step is to allow N to be ring width year -1
  if(length(N) == 0){
    N <- which(tree_df$Year + 1 == tree_df$MEASYEAR[1])
  }
  Species <- tree_df$SPCD[1]
  if(length(N) > 0 & Species %in% bratio_df$species){
    Curr_row <- N-1 #each time through subtract 1 and move down one row (or back one year)
    tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0 & !is.na(tree_df$DIA_C[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
      RW1 <- tree_df$RW[Curr_row+1] 
      #convert ring width from mm to inches
      RW1 = RW1 * 0.0393701
      b1 <- bratio_df$b1[bratio_df$species == Species]
      b2 <- bratio_df$b2[bratio_df$species == Species]
      exp <- bratio_df$exp[bratio_df$species == Species]
      tree_df$DIA_C[Curr_row] <- DIA_1 - ((2*RW1)/(b1+b2/(DIA_1^exp)))
      #stop back calculating for small trees (<1 inch)
      if(tree_df$DIA_C[Curr_row] < 1){
        tree_df$DIA_C[Curr_row] <- NA
      }
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_C)
}

incr_imputed <- incr_comb %>%
  group_by(TRE_CN) %>% #for each tree calculate dbh
  arrange(Year) %>%
    mutate(DIA_C = calculateDIA(TRE_CN = TRE_CN,DIA_t,MEASYEAR,Year,RW,SPCD))

#check
#stop when DIA is less than 1
min(incr_imputed$DIA_C,na.rm = T) #1.000038

#did DIA_C = DIA_t when last RW year was 1 year less than MEASYEAR
check_data <- incr_imputed[which(incr_imputed$Year + 1 == incr_imputed$MEASYEAR),]
check_data <- check_data[check_data$SPCD == 202,]

#filter for trees with back calculated DBH
length(unique(incr_imputed$TRE_CN)) #670
incr_imputed <- incr_imputed %>%
  filter(!is.na(DIA_C)) %>%
  #filter for >3" - check variant for large tree growth threshold - species specific
  filter(DIA_C >= 3)
length(unique(incr_imputed$TRE_CN)) #568

length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 202])) #131, 136
length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 122])) #73, 87
length(unique(incr_imputed$TRE_CN[incr_imputed$SPCD == 93]))  #50, 95

#save dataframe
save(incr_imputed,file = "./data/formatted/incr_imputed.Rdata")
  
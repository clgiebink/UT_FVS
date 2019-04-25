#Annualization of DBH
#Courtney Giebink
#4-17-19
#with Jeff Oliver


#load the data, long format with trees stacked
load(file = "./data/formatted/glmm.data")

#dataframe of coefficients for bark ratio calculation
#from Utah variant guide
bratio_df <- data.frame(species=c(93,202,122),
                        b1 = c(0.9502,0.867,0.8967),
                        b2 = c(-0.2528, 0, -0.4448)) #can add more species later 
  
#function to annualize, or back calculate dbh using diameter increment data (2*RW)
library(tidyverse)
calculateDIA <- function(TRE_CN,DIA_t,MEASYEAR.y,Year,RW,SPCD){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR.y,Year,RW,SPCD,DIA_C = NA)
  #N is the row where measure year and ring width year are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR.y[1]) #next step is to allow N to be ring width year -1
  Species <- tree_df$SPCD[1]
  if(length(N) > 0 & Species %in% bratio_df$species){
    Curr_row <- N-1 #each time through subtract 1 and move down one row
    tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
      RW1 <- tree_df$RW[Curr_row+1] 
      #TODO convert ring width from mm to inches
      RW1 = RW1 * 0.0393701
      b1 <- bratio_df$b1[bratio_df$species == Species]
      b2 <- bratio_df$b2[bratio_df$species == Species]
      tree_df$DIA_C[Curr_row] <- DIA_1 - ((2*RW1)/(b1+b2/DIA_1))
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_C)
}

glmm.data.imputed <- glmm.data %>%
  group_by(TRE_CN) %>% #for each tree calculate dbh
  mutate(DIA_C = calculateDIA(TRE_CN = treeID,DIA_t,MEASYEAR.y,Year,RW,SPCD))

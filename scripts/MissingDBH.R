#Back calculate missing DBH
#Courtney Giebink
#clgiebink@gmail.com
#26 June 2019

#From User's Guide to the Stand Prognosis Model
#Wykoff, Crookston, Stage
#pg 48
#Backdating imput diameters
##with trees with increment data calculate BAR (basal area ratio)
###BAR = DBH_0^2/DBH^2 -- average over species
##with missing trees
###DBH_0 = sqrt(BAR * DBH^2)

#First calculate BAR for every year for each tree
#create column on glmm.data.imputed

glmm.data.imputed <- glmm.data.imputed %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(BAR = (lag(DIA_C)^2)/DIA_C^2)

#average species BAR for each plot

glmm.data.imputed <- glmm.data.imputed %>%
  group_by(PLT_CN.y,Year,SPCD) %>%
  mutate(BAR_av = mean(BAR, na.rm = TRUE)) %>%
  mutate(BAR_av = ifelse(is.na(BAR_av), 1, BAR_av))

#remove trees that haven't been back calculate

incr_data <- glmm.data.imputed[!is.na(glmm.data.imputed$DIA_C), c("TRE_CN","PLT_CN.y","SPCD","DIA_t","TPA_UNADJ","Year","DIA_C","BAR","BAR_av")]



#create dataframe of trees without increment cores in plots with trees that have increment cores
plot_rw <- unique(incr_data$PLT_CN.y)
tree_rw <- unique(incr_data$TRE_CN)
miss_data <- UT_tree[(UT_tree$PLT_CN %in% plot_rw) & !(UT_tree$CN %in% tree_rw),c("CN","PLT_CN","SPCD","DIA","TPA_UNADJ")]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including trees with increment data; 508
colnames(miss_data)[colnames(miss_data)=="CN"] <- "TRE_CN"
colnames(miss_data)[colnames(miss_data)=="DIA"] <- "DIA_t"
miss_data$MEASYEAR <- plots$MEASYEAR[match(miss_data$PLT_CN, plots$CN)]
miss_data$Year <- NA #important for mutate(Year) to work
miss_data$BAR_av <- NA
miss_data$DIA_C <- NA

#check
length(plot_rw) #438
length(unique(miss_data$PLT_CN)) #438

#empty (year&DIA_C) dataframe?
miss_data <- miss_data %>% 
  slice(rep(1:n(), each = 40)) %>% #repeat each row 40 times
  group_by(TRE_CN) %>%
  mutate(Year = c((MEASYEAR[1]-39):MEASYEAR[1])) %>% #40 yrs is arbitrary; model will likely go back 30 yrs
  ungroup()


#match BAR_av from incr_data to miss_data using plot, species and year information
#match function does not work
for(i in 1:nrow(miss_data)){
  BAR_av <- incr_data$BAR_av[incr_data$PLT_CN.y == miss_data$PLT_CN[i] &
                            incr_data$SPCD == miss_data$SPCD[i] &
                            incr_data$Year == miss_data$Year[i]]
  if(length(BAR_av) == 0){
    BAR_av <- c(1)
  }
  miss_data$BAR_av[i] <- BAR_av[1]
}

#check how many trees there is no BAR for
length(unique(miss_data$TRE_CN[miss_data$BAR_av == 1]))
#5514
length(unique(miss_data$TRE_CN))
#8474

#calculate DIA from BAR
DIA_BAR <- function(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av,DIA_C = NA)
  #N is the row where measure year and year of growth are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR[1])
  tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of growth and measure year are equal
  Curr_row <- N-1 #each time through subtract 1 and move up one row to the previous year
  while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
    DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
    BAR_av <- tree_df$BAR_av[Curr_row+1] 
    tree_df$DIA_C[Curr_row] <- sqrt(BAR_av * (DIA_1^2))
    #continue loop for next row until curr_row>0
    Curr_row = Curr_row - 1 
  }
  return(tree_df$DIA_C)
}

miss_data_imputed <- miss_data %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(DIA_C = DIA_BAR(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av))


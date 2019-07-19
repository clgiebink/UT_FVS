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

library(tidyverse)

#First calculate BAR for every year for each tree
#create column on glmm.data.imputed

incr_imputed <- incr_imputed %>%
  group_by(TRE_CN) %>%
  mutate(BAR = (lag(DIA_C)^2)/DIA_C^2)

#remove trees that haven't been back calculate

incr_data <- incr_imputed[!is.na(incr_imputed$BAR), c("TRE_CN","PLT_CN.y","SPCD","DIA_t","TPA_UNADJ","Year","MEASYEAR.y","BAR","DIA_C")]
unique(incr_imputed$SPCD) #Values of BAR can be used for other species
#[1] 106 202 122  93  15  65  19  96
colnames(incr_data)[colnames(incr_data)=="PLT_CN.y"] <- "PLT_CN"
colnames(incr_data)[colnames(incr_data)=="MEASYEAR.y"] <- "MEASYEAR"


#create dataframe of trees without increment cores in plots with trees that have increment cores
plot_rw <- unique(incr_data$PLT_CN.y)
tree_rw <- unique(incr_data$TRE_CN)
miss_data <- UT_tree[(UT_tree$PLT_CN %in% plot_rw) & !(UT_tree$CN %in% tree_rw),c("CN","PLT_CN","SPCD","DIA","TPA_UNADJ")]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including trees with increment data; 508
colnames(miss_data)[colnames(miss_data)=="CN"] <- "TRE_CN"
colnames(miss_data)[colnames(miss_data)=="DIA"] <- "DIA_t"
miss_data$MEASYEAR <- plot$MEASYEAR[match(miss_data$PLT_CN, plot$CN)]
miss_data$Year <- NA #important for mutate(Year) to work
miss_data$BAR_av <- NA
miss_data$DIA_C <- NA

#check
length(plot_rw) #238
length(unique(miss_data$PLT_CN)) #238

#empty (year&DIA_C) dataframe?
miss_data <- miss_data %>% 
  slice(rep(1:n(), each = 40)) %>% #repeat each row 40 times
  group_by(TRE_CN) %>%
  mutate(Year = c((MEASYEAR[1]-39):MEASYEAR[1])) %>% #40 yrs is arbitrary; model will likely go back 30 yrs
  ungroup()


#match BAR_av from incr_data to miss_data using plot, species and year information
#match function does not work..why? NA values?
#for loop works but takes a long time
for(i in 1:nrow(miss_data)){ #miss_data includes all trees without increment data
  species <- miss_data$SPCD[i]
  if(species %in% c(106,202,122,93)){ #focal species
    BAR <- incr_data$BAR[incr_data$PLT_CN.y == miss_data$PLT_CN[i] &
                           incr_data$SPCD == miss_data$SPCD[i]]
    #first average within a plot for a specific species (over years)
  }
  if(length(BAR) == 0){
    BAR <- incr_data$BAR[incr_data$SPCD == miss_data$SPCD[i]]
    #if no species on the plot, then just average species across plots
  }
  if(!(species %in% c(106,122,202,93))){ #all other, non-focal species, includes 15, 19, 65, 66, 96, 108, 102, 113, 322, 475, 746, 749, & 814
    BAR <- incr_data$BAR[!(species %in% c(106,122,202,93))]
  }
  miss_data$BAR[i] <- mean(BAR, na.rm = TRUE)
}

#check how many trees there is no BAR for
length(unique(miss_data$TRE_CN[is.na(miss_data$BAR_av)]))
#0
#BAR = 1: 2970 for just plot, species, year
length(unique(miss_data$TRE_CN))
#4611
#4611

#unique(miss_data$SPCD[miss_data$BAR_av == 1])
#[1]  66 321 202 746  65 475  15 814 113  19 108  93  96 122 106 102

#calculate DIA from BAR
DIA_BAR <- function(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av){
  #create data frame with empty column for annualized dbh
  tree_df <- data.frame(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av,DIA_C = NA)
  #N is the row where measure year and year of growth are the same
  N <- which(tree_df$Year == tree_df$MEASYEAR[1])
  tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of growth and measure year are equal
  Curr_row <- N-1 #each time through subtract 1 and move up one row to the previous year
  while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
    #!is.na(tree_df$DIA_C[Curr_row + 1])
    DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
    BAR_av <- tree_df$BAR_av[Curr_row+1] 
    tree_df$DIA_C[Curr_row] <- sqrt(BAR_av * (DIA_1^2))
    #if(tree_df$DIA_C[Curr_row] < 1){
    #  tree_df$DIA_C <- NA
    #}
    #continue loop for next row until curr_row>0
    Curr_row = Curr_row - 1 
  }
  return(tree_df$DIA_C)
}

miss_data_imputed <- miss_data %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(DIA_C = DIA_BAR(TRE_CN,DIA_t,MEASYEAR,Year,BAR_av))

#check
unique(incr_imputed$TRE_CN[incr_imputed$BAR > abs(1)])
#NA
length(which(miss_data_imputed$DIA_C <= 1))
#4254; get rid of these?
length(which(miss_data_imputed$DIA_C <= 0))
#0
unique(incr_percov$CONDID)
#[1] 1
miss_data$CONDID <- cond$CONDID[match(miss_data$PLT_CN, cond$PLT_CN)]
unique(miss_data$CONDID)
#[1] 1
trees_plot <- density_data %>%
  group_by(PLT_CN) %>%
  summarise(trees_plot <- length(unique(TRE_CN))) %>%
  View
# A tibble: 238 x 2
#PLT_CN `trees_plot <- length(unique(TRE_CN))`
#<dbl>                                  <int>
#1 2848805010690                                     32
#2 2849106010690                                     14
#3 2849130010690                                     31
#4 2849662010690                                     26
#5 2853808010690                                     16
#6 2854161010690                                     15
#7 2854243010690                                     17
#8 2854375010690                                     11
#9 2855092010690                                      6
#10 2855482010690                                    21
# ... with 228 more rows

save(miss_data_imputed,file = "./data/formatted/miss_data_imputed")

#join two dataframes to compute stand variables
#first - trees back calculated with tree rings
##incr_data
#second - trees back calculated with BAR
##miss_data_imputed
#colnames(miss_data_imputed)[colnames(miss_data_imputed)=="BAR_av"] <- "BAR"
density_data <- rbind(incr_data,miss_data_imputed)

save(density_data,file = "./data/formatted/density_data")


##what about the species (SPCD) that don't have increment data to calculate BAR?
#can we use an average BAR accross species?
hist(incr_data$BAR, breaks = 50) #also per species
#anova across species to determine difference

#Specify the order of factor levels
#library(dplyr)
incr_data_aov <- incr_data %>% 
  ungroup() %>%
  mutate(SPCD = factor(SPCD, levels=unique(SPCD)))

#Produce summary statistics
library(FSA)   
Summarize(BAR ~ SPCD,
          data=incr_data_aov,
          digits=3)
#SPCD     n  mean    sd   min    Q1 median    Q3   max
#1  106 12651 0.977 0.023 0.734 0.971  0.985 0.992 1.000
#2  202  5527 0.964 0.037 0.664 0.953  0.976 0.988 0.999
#3  122  3476 0.956 0.052 0.398 0.950  0.972 0.986 1.000
#4   93  3055 0.972 0.028 0.634 0.966  0.981 0.989 1.000
#5   15   568 0.960 0.042 0.672 0.955  0.973 0.985 0.997
#6   65    88 0.992 0.004 0.983 0.990  0.992 0.995 0.999
#7   19    62 0.924 0.055 0.787 0.887  0.944 0.967 0.984
#8   96    59 0.917 0.074 0.663 0.886  0.943 0.972 0.989

#Fit the linear model and conduct ANOVA
model = lm(BAR ~ SPCD, 
           data=incr_data_aov)
#or
#model2 = aov(BAR ~ SPCD, 
           #data=incr_data_aov)

library(car)
Anova(model, type="II")                    # Can use type="III"
### If you use type="III", you need the following line before the analysi
### options(contrasts = c("contr.sum", "contr.poly"))
#Response: BAR
#Sum Sq    Df F value    Pr(>F)    
#SPCD       1.9867     7  262.97 < 2.2e-16 ***
#  Residuals 27.4980 25478

#Checking assumptions of the model
hist(residuals(model), 
     col="darkgray") #not normal
plot(fitted(model), 
     residuals(model))

#Tukey comparisons in agricolae package
library(agricolae)
(HSD.test(model, "SPCD"))          # outer parentheses print result
# Means sharing the same letter are not significantly different
#BAR groups
#65  0.9922310      a
#106 0.9774616      b
#93  0.9719057      c
#202 0.9636688      d
#15  0.9596544     de
#122 0.9563083      e
#19  0.9236055      f
#96  0.9174837      f

#Unbalanced mixed effect ANOVA for repeated measures
library(lme4)
test_fit <- lmer(BAR ~ SPCD + (1|TRE_CN), data = incr_data_aov)
anova(test_fit)

library(lmerTest)
difflsmeans(test_fit, test.effs = "SPCD")
#Estimate  Std. Error    df t value       lower       upper  Pr(>|t|)    
#SPCD106 - SPCD202  1.3742e-02  3.0666e-03 249.9  4.4813  7.7027e-03  1.9782e-02 1.130e-05 ***
#SPCD106 - SPCD122  2.2060e-02  3.2344e-03 253.3  6.8205  1.5690e-02  2.8430e-02 6.633e-11 ***
#SPCD106 - SPCD93   2.3659e-03  4.1547e-03 246.4  0.5694 -5.8173e-03  1.0549e-02  0.569570    
#SPCD106 - SPCD15   1.3793e-02  8.0443e-03 248.2  1.7146 -2.0512e-03  2.9636e-02  0.087670 .  
#SPCD106 - SPCD65  -2.1129e-02  1.9297e-02 247.8 -1.0949 -5.9137e-02  1.6879e-02  0.274613    
#SPCD106 - SPCD19   4.7496e-02  1.9398e-02 253.0  2.4485  9.2941e-03  8.5699e-02  0.015024 *  
#SPCD106 - SPCD96   5.3618e-02  1.9415e-02 253.9  2.7616  1.5383e-02  9.1854e-02  0.006171 ** 
#SPCD202 - SPCD122  8.3174e-03  3.6902e-03 252.0  2.2539  1.0499e-03  1.5585e-02  0.025060 *  
#SPCD202 - SPCD93  -1.1377e-02  4.5186e-03 246.7 -2.5177 -2.0276e-02 -2.4767e-03  0.012446 *  
#SPCD202 - SPCD15   5.0214e-05  8.2382e-03 248.2  0.0061 -1.6175e-02  1.6276e-02  0.995142    
#SPCD202 - SPCD65  -3.4872e-02  1.9379e-02 247.8 -1.7995 -7.3040e-02  3.2969e-03  0.073163 .  
#SPCD202 - SPCD19   3.3754e-02  1.9479e-02 253.0  1.7328 -4.6082e-03  7.2116e-02  0.084347 .  
#SPCD202 - SPCD96   3.9876e-02  1.9496e-02 253.9  2.0453  1.4804e-03  7.8271e-02  0.041858 *  
#SPCD122 - SPCD93  -1.9694e-02  4.6341e-03 248.5 -4.2498 -2.8821e-02 -1.0567e-02 3.028e-05 ***
#SPCD122 - SPCD15  -8.2672e-03  8.3021e-03 248.7 -0.9958 -2.4619e-02  8.0841e-03  0.320315    
#SPCD122 - SPCD65  -4.3189e-02  1.9406e-02 247.9 -2.2255 -8.1411e-02 -4.9669e-03  0.026946 *  
#SPCD122 - SPCD19   2.5436e-02  1.9506e-02 253.1  1.3040 -1.2979e-02  6.3852e-02  0.193415    
#SPCD122 - SPCD96   3.1558e-02  1.9524e-02 253.9  1.6164 -6.8903e-03  7.0007e-02  0.107244    
#SPCD93 - SPCD15    1.1427e-02  8.7020e-03 247.6  1.3131 -5.7126e-03  2.8566e-02  0.190356    
#SPCD93 - SPCD65   -2.3495e-02  1.9581e-02 247.7 -1.1999 -6.2061e-02  1.5071e-02  0.231321    
#SPCD93 - SPCD19    4.5130e-02  1.9680e-02 252.7  2.2932  6.3731e-03  8.3888e-02  0.022656 *  
#SPCD93 - SPCD96    5.1252e-02  1.9697e-02 253.6  2.6020  1.2462e-02  9.0043e-02  0.009812 ** 
#SPCD15 - SPCD65   -3.4922e-02  2.0757e-02 247.8 -1.6824 -7.5804e-02  5.9607e-03  0.093748 .  
#SPCD15 - SPCD19    3.3704e-02  2.0851e-02 252.3  1.6164 -7.3597e-03  7.4767e-02  0.107249    
#SPCD15 - SPCD96    3.9826e-02  2.0867e-02 253.1  1.9086 -1.2689e-03  8.0920e-02  0.057448 .  
#SPCD65 - SPCD19    6.8625e-02  2.7247e-02 250.4  2.5186  1.4962e-02  1.2229e-01  0.012406 *  
#SPCD65 - SPCD96    7.4747e-02  2.7260e-02 250.8  2.7420  2.1060e-02  1.2843e-01  0.006546 ** 
#SPCD19 - SPCD96    6.1219e-03  2.7331e-02 253.5  0.2240 -4.7703e-02  5.9947e-02  0.822947  

library(emmeans)
emmeans(test_fit, list(pairwise ~ SPCD), adjust = "tukey")
#$`pairwise differences of SPCD`
#contrast   estimate      SE  df z.ratio p.value
#106 - 202  1.37e-02 0.00307 Inf  4.481  0.0002 
#106 - 122  2.21e-02 0.00323 Inf  6.820  <.0001 
#106 - 93   2.37e-03 0.00415 Inf  0.569  0.9992 
#106 - 15   1.38e-02 0.00804 Inf  1.715  0.6778 
#106 - 65  -2.11e-02 0.01930 Inf -1.095  0.9581 
#106 - 19   4.75e-02 0.01940 Inf  2.449  0.2181 
#106 - 96   5.36e-02 0.01942 Inf  2.762  0.1048 
#202 - 122  8.32e-03 0.00369 Inf  2.254  0.3195 
#202 - 93  -1.14e-02 0.00452 Inf -2.518  0.1878 
#202 - 15   5.02e-05 0.00824 Inf  0.006  1.0000 
#202 - 65  -3.49e-02 0.01938 Inf -1.799  0.6205 
#202 - 19   3.38e-02 0.01948 Inf  1.733  0.6656 
#202 - 96   3.99e-02 0.01950 Inf  2.045  0.4511 
#122 - 93  -1.97e-02 0.00463 Inf -4.250  0.0006 
#122 - 15  -8.27e-03 0.00830 Inf -0.996  0.9752 
#122 - 65  -4.32e-02 0.01941 Inf -2.226  0.3362 
#122 - 19   2.54e-02 0.01951 Inf  1.304  0.8977 
#122 - 96   3.16e-02 0.01952 Inf  1.616  0.7405 
#93 - 15    1.14e-02 0.00870 Inf  1.313  0.8942 
#93 - 65   -2.35e-02 0.01958 Inf -1.200  0.9322 
#93 - 19    4.51e-02 0.01968 Inf  2.293  0.2972 
#93 - 96    5.13e-02 0.01970 Inf  2.602  0.1550 
#15 - 65   -3.49e-02 0.02076 Inf -1.682  0.6988 
#15 - 19    3.37e-02 0.02085 Inf  1.616  0.7405 
#15 - 96    3.98e-02 0.02087 Inf  1.909  0.5449 
#65 - 19    6.86e-02 0.02725 Inf  2.519  0.1874 
#65 - 96    7.47e-02 0.02726 Inf  2.742  0.1102 
#19 - 96    6.12e-03 0.02733 Inf  0.224  1.0000 

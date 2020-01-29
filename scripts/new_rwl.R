#Increase sample size
#with newly verified wood from USU lab
#December 2019
#clgiebink@gmail.com

#Tree-ring data ----
#.rwl files to long format
#original code from Justin DeRose

#### Trial code to turn county 1 in Utah
####
library(dplR)
library(dplyr)


#### Newly verified from county 13..
#copy them here...
cn289856142489998 <- read.rwl('./data/raw/UT Periodic/13/5/04006093.txt'); names(cn289856142489998)[1]<-'RWI'; cn289856142489998$Year<-as.numeric(row.names(cn289856142489998)); cn289856142489998$CN<-289856142489998
cn289856143489998 <- read.rwl('./data/raw/UT Periodic/13/8/02008093.txt'); names(cn289856143489998)[1]<-'RWI'; cn289856143489998$Year<-as.numeric(row.names(cn289856143489998)); cn289856143489998$CN<-289856143489998
cn289856144489998 <- read.rwl('./data/raw/UT Periodic/13/8/02009093.txt'); names(cn289856144489998)[1]<-'RWI'; cn289856144489998$Year<-as.numeric(row.names(cn289856144489998)); cn289856144489998$CN<-289856144489998
cn289856152489998 <- read.rwl('./data/raw/UT Periodic/13/19/04002093.txt'); names(cn289856152489998)[1]<-'RWI'; cn289856152489998$Year<-as.numeric(row.names(cn289856152489998)); cn289856152489998$CN<-289856152489998
cn289856151489998 <- read.rwl('./data/raw/UT Periodic/13/19/02010093.txt'); names(cn289856151489998)[1]<-'RWI'; cn289856151489998$Year<-as.numeric(row.names(cn289856151489998)); cn289856151489998$CN<-289856151489998
cn289856153489998 <- read.rwl('./data/raw/UT Periodic/13/20/03004093.txt'); names(cn289856153489998)[1]<-'RWI'; cn289856153489998$Year<-as.numeric(row.names(cn289856153489998)); cn289856153489998$CN<-289856153489998
cn289856154489998 <- read.rwl('./data/raw/UT Periodic/13/20/03005093.txt'); names(cn289856154489998)[1]<-'RWI'; cn289856154489998$Year<-as.numeric(row.names(cn289856154489998)); cn289856154489998$CN<-289856154489998
cn289856155489998 <- read.rwl('./data/raw/UT Periodic/13/21/02004093.txt'); names(cn289856155489998)[1]<-'RWI'; cn289856155489998$Year<-as.numeric(row.names(cn289856155489998)); cn289856155489998$CN<-289856155489998
cn289856156489998 <- read.rwl('./data/raw/UT Periodic/13/21/02005093.txt'); names(cn289856156489998)[1]<-'RWI'; cn289856156489998$Year<-as.numeric(row.names(cn289856156489998)); cn289856156489998$CN<-289856156489998
cn289856159489998 <- read.rwl('./data/raw/UT Periodic/13/23/01006093.txt'); names(cn289856159489998)[1]<-'RWI'; cn289856159489998$Year<-as.numeric(row.names(cn289856159489998)); cn289856159489998$CN<-289856159489998
cn289856161489998 <- read.rwl('./data/raw/UT Periodic/13/25/03001093.txt'); names(cn289856161489998)[1]<-'RWI'; cn289856161489998$Year<-as.numeric(row.names(cn289856161489998)); cn289856161489998$CN<-289856161489998
cn289856162489998 <- read.rwl('./data/raw/UT Periodic/13/25/04005093.txt'); names(cn289856162489998)[1]<-'RWI'; cn289856162489998$Year<-as.numeric(row.names(cn289856162489998)); cn289856162489998$CN<-289856162489998
cn289856163489998 <- read.rwl('./data/raw/UT Periodic/13/26/03004093.txt'); names(cn289856163489998)[1]<-'RWI'; cn289856163489998$Year<-as.numeric(row.names(cn289856163489998)); cn289856163489998$CN<-289856163489998
cn289856165489998 <- read.rwl('./data/raw/UT Periodic/13/29/04005093.txt'); names(cn289856165489998)[1]<-'RWI'; cn289856165489998$Year<-as.numeric(row.names(cn289856165489998)); cn289856165489998$CN<-289856165489998
cn289856167489998 <- read.rwl('./data/raw/UT Periodic/13/30/04005093.txt'); names(cn289856167489998)[1]<-'RWI'; cn289856167489998$Year<-as.numeric(row.names(cn289856167489998)); cn289856167489998$CN<-289856167489998
cn289856168489998 <- read.rwl('./data/raw/UT Periodic/13/32/02001093.txt'); names(cn289856168489998)[1]<-'RWI'; cn289856168489998$Year<-as.numeric(row.names(cn289856168489998)); cn289856168489998$CN<-289856168489998
cn289856171489998 <- read.rwl('./data/raw/UT Periodic/13/33/02002093.txt'); names(cn289856171489998)[1]<-'RWI'; cn289856171489998$Year<-as.numeric(row.names(cn289856171489998)); cn289856171489998$CN<-289856171489998
cn289856172489998 <- read.rwl('./data/raw/UT Periodic/13/34/01005093.txt'); names(cn289856172489998)[1]<-'RWI'; cn289856172489998$Year<-as.numeric(row.names(cn289856172489998)); cn289856172489998$CN<-289856172489998
cn289856182489998 <- read.rwl('./data/raw/UT Periodic/13/40/05004093.txt'); names(cn289856182489998)[1]<-'RWI'; cn289856182489998$Year<-as.numeric(row.names(cn289856182489998)); cn289856182489998$CN<-289856182489998
cn289856183489998 <- read.rwl('./data/raw/UT Periodic/13/41/02010093.txt'); names(cn289856183489998)[1]<-'RWI'; cn289856183489998$Year<-as.numeric(row.names(cn289856183489998)); cn289856183489998$CN<-289856183489998
cn289856186489998 <- read.rwl('./data/raw/UT Periodic/13/43/03004093.txt'); names(cn289856186489998)[1]<-'RWI'; cn289856186489998$Year<-as.numeric(row.names(cn289856186489998)); cn289856186489998$CN<-289856186489998
cn289856191489998 <- read.rwl('./data/raw/UT Periodic/13/45/02011093.txt'); names(cn289856191489998)[1]<-'RWI'; cn289856191489998$Year<-as.numeric(row.names(cn289856191489998)); cn289856191489998$CN<-289856191489998
cn289856192489998 <- read.rwl('./data/raw/UT Periodic/13/46/02001093.txt'); names(cn289856192489998)[1]<-'RWI'; cn289856192489998$Year<-as.numeric(row.names(cn289856192489998)); cn289856192489998$CN<-289856192489998
cn289856197489998 <- read.rwl('./data/raw/UT Periodic/13/49/04003093.txt'); names(cn289856197489998)[1]<-'RWI'; cn289856197489998$Year<-as.numeric(row.names(cn289856197489998)); cn289856197489998$CN<-289856197489998
cn289856198489998 <- read.rwl('./data/raw/UT Periodic/13/50/01005093.txt'); names(cn289856198489998)[1]<-'RWI'; cn289856198489998$Year<-as.numeric(row.names(cn289856198489998)); cn289856198489998$CN<-289856198489998
cn289856207489998 <- read.rwl('./data/raw/UT Periodic/13/57/01001093.txt'); names(cn289856207489998)[1]<-'RWI'; cn289856207489998$Year<-as.numeric(row.names(cn289856207489998)); cn289856207489998$CN<-289856207489998


### join them with the second script
##utilizing a list
mylist <- list()
mylist[[1]] <- cn289856142489998
mylist[[2]] <- cn289856143489998
mylist[[3]] <- cn289856144489998
mylist[[4]] <- cn289856152489998
mylist[[5]] <- cn289856151489998
mylist[[6]] <- cn289856153489998
mylist[[7]] <- cn289856154489998
mylist[[8]] <- cn289856155489998
mylist[[9]] <- cn289856156489998
mylist[[10]] <- cn289856159489998
mylist[[11]] <- cn289856161489998
mylist[[12]] <- cn289856162489998
mylist[[13]] <- cn289856163489998
mylist[[14]] <- cn289856165489998
mylist[[15]] <- cn289856167489998
mylist[[16]] <- cn289856168489998
mylist[[17]] <- cn289856171489998
mylist[[18]] <- cn289856172489998
mylist[[19]] <- cn289856182489998
mylist[[20]] <- cn289856183489998
mylist[[21]] <- cn289856186489998
mylist[[22]] <- cn289856191489998
mylist[[23]] <- cn289856192489998
mylist[[24]] <- cn289856197489998
mylist[[25]] <- cn289856198489998
mylist[[26]] <- cn289856207489998

#county 17
cn289856685489998 <- read.rwl('./data/raw/UT Periodic/17/16/03005093.txt'); names(cn289856685489998)[1]<-'RWI'; cn289856685489998$Year<-as.numeric(row.names(cn289856685489998)); cn289856685489998$CN<-289856685489998
cn289856686489998 <- read.rwl('./data/raw/UT Periodic/17/19/01001093.txt'); names(cn289856686489998)[1]<-'RWI'; cn289856686489998$Year<-as.numeric(row.names(cn289856686489998)); cn289856686489998$CN<-289856686489998
cn289856687489998 <- read.rwl('./data/raw/UT Periodic/17/19/01002093.txt'); names(cn289856687489998)[1]<-'RWI'; cn289856687489998$Year<-as.numeric(row.names(cn289856687489998)); cn289856687489998$CN<-289856687489998
cn289856699489998 <- read.rwl('./data/raw/UT Periodic/17/57/05007093.txt'); names(cn289856699489998)[1]<-'RWI'; cn289856699489998$Year<-as.numeric(row.names(cn289856699489998)); cn289856699489998$CN<-289856699489998
cn289856700489998 <- read.rwl('./data/raw/UT Periodic/17/59/01002093.txt'); names(cn289856700489998)[1]<-'RWI'; cn289856700489998$Year<-as.numeric(row.names(cn289856700489998)); cn289856700489998$CN<-289856700489998
cn289856704489998 <- read.rwl('./data/raw/UT Periodic/17/61/01008093.txt'); names(cn289856704489998)[1]<-'RWI'; cn289856704489998$Year<-as.numeric(row.names(cn289856704489998)); cn289856704489998$CN<-289856704489998
cn289856705489998 <- read.rwl('./data/raw/UT Periodic/17/63/01003093.txt'); names(cn289856705489998)[1]<-'RWI'; cn289856705489998$Year<-as.numeric(row.names(cn289856705489998)); cn289856705489998$CN<-289856705489998
cn289856706489998 <- read.rwl('./data/raw/UT Periodic/17/63/02001093.txt'); names(cn289856706489998)[1]<-'RWI'; cn289856706489998$Year<-as.numeric(row.names(cn289856706489998)); cn289856706489998$CN<-289856706489998
cn289856722489998 <- read.rwl('./data/raw/UT Periodic/17/101/02006093.txt'); names(cn289856722489998)[1]<-'RWI'; cn289856722489998$Year<-as.numeric(row.names(cn289856722489998)); cn289856722489998$CN<-289856722489998
cn289856736489998 <- read.rwl('./data/raw/UT Periodic/17/139/02004093.txt'); names(cn289856736489998)[1]<-'RWI'; cn289856736489998$Year<-as.numeric(row.names(cn289856736489998)); cn289856736489998$CN<-289856736489998
cn289856737489998 <- read.rwl('./data/raw/UT Periodic/17/139/03001093.txt'); names(cn289856737489998)[1]<-'RWI'; cn289856737489998$Year<-as.numeric(row.names(cn289856737489998)); cn289856737489998$CN<-289856737489998
cn289856738489998 <- read.rwl('./data/raw/UT Periodic/17/140/02011093.txt'); names(cn289856738489998)[1]<-'RWI'; cn289856738489998$Year<-as.numeric(row.names(cn289856738489998)); cn289856738489998$CN<-289856738489998
cn289856739489998 <- read.rwl('./data/raw/UT Periodic/17/140/03008093.txt'); names(cn289856739489998)[1]<-'RWI'; cn289856739489998$Year<-as.numeric(row.names(cn289856739489998)); cn289856739489998$CN<-289856739489998
cn289856741489998 <- read.rwl('./data/raw/UT Periodic/17/142/02009202.txt'); names(cn289856741489998)[1]<-'RWI'; cn289856741489998$Year<-as.numeric(row.names(cn289856741489998)); cn289856741489998$CN<-289856741489998
cn289856744489998 <- read.rwl('./data/raw/UT Periodic/17/144/03002122.txt'); names(cn289856744489998)[1]<-'RWI'; cn289856744489998$Year<-as.numeric(row.names(cn289856744489998)); cn289856744489998$CN<-289856744489998
cn289856761489998 <- read.rwl('./data/raw/UT Periodic/17/180/01005093.txt'); names(cn289856761489998)[1]<-'RWI'; cn289856761489998$Year<-as.numeric(row.names(cn289856761489998)); cn289856761489998$CN<-289856761489998
cn289856762489998 <- read.rwl('./data/raw/UT Periodic/17/180/02009093.txt'); names(cn289856762489998)[1]<-'RWI'; cn289856762489998$Year<-as.numeric(row.names(cn289856762489998)); cn289856762489998$CN<-289856762489998
cn289856764489998 <- read.rwl('./data/raw/UT Periodic/17/181/03007093.txt'); names(cn289856764489998)[1]<-'RWI'; cn289856764489998$Year<-as.numeric(row.names(cn289856764489998)); cn289856764489998$CN<-289856764489998
cn289856765489998 <- read.rwl('./data/raw/UT Periodic/17/182/03003093.txt'); names(cn289856765489998)[1]<-'RWI'; cn289856765489998$Year<-as.numeric(row.names(cn289856765489998)); cn289856765489998$CN<-289856765489998
cn289856766489998 <- read.rwl('./data/raw/UT Periodic/17/182/04006093.txt'); names(cn289856766489998)[1]<-'RWI'; cn289856766489998$Year<-as.numeric(row.names(cn289856766489998)); cn289856766489998$CN<-289856766489998
cn289856799489998 <- read.rwl('./data/raw/UT Periodic/17/253/01004202.txt'); names(cn289856799489998)[1]<-'RWI'; cn289856799489998$Year<-as.numeric(row.names(cn289856799489998)); cn289856799489998$CN<-289856799489998
cn289856830489998 <- read.rwl('./data/raw/UT Periodic/17/332/05004122.txt'); names(cn289856830489998)[1]<-'RWI'; cn289856830489998$Year<-as.numeric(row.names(cn289856830489998)); cn289856830489998$CN<-289856830489998
cn289856831489998 <- read.rwl('./data/raw/UT Periodic/17/332/05005122.txt'); names(cn289856831489998)[1]<-'RWI'; cn289856831489998$Year<-as.numeric(row.names(cn289856831489998)); cn289856831489998$CN<-289856831489998
cn289856834489998 <- read.rwl('./data/raw/UT Periodic/17/335/04002122.txt'); names(cn289856834489998)[1]<-'RWI'; cn289856834489998$Year<-as.numeric(row.names(cn289856834489998)); cn289856834489998$CN<-289856834489998
cn289856838489998 <- read.rwl('./data/raw/UT Periodic/17/335/05004122.txt'); names(cn289856838489998)[1]<-'RWI'; cn289856838489998$Year<-as.numeric(row.names(cn289856838489998)); cn289856838489998$CN<-289856838489998
cn289856842489998 <- read.rwl('./data/raw/UT Periodic/17/339/02013202.txt'); names(cn289856842489998)[1]<-'RWI'; cn289856842489998$Year<-as.numeric(row.names(cn289856842489998)); cn289856842489998$CN<-289856842489998
cn289856845489998 <- read.rwl('./data/raw/UT Periodic/17/366/03006202.txt'); names(cn289856845489998)[1]<-'RWI'; cn289856845489998$Year<-as.numeric(row.names(cn289856845489998)); cn289856845489998$CN<-289856845489998
cn289856847489998 <- read.rwl('./data/raw/UT Periodic/17/367/03001122.txt'); names(cn289856847489998)[1]<-'RWI'; cn289856847489998$Year<-as.numeric(row.names(cn289856847489998)); cn289856847489998$CN<-289856847489998
cn289856857489998 <- read.rwl('./data/raw/UT Periodic/17/378/04004202.txt'); names(cn289856857489998)[1]<-'RWI'; cn289856857489998$Year<-as.numeric(row.names(cn289856857489998)); cn289856857489998$CN<-289856857489998
cn289856858489998 <- read.rwl('./data/raw/UT Periodic/17/378/04005093.txt'); names(cn289856858489998)[1]<-'RWI'; cn289856858489998$Year<-as.numeric(row.names(cn289856858489998)); cn289856858489998$CN<-289856858489998
cn289856867489998 <- read.rwl('./data/raw/UT Periodic/17/404/04003122.txt'); names(cn289856867489998)[1]<-'RWI'; cn289856867489998$Year<-as.numeric(row.names(cn289856867489998)); cn289856867489998$CN<-289856867489998
cn289856874489998 <- read.rwl('./data/raw/UT Periodic/17/412/01001122.txt'); names(cn289856874489998)[1]<-'RWI'; cn289856874489998$Year<-as.numeric(row.names(cn289856874489998)); cn289856874489998$CN<-289856874489998
cn289856886489998 <- read.rwl('./data/raw/UT Periodic/17/443/03002122.txt'); names(cn289856886489998)[1]<-'RWI'; cn289856886489998$Year<-as.numeric(row.names(cn289856886489998)); cn289856886489998$CN<-289856886489998
cn289856892489998 <- read.rwl('./data/raw/UT Periodic/17/452/02003122.txt'); names(cn289856892489998)[1]<-'RWI'; cn289856892489998$Year<-as.numeric(row.names(cn289856892489998)); cn289856892489998$CN<-289856892489998
cn289856918489998 <- read.rwl('./data/raw/UT Periodic/17/517/05006122.txt'); names(cn289856918489998)[1]<-'RWI'; cn289856918489998$Year<-as.numeric(row.names(cn289856918489998)); cn289856918489998$CN<-289856918489998
cn289856927489998 <- read.rwl('./data/raw/UT Periodic/17/1001/01009202.txt'); names(cn289856927489998)[1]<-'RWI'; cn289856927489998$Year<-as.numeric(row.names(cn289856927489998)); cn289856927489998$CN<-289856927489998
cn289856930489998 <- read.rwl('./data/raw/UT Periodic/17/1014/01005202.txt'); names(cn289856930489998)[1]<-'RWI'; cn289856930489998$Year<-as.numeric(row.names(cn289856930489998)); cn289856930489998$CN<-289856930489998
cn289856934489998 <- read.rwl('./data/raw/UT Periodic/17/1032/01005093.txt'); names(cn289856934489998)[1]<-'RWI'; cn289856934489998$Year<-as.numeric(row.names(cn289856934489998)); cn289856934489998$CN<-289856934489998
cn289856935489998 <- read.rwl('./data/raw/UT Periodic/17/1032/04001093.txt'); names(cn289856935489998)[1]<-'RWI'; cn289856935489998$Year<-as.numeric(row.names(cn289856935489998)); cn289856935489998$CN<-289856935489998
cn289856936489998 <- read.rwl('./data/raw/UT Periodic/17/1033/01001093.txt'); names(cn289856936489998)[1]<-'RWI'; cn289856936489998$Year<-as.numeric(row.names(cn289856936489998)); cn289856936489998$CN<-289856936489998
cn289856937489998 <- read.rwl('./data/raw/UT Periodic/17/1033/05003093.txt'); names(cn289856937489998)[1]<-'RWI'; cn289856937489998$Year<-as.numeric(row.names(cn289856937489998)); cn289856937489998$CN<-289856937489998
cn289856938489998 <- read.rwl('./data/raw/UT Periodic/17/1035/03001093.txt'); names(cn289856938489998)[1]<-'RWI'; cn289856938489998$Year<-as.numeric(row.names(cn289856938489998)); cn289856938489998$CN<-289856938489998
cn289856939489998 <- read.rwl('./data/raw/UT Periodic/17/1035/03002093.txt'); names(cn289856939489998)[1]<-'RWI'; cn289856939489998$Year<-as.numeric(row.names(cn289856939489998)); cn289856939489998$CN<-289856939489998
cn289856940489998 <- read.rwl('./data/raw/UT Periodic/17/1043/01002122.txt'); names(cn289856940489998)[1]<-'RWI'; cn289856940489998$Year<-as.numeric(row.names(cn289856940489998)); cn289856940489998$CN<-289856940489998
cn289856942489998 <- read.rwl('./data/raw/UT Periodic/17/1044/05002093.txt'); names(cn289856942489998)[1]<-'RWI'; cn289856942489998$Year<-as.numeric(row.names(cn289856942489998)); cn289856942489998$CN<-289856942489998
cn289856949489998 <- read.rwl('./data/raw/UT Periodic/17/1053/05001093.txt'); names(cn289856949489998)[1]<-'RWI'; cn289856949489998$Year<-as.numeric(row.names(cn289856949489998)); cn289856949489998$CN<-289856949489998
cn289856950489998 <- read.rwl('./data/raw/UT Periodic/17/1055/01002093.txt'); names(cn289856950489998)[1]<-'RWI'; cn289856950489998$Year<-as.numeric(row.names(cn289856950489998)); cn289856950489998$CN<-289856950489998
cn289856951489998 <- read.rwl('./data/raw/UT Periodic/17/1055/04003093.txt'); names(cn289856951489998)[1]<-'RWI'; cn289856951489998$Year<-as.numeric(row.names(cn289856951489998)); cn289856951489998$CN<-289856951489998
cn289856955489998 <- read.rwl('./data/raw/UT Periodic/17/1058/01001093.txt'); names(cn289856955489998)[1]<-'RWI'; cn289856955489998$Year<-as.numeric(row.names(cn289856955489998)); cn289856955489998$CN<-289856955489998
cn289856956489998 <- read.rwl('./data/raw/UT Periodic/17/1058/04001093.txt'); names(cn289856956489998)[1]<-'RWI'; cn289856956489998$Year<-as.numeric(row.names(cn289856956489998)); cn289856956489998$CN<-289856956489998
cn289856960489998 <- read.rwl('./data/raw/UT Periodic/17/1065/02002093.txt'); names(cn289856960489998)[1]<-'RWI'; cn289856960489998$Year<-as.numeric(row.names(cn289856960489998)); cn289856960489998$CN<-289856960489998
cn289856966489998 <- read.rwl('./data/raw/UT Periodic/17/1074/01001093.txt'); names(cn289856966489998)[1]<-'RWI'; cn289856966489998$Year<-as.numeric(row.names(cn289856966489998)); cn289856966489998$CN<-289856966489998
cn289856968489998 <- read.rwl('./data/raw/UT Periodic/17/1075/01006093.txt'); names(cn289856968489998)[1]<-'RWI'; cn289856968489998$Year<-as.numeric(row.names(cn289856968489998)); cn289856968489998$CN<-289856968489998
cn289856971489998 <- read.rwl('./data/raw/UT Periodic/17/1076/05001093.txt'); names(cn289856971489998)[1]<-'RWI'; cn289856971489998$Year<-as.numeric(row.names(cn289856971489998)); cn289856971489998$CN<-289856971489998
cn289856977489998 <- read.rwl('./data/raw/UT Periodic/17/1081/03001122.txt'); names(cn289856977489998)[1]<-'RWI'; cn289856977489998$Year<-as.numeric(row.names(cn289856977489998)); cn289856977489998$CN<-289856977489998
cn289856985489998 <- read.rwl('./data/raw/UT Periodic/17/1097/05002093.txt'); names(cn289856985489998)[1]<-'RWI'; cn289856985489998$Year<-as.numeric(row.names(cn289856985489998)); cn289856985489998$CN<-289856985489998
cn289856986489998 <- read.rwl('./data/raw/UT Periodic/17/1098/01001093.txt'); names(cn289856986489998)[1]<-'RWI'; cn289856986489998$Year<-as.numeric(row.names(cn289856986489998)); cn289856986489998$CN<-289856986489998
cn289856987489998 <- read.rwl('./data/raw/UT Periodic/17/1098/01002093.txt'); names(cn289856987489998)[1]<-'RWI'; cn289856987489998$Year<-as.numeric(row.names(cn289856987489998)); cn289856987489998$CN<-289856987489998
cn289856996489998 <- read.rwl('./data/raw/UT Periodic/17/1117/01004093.txt'); names(cn289856996489998)[1]<-'RWI'; cn289856996489998$Year<-as.numeric(row.names(cn289856996489998)); cn289856996489998$CN<-289856996489998
cn289856997489998 <- read.rwl('./data/raw/UT Periodic/17/1117/02002093.txt'); names(cn289856997489998)[1]<-'RWI'; cn289856997489998$Year<-as.numeric(row.names(cn289856997489998)); cn289856997489998$CN<-289856997489998
cn289857001489998 <- read.rwl('./data/raw/UT Periodic/17/1120/01011093.txt'); names(cn289857001489998)[1]<-'RWI'; cn289857001489998$Year<-as.numeric(row.names(cn289857001489998)); cn289857001489998$CN<-289857001489998
cn289857009489998 <- read.rwl('./data/raw/UT Periodic/17/1140/01003093.txt'); names(cn289857009489998)[1]<-'RWI'; cn289857009489998$Year<-as.numeric(row.names(cn289857009489998)); cn289857009489998$CN<-289857009489998
cn289857013489998 <- read.rwl('./data/raw/UT Periodic/17/1160/02001122.txt'); names(cn289857013489998)[1]<-'RWI'; cn289857013489998$Year<-as.numeric(row.names(cn289857013489998)); cn289857013489998$CN<-289857013489998
cn289857018489998 <- read.rwl('./data/raw/UT Periodic/17/1173/04005122.txt'); names(cn289857018489998)[1]<-'RWI'; cn289857018489998$Year<-as.numeric(row.names(cn289857018489998)); cn289857018489998$CN<-289857018489998
cn289857029489998 <- read.rwl('./data/raw/UT Periodic/17/1201/01006093.txt'); names(cn289857029489998)[1]<-'RWI'; cn289857029489998$Year<-as.numeric(row.names(cn289857029489998)); cn289857029489998$CN<-289857029489998
cn289857047489998 <- read.rwl('./data/raw/UT Periodic/17/1232/03002122.txt'); names(cn289857047489998)[1]<-'RWI'; cn289857047489998$Year<-as.numeric(row.names(cn289857047489998)); cn289857047489998$CN<-289857047489998
cn289857048489998 <- read.rwl('./data/raw/UT Periodic/17/1232/05001122.txt'); names(cn289857048489998)[1]<-'RWI'; cn289857048489998$Year<-as.numeric(row.names(cn289857048489998)); cn289857048489998$CN<-289857048489998
cn289857049489998 <- read.rwl('./data/raw/UT Periodic/17/1233/03002122.txt'); names(cn289857049489998)[1]<-'RWI'; cn289857049489998$Year<-as.numeric(row.names(cn289857049489998)); cn289857049489998$CN<-289857049489998
cn289857053489998 <- read.rwl('./data/raw/UT Periodic/17/1241/03003122.txt'); names(cn289857053489998)[1]<-'RWI'; cn289857053489998$Year<-as.numeric(row.names(cn289857053489998)); cn289857053489998$CN<-289857053489998
cn289857054489998 <- read.rwl('./data/raw/UT Periodic/17/1244/01002093.txt'); names(cn289857054489998)[1]<-'RWI'; cn289857054489998$Year<-as.numeric(row.names(cn289857054489998)); cn289857054489998$CN<-289857054489998
cn289857061489998 <- read.rwl('./data/raw/UT Periodic/17/1252/02003122.txt'); names(cn289857061489998)[1]<-'RWI'; cn289857061489998$Year<-as.numeric(row.names(cn289857061489998)); cn289857061489998$CN<-289857061489998
cn289857065489998 <- read.rwl('./data/raw/UT Periodic/17/1258/02001122.txt'); names(cn289857065489998)[1]<-'RWI'; cn289857065489998$Year<-as.numeric(row.names(cn289857065489998)); cn289857065489998$CN<-289857065489998
cn289857070489998 <- read.rwl('./data/raw/UT Periodic/17/1264/04003093.txt'); names(cn289857070489998)[1]<-'RWI'; cn289857070489998$Year<-as.numeric(row.names(cn289857070489998)); cn289857070489998$CN<-289857070489998

#add to list
mylist[[27]] <- cn289856685489998
mylist[[28]] <- cn289856686489998
mylist[[29]] <- cn289856687489998
mylist[[30]] <- cn289856699489998
mylist[[31]] <- cn289856700489998
mylist[[32]] <- cn289856704489998
mylist[[33]] <- cn289856705489998
mylist[[34]] <- cn289856706489998
mylist[[35]] <- cn289856722489998
mylist[[36]] <- cn289856736489998
mylist[[37]] <- cn289856737489998
mylist[[38]] <- cn289856738489998
mylist[[39]] <- cn289856739489998
mylist[[40]] <- cn289856741489998
mylist[[41]] <- cn289856744489998
mylist[[42]] <- cn289856761489998
mylist[[43]] <- cn289856762489998
mylist[[44]] <- cn289856764489998
mylist[[45]] <- cn289856765489998
mylist[[46]] <- cn289856766489998
mylist[[47]] <- cn289856799489998
mylist[[48]] <- cn289856830489998
mylist[[49]] <- cn289856831489998
mylist[[50]] <- cn289856834489998
mylist[[51]] <- cn289856838489998
mylist[[52]] <- cn289856842489998
mylist[[53]] <- cn289856845489998
mylist[[54]] <- cn289856847489998
mylist[[55]] <- cn289856857489998
mylist[[56]] <- cn289856858489998
mylist[[57]] <- cn289856867489998
mylist[[58]] <- cn289856874489998
mylist[[59]] <- cn289856886489998
mylist[[60]] <- cn289856892489998
mylist[[61]] <- cn289856918489998
mylist[[62]] <- cn289856927489998
mylist[[63]] <- cn289856930489998
mylist[[64]] <- cn289856934489998
mylist[[65]] <- cn289856935489998
mylist[[66]] <- cn289856936489998
mylist[[67]] <- cn289856937489998
mylist[[68]] <- cn289856938489998
mylist[[69]] <- cn289856939489998
mylist[[70]] <- cn289856940489998
mylist[[71]] <- cn289856942489998
mylist[[72]] <- cn289856949489998
mylist[[73]] <- cn289856950489998
mylist[[74]] <- cn289856951489998
mylist[[75]] <- cn289856955489998
mylist[[76]] <- cn289856956489998
mylist[[77]] <- cn289856960489998
mylist[[78]] <- cn289856966489998
mylist[[79]] <- cn289856968489998
mylist[[80]] <- cn289856971489998
mylist[[81]] <- cn289856977489998
mylist[[82]] <- cn289856985489998 
mylist[[83]] <- cn289856986489998
mylist[[84]] <- cn289856987489998
mylist[[85]] <- cn289856996489998
mylist[[86]] <- cn289856997489998
mylist[[87]] <- cn289857001489998
mylist[[88]] <- cn289857009489998
mylist[[89]] <- cn289857013489998
mylist[[90]] <- cn289857018489998
mylist[[91]] <- cn289857029489998
mylist[[92]] <- cn289857047489998
mylist[[93]] <- cn289857048489998
mylist[[94]] <- cn289857049489998
mylist[[95]] <- cn289857053489998
mylist[[96]] <- cn289857054489998
mylist[[97]] <- cn289857061489998
mylist[[98]] <- cn289857065489998
mylist[[99]] <- cn289857070489998

#county 25
cn289857349489998 <- read.rwl('./data/raw/UT Periodic/25/5/03001122.txt'); names(cn289857349489998)[1]<-'RWI'; cn289857349489998$Year<-as.numeric(row.names(cn289857349489998)); cn289857349489998$CN<-289857349489998
cn289857350489998 <- read.rwl('./data/raw/UT Periodic/25/5/03002122.txt'); names(cn289857350489998)[1]<-'RWI'; cn289857350489998$Year<-as.numeric(row.names(cn289857350489998)); cn289857350489998$CN<-289857350489998
cn289857352489998 <- read.rwl('./data/raw/UT Periodic/25/7/03001122.txt'); names(cn289857352489998)[1]<-'RWI'; cn289857352489998$Year<-as.numeric(row.names(cn289857352489998)); cn289857352489998$CN<-289857352489998
cn289857354489998 <- read.rwl('./data/raw/UT Periodic/25/10/02001202.txt'); names(cn289857354489998)[1]<-'RWI'; cn289857354489998$Year<-as.numeric(row.names(cn289857354489998)); cn289857354489998$CN<-289857354489998
cn289857357489998 <- read.rwl('./data/raw/UT Periodic/25/11/02002122.txt'); names(cn289857357489998)[1]<-'RWI'; cn289857357489998$Year<-as.numeric(row.names(cn289857357489998)); cn289857357489998$CN<-289857357489998
cn289857363489998 <- read.rwl('./data/raw/UT Periodic/25/16/02001122.txt'); names(cn289857363489998)[1]<-'RWI'; cn289857363489998$Year<-as.numeric(row.names(cn289857363489998)); cn289857363489998$CN<-289857363489998
cn289857371489998 <- read.rwl('./data/raw/UT Periodic/25/22/04003122.txt'); names(cn289857371489998)[1]<-'RWI'; cn289857371489998$Year<-as.numeric(row.names(cn289857371489998)); cn289857371489998$CN<-289857371489998
cn289857373489998 <- read.rwl('./data/raw/UT Periodic/25/23/02003122.txt'); names(cn289857373489998)[1]<-'RWI'; cn289857373489998$Year<-as.numeric(row.names(cn289857373489998)); cn289857373489998$CN<-289857373489998
cn289857382489998 <- read.rwl('./data/raw/UT Periodic/25/55/05003122.txt'); names(cn289857382489998)[1]<-'RWI'; cn289857382489998$Year<-as.numeric(row.names(cn289857382489998)); cn289857382489998$CN<-289857382489998
cn289857463489998 <- read.rwl('./data/raw/UT Periodic/25/376/04005122.txt'); names(cn289857463489998)[1]<-'RWI'; cn289857463489998$Year<-as.numeric(row.names(cn289857463489998)); cn289857463489998$CN<-289857463489998
cn289857470489998 <- read.rwl('./data/raw/UT Periodic/25/1005/03005122.txt'); names(cn289857470489998)[1]<-'RWI'; cn289857470489998$Year<-as.numeric(row.names(cn289857470489998)); cn289857470489998$CN<-289857470489998
cn289857480489998 <- read.rwl('./data/raw/UT Periodic/25/1013/05001122.txt'); names(cn289857480489998)[1]<-'RWI'; cn289857480489998$Year<-as.numeric(row.names(cn289857480489998)); cn289857480489998$CN<-289857480489998
cn289857484489998 <- read.rwl('./data/raw/UT Periodic/25/1023/03001122.txt'); names(cn289857484489998)[1]<-'RWI'; cn289857484489998$Year<-as.numeric(row.names(cn289857484489998)); cn289857484489998$CN<-289857484489998

#add to list
mylist[[100]] <- cn289857349489998
mylist[[101]] <- cn289857350489998
mylist[[102]] <- cn289857352489998
mylist[[103]] <- cn289857354489998
mylist[[104]] <- cn289857357489998
mylist[[105]] <- cn289857363489998
mylist[[106]] <- cn289857371489998
mylist[[107]] <- cn289857373489998
mylist[[108]] <- cn289857382489998
mylist[[109]] <- cn289857463489998
mylist[[110]] <- cn289857470489998
mylist[[111]] <- cn289857480489998
mylist[[112]] <- cn289857484489998
  
#county 47
cn289858423489998 <- read.rwl('./data/raw/UT Periodic/47/006/05001093.txt'); names(cn289858423489998)[1]<-'RWI'; cn289858423489998$Year<-as.numeric(row.names(cn289858423489998)); cn289858423489998$CN<-289858423489998
#cn289858425489998 <- read.rwl('./data/raw/UT Periodic/47/7/05009093.txt'); names(cn289858425489998)[1]<-'RWI'; cn289858425489998$Year<-as.numeric(row.names(cn289858425489998)); cn289858425489998$CN<-289858425489998
cn289858429489998 <- read.rwl('./data/raw/UT Periodic/47/016/03001093.txt'); names(cn289858429489998)[1]<-'RWI'; cn289858429489998$Year<-as.numeric(row.names(cn289858429489998)); cn289858429489998$CN<-289858429489998
cn289858430489998 <- read.rwl('./data/raw/UT Periodic/47/016/03003093.txt'); names(cn289858430489998)[1]<-'RWI'; cn289858430489998$Year<-as.numeric(row.names(cn289858430489998)); cn289858430489998$CN<-289858430489998
#cn289858440489998 <- read.rwl('./data/raw/UT Periodic/47/30/04001093.txt'); names(cn289858440489998)[1]<-'RWI'; cn289858440489998$Year<-as.numeric(row.names(cn289858440489998)); cn289858440489998$CN<-289858440489998
cn289858441489998 <- read.rwl('./data/raw/UT Periodic/47/031/01004093.txt'); names(cn289858441489998)[1]<-'RWI'; cn289858441489998$Year<-as.numeric(row.names(cn289858441489998)); cn289858441489998$CN<-289858441489998
#cn289858458489998 <- read.rwl('./data/raw/UT Periodic/47/60/04002093.txt'); names(cn289858458489998)[1]<-'RWI'; cn289858458489998$Year<-as.numeric(row.names(cn289858458489998)); cn289858458489998$CN<-289858458489998
cn289858542489998 <- read.rwl('./data/raw/UT Periodic/47/405/02006202.txt'); names(cn289858542489998)[1]<-'RWI'; cn289858542489998$Year<-as.numeric(row.names(cn289858542489998)); cn289858542489998$CN<-289858542489998
cn289858589489998 <- read.rwl('./data/raw/UT Periodic/47/440/03005202.txt'); names(cn289858589489998)[1]<-'RWI'; cn289858589489998$Year<-as.numeric(row.names(cn289858589489998)); cn289858589489998$CN<-289858589489998
cn289858610489998 <- read.rwl('./data/raw/UT Periodic/47/450/04004202.txt'); names(cn289858610489998)[1]<-'RWI'; cn289858610489998$Year<-as.numeric(row.names(cn289858610489998)); cn289858610489998$CN<-289858610489998
cn289858611489998 <- read.rwl('./data/raw/UT Periodic/47/451/01001122.txt'); names(cn289858611489998)[1]<-'RWI'; cn289858611489998$Year<-as.numeric(row.names(cn289858611489998)); cn289858611489998$CN<-289858611489998
cn289858612489998 <- read.rwl('./data/raw/UT Periodic/47/451/03001202.txt'); names(cn289858612489998)[1]<-'RWI'; cn289858612489998$Year<-as.numeric(row.names(cn289858612489998)); cn289858612489998$CN<-289858612489998
cn289858615489998 <- read.rwl('./data/raw/UT Periodic/47/452/05003202.txt'); names(cn289858615489998)[1]<-'RWI'; cn289858615489998$Year<-as.numeric(row.names(cn289858615489998)); cn289858615489998$CN<-289858615489998
cn289858637489998 <- read.rwl('./data/raw/UT Periodic/47/469/01002202.txt'); names(cn289858637489998)[1]<-'RWI'; cn289858637489998$Year<-as.numeric(row.names(cn289858637489998)); cn289858637489998$CN<-289858637489998

#add to list
mylist[[113]] <- cn289858423489998
mylist[[114]] <- cn289858429489998
mylist[[115]] <- cn289858430489998
mylist[[116]] <- cn289858441489998
mylist[[117]] <- cn289858542489998
mylist[[118]] <- cn289858589489998
mylist[[119]] <- cn289858610489998
mylist[[120]] <- cn289858611489998
mylist[[121]] <- cn289858612489998
mylist[[122]] <- cn289858615489998
mylist[[123]] <- cn289858637489998
#mylist[[114]] <- cn289858425489998
#mylist[[117]] <- cn289858440489998
#mylist[[119]] <- cn289858458489998

### put them together
verif_12_19 <- bind_rows(mylist)

#export
save(verif_12_19, file = "./data/formatted/verif_12_19.Rdata")


## check cnty1 for 'appropriate' ring-width values, i.e. no negative
summary(verif_12_19)
length(unique(verif_12_19$CN)) ## should be equal to last sequential number in 'mylist'
#123

#FIA ----
#taken from prac.R

#data from USU lab
UT_per <- read.csv("./data/raw/UT Periodic/Q-results.csv",header = T)
UT_rw <- verif_12_19

#merge TRE_CN and PLT_CN from tree table to UT_per
#use COUNTYCD, PLOT, SUBP, TREE

tree_cn <- tree %>%
  select(CN,PLT_CN,STATECD,COUNTYCD,PLOT,SUBP,TREE,SPCD)
colnames(tree_cn)[colnames(tree_cn)=="CN"] <- "TRE_CN"
tree_cn$MEASYEAR <- plot$MEASYEAR[match(tree_cn$PLT_CN, plot$CN)]
UT_per2 <- left_join(UT_per,tree_cn)

#did it work?
length(unique(UT_per2$CN[!is.na(UT_per2$TRE_CN)]))
#64
UT_per2 %>%
  filter(!is.na(TRE_CN)) %>%
  group_by(SPCD) %>%
  summarise(n = n())
#SPCD     n
# 93     46
# 122    14
# 202     4  

library(tidyverse)

#data from FIADB
plot <- read.csv("./data/raw/UT_PLOT.csv",header=T)
tree <- read.csv("./data/raw/UT_TREE.csv",header = T)
cond <- read.csv("./data/raw/UT_COND.csv",header = T)


#UT_cond - Stand density index for the condition, 
##FLDTYPCD_30, Site index for the condition, SIBASE Site index base age, SISP Site index species code

#grab covariates for model
covariates <- tree[,c("CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","CR","UNCRCD","SITREE","TPA_UNADJ")]

colnames(covariates)[colnames(covariates)=="CN"] <- "TRE_CN"
colnames(covariates)[colnames(covariates)=="DIA"] <- "DIA_t"
colnames(covariates)[colnames(covariates)=="SUBP"] <- "SUBP_t"

covariates$CONDID <- cond$CONDID[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$ASPECT <- cond$ASPECT[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$SLOPE <- cond$SLOPE[match(covariates$PLT_CN, cond$PLT_CN)]
#covariates$SDI <- conds$SDI_RMRS[match(covariates$PLT_CN, conds$PLT_CN)]
#all SDI are NA, need to calculate SDI
covariates$SDImax <- cond$SDIMAX_RMRS[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$SICOND <- cond$SICOND[match(covariates$PLT_CN, cond$PLT_CN)]

#BALIVE 
#covariates$BALIVE <- apply(X = grData_remeas[, c("PREV_PLT_CN", "PREV_CONDID")], 
#                              MARGIN = 1, # applies function to each row in grData_remeas
#                              FUN = function(x, conds.df) {
#                                conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
#                                                  conds.df$CONDID %in% x["PREV_CONDID"]]
#                              },
#                              conds.df = conds)
#grData_remeas[is.nan(grData_remeas$BALIVE), "BALIVE"] <- NA
covariates$BALIVE <- cond$BALIVE[match(covariates$PLT_CN, cond$PLT_CN)]

covariates$LAT <- plot$LAT[match(covariates$PLT_CN, plot$CN)]
covariates$LON <- plot$LON[match(covariates$PLT_CN, plot$CN)]
covariates$ELEV <- plot$ELEV[match(covariates$PLT_CN, plot$CN)]
covariates$MEASYEAR <- plot$MEASYEAR[match(covariates$PLT_CN, plot$CN)]
covariates$DESIGNCD <- plot$DESIGNCD[match(covariates$PLT_CN, plot$CN)]
covariates$SUBP_EXAM <- plot$SUBP_EXAMINE_CD[match(covariates$PLT_CN, plot$CN)]
#covariates$PREV_MEASYEAR <- plots$MEASYEAR[match(covariates$PREV_PLT_CN, plots$CN)]

UT_per <- UT_per %>%
  filter(!is.na(TRE_CN))
#data for glmm - ring widths
per_cov2 <- left_join(UT_per,covariates)
per_cov2[duplicated(per_cov2$TRE_CN),] #are there any dublicated trees?
#0

incr_percov2 <- left_join(UT_rw,per_cov2) #by CN
colnames(incr_percov2)[colnames(incr_percov2)=="RWI"] <- "RW"

#check species
length(per_cov2$TRE_CN[per_cov2$SPCD == 202]) #5
length(per_cov2$TRE_CN[per_cov2$SPCD == 122]) #15
length(per_cov2$TRE_CN[per_cov2$SPCD == 93]) #46

#is the dataset correct?
sum(is.na(incr_percov2$RW)) #0
sum(is.na(UT_rw$RW)) #0
length(unique(incr_percov2$TRE_CN)) #67

save(per_cov2,file = "./data/formatted/per_cov2.Rdata")
save(incr_percov2,file = "./data/formatted/incr_percov2.Rdata")

# Annualize ----
#see AnnualizeDBH.R

bratio_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                        #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                        b1 = c(0.9502,0.867,0.8967,0.890,0.890,0.9002,0.9502,0.9002,0.9625,0.9002,0.93789),
                        b2 = c(-0.2528, 0, -0.4448,0,0,-0.3089,-0.2528,-0.3089,-0.1141,-0.3089,-0.24096)) #can add more species later 

#annualized DBH
#DBH0 = DBH - k * DG , where k = 1/BRATIO and DG = 2 * RW  
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
    Curr_row <- N-1 #each time through subtract 1 and move down one row
    tree_df$DIA_C[N] <- tree_df$DIA_t[N] #dbh when year of ring width and measure year are equal
    while (Curr_row > 0 & !is.na(tree_df$DIA_C[Curr_row + 1])) { #loop will stop when it gets to the end of data for that tree
      DIA_1 <- tree_df$DIA_C[Curr_row+1] #or DIA_t[N] for the first round
      RW1 <- tree_df$RW[Curr_row+1] 
      #TODO convert ring width from mm to inches
      RW1 = RW1 * 0.0393701
      b1 <- bratio_df$b1[bratio_df$species == Species]
      b2 <- bratio_df$b2[bratio_df$species == Species]
      tree_df$DIA_C[Curr_row] <- DIA_1 - ((2*RW1)/(b1+b2/DIA_1))
      if(tree_df$DIA_C[Curr_row] < 1){
        tree_df$DIA_C[Curr_row] <- NA
      }
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row - 1 
    }
  }
  return(tree_df$DIA_C)
}

incr_imputed2 <- incr_percov2 %>%
  group_by(TRE_CN) %>% #for each tree calculate dbh
  arrange(Year) %>%
  mutate(DIA_C = calculateDIA(TRE_CN = TRE_CN,DIA_t,MEASYEAR,Year,RW,SPCD))

#filter for trees with back calculated DBH
length(unique(incr_imputed2$TRE_CN)) #67
incr_imputed2 <- incr_imputed2 %>%
  filter(!is.na(DIA_C)) #filter for >5" later
length(unique(incr_imputed2$TRE_CN)) # 64

length(unique(incr_imputed2$TRE_CN[incr_imputed2$SPCD == 202])) #5
length(unique(incr_imputed2$TRE_CN[incr_imputed2$SPCD == 122])) #14
length(unique(incr_imputed2$TRE_CN[incr_imputed2$SPCD == 93])) #45

# Unite ----
#add new data to previous data 
#to calculate density parameters
load(file = "./data/formatted/incr_imputed.Rdata")

incr_imputed <- bind_rows(incr_imputed,incr_imputed2)

save(incr_imputed2,file = "./data/formatted/incr_imputed.Rdata")
save(incr_imputed,file = "./data/formatted/incr_imputed.Rdata")

# Missing ----
#see MissingDBH.R

incr_imputed <- incr_imputed %>%
  group_by(TRE_CN) %>%
  mutate(BAR = (lag(DIA_C)^2)/DIA_C^2)

#create dataframe of trees without increment cores in plots with trees that have increment cores
plot_rw <- unique(incr_imputed$PLT_CN) #475
tree_rw <- unique(incr_imputed$TRE_CN) #568
miss_data <- tree[(tree$PLT_CN %in% plot_rw) & !(tree$CN %in% tree_rw),c("CN","PLT_CN","SUBP","SPCD","DIA","TPA_UNADJ")]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including trees with increment data; 508
colnames(miss_data)[colnames(miss_data)=="CN"] <- "TRE_CN"
colnames(miss_data)[colnames(miss_data)=="DIA"] <- "DIA_t"
colnames(miss_data)[colnames(miss_data)=="SUBP"] <- "SUBP_t"
miss_data$MEASYEAR <- plot$MEASYEAR[match(miss_data$PLT_CN, plot$CN)]
miss_data$DESIGNCD <- plot$DESIGNCD[match(miss_data$PLT_CN, plot$CN)]
miss_data$CONDID <- cond$CONDID[match(miss_data$PLT_CN, cond$PLT_CN)]
miss_data$Year <- NA #important for mutate(Year) to work
miss_data$BAR_av <- NA
miss_data$DIA_C <- NA

#check
length(plot_rw) #475
length(unique(miss_data$PLT_CN)) #474

#empty (year&DIA_C) dataframe?
miss_data <- miss_data %>% 
  slice(rep(1:n(), each = 40)) %>% #repeat each row 40 times
  group_by(TRE_CN) %>%
  mutate(Year = c((MEASYEAR[1]-39):MEASYEAR[1])) %>% #40 yrs is arbitrary; model will likely go back 30 yrs
  ungroup()

#match BAR_av from incr_imputed to miss_data using plot, species and year information
#match function does not work..why? NA values?
#for loop works but takes a long time
for(i in 1:nrow(miss_data)){ #miss_data includes all trees without increment data
  species <- miss_data$SPCD[i]
  if(species %in% c(106,202,122,93)){ #focal species
    BAR <- incr_imputed$BAR[incr_imputed$PLT_CN == miss_data$PLT_CN[i] &
                              incr_imputed$SPCD == miss_data$SPCD[i]]
    #first average within a plot for a specific species (over years)
    if(length(BAR) == 0){
      BAR <- incr_imputed$BAR[incr_imputed$SPCD == miss_data$SPCD[i]]
      #if no species on the plot, then just average species across plots
    }
  }
  if(!(species %in% c(106,122,202,93))){ #all other, non-focal species 
    #includes 15, 19, 65, 66, 96, 108, 102, 113, 322, 475, 746, 749, & 814
    BAR <- incr_imputed$BAR[!(species %in% c(106,122,202,93))]
  }
  miss_data$BAR_av[i] <- mean(BAR, na.rm = TRUE)
}

length(which(is.na(miss_data$BAR_av)))
#0

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

unique(miss_data_imputed$CONDID) #1

save(miss_data_imputed,file = "./data/formatted/miss_data_imputed.Rdata")

#join two dataframes to compute stand variables
#first - trees back calculated with tree rings
##incr_imputed
#second - trees back calculated with BAR
##miss_data_imputed
density_data <- bind_rows(incr_imputed,miss_data_imputed)

save(density_data,file = "./data/formatted/density_data.Rdata")

# Density ----
#follow CCF.R, BAL.R, & CR.R

# PRISM ----
#see PRISM.R
#try to do only new trees and combine with old dataset
load('./data/formatted/per_cov2.Rdata')

#trees with increment cores
load('./data/formatted/incr_calcov.Rdata')
#data after tree and stand covariates calculated and filtered

library(raster)

#wide format
final_trees <- per_cov2[per_cov2$TRE_CN %in% incr_calcov$TRE_CN,]

ut_tree_spat <- SpatialPointsDataFrame(coords = cbind(final_trees$LON, final_trees$LAT), 
                                       data = final_trees, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, ut_tree_spat) # this step takes about 8 minutes each (laptop)
tmin.extr <- raster::extract(tmin, ut_tree_spat)
tmax.extr <- raster::extract(tmax, ut_tree_spat)

#Jan 1895 - Dec 2000
# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1:1272] #1895 - 2000
tmin.extr <- tmin.extr[, 1:1272]
tmax.extr <- tmax.extr[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr2 <- as.data.frame(ppt.extr)
tmin.extr2 <- as.data.frame(tmin.extr)
tmax.extr2 <- as.data.frame(tmax.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr2) <- paste0("ppt_", colNames)
colnames(tmin.extr2) <- paste0("tmin_", colNames)
colnames(tmax.extr2) <- paste0("tmax_", colNames)

# Export climate data
processed.path <- "./data/formatted/"
write.csv(ppt.extr2, paste0(processed.path,"ppt_extr2.csv"), row.names = F)
write.csv(tmin.extr2, paste0(processed.path,"tmin_extr2.csv"), row.names = F)
write.csv(tmax.extr2, paste0(processed.path,"tmax_extr2.csv"), row.names = F)

# Unite 2 ----

#forest service predicts change in squared inside bark diameter
#TODO convert ring width from mm to inches
#RW = RW * 0.0393701
incr_calcov <- incr_calcov %>%
  mutate(dds = (2*RW*0.0393701)^2)
save(incr_calcov,file = "./data/formatted/incr_calcov.Rdata")

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr2 <- read.csv(paste(processed.path,"ppt_extr2.csv",sep=''), header = T)
tmin.extr2 <- read.csv(paste(processed.path,"tmin_extr2.csv",sep=''), header = T)
tmax.extr2 <- read.csv(paste(processed.path,"tmax_extr2.csv",sep=''), header = T)

#for all trees
#same dataset LAT/LONG used to extract climate
new_trees <- unique(final_trees$TRE_CN)

clim_col <- function(PRISM,clim_var,TRE_CN){
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
  
  prism_id <- colnames(ppt.extr2)
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  #clim_var <- str_sub(prism_id,1,4) #1:5 for tmin_ and tmax_
  climate_list <- lapply(climate_list, setNames, nm = paste0(clim_var,months))
  
  #years <- seq(from=start_yr,to=end_yr,by=1)
  prism_yr <- unique(sub('\\D*(\\d{4}).*', '\\1', prism_id))
  names(climate_list) <- as.integer(prism_yr)
  
  climate_stack <- bind_rows(climate_list, .id = "Year")
  climate_stack$TRE_CN <- rep(TRE_CN,times=n)
  return(climate_stack)
}
#could make function for all climate variables
#find way to extract start_yr and end_yr
#lubridate?

new_ppt <- clim_col(ppt.extr2,clim_var = "ppt_",TRE_CN = new_trees)
new_tmin <- clim_col(tmin.extr2,clim_var = "tmin_",TRE_CN = new_trees)
new_tmax <- clim_col(tmax.extr2,clim_var = "tmax_",TRE_CN = new_trees)

new_clim <- full_join(new_ppt,new_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,new_tmax,by = c("TRE_CN","Year"))

#add 'old' data from glmm_data.R
load('./data/formatted/data_all.Rdata')
old_clim <- data_all %>%
  ungroup() %>%
  select(TRE_CN, Year, 
         ppt_Jan, ppt_Feb, ppt_Mar, ppt_Apr, ppt_May, ppt_Jun,
         ppt_Jul, ppt_Aug, ppt_Sep, ppt_Oct, ppt_Nov, ppt_Dec,
         tmax_Jan, tmax_Feb, tmax_Mar, tmax_Apr, tmax_May, tmax_Jun,
         tmax_Jul, tmax_Aug, tmax_Sep, tmax_Oct, tmax_Nov, tmax_Dec,
         tmin_Jan, tmin_Feb, tmin_Mar, tmin_Apr, tmin_May, tmin_Jun,
         tmin_Jul, tmin_Aug, tmin_Sep, tmin_Oct, tmin_Nov, tmin_Dec)

all_clim <- bind_rows(old_clim,new_clim)

all_clim$Year <- as.integer(all_clim$Year)
data_all <- full_join(incr_calcov,all_clim, by = c("TRE_CN","Year"))

save(data_all, file = "./data/formatted/data_all2.Rdata")

#check
length(unique(data_all$TRE_CN)) #568

# Climate ----
# refer to glmm_data.R
#make seasonal climate variables
#refer to climate-growth analysis
#first filter by species

#DF
data_all_df <- data_all %>%
  filter(SPCD == 202)
length(unique(data_all_df$TRE_CN)) #136

#total ppt
#1 month: pOct,pDec, Jun, Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOct = lag(ppt_Oct),
         ppt_pDec = lag(ppt_Dec))

#3 month: pJun-pAug, pAug-pOct, May-Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunAug = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug),
         ppt_pAugOct = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct),
         ppt_MayJul = ppt_May + ppt_Jun + ppt_Jul)

#6 month + : pNov, Jul, wateryr
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunNov = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov),
         ppt_FebJul = ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul,
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#16 months: previous June to current Sept
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: Feb, Jul
#Feb tmin
#Jul tmax

#3 month: tmax_Jun-Aug, tmax_pJul-pSep, tmin_Jan-Mar
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pJulSep = (lag(tmax_Jul) + lag(tmax_Aug) + lag(tmax_Sep))/3,
         tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3,
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3,
         tmax_JanMar = (tmax_Jan + tmax_Feb + tmax_Mar)/3)

#6 month + : Jun, Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JanJun = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun)/6,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6,
         tmin_JanJun = (tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr + tmin_May + tmin_Jun)/6)

save(data_all_df, file = "./data/formatted/data_all_df.Rdata")

##PP
data_all_pp <- data_all %>%
  filter(SPCD == 122)
length(unique(data_all_pp$TRE_CN)) #87

#total ppt
#1 month: ,pDec, Jun, Jul, pOct
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOct = lag(ppt_Oct),
         ppt_pDec = lag(ppt_Dec))

#3 month: pAug-pOct, pOct-pDec, pNov-Jan, May-Jul
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOctDec = lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec),
         ppt_pAugOct = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct),
         ppt_pNovJan = lag(ppt_Nov) + lag(ppt_Dec) + ppt_Jan,
         ppt_MayJul = ppt_May + ppt_Jun + ppt_Jul)

#6 month + : Jan, wateryr
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pAugJan = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec) + ppt_Jan,
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         ppt_pAugJul = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul)

#16 months: previous June to current Sept
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: Jun
#Jun tmax

#3 month: tmax_Jun-Aug
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3)

save(data_all_pp, file = "./data/formatted/data_all_pp.Rdata")

#ES
data_all_es <- data_all %>%
  filter(SPCD == 93)
length(unique(data_all_es$TRE_CN)) #95

#total ppt
#1 month: Jul, Apr

#3 month: pJun-pAug, Jun-Aug, pJul-pSep, pOct-pDec
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunAug = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug),
         ppt_pJulSep = lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep),
         ppt_JunAug = ppt_Jun + ppt_Jul+ ppt_Aug,
         ppt_pOctDec = lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec))

#6 month + : pNov, wateryr
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunNov = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov),
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         ppt_pAugJul = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#16 months: previous June to current Sept
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

  #average temp
#1 month: tmax_pAug, Feb, Mar
#Feb tmin
#Mar tmax
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pAug = lag(tmax_Aug))

#3 month: tmin_Feb-Apr, tmax_pJul-pSep, tmin_Jan-Mar
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pJulSep = (lag(tmax_Jul) + lag(tmax_Aug) + lag(tmax_Sep))/3,
         tmin_FebApr = (tmin_Feb + tmin_Mar + tmin_Apr)/3,
         tmax_FebApr = (tmax_Feb + tmax_Mar + tmax_Apr)/3,
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3)

#6 month + : tmin_Apr
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmin_pNovApr = (lag(tmin_Nov) + lag(tmin_Dec) + tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr)/6,
         tmax_pNovApr = (lag(tmax_Nov) + lag(tmax_Dec) + tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr)/6)

save(data_all_es, file = "./data/formatted/data_all_es.Rdata")

#filtering
#by species
#only going back 30 yrs - 1958
#need response: RW, dds
#fixed effects: DBH (DIA), CR/CR_weib, PCCF, CCF, BAL, Climate
#random effect: TRE_CN, Year


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

library(tidyverse)

#data from USU lab
UT_per <- read.csv("./data/raw/UT Periodic/Q-results.csv",header = T, stringsAsFactors = F)
UT_rw <- verif_12_19

#connect to SQLite database to get metadata
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
tree <- tbl(UT_FIA, sql("SELECT * FROM TREE")) %>%
  collect()
plot <- tbl(UT_FIA, sql("SELECT * FROM PLOT")) %>%
  collect()
colnames(plot)[colnames(plot)=="CN"] <- "PLT_CN"
cond <- tbl(UT_FIA, sql("SELECT * FROM COND")) %>%
  collect()

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

sample_list<-lapply(dir('RData/',full.names = T),function(x) {
  tmp<-readRDS(x)
  if(x=='RData/Sample_2_Crop_5_10.RData'){
  tmp<-data.frame(Sample=regmatches(x,regexpr('[[:word:]]+(?=[.])',x,perl=TRUE)),
                    Time=c(seq(0,120,1),seq(160,360,20),seq(420,7200,600)),
                    do.call(rbind,tmp))
  }else{
  if(x=='RData/Sample_6_Nat_0_5.RData'){
    tmp<-data.frame(Sample=regmatches(x,regexpr('[[:word:]]+(?=[.])',x,perl=TRUE)),
                    Time=c(seq(0,120,1),seq(140,360,20),seq(420,4200,600)),
                    do.call(rbind,tmp))
    }else{
    if(x=='RData/Sample_30_Crop_5_10.RData'){
      tmp<-data.frame(Sample=regmatches(x,regexpr('[[:word:]]+(?=[.])',x,perl=TRUE)),
                      Time=c(seq(0,120,1),seq(140,360,20),seq(420,6600,600)),
                      do.call(rbind,tmp))
      }else{
    
    tmp<-data.frame(Sample=regmatches(x,regexpr('[[:word:]]+(?=[.])',x,perl=TRUE)),
                  Time=c(seq(0,120,1),seq(140,360,20),seq(420,7200,600)),
                  do.call(rbind,tmp))
      }
    }
  }
})

sapply(sample_list,names)

names(sample_list[[5]])[7]<-'X5'
names(sample_list[[7]])[7]<-'X5'
names(sample_list[[8]])[7]<-'X5'
names(sample_list[[11]])[7]<-'X5'
names(sample_list[[15]])[7]<-'X5'
names(sample_list[[13]])[7]<-'X5'

Models_John <- data.frame(do.call(rbind,sample_list))

require(ggplot2)
require(reshape2)

Models_John<-melt(Models_John,id.vars = c('Time','Sample'))

ggplot(Models_John,aes(Time,value))+
  geom_line(aes(colour=variable),size=2)+
  facet_wrap(~Sample)+
  scale_x_log10()


Models_John <- data.frame(do.call(rbind,sample_list))
names(Models_John)<-c("Sample","Time","Ag_1","Ag_2","Ag_3","Ag_4","Ag_5")
write.csv(Models_John,file='Files/Disaggregation_test.csv')


require(EBImage)
require(plyr)
require(ggplot2)
require(reshape2)
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
sample_list<-lapply(sample_list,function(x) {
  colnames(x) <-c("Sample","Time","Ag_1","Ag_2","Ag_3","Ag_4","Ag_5")
  x})

####Convert to cm by reading standards####

standards <- data.frame(Standard=paste0('Camera',seq(1,4)),Location=c(
                        'Images/Sample_37_Nat_5_10/DSC_3939.JPG',
                        'Images/Sample_38_Nat_0_5/DSC_0030.JPG',
                        'Images/Sample_40_Nat_0_5/DSC_0001.JPG',
                        'Images/Sample_46_Nat_0_5/DSC_0001.JPG'))

pixels_per_c2m <- lapply(standards[,2],function(x){
  img<-readImage(as.character(x)) 
#   display(img)
  num_agregates <- 1
  cutting_area<-list(c(0:740),c(500:1500))
  a<-!img[cutting_area[[1]],cutting_area[[2]],3]>.8
  #   display(a,method='raster')
  #Apply some filters for taking the '0' values inside de agregate#
  y <- closing(a, makeBrush(5, shape='disc'))
  #check#
  # display(y,method='raster')
  ## Recognize and label each agregate as a differen object##
  z <- bwlabel(y)
  agregates<-as.numeric(names(sort(table(z),decreasing = T))[0:num_agregates+1])
  Ag_count <-z*(matrix(z%in%c(agregates),ncol=dim(z)[2]))
  
  ids<-unique(as.factor(Ag_count))
  for (i in 1:num_agregates){
    Ag_count[Ag_count==ids[i]]<-i-1
  }
  ## re-color agregates in colors##
  cols = c('black', terrain.colors(n=num_agregates))
  Ag_count_colored = Image(cols[1+Ag_count], dim=dim(Ag_count))
  result<-list()
  result$image<-Ag_count_colored
  #check#
  #   display(img,method = 'raster')
  #   display(Ag_count_colored,method = 'raster')
  result$area<-computeFeatures.shape(Ag_count)[,1]
result
})

#check#
#lapply(pixels_per_c2m,function(x) display(x[[1]],method = 'raster'))
cameras<-read.csv('Images/Calibration_samples.csv')[,1:5]
cameras$Sample <- as.character(paste('Sample',cameras$Site,cameras$System,cameras$top,cameras$bottom,sep='_'))

pixels<-data.frame(Camera=1:4,equivalent=do.call(c,lapply(pixels_per_c2m,function(x) x[[2]])))
standards <- join(do.call(rbind,sample_list),cameras,'Sample')
standards <- join(standards,pixels,'Camera')
standards<-split(standards,standards$Sample)

standards<-lapply(standards,function(x) {
  x$Mean_Diam <- sqrt((rowMeans(x[,4:8])/x$equivalent)/pi)*2
  x$Initial_size <-x$Mean_Diam[1]
  x$SD <- adply(sqrt(((x[,4:8])/x$equivalent)/pi)*2,1,sd)[,6]
  x
})

standards<-do.call(rbind,standards)
standards$ranges<-round(standards$Initial_size,1)

ggplot(standards,aes(Time,((Mean_Diam-Initial_size)/Initial_size),group=Sample))+
geom_line(aes(colour=top),size=2)+
scale_x_log10()+
labs(ylab('Cm2'))+
facet_wrap(~ranges)+
theme_bw()

# write.csv(standards[,-c(3:7,13)],file='Files/Disaggregation_test.csv')


DATA <-split(standards,standards$Sample)

DATA <-lapply(DATA,function(x) {
  x$Mean_Diam_Norm<-x$Mean_Diam/x$Mean_Diam[which.max(x$Mean_Diam)]
  x$Time <- x$Time+1
  x$Time_log <-log(x$Time)
  #sampling in logarithmic intervals
#   x<-x[x$Time%in%c(c(2,7,20,54,161,421,1621,2821),as.integer(exp(x$Time_log[length(x$Time_log)]))),]
  x
})


####Take the sample 18 out ... something bad happened in that one####
DATA <- DATA[-18]

###Fitting exponential curves####

values_expon<-lapply(DATA,function(x) {
  results<-lm(x$Mean_Diam ~ x$Time_log)
  results$fitted.values})

####Fitting sigmoid curves

sigmoid_fit <-function(data,par){
#   with(data,sqrt(mean(Mean_Diam_Norm-(par[4]+((par[1]-par[4])/1+(Time_log/par[3])^par[2])))^2))#first function
  with(data,sqrt(mean(Mean_Diam-(par[1] / (1 + exp(-par[2] * (Time - par[3]))))))) #second_function
}
#optimize fit based in RMSE function previously created#
parameters<-lapply(DATA,function(x){
  sigm_fit<-optim(par=c(0,25,50),sigmoid_fit,data=x)
})

#Predict values based in optimized parameters#
values_sigmoid<-mapply(function(x,par){
#   par$par[4]+((par$par[1]-par$par[4])/1+(x$Time_log/par$par[3])^par$par[2]) #first function
  par$par[1] / (1 + exp(-par$par[2] * (x$Time - par$par[3]))) #second function
  
},DATA,parameters,SIMPLIFY=F)

###Fitting glm logit curves####
# 
# values_logit<-lapply(DATA,function(x) {
#   results<-glm(x$Mean_Diam_Norm ~ x$Time_log,family = quasibinomial(link="logit"))
#   results$fitted.values})


values_fit<-lapply(1:25,function(x) {
#   rmse_logit <- sqrt(mean((DATA[[x]]$Mean_Diam_Norm-values_logit[[x]])^2))
  rmse_expo <- sqrt(mean((DATA[[x]]$Mean_Diam-values_expon[[x]])^2))
  rmse_sigmoid <- sqrt(mean((DATA[[x]]$Mean_Diam-values_sigmoid[[x]])^2))
  if(rmse_sigmoid>rmse_expo) {
    fit <-values_expon[[x]]
    fit<-data.frame(Type='Exponential',fit)
  } else{
    fit <-values_sigmoid[[x]]
    fit<-data.frame(Type='Sigmoid',fit)
    } 
})

standards<-data.frame(do.call(rbind,DATA),do.call(rbind,values_fit))

ggplot(standards,aes(Time_log,Mean_Diam,group=Sample))+
  geom_point(size=2,colour='blue')+
  labs(y='Cm2',x='Seconds in log scale')+
  facet_wrap(~System)+
  theme_bw()+
  geom_line(aes(Time_log,fit,colour=System))

DATA <-split(standards,standards$Sample)

DATA <-lapply(DATA,function(x) {
  tmp<-x$fit[1]
  x$fit_dif <- (x$fit-tmp)/tmp
  x})

standards<-data.frame(do.call(rbind,DATA),do.call(rbind,values_fit))

ggplot(standards,aes(Time_log,(Mean_Diam-Initial_size)/Initial_size,group=Sample))+
  geom_point(aes(Time_log,fit_dif,colour=System))+
  labs(y='Cm2',x='Seconds in log scale')+
  facet_wrap(~System)+
  theme_bw()
  


# 
# for (i in 1:25){
# plot(main=standards$Type[standards$Sample==as.character(unique(standards$Sample)[i])][1],
#      standards$Mean_Diam_Norm[standards$Sample==as.character(unique(standards$Sample)[i])])
# lines(standards$fit[standards$Sample==as.character(unique(standards$Sample)[i])])
# }

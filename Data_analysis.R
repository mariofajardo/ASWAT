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

####Homogenize time lapses####
#weird behaviour of adply..solved anyway
DATA<-lapply(sample_list,function(x) data.frame(x[,1:2],Size=as.numeric(rowMeans(x[1,-c(1:2)])),Mean=rowMeans(x[,-c(1:2)]),SD=adply(x[,-c(1:2)],1,function(y) sd(y))[,4]))

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
cameras$Sample <- paste('Sample',cameras$Site,cameras$System,cameras$top,cameras$bottom,sep='_')

pixels<-data.frame(Camera=1:4,equivalent=do.call(c,lapply(pixels_per_c2m,function(x) x[[2]])))
standards <- merge(data.frame(do.call(rbind,DATA)),cameras,'Sample')
standards <- merge(standards,pixels,'Camera')
standards$Size<-round(standards$Size/standards$equivalent,1)

ggplot(standards,aes(Time,sqrt((((Mean-Size)/Size)/equivalent)/pi),group=Sample))+
geom_line(aes(colour=System),size=2)+
scale_x_log10()+
labs(ylab('Cm2'))+
facet_wrap(~Size)+
theme_bw()



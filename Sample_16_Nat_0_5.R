library(EBImage)
library(CRImage)
require(plyr)
require(foreach)
require(doSNOW)
require(RevobaseEnt)
require(Revobase)

ncluster <-4
setMKLthreads(n=2)
cl <- makeCluster(ncluster) 
registerDoSNOW(cl)

FILES<-dir(path = 'Images/Sample_16_Nat_0_5/',full.names = T)[1:6]

areas<-foreach(filei=FILES,.packages = 'EBImage') %dopar%  {
  img<-readImage(filei) 
  #   display(img)
  num_agregates <- 5
  cutting_area<-list(c(1000:2000),c(800:1600))
  a<-!img[cutting_area[[1]],cutting_area[[2]],3]>.8
  #   display(a,method='raster')
  #Apply some filters for taking the '0' values inside de agregate#
  y <- closing(a, makeBrush(5, shape='disc'))
  #check#
  #   display(y,method='raster')
  ## Recognize and label each agregate as a differen object##
  z <- bwlabel(y)
  agregates<-as.numeric(names(sort(table(z),decreasing = T))[0:num_agregates+1])
  Ag_count <-z*(matrix(z%in%c(agregates),ncol=dim(z)[2]))
  
  ids<-unique(as.factor(Ag_count))
  for (i in 1:num_agregates){
    Ag_count[Ag_count==ids[i]]<-i-1
  }
  ## re-color agregates in colors##
  cols = c('black', rainbow(n=num_agregates))
  result<-list()
  result$Ag_count_colored = Image(cols[1+Ag_count], dim=dim(Ag_count),colormode = 'Color')
  result$original <- img
  result
  #check#
  #   display(img,method = 'raster')
  #   display(Ag_count_colored,method = 'raster')
  #     computeFeatures.shape(Ag_count)[,1]
}

stopCluster(cl)

pdf(file = 'Plots/Check_slacking.pdf')
lapply(areas, function(x) {
  display(x[[1]],method='raster')
  display(x[[2]],method='raster')
})
dev.off()
setwd('Plots/')
shell.exec('Check_slacking.pdf')
setwd('..')
#####now do all of them ####
ncluster <-4
setMKLthreads(n=2)
cl <- makeCluster(ncluster) 
registerDoSNOW(cl)

FILES<-dir(path = 'Images/Sample_16_Nat_0_5/',full.names = T)

areas<-foreach(filei=FILES,.packages = 'EBImage') %dopar%  {
  img<-readImage(filei) 
  # display(img)
  num_agregates <- 5
  cutting_area<-list(c(1000:2000),c(800:1600))
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
  result<-list()
  result$Ag_count_colored = Image(cols[1+Ag_count], dim=dim(Ag_count))
  result$original <- img
  result
  #check#
  #   display(img,method = 'raster')
  #   display(Ag_count_colored,method = 'raster')
  computeFeatures.shape(Ag_count)[,1]
  
}

stopCluster(cl)

# areas[[1]]<-areas[[1]][c(1,2,3,5,4)];names(areas[[1]])<-c(1:5)


saveRDS(areas,file = 'RData/Sample_16_Nat_0_5.RData')
num_agregates <- 5
observations<-c(seq(0,120,1),seq(140,360,20),seq(420,7200,600))
for (i in 1:num_agregates){
  Final <-do.call(rbind,areas)  
  Final <- c(0,sapply(2:nrow(Final),function(x) (Final[x,i]-Final[1,1])/Final[1,1])) 
  plot(observations,Final,type='b',main=paste0('agregate',i),lwd=1,xlab='seconds')
}

Area_aver <- data.frame(Time=observations,do.call(rbind,areas))

require(ggplot2)
require(reshape2)

Area_aver<-melt(Area_aver,id.vars = 'Time')
ggplot(Area_aver,aes(Time,value,colour=variable))+
  geom_point(size=3)+
  scale_x_log10()+
  labs(x='Time (log_scale)')
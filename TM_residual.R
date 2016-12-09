library(ggplot2)
library(gplots)
library(raster)

path='C:/Users/yiqi7710/work_CU/data/Modified_2/'
Fun=c('Closest Centroid', 'Weighted Average','BiLinear','BiQuadratic','BiCubic','NN','TIN')
study_area=c('Louisiana','Colorado','NC','Nebraska','Washington','Texas')
Tr_Indx=c('DEMST','TRP','TRI','SV')
res_list=c(3,9,27,81,653)
tranID=1
agg_interval=10

res=9
i=2

GetAverage <- function(x,y,Ma){
  Len=length(Ma);
  if(Len %% 2 == 0){
    Hei=Len/2
  }
  else{
    Hei=(Len+1)/2
  }

  Lx=x-(y-1);
  Ly=1;
  Rx=x+(y-1);
  Ry=1;
  
#  print(paste('x: ',x))
#  print(paste('y: ',y))
  
  
  if((y>x) | (y>(Len+1-x))){
    av=NaN;
  }
  
  else{
    #total=0;
    num=Rx-Lx+1;
    # for (a in Lx:Rx){
    #   total=total+Ma[a];
    # }
    av=mean(Ma[Lx:Rx])
    
  }
  
  return(av)

}
GetAvTM_file <- function(csvfile,fig_title,column,agg_interval,plotting){
  
  data1=read.csv(csvfile)
  resids=data1[10:20]
  len=length(data1[[1]])
  agg_len=as.integer(len/agg_interval)

  
  agg1=resids[column]
  agg2=agg1[1:(agg_len*agg_interval),1]
  dim(agg2)=c(agg_interval,agg_len)
  agg2=colMeans(agg2)
  
  if (len!=agg_len*agg_interval){
    remain_mean=mean(agg1[(agg_len*agg_interval+1):len,1])
    agg2=append(agg2,remain_mean)
  }
  
  Series=agg2;

  Len=length(agg2)
  if (Len %% 2 ==0){
    Hei=Len/2
  }
  else{
    Hei=(Len+1)/2
  }
  
  AvTM=matrix(0, nrow=Len,ncol=Hei)

  AvTM[,1]=Series
  for (y in 2: Hei){
    print(paste('processing level: ',y))
    for (x in 1:Len){
      AvTM[x,y]=GetAverage(x,y,Series)
    }
  }
  return(abs(AvTM))
}
GetAvTM_vector <- function(Series,fig_title,column,agg_interval,plotting){
  
  len=dim(series)[1]
  agg_len=as.integer(len/agg_interval)
  
  
  agg1=series
  agg2=agg1[1:(agg_len*agg_interval),1]
  dim(agg2)=c(agg_interval,agg_len)
  agg2=colMeans(agg2)
  
  if (len!=agg_len*agg_interval){
    remain_mean=mean(agg1[(agg_len*agg_interval+1):len,1])
    agg2=append(agg2,remain_mean)
  }
  
  Series=agg2;
  
  Len=length(agg2)
  if (Len %% 2 ==0){
    Hei=Len/2
  }
  else{
    Hei=(Len+1)/2
  }
  
  AvTM=matrix(0, nrow=Len,ncol=Hei)
  
  AvTM[,1]=Series
  Series=unlist(Series)
  
  for (y in 2: Hei){
    print(paste('processing level: ',y))
    ptm <- proc.time()[3]
    # AvTM[,y]=mapply(GetAverage, 1:Len, MoreArgs = list(y,Series))
    for (x in 1:Len){
      AvTM[x,y]=GetAverage(x,y,Series)
    }
    print(paste0('time used for this level: ',(proc.time()[3]-ptm)))

  }
  return(abs(AvTM))
}




#for (res in res_list){
#  for (i in 2:7){
input_file=paste0(path,study_area[4],'/sample_points/','dist_tran1_res',toString(res),'.csv')
fig_title=paste0('Residual of ', Fun[i],' Transect ',toString(tranID), ' at ', toString(res),'m resolution in ',study_area[4])
#TM=GetAvTM_file(input_file,fig_title,i,agg_interval,true);

data1=read.csv(input_file)
resids=data1[10:20]
Series=resids[1]
TM2=GetAvTM_vector(Series,fig_title,agg_interval,TRUE)

TM_stdv=sd(TM,na.rm=TRUE)
TM_mean=mean(TM,na.rm=TRUE)
Max=max(TM,na.rm=TRUE)
Min=min(TM,na.rm=TRUE)
Up2stdv=TM_mean+2*TM_stdv
Low2stdv=TM_mean-2*TM_stdv

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

br=seq(Low2stdv,Up2stdv,(Up2stdv-Low2stdv)/100)
br1=c(Min,br,Max)

#
cPalette=colorRamp(brewer.pal(11,"RdYlBu"))
r_TM=t(TM)[ncol(TM):1,]
plot(raster(r_TM),breaks=br1,col=jet.colors(102),legend=FALSE)


#print(TM)
#  }
#}

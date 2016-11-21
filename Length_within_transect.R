#Plot the moving average of residuals within a transect 

library(forecast)
library(reshape2)
library(ggplot2)
sa=vector(mode="list", length=6)
names(sa) <- c("Colorado", "Louisiana", "NC","Nebraska", "Texas", "Washington")
sub_dir='/sample_points/'

sa[[1]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/Colorado',sub_dir)
sa[[2]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/Louisiana',sub_dir)
sa[[3]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/NC',sub_dir)
sa[[4]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/Nebraska',sub_dir)
sa[[5]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/Texas',sub_dir)
sa[[6]]=paste0('C:/Users/yiqi7710/work_CU/data/Modified_2/Washington',sub_dir)

res_co=c(3,9,27,82,925)
res_la=c(3,9,29,84,919)
res_nc=c(3,9,27,84,959)
res_ne=c(3,9,27,81,653)
res_tx=c(3,9,27,85,941)
res_wa=c(3,10,30,100,662)

study_area='Nebraska'
tran_FID=1

for (res in res_ne){
  #Plot residual
  fn_ne_tran1_res9=paste0(sa$Nebraska,'dist_tran',tran_FID, '_res', res,'.csv')
  ne_tran1_res9=read.csv(fn_ne_tran1_res9)
  
  order=500
  
  Pt_indx=seq.int(nrow(ne_tran1_res9))
  Closest_re_ma=ma(ne_tran1_res9$Closest_re,order,TRUE)*order #make it moving sum
  weiAve_re_ma=ma(ne_tran1_res9$WeiAve_re,order,TRUE)*order
  BiLinear_re_ma=ma(ne_tran1_res9$BiLinear_re,order,TRUE)*order
  BiQuadratic_re_ma=ma(ne_tran1_res9$BiQuadratic_re,order,TRUE)*order
  BiQubic_re_ma=ma(ne_tran1_res9$BiQubic_re,order,TRUE)*order
  TIN_re_ma=ma(ne_tran1_res9$TIN_re,order,TRUE)*order
  NN_re_ma=ma(ne_tran1_res9$NN_re,order,TRUE)*order
    
  re_ma=data.frame(Pt_indx,weiAve_re_ma,BiLinear_re_ma,BiQuadratic_re_ma,BiQubic_re_ma,TIN_re_ma,NN_re_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  
  fn=paste0("Z:/Discussion/figures/",study_area,'_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+ylim(-50,50)+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)

  #Plot terrain roughness indices
  #Plot DEM standard deviation
  DEMst_ma=ma(ne_tran1_res9$DEM_ST,order,TRUE)
  re_ma=data.frame(Pt_indx,DEMst_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  fn=paste0("Z:/Discussion/figures/",study_area,'_dem_st','_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
  
  #Plot TRP
  TRP_ma=ma(ne_tran1_res9$TRP,order,TRUE)
  re_ma=data.frame(Pt_indx,TRP_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  fn=paste0("Z:/Discussion/figures/",study_area,'_TRP','_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
  
  #Plot TRI
  TRI_ma=ma(ne_tran1_res9$TRI,order,TRUE)
  re_ma=data.frame(Pt_indx,TRI_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  fn=paste0("Z:/Discussion/figures/",study_area,'_TRI','_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
  
  #Plot SV
  SV_ma=ma(ne_tran1_res9$SV,order,TRUE)
  re_ma=data.frame(Pt_indx,SV_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  fn=paste0("Z:/Discussion/figures/",study_area,'_SV','_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
}
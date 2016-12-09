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

study_area='Louisiana'
tran_FID=1

for (res in res_la){
  #Plot residual
  fn_tran1_res9=paste0(sa$Louisiana,'dist_tran',tran_FID, '_res', res,'.csv')
  tran1_res9=read.csv(fn_tran1_res9)
  
  order=500
  
  Pt_indx=seq.int(nrow(tran1_res9))
  #Residual of different methods
  Closest_re_ma=ma(tran1_res9$Closest_re,order,TRUE)*order #make it moving sum
  weiAve_re_ma=ma(tran1_res9$WeiAve_re,order,TRUE)*order
  BiLinear_re_ma=ma(tran1_res9$BiLinear_re,order,TRUE)*order
  BiQuadratic_re_ma=ma(tran1_res9$BiQuadratic_re,order,TRUE)*order
  BiQubic_re_ma=ma(tran1_res9$BiQubic_re,order,TRUE)*order
  TIN_re_ma=ma(tran1_res9$TIN_re,order,TRUE)*order
  NN_re_ma=ma(tran1_res9$NN_re,order,TRUE)*order
  #Terrain Roughness Indices
  DEMst_ma=ma(tran1_res9$DEM_ST,order,TRUE)
  RTP_ma=ma(tran1_res9$RTP,order,TRUE)
  TRI_ma=ma(tran1_res9$TRI,order,TRUE)
  SV_ma=ma(tran1_res9$SV,order,TRUE)
  
    
  re_ma=data.frame(Pt_indx,weiAve_re_ma,BiLinear_re_ma,BiQuadratic_re_ma,BiQubic_re_ma,TIN_re_ma,NN_re_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  
  fn=paste0("Z:/Discussion/figures/",study_area,'_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+ylim(0,10)+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)


  #Plot DEM_ST and TRI
  re_ma=data.frame(Pt_indx,DEMst_ma, TRI_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  
  fn=paste0("Z:/Discussion/figures/",study_area,'_Indices_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
  
  re_ma=data.frame(Pt_indx, RTP_ma)
  re_ma=melt(re_ma,id='Pt_indx')
  
  fn=paste0("Z:/Discussion/figures/",study_area,'_RTP_tran',tran_FID,'_res',res,'.png')
  ggplot(data=re_ma,aes(x=Pt_indx, y=value, colour=variable)) +geom_line()+xlab('Sample point index')+ylab('Residual')+ggtitle(fn)
  ggsave(file=fn,width = 10,height = 5)
}
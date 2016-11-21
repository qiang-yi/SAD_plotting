#Plot residual with p2p
library(ggplot2)
library(reshape2)

sa=vector(mode="list", length=6)
sa <- c("Colorado","Louisiana","NC","Nebraska","Texas","Washington")
path='C:/Users/yiqi7710/work_CU/data/Modified_2/'

RMSE_L=list()
for (study_area in sa){
  fn_ne=paste0(path,study_area,'/transects_2.csv')
  
#fn_ne='C:/Users/yiqi7710/work_CU/data/Modified_2/Colorado/transects_2.csv'
#Change the path of the file
  result_ne=read.csv(fn_ne)
  
  num_tran=length(result_ne$Transect_ID)
  meds <- c("p2p", "clos", "wavg","biLin", "biQua", "biQub", "TIN","NN")
  
  
  res_L=c('10','30','100','1000')
  resolutions<-c(10,30,100,1000)
  num_meds<-length(meds)
  
  RMSE=matrix(0, ncol=length(res_L),nrow=num_tran,byrow = TRUE)
  dimnames(RMSE) = list(result_ne$Transect_ID, res_L)
  for (res_ID in (1:length(res_L))){
    resid_m = matrix(0, nrow=num_tran,ncol=length(meds),byrow = TRUE)
    for (tran_ID in 1:num_tran){
      for (med_ID in 1:num_meds){
        #---------change c(medx,...) to exclude methods-----------
        if (med_ID %in% c(1)){
          resid_m[tran_ID,med_ID]=NaN
        } 
        else{
          resid_m[tran_ID,med_ID]=result_ne[tran_ID,3]-result_ne[tran_ID,1+med_ID+res_ID*length(meds)]
        }
      }
      RMSE[tran_ID,res_ID]=sqrt(sum(resid_m[tran_ID,]^2,na.rm=TRUE)/length(resid_m[tran_ID,][!is.na(resid_m[tran_ID,])])) 

    }
  }
  RMSE_L=c(RMSE_L,list(RMSE))
}

list_of_dfs <- lapply(RMSE_L, melt, varnames = c("transect_id", 'resolution'))
nrow_each <- lapply(list_of_dfs, nrow)
big_df <- do.call(rbind, list_of_dfs)
big_df$study_area <- rep(sa, unlist(nrow_each))

fn=paste0("Z:/Discussion/figures/",'REMS.png')
ggplot(big_df, aes(x = factor(resolution), y = value)) +
  geom_point(aes(group = transect_id, color = factor(transect_id)), size=1)+
  geom_line(aes(group = transect_id, color = factor(transect_id)), size = 0.5) +
  facet_wrap(~study_area)+xlab('Resolution')+ylab('Residual')+ggtitle(fn)+
  scale_colour_brewer(palette = "Set1")
  ggsave(file=fn,width = 10,height = 5)



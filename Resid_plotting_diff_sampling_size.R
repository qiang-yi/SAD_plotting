#Plot residual with p2p
library(ggplot2)
library(reshape2)

sa=vector(mode="list", length=6)
sa <- c("Colorado","Louisiana","NC","Nebraska","Texas","Washington")
path='C:/Users/yiqi7710/work_CU/data/Modified_2/'


s_size_list=c('3','10','30','100','500','1000','3000')

res_L=c('10','30','100','1000')
for (study_area in sa){
  for (res_ID in 1:length(res_L)){
    fn_bn=paste0(path,study_area,'/transects_2.csv')

    #Change the path of the file
    bn=read.csv(fn_bn)
    
    result_L=list()
    for (s_size in s_size_list){
      fn_in=paste0(path,study_area,'/sample_points3/tran_',s_size,'.csv')
      result=read.csv(fn_in)
      result_L=c(result_L,list(result))
    }
    
    resid_L=list()
    
    meds <- c("p2p", "clos", "wavg","biLin", "biQua", "biQub", "TIN","NN")
    num_tran<-length(result$Transect_ID)
    
    RMSE=matrix(0, ncol=length(s_size_list),nrow=num_tran,byrow = TRUE)
    dimnames(RMSE) = list(result$Transect_ID, s_size_list)
    
    #res_L=c('10')
    num_meds<-length(meds)
    for (s_size_ID in (1:length(s_size_list))){
      result=result_L[[s_size_ID]]
      
      #for (res_ID in (1:length(res_L))){
      resid_m = matrix(0, nrow=num_tran,ncol=length(meds),byrow = TRUE)
      dimnames(resid_m) = list(result$Transect_ID,meds)
      for (tran_ID in 1:num_tran){
        for (med_ID in 1:num_meds){
          
          if (med_ID %in% c(1,2)){
            resid_m[tran_ID,med_ID]=NaN
          } 
          else{
            resid_m[tran_ID,med_ID]=bn[tran_ID,3]-result[tran_ID,1+med_ID+res_ID*length(meds)]
          }
        }
        #RMSE[tran_ID,s_size_ID]=sqrt(sum(resid_m[tran_ID,]^2,na.rm=TRUE)/length(resid_m[tran_ID,][!is.na(resid_m[tran_ID,])])) 
      }
      resid_L=c(resid_L,list(resid_m))
    }
    
    list_of_dfs <- lapply(resid_L, melt, varnames = c('transect_id','method'))
    nrow_each <- lapply(list_of_dfs, nrow)
    big_df <- do.call(rbind, list_of_dfs)
    #big_df$sample_size <- rep(s_size_list, unlist(nrow_each))
    big_df$sample_size <- rep(s_size_list, unlist(nrow_each))
    
    #----------Modify to select method to be plotted-----------------------
    meds <- c("wavg","biLin", "biQua", "biQub", "TIN","NN") #excluding p2p,clos3
    big_df=big_df[big_df$method %in% meds, ]
    
    
    #----------Modify to select method to be plotted-----------------------
    resolution=res_L[res_ID]
    fn=paste0("Z:/Discussion/figures/",'resid_',study_area,'_res',resolution,'.png')
    big_df$sample_size <- factor(big_df$sample_size, levels = c('3','10','30','100','500','1000','3000'))
    ggplot(big_df, aes(x = factor(sample_size), y = value)) +
      geom_point(aes(group = transect_id, color = factor(transect_id)), size=1)+
      geom_line(aes(group = transect_id, color = factor(transect_id)), size = 0.5) +
      facet_wrap(~method)+xlab('Sample size')+ylab('Residual')+ggtitle(fn)+
      scale_colour_brewer(palette = "Set1")
    ggsave(file=fn,width = 10,height = 5)
  }
}

#Plot residual without p2p
library(ggplot2)
library(reshape2)

sa=vector(mode="list", length=6)
sa <- c("Colorado", "Louisiana", "NC","Nebraska", "Texas", "Washington")
path='C:/Users/yiqi7710/work_CU/data/Modified_2/'

for (study_area in sa){
  fn_ne=paste0(path,study_area,'/transects_2.csv')
  #Change the path of the file
  result_ne=read.csv(fn_ne)
  
  num_tran=length(result_ne$Transect_ID)
  col_names=colnames(result_ne)
  
  meds <- c("p2p", "clos", "wavg","biLin", "biQua", "biQub", "TIN","NN")
  
  
  res_L=c('10','30','100','1000')
  
  resid_L=list()
  for (med_ID in (1:length(meds))){
    resid_m = matrix(0, ncol=num_tran,nrow=length(res_L),byrow = TRUE)
    dimnames(resid_m) = list(res_L,result_ne$Transect_ID)
    for (tran_ID in 1:num_tran){
      for (res_ID in 1:4){
        resid_m[res_ID,tran_ID]=result_ne[tran_ID,3]-result_ne[tran_ID,1+med_ID+res_ID*length(meds)]
        
      }
    }
    #How to plot the matrix (resid_m) as a line chart in which each column is represented by a line, x axis is the row names.
    resid_L<-c(resid_L,list(resid_m))
    
  }
  

    
  list_of_dfs <- lapply(resid_L, melt, varnames = c("resolution", 'transect_id'))
  nrow_each <- lapply(list_of_dfs, nrow)
  big_df <- do.call(rbind, list_of_dfs)
  big_df$med_ID <- rep(meds, unlist(nrow_each))
  
  #----------Modify to select method to be plotted-----------------------
  meds <- c("clos","wavg","biLin", "biQua", "biQub", "TIN","NN") #excluding p2p
  big_df=big_df[big_df$med_ID %in% meds, ]
  
  
  #----------Modify to select method to be plotted-----------------------
  
  fn=paste0("Z:/Discussion/figures/",'resid_',study_area,'.png')
  ggplot(big_df, aes(x = factor(resolution), y = value)) +
    geom_point(aes(group = transect_id, color = factor(transect_id)), size=1)+
    geom_line(aes(group = transect_id, color = factor(transect_id)), size = 0.5) +
    facet_wrap(~med_ID)+xlab('Resolution')+ylab('Residual')+ggtitle(fn)+
    scale_colour_brewer(palette = "Set1")
    ggsave(file=fn,width = 10,height = 5)
}

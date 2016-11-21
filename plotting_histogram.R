# Plot the histogram of transect length, orientation, distance to edge and so on

co=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/Colorado/simulation/tran_sim.csv')
la=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/Louisiana/simulation/tran_sim.csv')
nc=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/NC/simulation/tran_sim.csv')
ne=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/Nebraska/simulation/tran_sim.csv')
wa=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/Washington/simulation/tran_sim.csv')
tx=read.csv('C:/Users/yiqi7710/work_CU/data/Modified_2/Texas/simulation/tran_sim.csv')

#plot histograms of the transects length
png('Z:/Discussion/figures/length.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
hist(co$Geodesic_L,80,xlab="Length", main="Colorado",ylim=c(0,50))
hist(la$Geodesic_L,80,xlab="Length", main="Louisiana",ylim=c(0,50))
hist(nc$Geodesic_L,80,xlab="Length", main="North Carolina",ylim=c(0,50))
hist(ne$Geodesic_L,80,xlab="Length", main="Nebraska",ylim=c(0,50))
hist(wa$Geodesic_L,80,xlab="Length", main="Washington",ylim=c(0,50))
hist(tx$Geodesic_L,80,xlab="Length", main="Texas",ylim=c(0,50))
mtext("Transect length", side = 3, line = -1, outer = TRUE)
dev.off()

#plot histograms of the Orientation
png('Z:/Discussion/figures/orientation.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
hist(co$Orient,40,xlab="Orientation", main="Colorado",ylim=c(0,200))
hist(la$Orient,40,xlab="Orientation", main="Louisiana",ylim=c(0,100))
hist(nc$Orient,40,xlab="Orientation", main="North Carolina",ylim=c(0,100))
hist(ne$Orient,40,xlab="Orientation", main="Nebraska",ylim=c(0,100))
hist(wa$Orient,40,xlab="Orientation", main="Washington",ylim=c(0,100))
hist(tx$Orient,40,xlab="Orientation", main="Texas",ylim=c(0,100))
mtext("Transect orientation", side = 3, line = -1, outer = TRUE)
dev.off()

#plot the histogram of the distance of the transect to the edge
png('Z:/Discussion/figures/dist_edge.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
hist(co$D_edge,80,xlab="Distance", main="Colorado",ylim=c(0,100))
hist(la$D_edge,80,xlab="Distance", main="Louisiana",ylim=c(0,100))
hist(nc$D_edge,80,xlab="Distance", main="North Carolina",ylim=c(0,100))
hist(ne$D_edge,80,xlab="Distance", main="Nebraska",ylim=c(0,100))
hist(wa$D_edge,80,xlab="Distance", main="Washington",ylim=c(0,100))
hist(tx$D_edge,80,xlab="Distance", main="Texas",ylim=c(0,100))
mtext("Distance of transects to the edge", side = 3, line = -1, outer = TRUE)
dev.off()

#plot histograms of the distance of the transect to the centroid
png('Z:/Discussion/figures/dist_centroid.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
hist(co$D_ctr,80,xlab="Distance", main="Colorado",ylim=c(0,60))
hist(la$D_ctr,80,xlab="Distance", main="Louisiana",ylim=c(0,60))
hist(nc$D_ctr,80,xlab="Distance", main="North Carolina",ylim=c(0,60))
hist(ne$D_ctr,80,xlab="Distance", main="Nebraska",ylim=c(0,60))
hist(wa$D_ctr,80,xlab="Distance", main="Washington",ylim=c(0,60))
hist(tx$D_ctr,80,xlab="Distance", main="Texas",ylim=c(0,60))
mtext("Distance of transects to the centroid", side = 3, line = -1, outer = TRUE)
dev.off()


#plot histograms of the nearest distance of the transect vertice to the centroid
par(mfrow=c(3,2))
hist(co$D_ctr_nr,80,xlab="Distance", main="Colorado")
hist(la$D_ctr_nr,80,xlab="Distance", main="Louisiana")
hist(nc$D_ctr_nr,80,xlab="Distance", main="North Carolina")
hist(ne$D_ctr_nr,80,xlab="Distance", main="Nebraska")
hist(wa$D_ctr_nr,80,xlab="Distance", main="Washington")
hist(tx$D_ctr_nr,80,xlab="Distance", main="Texas")
mtext("Distance of the nearer end point to the centroid", side = 3, line = -1, outer = TRUE)


#plot histograms of the furthest distance of the transect vertice to the centroid
par(mfrow=c(3,2))
hist(co$D_ctr_far,80,xlab="Distance", main="Colorado")
hist(la$D_ctr_far,80,xlab="Distance", main="Louisiana")
hist(nc$D_ctr_far,80,xlab="Distance", main="North Carolina")
hist(ne$D_ctr_far,80,xlab="Distance", main="Nebraska")
hist(wa$D_ctr_far,80,xlab="Distance", main="Washington")
hist(tx$D_ctr_far,80,xlab="Distance", main="Texas")
mtext("Distance of the further end point to the centroid", side = 3, line = -1, outer = TRUE)


#Scatter plot between transect length and orientation
png('Z:/Discussion/figures/length_orientation.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
plot(co$Geodesic_L, co$Orient, main="Colorado", xlab="Transect length ", ylab="Orientation", pch=19)
plot(la$Geodesic_L, la$Orient, main="Louisiana", xlab="Transect length ", ylab="Orientation", pch=19)
plot(nc$Geodesic_L, nc$Orient, main="North Carolina", xlab="Transect length ", ylab="Orientation", pch=19)
plot(ne$Geodesic_L, ne$Orient, main="Nebraska", xlab="Transect length ", ylab="Orientation", pch=19)
plot(wa$Geodesic_L, wa$Orient, main="Washington", xlab="Transect length ", ylab="Orientation", pch=19)
plot(tx$Geodesic_L, tx$Orient, main="Texas", xlab="Transect length ", ylab="Orientation", pch=19)
mtext("Scatter plot between transect length and orientation", side = 3, line = -1, outer = TRUE)
dev.off()

#Scatter plot between transect length and distance to centroid
png('Z:/Discussion/figures/length_dist_ctr.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
plot(co$Geodesic_L, co$D_ctr, main="Colorado", xlab="Transect length ", ylab="Distance to centroid", pch=19)
plot(la$Geodesic_L, la$D_ctr, main="Louisiana", xlab="Transect length ", ylab="Distance to centroid", pch=19)
plot(nc$Geodesic_L, nc$D_ctr, main="North Carolina", xlab="Transect length ", ylab="Distance to centroid", pch=19)
plot(ne$Geodesic_L, ne$D_ctr, main="Nebraska", xlab="Transect length ", ylab="Distance to centroid", pch=19)
plot(wa$Geodesic_L, wa$D_ctr, main="Washington", xlab="Transect length ", ylab="Distance to centroid", pch=19)
plot(tx$Geodesic_L, tx$D_ctr, main="Texas", xlab="Transect length ", ylab="Distance to centroid", pch=19)
mtext("Scatter plot between transect length and distance to centroid", side = 3, line = -1, outer = TRUE)
dev.off()

#Scatter plot between transect length and distance to edge
png('Z:/Discussion/figures/length_dist_edge.png',width = 1037, height  = 544)
par(mfrow=c(3,2))
plot(co$Geodesic_L, co$D_edge, main="Colorado", xlab="Transect length ", ylab="Distance to edge", pch=19)
plot(la$Geodesic_L, la$D_edge, main="Louisiana", xlab="Transect length ", ylab="Distance to edge", pch=19)
plot(nc$Geodesic_L, nc$D_edge, main="North Carolina", xlab="Transect length ", ylab="Distance to edge", pch=19)
plot(ne$Geodesic_L, ne$D_edge, main="Nebraska", xlab="Transect length ", ylab="Distance to edge", pch=19)
plot(wa$Geodesic_L, wa$D_edge, main="Washington", xlab="Transect length ", ylab="Distance to edge", pch=19)
plot(tx$Geodesic_L, tx$D_edge, main="Texas", xlab="Transect length ", ylab="Distance to edge", pch=19)
mtext("Scatter plot between transect length and Distance to edge", side = 3, line = -1, outer = TRUE)
dev.off()

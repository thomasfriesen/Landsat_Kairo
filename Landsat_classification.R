library(landsat)
library(raster)
library(RStoolbox)
dateien=list.files(".",pattern = ".TIF")
bild=stack(dateien)
bild=brick(bild)
plot(bild)
plotRGB(bild,r=4,g=3,b=2)

e <- as(extent(320000, 370000, 3245385+55000, 3459015-55000), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
rc1=crop(bild,e)



e <- as(extent(320000, 470000, 3245385+30000, 3459015-30000), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
rc=crop(bild,e)
plot(rc[[1]])

save1=ggRGB(rc)
postscript("/home/thomas/Latex/Landsat/Landsat_Kairo.eps",horiz=TRUE,onefile=TRUE,height=8,width=10,paper="a4")
save1
dev.off()


save1=ggRGB(rc)
jpeg("/home/thomas/Latex/Landsat/Landsat_Kairo.jpeg",res=72,height=8,width=10,unit="in")
save1
dev.off()

neuerVersuch3=unsuperClass(bild,nsamples=200,nClasses=3,nstart=40,clusterMap = T)
plot(neuerVersuch3$map)


versuch2=unsuperClass(rc,nsamples=200,nClasses=2,nstart=40,clusterMap = T)
versuch3=unsuperClass(rc,nsamples=200,nClasses=3,nstart=40,clusterMap = T)
versuch4=unsuperClass(rc,nsamples=200,nClasses=4,nstart=40,clusterMap = T)
versuch5=unsuperClass(rc,nsamples=200,nClasses=5,nstart=40,clusterMap = T)
versuch6=unsuperClass(rc,nsamples=200,nClasses=6,nstart=40,clusterMap = T)
versuch7=unsuperClass(rc,nsamples=200,nClasses=7,nstart=40,clusterMap = T)
versuch8=unsuperClass(rc,nsamples=200,nClasses=8,nstart=40,clusterMap = T)
versuch9=unsuperClass(rc,nsamples=200,nClasses=9,nstart=40,clusterMap = T)
versuch10=unsuperClass(rc,nsamples=200,nClasses=10,nstart=40,clusterMap = T)
versuch11=unsuperClass(rc,nsamples=200,nClasses=11,nstart=40,clusterMap = T)
versuch12=unsuperClass(rc,nsamples=200,nClasses=12,nstart=40,clusterMap = T)
versuch13=unsuperClass(rc,nsamples=200,nClasses=13,nstart=40,clusterMap = T)
versuch14=unsuperClass(rc,nsamples=200,nClasses=14,nstart=40,clusterMap = T)
versuch16=unsuperClass(rc,nsamples=200,nClasses=16,nstart=40,clusterMap = T)
versuch18=unsuperClass(rc,nsamples=200,nClasses=18,nstart=40,clusterMap = T)
versuch20=unsuperClass(rc,nsamples=200,nClasses=20,nstart=40,clusterMap = T)
versuch22=unsuperClass(rc,nsamples=200,nClasses=22,nstart=40,clusterMap = T)

ss=data.frame(sumsquared=c(versuch2$model$tot.withinss,
versuch3$model$tot.withinss,
versuch4$model$tot.withinss,
versuch5$model$tot.withinss,
versuch6$model$tot.withinss,
versuch7$model$tot.withinss,
versuch8$model$tot.withinss,
versuch9$model$tot.withinss,
versuch10$model$tot.withinss,
versuch11$model$tot.withinss,
versuch12$model$tot.withinss,
versuch13$model$tot.withinss,
versuch14$model$tot.withinss,
versuch16$model$tot.withinss,
versuch18$model$tot.withinss,
versuch20$model$tot.withinss,
versuch22$model$tot.withinss),index=c(2,3,4,5,6,7,8,9,10,11,12,13,14,16,18,20,22))


save1=ggplot(data=ss)+
  geom_point(aes(x=index,y=sumsquared))+ggtitle("Scree-Plot for kmeans")+ylab("Sum of squared")+
  xlab("Number of Clusters")
jpeg("/home/thomas/Latex/Landsat/figures/Scree_plot.jpeg",res=72,height=8,width=10,unit="in")
save1
dev.off()


save1=ggplot(data=ss)+
  geom_point(aes(x=index,y=sumsquared))+ggtitle("Scree-Plot for kmeans")+ylab("Sum of squared")+
  xlab("Number of Clusters")
jpeg("./Scree_plot.jpeg",res=72,height=8,width=10,unit="in")
save1
dev.off()


plot(versuch8$map,col=terrain.colors(8,alpha=0.8))
postscript("/home/thomas/Latex/Landsat/Landsat_Cluster_8.eps",horiz=TRUE,onefile=TRUE,height=8,width=10,paper="a4")
save1
dev.off()

colors10=terrain.colors(10,alpha=0.8)
colors9=colors10[c(1,2,3,5,6,7,8,9,10)]
colors8=colors10[c(1,2,3,5,6,8,9,10)]

jpeg("/home/thomas/Latex/Landsat/figures/Landsat_Cluster_8.jpeg",res=72,height=8,width=10,unit="in")
plot(versuch8$map,col=colors8)
dev.off()

jpeg("/home/thomas/Latex/Landsat/figures/Landsat_Cluster_9.jpeg",res=72,height=8,width=10,unit="in")
plot(versuch9$map,col=colors9)
dev.off()
plot(versuch6$map,col=terrain.colors(6,alpha=0.8))
plot(versuch7$map,col=terrain.colors(7,alpha=0.8))



jpeg("/home/thomas/Latex/Landsat/figures/Landsat_Cluster_10.jpeg",res=72,height=8,width=10,unit="in")
plot(versuch10$map,col=colors10)
dev.off()


#Figures for results
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

#loading in datasets
mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")
mntd_all_sp_in_genus <- read_csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read_csv("mntd.single.sp.in.genus.csv")

#Figure 1
mntd<-cbind(rep("MNTD", length(mntd_all_sp_in_genus$mntd.obs.z)),mntd_all_sp_in_genus$mntd.obs.z)
mpd<-cbind(rep("MPD", length(mpd_all_sp_in_genus$mpd.obs.z)),mpd_all_sp_in_genus$mpd.obs.z)
phylomet<-as.data.frame(rbind(mntd, mpd), stringsAsFactors=FALSE)

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/phylogenetic_metrics.pdf")
par(mfrow= c(1,1))
boxplot(as.numeric(V2) ~ as.factor(V1), data=phylomet, ylab = "SES", main = "MNTD and MPD for Winegrape Pathogens")
abline(h=0, col=2, lty=2)
dev.off()




single.sp<-cbind(rep("single.species", length(mpd_single_sp_in_genus$mpd.obs.z)),mpd_single_sp_in_genus$mpd.obs.z)
all.genus<-cbind(rep("all.genus", length(mpd_all_sp_in_genus$mpd.obs.z)),mpd_all_sp_in_genus$mpd.obs.z)
mpd.z<-as.data.frame(rbind(single.sp, all.genus), stringsAsFactors=FALSE)


pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/Mean pairwise distances.pdf")
par(mfrow= c(1,1))
boxplot(as.numeric(V2) ~ as.factor(V1), data=mpd.z, ylab = "SES.MPD", main = "Mean pairwise distances between hosts")
abline(h=0, col=2, lty=2)
dev.off()

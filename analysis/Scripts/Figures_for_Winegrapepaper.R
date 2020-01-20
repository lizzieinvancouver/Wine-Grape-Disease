#Figures for results
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/GitHub/Wine-Grape-Disease/analysis/output/")

#loading in datasets
mpd_all_sp_in_genus <- read.csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read.csv("mpd.single.sp.in.genus.csv")
mntd_all_sp_in_genus <- read.csv("mntd_all_sp_in_genus.csv")
mntd_single_sp_in_genus <- read.csv("mntd.single.sp.in.genus.csv")
MNTD_MPDcomparison <-read_excel("MNTD_MPDcomparison.xlsx")

#Figure 1
mntd<-cbind(rep("MNTD", length(mntd_all_sp_in_genus$mntd.obs.z)),mntd_all_sp_in_genus$mntd.obs.z)
mpd<-cbind(rep("MPD", length(mpd_all_sp_in_genus$mpd.obs.z)),mpd_all_sp_in_genus$mpd.obs.z)
phylomet<-as.data.frame(rbind(mntd, mpd), stringsAsFactors=FALSE)
plot(phylomet)





pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/phylogenetic_metrics.pdf")
boxplot(as.numeric(V2) ~ as.factor(V1), data=phylomet, ylab = "SES")
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


pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/MPDvsMNTD.pdf")
par(mfrow= c(1,1))
plot(MNTD_MPDcomparison$`All Species in Genus`~MNTD_MPDcomparison$...3, data = MNTD_MPDcomparison, ylab = "SES.MNTD", 
     xlab = "SES.MPD")
abline(v=0, col=2, lty=2)
abline(h=0, col=2, lty=2)
dev.off()

pdf("~/Documents/GitHub/Wine-Grape-Disease/figures/MPDdatabytype.pdf")
boxplot(mpd_all_sp_in_genus$mpd.obs.z~mpd_all_sp_in_genus$Type, data = mpd_all_sp_in_genus, 
        ylab = "SES.MPD", xlab="")
abline(h=0, col=2, lty=2)
dev.off()

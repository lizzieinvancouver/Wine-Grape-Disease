#sentivity analysis based on type of pathogen
setwd("~/Documents/GitHub/Wine-Grape-Disease/data/")


mpd_all_sp_in_genus <- read_csv("mpd_all_sp_in_genus.csv")
mpd_single_sp_in_genus <- read_csv("mpd.single.sp.in.genus.csv")

testmpdall <- mpd_all_sp_in_genus[c(1,7,10)]
testmpdall <- na.omit(testmpdall)
fit <- aov(mpd.obs.z~ Type, data= testmpdall)
summary(fit)


testmpsin<- mpd_single_sp_in_genus[c(1,7,10)]
testmpsin <- na.omit(testmpsin)

fit2 <- aov(mpd.obs.z~ Type, data= testmpsin)
summary(fit2)

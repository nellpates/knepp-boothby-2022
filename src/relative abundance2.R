# Knepp and Boothby write up
# Nell Pates
# Whittaker plots - relative abundance vs species rank - 2022, 2023 and 2024 
# 5 September 2024 - FINAL

# CAN SKIP ALL THIS AND LOAD RDSs - SEE BELOW

# Load clean and formatted data from Knepp and Boothby surveys: 2022, 2023, 2024
knepp_plants2022 <- read.csv("clean-data/knepp_clean2022.csv")
boothby_plants2022 <- read.csv("clean-data/boothby_clean2022.csv")
knepp_plants2023 <- read.csv("clean-data/knepp_clean2023.csv")
boothby_plants2023 <- read.csv("clean-data/boothby_clean2023.csv")
knepp_plants2024 <- read.csv("clean-data/knepp_clean2024.csv")
boothby_plants2024 <- read.csv("clean-data/boothby_clean2024.csv")

kneppcount2022 <- knepp_plants2022%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
kneppcount2022 <- as.data.frame(kneppcount2022)
rownames(kneppcount2022) <- kneppcount2022$Species 
colnames(kneppcount2022)[2] = "Knepp"
totalcount_knepp2022 <- sum(kneppcount2022$Knepp)
kneppcount2022$pKnepp <- kneppcount2022$Knepp/totalcount_knepp2022

kneppcount2023 <- knepp_plants2023%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
kneppcount2023 <- as.data.frame(kneppcount2023)
rownames(kneppcount2023) <- kneppcount2023$Species 
colnames(kneppcount2023)[2] = "Knepp"
totalcount_knepp2023 <- sum(kneppcount2023$Knepp)
kneppcount2023$pKnepp <- kneppcount2023$Knepp/totalcount_knepp2023

kneppcount2024 <- knepp_plants2024%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
kneppcount2024 <- as.data.frame(kneppcount2024)
rownames(kneppcount2024) <- kneppcount2024$Species 
colnames(kneppcount2024)[2] = "Knepp"
totalcount_knepp2024 <- sum(kneppcount2024$Knepp)
kneppcount2024$pKnepp <- kneppcount2024$Knepp/totalcount_knepp2024

boothbycount2022<- boothby_plants2022%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
boothbycount2022 <- as.data.frame(boothbycount2022)
rownames(boothbycount2022) <- boothbycount2022$Species 
colnames(boothbycount2022)[2] = "Boothby"
totalcount_boothby2022 <- sum(boothbycount2022$Boothby)
boothbycount2022$pBoothby <- boothbycount2022$Boothby/totalcount_boothby2022

boothbycount2023<- boothby_plants2023%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
boothbycount2023 <- as.data.frame(boothbycount2023)
rownames(boothbycount2023) <- boothbycount2023$Species 
colnames(boothbycount2023)[2] = "Boothby"
totalcount_boothby2023 <- sum(boothbycount2023$Boothby)
boothbycount2023$pBoothby <- boothbycount2023$Boothby/totalcount_boothby2023

boothbycount2024<- boothby_plants2024%>%select(Site, Plot, Species)%>%
  unique()%>%group_by(Species)%>%count()
boothbycount2024 <- as.data.frame(boothbycount2024)
rownames(boothbycount2024) <- boothbycount2024$Species 
colnames(boothbycount2024)[2] = "Boothby"
totalcount_boothby2024 <- sum(boothbycount2024$Boothby)
boothbycount2024$pBoothby <- boothbycount2024$Boothby/totalcount_boothby2024

data2022 <- merge(kneppcount2022, boothbycount2022, by.x= "Species", all = TRUE)
data2022[is.na(data2022)] <- 0

data2023 <- merge(kneppcount2023, boothbycount2023, by.x= "Species", all = TRUE)
data2023[is.na(data2023)] <- 0

data2024 <- merge(kneppcount2024, boothbycount2024, by.x= "Species", all = TRUE)
data2024[is.na(data2024)] <- 0

saveRDS(data2022, "clean-data/data2022.RDS")
saveRDS(data2023, "clean-data/data2023.RDS")
saveRDS(data2024, "clean-data/data2024.RDS")



##### START HERE #####
setwd("knepp-boothby")
source("src/headers.R") # load all required packages
data2022 <- readRDS("clean-data/data2022.RDS")
data2023 <- readRDS("clean-data/data2023.RDS")
data2024 <- readRDS("clean-data/data2024.RDS")

# Sort data by proportion

knepp_prop2022 <- data2022[order(-data2022$pKnepp), ]
boothby_prop2022 <- data2022[order(-data2022$pBoothby), ]

knepp_prop2023 <- data2023[order(-data2023$pKnepp), ]
boothby_prop2023 <- data2023[order(-data2023$pBoothby), ]

knepp_prop2024 <- data2024[order(-data2024$pKnepp), ]
boothby_prop2024 <- data2024[order(-data2024$pBoothby), ]

knepp_prop2022$n <- 1:nrow(knepp_prop2022)
knepp_prop2023$n <- 1:nrow(knepp_prop2023)
knepp_prop2024$n <- 1:nrow(knepp_prop2024)

boothby_prop2022$n <- 1:nrow(boothby_prop2022)
boothby_prop2023$n <- 1:nrow(boothby_prop2023)
boothby_prop2024$n <- 1:nrow(boothby_prop2024)

# Whittaker plot:

par(mfrow = c(2,1))
par(mar=c(2,6,2,6))


plot(knepp_prop2022$pKnepp, type = "l", xlim = c(1,290), ylim = c(0,0.13), cex.lab = 1.5,
     lwd = 2, col = "red4", pch = 5, cex = 0.8, xlab = "Species rank", ylab = "Relative abundance", bty = "n", xaxt = "n", yaxt = "n")
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 0, cex.axis = 1.5)
kmodel2022 <- lm(knepp_prop2022$pKnepp~knepp_prop2022$n)
abline(kmodel2022, col = "red4")

lines(knepp_prop2023$pKnepp, type = "l",
      lwd = 2, col = "red3", pch = 5, cex = 0.8)
kmodel2023 <- lm(knepp_prop2023$pKnepp~knepp_prop2023$n)
abline(kmodel2023, col = "red3")

lines(knepp_prop2024$pKnepp, type = "l",
      lwd = 2, col = "red", pch = 5, cex = 0.8)
kmodel2024 <- lm(knepp_prop2024$pKnepp~knepp_prop2024$n)
abline(kmodel2024, col= "red")


legend(x = "topright", legend=c("Knepp 2022", "Knepp 2023", "Knepp 2024"),
       col=c("red4", "red3", "red"), lty=1, lwd=3, cex=1.5, title="Data", text.font=1)


par(mar=c(6,6,2,6))


plot(boothby_prop2022$pBoothby, type = "l", xlim = c(1,290), ylim = c(0,0.13), cex.lab = 1.5,
     lwd = 2, col = "royalblue4", pch = 1, cex = 0.8, xlab = "Species rank", ylab = "Relative abundance", bty = "n", xaxt = "n", yaxt = "n")
box("plot", bty = "l", lwd = 2)
axis(side = 1, lwd = 0, lwd.ticks = 2, cex.axis = 1.5)
axis(side = 2, lwd = 0, lwd.ticks = 2, las = 0, cex.axis = 1.5)
bmodel2022 <- lm(boothby_prop2022$pBoothby~boothby_prop2022$n)
abline(bmodel2022, col = "royalblue4")

lines(boothby_prop2023$pBoothby, type = "l",
      lwd = 2, col = "royalblue2", pch = 1, cex = 0.8)
bmodel2023 <- lm(boothby_prop2023$pBoothby~boothby_prop2023$n)
abline(bmodel2023, col = "royalblue2")

lines(boothby_prop2024$pBoothby, type = "l", 
      lwd = 2, col = "lightblue", pch = 1, cex = 0.8)
bmodel2024 <- lm(boothby_prop2024$pBoothby~boothby_prop2024$n)
abline(bmodel2024, col = "lightblue")
legend(x = "topright", legend=c("Boothby 2022", "Boothby 2023", "Boothby 2024"),
       col=c("royalblue4", "royalblue2", "lightblue"), lty=1, lwd=3, cex=1.5, title="Data", text.font=1)

# Local regression: Loess?
dev.off()
par(mfrow = c(2,1))
par(mar=c(2,6,2,6))

plot(knepp_prop2022$pKnepp,type = "p", xlim = c(1,290), ylim = c(0,0.13), cex.lab = 1.5, cex.axis = 1.5,
     lwd = 2, col = "white", pch = 1, cex = 0.8, xlab = "Species rank", ylab = "Relative abundance")
kmodel2022 <- loess(knepp_prop2022$pKnepp~knepp_prop2022$n)
lines.loess(kmodel2022, col = "red4")

kmodel2023 <- loess(knepp_prop2023$pKnepp~knepp_prop2023$n)
lines.loess(kmodel2023, col = "red3")

kmodel2024 <- loess(knepp_prop2024$pKnepp~knepp_prop2024$n)
lines.loess(kmodel2024, col= "red")

legend(x = "topright", legend=c("Knepp 2022", "Knepp 2023", "Knepp 2024"),
       col=c("red4", "red3", "red"), lty=1, lwd=3, cex=1.5, title="Data", text.font=1)

par(mar=c(6,6,2,6))

plot(boothby_prop2022$pBoothby, type = "p", xlim = c(1,290), ylim = c(0,0.13), cex.lab = 1.5, cex.axis = 1.5,
     lwd = 2, col = c(alpha = 0.5, "white"), pch = 1, cex = 0.8, xlab = "Species rank", ylab = "Relative abundance")
bmodel2022 <- loess(boothby_prop2022$pBoothby~boothby_prop2022$n)
lines.loess(bmodel2022, col = "royalblue4", conf.level = 0.95)

bmodel2023 <- loess(boothby_prop2023$pBoothby~boothby_prop2023$n, family = "gaussian", method = "loess")
lines.loess(bmodel2023, col = "royalblue2")

bmodel2024 <- loess(boothby_prop2024$pBoothby~boothby_prop2024$n)
lines.loess(bmodel2024, col = "lightblue")

legend(x = "topright", legend=c("Boothby 2022", "Boothby 2023", "Boothby 2024"),
       col=c("royalblue4", "royalblue2", "lightblue"), lty=1, lwd=3, cex=1.5, title="Data", text.font=1)





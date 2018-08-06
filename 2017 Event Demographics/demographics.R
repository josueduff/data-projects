library(tidyverse)
library(ggplot2)
library(lubridate)
library(sm)

race_names <- c("Keji", "Annapolis", "Turbocrit", "Highland")
files <- c("#1_keji.csv", "#2_annapolis.csv", "#3_turbocrit.csv", "#4_highland.csv")

df <- data.frame();

for (i in 1:4) {
  print(files[i])
  data <- read.csv(paste('Data/Registrations/', files[i], sep=""))
  data$raceName=race_names[i]
  df <- rbind(data[c("Age", "raceName")], df)
}


ggplot(df, aes(x=Age, colour=raceName)) +
  geom_density() +
  geom_hline(colour="white", yintercept=0)+
  geom_vline(colour="white", xintercept=0)


plot(density(df$Age), ylim=c(0,0.04), lty=2, ann=F, axes=F)
axis(1, at=seq(0,80,10), labs=seq(0,80, 10))
axis(2, las=1, at=seq(0,0.04,0.01), labs=seq(0,0.04,0.01))

box()

legend(1,0.04,legend=c("Aggregated", "Keji","Annapolis","Turbocrit","Highland"), col=c("black", "#003f5c","#7a5195","#ef5675","#ffa600"), lty = c(2,rep(1,4)), cex=0.8)

title(main="Registration age distrubution", sub = "N = 273  Bandwidth = 3.975", xlab = "Age", ylab = "Density")

lines(density(filter(df, raceName=="Keji")$Age), col="#003f5c")
lines(density(filter(df, raceName=="Annapolis")$Age), col="#7a5195")
lines(density(filter(df, raceName=="Turbocrit")$Age), col="#ef5675")
lines(density(filter(df, raceName=="Highland")$Age), col="#ffa600")


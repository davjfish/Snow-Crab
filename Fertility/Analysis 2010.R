setwd("U:/Snow Crab/Fertility")

x <- read.table( "Feritlity 2010.csv", header = TRUE, sep = ",")

x$sample.weight <- as.numeric(as.character(x$masse.s.echantillon))
x$sample.count <- as.numeric(as.character(x$X.ufs.compt.s))

index <- !is.na(x$LC) & !is.na(x$sample.weight) & !is.na(x$sample.count) 
x <- x[index, ]

plot(x$LC, 1000000 * x$sample.weight / x$sample.count,
     xlab = "Carapace length (mm)", ylab = "Average Dry Egg Weight (micro g)", cex.lab = 1.5,
     pch = 21, bg = "grey", cex = 1.15)

yy <- x$sample.weight / x$sample.count
xx <- x$LC

m <- lm(yy ~ xx)

index <- x$status.reprod == "M"
plot(xx[index], yy[index])
index <- x$status.reprod == "P"
points(xx[index], yy[index], pch = 21, bg = "red")

index <- x$Couleur.oeufs == "OC"
plot(xx[index], yy[index])
index <- x$Couleur.oeufs == "OF"
points(xx[index], yy[index], pch = 21, bg = "red")


x$egg.mass <- as.numeric(as.character(x$masse.total..oeufs.))

index <- x$status.reprod == "M"
plot(xx[index], x$egg.mass[index] * yy[index])
index <- x$status.reprod == "P"
points(xx[index], x$egg.mass[index] * yy[index], pch = 21, bg = "red")

index <- x$Couleur.oeufs == "OC"
plot(xx[index], x$egg.mass[index] * yy[index])
index <- x$Couleur.oeufs == "OF"
points(xx[index], x$egg.mass[index] * yy[index], pch = 21, bg = "red")

index <- x$Couleur.oeufs == "OC"
plot(xx[index], x$egg.mass[index] * yy[index])
index <- x$Couleur.oeufs == "OF"
points(xx[index], x$egg.mass[index] * yy[index], pch = 21, bg = "red")





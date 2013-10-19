
## ----echo=FALSE, results='hide'------------------------------------------
opts_chunk$set(comment=NA, background="#ccd9ff", out.width='\\linewidth', tidy=FALSE, size = "footnotesize")
options(width=60)


## ----echo = FALSE--------------------------------------------------------
opts_chunk$set(tidy = FALSE, size = 'footnotesize')


## ------------------------------------------------------------------------
## Datensatz erstellen
dat <- data.frame(v1 = sample(1:10, 5, 
                              replace = TRUE),
                  v2 = sample(0:1, 5, 
                              replace = TRUE))
## Einen missing value einbauen
dat$v1[2] <- NA
dat



## ------------------------------------------------------------------------
## Fuer 2d-array: 
## MARGIN = 1: rowwise, MARGIN = 2: columnwise 
##
## Spaltenmittelwerte
apply(dat, MARGIN = 2, mean, na.rm = TRUE)

## Zeilenmittelwerte
apply(dat, MARGIN = 1, mean, na.rm = TRUE) 

## Anzahl missing values
apply(dat, 2, FUN = function(x)sum(is.na(x)))



## ------------------------------------------------------------------------
## Datensatz erstellen
dat <- data.frame(g1 = c(1, 1, 1, 2, 2, 2),
                  g2 = c(1, 1, 2, 2, 3, 3),
                  ## M_1 = 2, M_2 = 1
                  x = c(1, 2, 3, 0.5, 1, 1.5))
dat


## ------------------------------------------------------------------------
## Gruppenmittelwerte berechnen
aggregate(dat$x, by = list(dat$g1, dat$g2), mean)
aggregate(. ~ g1 + g2, dat, mean)



## ------------------------------------------------------------------------
## Datensatz erstellen
dat <- data.frame(
    g1 = factor(c("A", "A", "A", "B", "B", "B")),
    g2 = c(1, 1, 2, 2, 3, 3),
    ## M_1 = 2, M_2 = 1
    x = c(1, 2, 3, 0.5, 1, 1.5))
dat


## ------------------------------------------------------------------------
## Gruppenmittelwerte berechnen 
## (missings entsprechen fehlenden Gruppen)
tapply(dat$x, INDEX = list(dat$g1, dat$g2), mean)



## ------------------------------------------------------------------------
## Datensatz erstellen
dat <- data.frame(g1 = c(1, 1, 2, 2, 3, 3),
                  ## M_1 = 2, M_2 = 1
                  x = c(10, 21, 3, 0.5, 1, 1.5))
dat


## ------------------------------------------------------------------------

## Gruppenmittelwerte berechnen
ave(dat[, "x"], dat$g1, FUN = mean)




## ------------------------------------------------------------------------
library(plyr)
dat <- data.frame(g = c(1, 1, 1, 1, 2, 2, 2, 2), 
                  ## M_1 = 2, M_2 = 1
                  x = c(1, 2, 3, NA, 0.5, 1, 1.5, NA))
dat



## ----tidy = FALSE--------------------------------------------------------
countMiss <- function(x){return(sum(is.na(x)))}
ddply(dat, .(g), 
      summarize, 
      m = mean(x, na.rm=TRUE), ## Mittelwert
      sd = sd(x, na.rm=TRUE),  ## Standardabweichung
      n.miss = countMiss(x))   ## Anzahl missings


## ------------------------------------------------------------------------
library(plyr)
set.seed(12)
dat <- data.frame(g = factor(rbinom(100, 1, 0.5), 
                             labels = c("males", "females")), 
                  x = rnorm(100),
                  y = rnorm(100),
                  z = rnorm(100))
head(dat)



## ----tidy = FALSE, size = "tiny"-----------------------------------------

## Funktion zur Berechnung der Korr.matrix
## Die 1. Spalte mit der Gruppierungsvariablen ausschliessen
calcCorr <- function(x){
    cor(x[, -1])
}
## Eingabe: data.frame, Ausgabe: list
dlply(dat, .(g), calcCorr)



## ------------------------------------------------------------------------
library(plyr)
dat <- data.frame(g = c(1, 1, 1, 2, 2, 2), 
                  ## M_1 = 2, M_2 = 1
                  x = c(1, 2, 3, 0.5, 1, 1.5))
dat



## ------------------------------------------------------------------------

ddply(dat, .(g), transform, 
      x.c = scale(x, scale = FALSE))



## ----cache = TRUE--------------------------------------------------------
## Anzahl Datensaetze
n <- 100
## data.frame-Objekt mit Parameterangaben und Dateinamen
means <- sample(0:20, n, replace = TRUE)
sd <- sample(1:10, n, replace = TRUE)
param <- data.frame(m = means, sd)
## Dateinamen
param$filename <- paste0("../../data/n_files/dat", 
                         1:n, ".dat")

## Von data.frame zu list
param <- split(param, 1:nrow(param))

head(param, n = 2)



## ------------------------------------------------------------------------

## Funktion zum erzeugen der Datensaetze & speichern
genData <- function(x){
    mySample <- rnorm(n, x$m, x$sd)
    write.table(mySample, file = x$filename, 
                row.names = FALSE, col.names = FALSE)
}



## ----cache = TRUE--------------------------------------------------------
## Die "eigentliche Schleife"
invisible(lapply(param, genData))
## invisible() unterdrueckt die Ausgabe...



## ------------------------------------------------------------------------
## Jetzt die n Datenfiles wieder einlesen 
readData <- function(x){
    dat <- read.table(file = x$filename)
    return(dat)
}



## ------------------------------------------------------------------------

## Die "eigentliche Schleife" zum Einlesen
lData <- lapply(param, readData)



## ------------------------------------------------------------------------
tmp <- list.files(path = "../../data/n_files")
length(tmp)
head(tmp, n = 10)


## ------------------------------------------------------------------------
## Einen data.frame mit deskr. Statistiken
## erstellen

calcDesc <- function(x){
    m <- mean(x$V1)
    sd <- sd(x$V1)
    minimum <- min(x$V1)
    maximum <- max(x$V1)
    return(data.frame(m, sd, minimum, maximum))
}

lDesc <- lapply(lData, calcDesc)

## Von list zu data.fram
dDesc <- do.call("rbind", lDesc)

head(dDesc)



## ----cache = TRUE--------------------------------------------------------
## Anzahl Datensaetze
n <- 100
## data.frame-Objekt mit Parameterangaben und Dateinamen
means <- sample(0:20, n, replace = TRUE)
sd <- sample(1:10, n, replace = TRUE)
param <- data.frame(m = means, sd)
param$filename <- paste0("../../data/n_files/dat", 
                         1:n, ".dat")
head(param, n = 2)



## ------------------------------------------------------------------------

## Funktion zum erzeugen der Datensaetze & abspeichern
genData <- function(x){
    mySample <- rnorm(n, x$m, x$sd)
    write.table(mySample, file = x$filename, 
                row.names = FALSE, col.names = FALSE)
}



## ----cache = TRUE--------------------------------------------------------
## Die "eigentliche Schleife"
invisible(ddply(param, .(1:n), genData))
## invisible() unterdrueckt die Ausgabe...



## ------------------------------------------------------------------------
## Jetzt die n Datenfiles wieder einlesen 
readData <- function(x){
    dat <- read.table(file = x$filename)
    return(dat)
}



## ------------------------------------------------------------------------

## Die "eigentliche Schleife" zum Einlesen
lData <- dlply(param, .(1:n), readData)



## ------------------------------------------------------------------------
## Einen data.frame mit deskr. Statistiken
## erstellen

calcDesc <- function(x){
    m <- mean(x$V1)
    sd <- sd(x$V1)
    minimum <- min(x$V1)
    maximum <- max(x$V1)
    return(data.frame(m, sd, minimum, maximum))
}

## Von list zu data.fram
dDesc1 <- ldply(lData, calcDesc)
head(dDesc1)




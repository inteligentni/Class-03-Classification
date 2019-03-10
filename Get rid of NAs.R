get.rid.of.NAs <- function(x) {
  x$Duration[is.na(x$Duration)] <- 150
  x$Other.releases[is.na(x$Other.releases)] <- 0
  x$Covered.by[is.na(x$Covered.by)] <- 0
  x$Top.50.Billboard[is.na(x$Top.50.Billboard)] <- -1
  x$Top.50.Billboard <- 51 - x$Top.50.Billboard
  x$Top.50.Billboard[x$Top.50.Billboard == 52] <- 0
  x$Top.50.Rolling.Stone[is.na(x$Top.50.Rolling.Stone)] <- -1
  x$Top.50.Rolling.Stone <- 51 - x$Top.50.Rolling.Stone
  x$Top.50.Rolling.Stone[x$Top.50.Rolling.Stone == 52] <- 0
  x$Top.50.NME[is.na(x$Top.50.NME)] <- -1
  x$Top.50.NME <- 51 - x$Top.50.NME
  x$Top.50.NME[x$Top.50.NME == 52] <- 0
  x
}

factorize <- function(x) {
  x$Year[69] <- 1969
  x$Year[74] <- 1977
  x$Year[211] <- 1980
  x$Year <- as.factor(x$Year)
  
  x$Single.certification[x$Single.certification == ""] <- "No"
  x$Single.certification[x$Single.certification == "BPI Silver; RIAA Gold"] <- "RIAA Gold, BPI Silver"
  x$Single.certification <- as.factor(x$Single.certification)
  
  x$Cover <- as.factor(x$Cover)
  levels(x$Cover) <- c("N", "Y")

  x  
}

fix.the.dataset <- function(x) {
  x <- get.rid.of.NAs(x)
  x <- factorize(x)
  x
}

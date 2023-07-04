new_compute.fsd <- function (Y, X, verbose = F)
{
  if (length(Y) != length(X)) {
    stop("Input vectors are of different length !!!")
  }
  lengthNAX <- sum(is.na(X))
  if (lengthNAX > 0) {
    warning(paste("Vector of true values contains ", lengthNAX,
                  " NA !!! NA excluded", sep = ""))
  }
  lengthNAY <- sum(is.na(Y))
  if (lengthNAY > 0) {
    warning(paste("Vector of imputed values contains ", lengthNAY,
                  " NA !!! NA excluded", sep = ""))
  }
  sd1 = sd(Y, na.rm = T)
  sd2 = sd(X, na.rm = T)
  if (sd1 == sd2) {
    if (mean(X, na.rm = T) == mean(Y, na.rm = T)) {
      warning("Vectors of true and imputed values are constant and equal !!! By definition FSD=0")
      FS <- 0
      if (verbose) {
        print("acceptable model")
      }
    }
    else {
      FS <- NA
    }
    out <- FS
    return(out)
  }
  else {
    FS <- 2 * abs((sd1 - sd2)/(sd1 + sd2))
    if (verbose) {
      if (abs(FS) < 0.5) {
        print("acceptable model")
      }
      else {
        print("non acceptable FS")
      }
    }
    out <- FS
    return(out)
  }
}

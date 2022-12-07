# R-Code from:
# https://www.r-bloggers.com/2012/02/reflect-your-personnel-selection-r-taylor-russell-tables/

# define functions:
f1 <- function(p) {
  split <- 0.42
  a0 <- 2.50662823884
  a1 <- -18.61500062529
  a2 <- 41.391199773534
  a3 <- -25.44106049637
  b1 <- -8.4735109309
  b2 <- 23.08336743743
  b3 <- -21.06224101826
  b4 <- 3.13082909833
  c0 <- -2.78718931138
  c1 <- -2.29796479134
  c2 <- 4.85014127135
  c3 <- 2.32121276858
  d1 <- 3.54388924762
  d2 <- 1.63706781897
  q <- p - 0.5
  if (abs(q) <= split) {
    r <- q*q
    ppn <- q * (((a3 * r + a2) * r + a1) * r + a0) / ((((b4 * r + b3) * r + b2) * r + b1)*r +1.0)
    return(ppn)
  }
  r <- p
  if (q > 0) {r =1.0-p}
  if (r <= 0) {
    print("Error.")
    return(0)
  }
  r <- sqrt(-log(r))
  ppn <- (((c3 * r + c2) * r + c1) * r + c0) / ((d2 * r + d1) * r + 1.0)
  if (q < 0) {ppn =-ppn}
  return(ppn)
}

f2 <- function(x) {
  p1a <- 242.667955230532
  p1b <- 21.97926616182942
  p1c <- 6.996383488661914
  p1d <- -3.5609843701815e-02
  q1a <- 215.058875869861
  q1b <- 91.1649054045149
  q1c <- 15.0827976304078
  q1d <- 1.0
  p2a <- 300.459261020162
  p2b <- 451.918953711873
  p2c <- 339.320816734344
  p2d <- 152.98928504694
  p2e <- 43.1622272220567
  p2f <- 7.21175825088309
  p2g <- .564195517478994
  p2h <- -1.36864857382717e-07
  q2a <- 300.459260956983
  q2b <- 790.950925327898
  q2c <- 931.35409485061
  q2d <- 638.980264465631
  q2e <- 277.585444743988
  q2f <- 77.0001529352295
  q2g <- 12.7827273196294
  q2h <- 1.0
  p3a <- -2.99610707703542e-03
  p3b <- -4.94730910623251e-02
  p3c <- -.226956593539687
  p3d <- -.278661308609648
  p3e <- -2.23192459734185e-02
  q3a <- 1.06209230528468e-02
  q3b <- .19130892610783
  q3c <- 1.05167510706793
  q3d <- 1.98733201817135
  q3e <- 1.0
  sqrt2 <- 1.4142135623731
  sqrtpi <- 1.77245385090552
  
  y <- x/sqrt2
  if (y < 0) {
    y <- -y
    sn <- -1.0
  } else {
    sn <- 1.0
  }
  y2 <- y * y
  if (y < 0.46875) {
    r1 <- ((p1d * y2 + p1c) * y2 + p1b) * y2 + p1a
    r2 <- ((q1d * y2 + q1c) * y2 + q1b) * y2 + q1a
    erfval <- y * r1 / r2
    if (sn == 1) loarea <- 0.5 + 0.5 * erfval
    else loarea <- 0.5 - 0.5 * erfval
  } else {
    if (y < 4.0) {
      r1 <- ((((((p2h * y + p2g) * y + p2f) * y + p2e) * y + p2d) * y + p2c) * y + p2b) * y + p2a
      r2 <- ((((((q2h * y + q2g) * y + q2f) * y + q2e) * y + q2d) * y + q2c) * y + q2b) * y + q2a
      erfcval <- exp(-y2) * r1 / r2
    } else {
      z <- y2 * y2
      r1 <- (((p3e * z + p3d) * z + p3c) * z + p3b) * z + p3a
      r2 <- (((q3e * z + q3d) * z + q3c) * z + q3b) * z + q3a
      erfcval <- (exp(-y2) / y) * (1.0 / sqrtpi + r1 / (r2 * y2))
    }
    if (sn == 1) loarea <- 1.0 - 0.5 * erfcval
    else loarea <- 0.5 * erfcval
  }
  uparea <- 1.0 - loarea
  return(uparea)
}

f3 <- function(h1, hk, r) {
  x <- c(0.04691008, 0.23076534, 0.5, 0.76923466, 0.95308992)
  w <- c(0.018854042, 0.038088059, 0.0452707394, 0.038088059, 0.018854042)
  h2 <- hk
  h12 <- (h1*h1 + h2*h2)/2.0
  bv <- 0
  if (abs(r) >= 0.7) {
    r2 <- 1.0-r*r
    r3 <- sqrt(r2)
    if (r < 0) h2 <- -h2
    h3 <- h1*h2
    h7 <- exp(-h3 / 2.0)
    if (r2 != 0) {
      h6 <- abs(h1 - h2)
      h5 <- h6 * h6 / 2.0
      h6 <- h6 / r3
      aa <- 0.5 - (h3 / 8.0)
      ab <- 3.0 - (2.0 * aa * h5)
      bv <- 0.13298076 * h6 * ab * f2(h6) - exp(-h5 / r2) * (ab + aa * r2) * 0.053051647
      for (i in 1:5) {
        r1 <- r3 * x[i]
        rr <- r1 * r1
        r2 <- sqrt( 1.0- rr)
        bv <- bv - w[i] * exp(-h5 / rr) * (exp(-h3 / (1.0 + r2)) / r2 / h7 - 1.0 - aa * rr)
      }
    }
    if (r > 0 & h1 > h2) {
      bv <- bv * r3 * h7 + f2(h1)
      return(bv)
    }
    if (r > 0 & h1 <= h2) {
      bv <- bv * r3 * h7 + f2(h2)
      return(bv)
    }
    if (r < 0 & (f2(h1) - f2(h2)) < 0) {
      bv <- 0 - bv * r3 * h7
      return(bv)
    }
    if (r < 0 & (f2(h1) - f2(h2)) >= 0) {
      bv <- (f2(h1) - f2(h2)) - bv * r3 * h7
      return(bv)
    }
  }
  h3 <- h1 * h2
  for (i in 1:5)
  {
    r1 <- r * x[i]
    rr2 <- 1.0 - r1 * r1
    bv <- bv + w[i] * exp((r1 * h3 - h12) / rr2) / sqrt(rr2)
  }
  bv <- f2(h1) * f2(h2) + r * bv
  return(bv)
}



# define functions for contingency table
true_positives <- function(n, toselect, baserate, validity) {round(f3(f1(1.0-toselect/n), f1(1.0-baserate), validity)/(toselect/n)*toselect,1)}
false_positives <- function(n, toselect, baserate, validity) {round(toselect-f3(f1(1.0-toselect/n), f1(1.0-baserate), validity)/(toselect/n)*toselect,1)}
false_negatives <- function(n, toselect, baserate, validity) {round(n*baserate - f3(f1(1.0-toselect/n), f1(1.0-baserate), validity)/(toselect/n)*toselect,1)}
true_negatives <- function(n, toselect, baserate, validity) {n - true_positives(n, toselect,baserate, validity) - false_positives(n, toselect,baserate, validity) - false_negatives(n, toselect,baserate, validity)}


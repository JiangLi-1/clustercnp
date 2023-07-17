#' Title
#'
#' @param a data frame to be added cluster label
#'
#' @return a data frame with cluster label
#' @export
#'
#' @examples clustered <- cluster(validation_data)
cluster <- function(dt_val) {
  pearson_distance <- function(p1, p2) {
    cov_xy <- cov(p1, p2)
    sd_x <- sd(p1)
    sd_y <- sd(p2)

    1 - cov_xy / (sd_x * sd_y)
  }

  center1 <- c(-2.7282584, 0.1434453, 0.3534714, 0.6247529, 0.7738373, -0.1551104, 1.8754291)
  center2 <- c(-1.2839604, -0.9252129, -1.1387279, -1.0104169, -1.5255640, 3.2600753, -1.2208746)
  center3 <- c(2.0482773, 0.6137700, 0.8086425, -1.4112460, 0.4860176, -1.2754554, 2.4103148)
  center4 <- c(0.24637712, 0.31268810, -0.06948763, 0.17350814, 0.62038296, -0.28304073, -3.34326024)
  center5 <- c(0.9673664, 0.8233279, 0.1184735, 2.6922229, 0.3441430, -0.9638551, 0.7633165)


  dt_val1 <- dt_val

  colnames(dt_val1)[1:8] <- c("eid", "age", "meta", "card", "resp", "imcv", "ment", "acu")

  for (i in 1:nrow(dt_val1)) {
    o1 <- dt_val$age[i]
    v1 <- age$st[which(o1 == age$orin)]
    dt_val1$age[i] <- v1[1]

    o2 <- dt_val$meta[i]
    v2 <- meta$st[which(o2 == meta$orin)]
    dt_val1$meta[i] <- v2[1]

    o3 <- dt_val$card[i]
    v3 <- card$st[which(o3 == card$orin)]
    dt_val1$card[i] <- v3[1]

    o4 <- dt_val$resp[i]
    v4 <- resp$st[which(o4 == resp$orin)]
    dt_val1$resp[i] <- v4[1]

    o5 <- dt_val$imcv[i]
    v5 <- imcv$st[which(o5 == imcv$orin)]
    dt_val1$imcv[i] <- v5[1]

    o6 <- dt_val$ment[i]
    v6 <- ment$st[which(o6 == ment$orin)]
    dt_val1$ment[i] <- v6[1]

    o7 <- dt_val$acu[i]
    v7 <- acu$st[which(o7 == acu$orin)]
    dt_val1$acu[i] <- v7[1]
  }

  dt_val1$cluster <- NA

  for (i in 1:nrow(dt_val1)) {
    x <- as.matrix(dt_val1[i, 2:8])[1, ]
    dis <- c(pearson_distance(x, center1), pearson_distance(x, center2), pearson_distance(x, center3), pearson_distance(x, center4), pearson_distance(x, center5))
    dt_val1$cluster[i] <- which(dis == min(dis))
  }

  return(dt_val1)
}






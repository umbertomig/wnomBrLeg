## Code for scoring Brazilian Chamber
## Author: Umberto Mignozzetti
## Warning: takes long time to run
require(wnominate)
require(tidyverse)
require(congressbr)

# Chamber votes 2019
vots <- cham_plenary_bills(year = 2019)
vots2 <- cham_votes('PL', '1292', '1995')
rcdat <- vots2 %>% 
  select(legislator_id, legislator_name, legislator_party) %>%
  unique()
rcdat[514,] = c(1, 'Lider do Governo', 'GOVERNO')

for (i in 1:dim(vots)[1]) {
  bill <- strsplit(vots$bill_name[i], '( |/)', )[[1]]
  aux <- cham_votes(bill[1], bill[2], bill[3])
  aux <- aux %>%
    select_if(names(.) %in% c('rollcall_id', 'legislator_id', 
                              'legislator_vote', 'GOV_orientation'))
  if(sum(names(aux)=='GOV_orientation')==1) {
    aux$GOV_orientation <- recode(aux$GOV_orientation, Sim = 'Sim', Nao = 'Não', 
                                  Obstrucao = 'Não', .default = 'Abst', .missing = 'Faltou') 
  } else {
    aux$GOV_orientation = 'Faltou'
  }
  aux$legislator_vote <- recode(aux$legislator_vote, Sim = 'Sim', Nao = 'Não', 
                                Obstrucao = 'Não', .default = 'Abst', .missing = 'Faltou')
  ids <- unique(aux$rollcall_id)
  for (j in ids) {
    if (sum(names(rcdat) %in% j)==0) {
      aux2 <- aux %>%
        filter(rollcall_id == j)
      lgvt <- aux2$GOV_orientation[1]
      aux2 <- aux2 %>%
        select(legislator_id, legislator_vote) %>%
        rename(!!j := legislator_vote)
      aux2[dim(aux2)[1]+1,] <- c(1,lgvt)
      rcdat <- left_join(rcdat, aux2)
    }
  }
}

rcdat <- data.frame(rcdat)
for (i in 4:dim(rcdat)[2]) {
  rcdat[is.na(rcdat[,i]),i] = 9
  rcdat[rcdat[,i]=='Abst',i] = 9
  rcdat[rcdat[,i]=='Faltou',i] = 9
  rcdat[rcdat[,i]=='Não',i] = 0
  rcdat[rcdat[,i]=='Sim',i] = 1
}
names(rcdat)[3] <- 'party'

rcdat$party <- toupper(rcdat$party)

# Parties
table(rcdat$party)

# Function to plot W-Nom scores
plot.coords_br <-
  function (x, main.title = "W-NOMINATE Coordinates", d1.title = "First Dimension", 
            d2.title = "Second Dimension", dims = c(1, 2), plotBy = "party", 
            color = TRUE, shape = TRUE, cutline = NULL, Legend = TRUE, 
            legend.x = 1, legend.y = 1, ...) {
    if (!class(x) == "nomObject") 
      stop("Input is not of class 'nomObject'.")
    if (!any(colnames(x$legislators) == plotBy)) {
      warning("Variable '", plotBy, "' does not exist in your W-NOMINATE object.")
      types <- rep("Leg", dim(x$legislators)[1])
    }
    else {
      types <- x$legislators[, plotBy]
    }
    if (length(dims) != 2 & x$dimensions != 1) 
      stop("'dims' must be an integer vector of length 2.")
    nparties <- length(unique(types))
    colorlist <- c("pink1", "violet", "mediumpurple1", "slateblue1", "purple", "purple3",
                   "turquoise2", "skyblue", "steelblue", "blue2", "navyblue",
                   "orange", "tomato", "coral2", "palevioletred", "violetred", "red2",
                   "springgreen2", "yellowgreen", "palegreen4",
                   "wheat2", "tan", "tan2", "tan3", "brown",
                   "grey70", "grey50", "grey30", 'black', 'red')
    shapes <- rep(c(16, 15, 17, 18, 19, 3, 4, 8, 20, 23), 3)
    if (color == FALSE) 
      colorlist <- sample(colors()[160:220], 50)
    if (shape == FALSE) 
      shapes <- rep(16, 50)
    if (x$dimensions == 1) {
      coord1D <- x$legislators[, "coord1D"]
      ranking <- rank(x$legislators[, "coord1D"])
      plot(seq(-1, 1, length = length(coord1D)), 1:length(coord1D), 
           type = "n", cex.main = 1.2, cex.lab = 1.2, font.main = 2, 
           xlab = "First Dimension Nominate", ylab = "Rank", 
           main = "1D W-NOMINATE Plot")
      if (Legend) 
        legend(0.67, 0.7 * length(coord1D), unique(types), 
               pch = shapes[1:nparties], col = colorlist[1:nparties], 
               cex = 0.7)
      for (i in 1:nparties) suppressWarnings(points(coord1D[types == 
                                                              unique(types)[i]], ranking[types == unique(types)[i]], 
                                                    pch = shapes[i], col = colorlist[i], cex = 1.1, lwd = 2))
    }
    else {
      coord1D <- x$legislators[, paste("coord", dims[1], "D", 
                                       sep = "")]
      coord2D <- x$legislators[, paste("coord", dims[2], "D", 
                                       sep = "")]
      suppressWarnings(symbols(x = 0, y = 0, circles = 1, inches = FALSE, 
                               asp = 1, main = main.title, xlab = d1.title, ylab = d2.title, 
                               xlim = c(-1, 1), ylim = c(-1, 1), cex.main = 1.2, 
                               cex.lab = 1.2, font.main = 2, lwd = 2, fg = "grey", 
                               frame.plot = FALSE, ...))
      if (!is.null(cutline)) {
        for (i in 1:length(cutline)) {
          if (all(is.na(x$rollcalls[cutline[i], ]))) 
            stop("Roll call for cutline did not meet minimum lopsidedness requirements.")
          add.cutline(c(x$rollcalls[cutline[i], paste("midpoint", 
                                                      dims[1], "D", sep = "")], x$rollcalls[cutline[i], 
                                                                                            paste("spread", dims[1], "D", sep = "")], x$rollcalls[cutline[i], 
                                                                                                                                                  paste("midpoint", dims[2], "D", sep = "")], 
                        x$rollcalls[cutline[i], paste("spread", dims[2], 
                                                      "D", sep = "")]), weight = x$weights[dims[2]]/x$weights[dims[1]], 
                      lwd = 2)
        }
      }
      if (Legend) 
        legend(legend.x, legend.y, unique(types), pch = shapes[1:nparties], 
               bty = "n", col = colorlist[1:nparties], cex = 0.7, ncol = 2, text.width = 0.2)
      for (i in 1:nparties) suppressWarnings(points(coord1D[types == 
                                                              unique(types)[i]], coord2D[types == unique(types)[i]], 
                                                    pch = shapes[i], col = colorlist[i], cex = 1.1, lwd = 2))
    }
  }
rc <- rollcall(rcdat[,-c(1:3)], yea = 1, nay = 0,
               legis.names = rcdat$legislator_name, 
               vote.names = names(rcdat)[-c(1:3)],
               legis.data = rcdat[,c(1:3)])

summary(rc)

wnom <- wnominate(rc, 
                  polarity = c(514, 351), 
                  dims = 2)

plot.coords_br(wnom)
plot(wnom)

# End

########### Testing for colinearity between  variables ############


# Input: a matrix of the column values you want to investigate

# Example of running this function:
# cor.prob2(as.matrix(wolfkde[,c("deer_w2","elk_w2", "moose_w2", "sheep_w2", "goat_w2", "Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2","openConif", "closedConif", "modConif", "burn", "herb", "decid", "burn", "alpine")]))



# Correlations appear below the diagonal and significance probabilities above the diagonal 
cor.prob2 <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  Rstar = ifelse(R[above]<0.05, "***", "NS")
  R[above]=paste(R[above],Rstar)
  R
}


# which outputs to be worried about? Anything with an absolute value greater than .3 to .5

# For these, go through and see if the sign flips to see if they are an unstable coefficient
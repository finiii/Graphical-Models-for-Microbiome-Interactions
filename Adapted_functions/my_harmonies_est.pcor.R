my_harmonies_est.pcor <- function (alpha.matrix, beta.stars = 0.1, n.rep = 20, seed = 123) 
{
  cor.mat = cor(alpha.matrix)
  diag(cor.mat) = 0
  lmd.grid = seq(max(cor.mat), max(cor.mat)/100, by = -0.01)
  hugeargs = list(lambda = lmd.grid, method = "glasso", verbose = F)
  cat("Estimating the sparse precision matrix... \n")
  pulsar.output = pulsar::pulsar(alpha.matrix, fun = huge::huge, fargs = hugeargs, 
                         rep.num = n.rep, thresh = beta.stars, criterion = "stars", 
                         lb.stars = TRUE, ub.stars = TRUE, seed = seed)
  pulsar.fit = refit(pulsar.output)
  icov.refit = pulsar.fit[["est"]][["icov"]]
  sel.num = unlist(base::lapply(icov.refit, function(x) {
    sum(x != 0) - ncol(alpha.matrix)
  }))
  lam.ind.sel = grep(sum(pulsar.fit[["refit"]]$stars), sel.num)[1]
  precision.est = icov.refit[[lam.ind.sel]]
  pcorr = qgraph::wi2net(precision.est)
  pcorr = as.matrix(pcorr)
  return(list(pcorr, pulsar.output))
}

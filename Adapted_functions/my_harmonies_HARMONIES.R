my_harmonies_HARMONIES <- function (count.matrix, phenotype, N.mcmc = 10000, b = 1, h = 20, 
          sparsity.cutoff = 0.5, beta.stars = 0.05, n.rep = 20, bayes.fdr = 0.05, 
          seed = 123) 
{
  if (!all(count.matrix == floor(count.matrix)) | any(count.matrix < 
                                                      0)) {
    stop("Elements in the input count matrix must be nonnegative integers")
  }
  phenotype = check.phenotype(phenotype)
  seed = check.seed(seed)
  if (b <= 0 | h <= 0) {
    stop("parameter 'b' and 'h' must be strictly positive")
  }
  if (N.mcmc != floor(N.mcmc)) {
    stop("N.mcmc should be a large positive integer to ensure convergence (suggested value: >= 10000)")
  }
  if (sparsity.cutoff > 1 | sparsity.cutoff < 0) {
    stop("sparsity.cutoff must be between 0 and 1 (suggested value: 0.5)")
  }
  if (beta.stars < 0 | beta.stars > 1) {
    stop("The instability parameter beta.stars must be between 0-1 (suggested value: 0.05)")
  }
  if (n.rep < 0 | n.rep != floor(n.rep)) {
    stop("Number of subsamples in StARS must be a positive integer (suggested value: 20)")
  }
  if (bayes.fdr < 0 | bayes.fdr > 0.1) {
    stop("bayes.fdr controls the FDR of zero imputation (suggested value: 0-0.1)")
  }
  COUNT.MIN = 2
  mcmc.output = run.ZINBDPP(count.matrix = count.matrix, phenotype = phenotype, 
                            N.mcmc = N.mcmc, b = b, h = h, count.min = COUNT.MIN, 
                            seed = seed)
  if (is.null(colnames(count.matrix))) {
    taxa.names = paste0("taxon", seq(1, ncol(count.matrix)))
  }
  else {
    taxa.names = colnames(count.matrix)
  }
  my_harmonies_get.network_output = my_harmonies_get.network(mcmc.output = mcmc.output, 
                                   count.matrix = count.matrix, phenotype = phenotype, 
                                   sparsity.cutoff = sparsity.cutoff, taxa.names = taxa.names, 
                                   beta.stars = beta.stars, n.rep = n.rep, bayes.fdr = bayes.fdr, 
                                   seed = seed)
  HARMONIES.res.list = my_harmonies_get.network_output[[1]]
  pulsaroutput = my_harmonies_get.network_output[[2]]
  return(list(HARMONIES.res.list, pulsaroutput))
}

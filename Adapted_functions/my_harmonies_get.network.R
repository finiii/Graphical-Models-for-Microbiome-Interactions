my_harmonies_get.network <- function (mcmc.output, count.matrix, phenotype, sparsity.cutoff = 0.5, 
          taxa.names = NULL, beta.stars = 0.1, n.rep = 20, bayes.fdr = 0.01, 
          seed = 123) 
{
  if (!inherits(mcmc.output, "mcmc.zinbdpp")) {
    stop("mcmc.output must be the output from function 'run.MCMC.R' ")
  }
  if (!all(count.matrix == floor(count.matrix)) | any(count.matrix < 
                                                      0)) {
    stop("Elements in the input count matrix must be nonnegative integers")
  }
  phenotype = check.phenotype(phenotype)
  seed = check.seed(seed)
  if (sparsity.cutoff > 1 | sparsity.cutoff < 0) {
    stop("sparsity.cutoff must be between 0 and 1 (suggested value: 0.5)")
  }
  if (beta.stars < 0 | beta.stars > 1) {
    stop("The instability parameter beta.stars must be between 0-1 (suggested value: 0.05)")
  }
  if (n.rep < 0 | n.rep != floor(n.rep)) {
    stop("Number of subsamples in StARS must be a positive integer (suggested value: 20)")
  }
  mcmc.summary = summarize.zinbdpp(mcmc.output = mcmc.output, 
                                   count.matrix = count.matrix, phenotype = phenotype, 
                                   bayes.fdr = bayes.fdr)
  rm(mcmc.output)
  taxa.remove = mcmc.summary$taxa.remove
  count.matrix = count.matrix[, taxa.remove == 0]
  if (is.null(colnames(count.matrix))) {
    taxa.names = paste0("taxon", seq(1, ncol(count.matrix)))
  }
  else {
    taxa.names = colnames(count.matrix)
  }
  taxa.names = taxa.names[taxa.remove == 0]
  rm.char = c("unclassified", "noname", "Virus", "virus")
  rm.idx = sapply(taxa.names, function(x) {
    any(sapply(rm.char, grepl, x))
  })
  rm.idx = which(rm.idx)
  sparse.idx = filter.sparsetaxa(count.matrix[phenotype == 
                                                0, ], sparsity.cutoff = sparsity.cutoff)
  filter.idx = intersect(rm.idx, sparse.idx)
  keep.idx = seq(1, sum(taxa.remove == 0))
  if (length(filter.idx) != 0) {
    keep.idx = keep.idx[-filter.idx]
  }
  if (length(unique(phenotype)) == 2) {
    sparse.idx = filter.sparsetaxa(count.matrix[phenotype == 
                                                  1, ], sparsity.cutoff = sparsity.cutoff)
    filter.idx1 = intersect(rm.idx, sparse.idx)
    keep.idx1 = seq(1, sum(taxa.remove == 0))
    if (length(filter.idx1) != 0) {
      keep.idx1 = keep.idx1[-filter.idx]
    }
  }
  my_harmonies_est.pcor_output = my_harmonies_est.pcor(alpha.matrix = as.matrix(mcmc.summary$nm.alpha0[, 
                                                                        keep.idx]), beta.stars = beta.stars, n.rep = n.rep, 
                        seed = seed)
  pcorr.grp0 = my_harmonies_est.pcor_output[[1]]
  edge.grp0 = summarize.edge(pcorr = pcorr.grp0, taxa.name = taxa.names[keep.idx])
  node.names0 = c()
  if (nrow(edge.grp0) == 0) {
    cat("No detected edge for group labeled with 0 under the current parameter setting. \n")
    abundance.grp0 = data.frame(Taxon = character(), Abundance = numeric(), 
                                stringsAsFactors = F)
  }
  else {
    abundance.ref0 = get.abundance(taxa.names = taxa.names, 
                                   alpha.matrix = mcmc.summary$nm.alpha0, count.matrix = count.matrix[phenotype == 
                                                                                                        0, ])
    node.names0 = unique(c(edge.grp0[, 1], edge.grp0[, 2]))
    name.idx = sapply(node.names0, function(x) {
      which(abundance.ref0[, 1] == x)
    })
    abundance.grp0 = abundance.ref0[name.idx, ]
  }
  if (length(unique(phenotype)) == 1) {
    res.list = list(partial.corr = pcorr.grp0, edge.estimation = edge.grp0, 
                    node.estimation = abundance.grp0)
    pulsaroutput = my_harmonies_est.pcor_output[[2]]
  }
  else {
    node.names1 = c()
    pcorr.grp1 = my_harmonies_est.pcor(alpha.matrix = as.matrix(mcmc.summary$nm.alpha1[, 
                                                                          keep.idx1]), beta.stars = beta.stars, n.rep = n.rep, 
                          seed = seed)[[1]]
    edge.grp1 = summarize.edge(pcorr = pcorr.grp1, taxa.name = taxa.names[keep.idx1])
    node.names1 = unique(c(edge.grp1[, 1], edge.grp1[, 2]))
    gamma.ppi = mcmc.summary$gamma.ppi
    fold.change = mcmc.summary$fold.change
    names(gamma.ppi) = names(fold.change) = taxa.names
    abundance.ref1 = get.abundance(taxa.names = taxa.names, 
                                   alpha.matrix = mcmc.summary$nm.alpha1, count.matrix = count.matrix[phenotype == 
                                                                                                        1, ])
    if (nrow(edge.grp1) == 0) {
      cat("No detected edge for group labeled with 1 under the current parameter setting. \n")
      abundance.grp1 = data.frame(Taxon = character(), 
                                  Abundance = numeric(), stringsAsFactors = F)
      if (nrow(edge.grp0) == 0) {
        abundance.df = data.frame(Taxon = character(), 
                                  AbundanceGroup0 = numeric(), AbundanceGroup1 = numeric(), 
                                  FoldChange = numeric(), PPI = numeric(), stringsAsFactors = F)
        res.list = list(partial.corr = list(Group0 = pcorr.grp0, 
                                            Group1 = pcorr.grp1), edge.estimation = list(Group0 = edge.grp0, 
                                                                                         Group1 = edge.grp1), node.estimation = abundance.df)
      }
      else {
        abundance.df = merge.abundance(taxa = node.names0, 
                                       abundance.df0 = abundance.ref0, abundance.df1 = abundance.ref1, 
                                       fold.change = fold.change, gamma.ppi = gamma.ppi)
        res.list = list(partial.corr = list(Group0 = pcorr.grp0, 
                                            Group1 = pcorr.grp1), edge.estimation = list(Group0 = edge.grp0, 
                                                                                         Group1 = edge.grp1), node.estimation = abundance.df)
      }
    }
    else {
      if (nrow(edge.grp0) == 0) {
        taxa.combine = node.names1
      }
      else {
        taxa.combine = unique(node.names0, node.names1)
      }
      abundance.df = merge.abundance(taxa = taxa.combine, 
                                     abundance.df0 = abundance.ref0, abundance.df1 = abundance.ref1, 
                                     fold.change = fold.change, gamma.ppi = gamma.ppi)
      res.list = list(partial.corr = list(Group0 = pcorr.grp0, 
                                          Group1 = pcorr.grp1), edge.estimation = list(Group0 = edge.grp0, 
                                                                                       Group1 = edge.grp1), node.estimation = abundance.df)
    }
  }
  return(list(res.list, pulsaroutput))
}

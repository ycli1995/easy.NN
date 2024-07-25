
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Roxygen2 calls ###############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.doc_links <- function(nm) {
  pkg <- .pkg_map[nm]
  paste0("\\code{\\link[", pkg, "]{", nm, "}}")
}

.dot_param <- "Arguments passed to other metheds."
.vb_param <- "Print progress."
.seed_param <- "Set a random seed. Setting `NULL` will not set a seed."
.cpu_param <- "Number of threads to use."
.byrow_param <- "If `TRUE` (default), each row of input matrix is treated as an
observation. Otherwise, each observation is stored by column."

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.pkg_map <- c(
  "hnsw_build" = "RcppHNSW",
  "hnsw_search" = "RcppHNSW",

  "nnd_knn" = "rnndescent",
  "rnnd_build" = "rnndescent",
  "rnnd_query" = "rnndescent",

  "AnnoyIndex" = "RcppAnnoy",

  "drop0" = "Matrix"
)









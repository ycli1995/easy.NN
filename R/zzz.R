
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data #########################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' A small example of PCA embeddings from single-cell RNA data
#'
#' A matrix of PCA embeddings. Each row represents a cell, and each column
#' represents a dimension in principal component  space. This matrix was
#' obtained from `pbmc_small` dataset in \pkg{SeuratObject}.
"pca"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package ######################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @importFrom Rcpp evalCpp
#' @importFrom rlang %||%
#' @importFrom magrittr %>%
#' @importFrom methods as is new
#' @importFrom easy.utils verboseMsg
#' @importMethodsFrom Matrix t
#' @useDynLib easy.NN
#' @keywords internal
"_PACKAGE"

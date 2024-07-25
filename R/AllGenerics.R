
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S3 Generics ##################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Nearest neighbors and sparse graph ##########################################

#' Coerce between sparse graph and k nearest-neighbor (kNN) indices and weights
#'
#' Convert between kNN indices and weights (for example, distances) matrices and
#' sparse matrix.
#'
#' @param object An input object.
#' \itemize{
#' \item For `graphToNN`, a sparse matrix (`.doc_links("CsparseMatrix")`
#' or `.doc_links("TsparseMatrix")`. Each column is an observation and should
#' contain "k" non-zero values.
#' \item For `nnToGraph`, a list containing `idx` matrix and `dist` matrix.
#' }
#' @param ... `r .dot_param`
#'
#' @name Graph-to-NN
NULL

#' @returns
#' \itemize{
#' \item `graphToNN`: A list containing an `idx` matrix and `dist` matrix. Each
#' row represents an observation in NN results.
#' }
#'
#' @rdname Graph-to-NN
#' @export graphToNN
graphToNN <- function(object, ...) {
  UseMethod(generic = "graphToNN", object = object)
}

#' @returns
#' \itemize{
#' \item `nnToGraph`: A square sparse matrix where each column and each row
#' represents an observation. The distances of k nearest neighbors are formatted
#' as column major, that is, each column should contains k - 1 non-zero values.
#' }
#'
#' @rdname Graph-to-NN
#' @export nnToGraph
nnToGraph <- function(object, ...) {
  UseMethod(generic = "nnToGraph", object = object)
}

## k Nearest neighbors and shared nearest neighbors ############################

#' Compute shared nearest neighbors from kNN results
#'
#' @param object An object storing kNN results.
#' @param ... `r .dot_param`
#'
#' @name knnToSNN
#' @export knnToSNN
knnToSNN <- function(object, ...) {
  UseMethod(generic = "knnToSNN", object = object)
}

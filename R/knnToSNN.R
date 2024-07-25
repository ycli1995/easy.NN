
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S3 Methods ###################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @param prune Sets the cutoff for acceptable Jaccard index when computing the
#' neighborhood overlap for the SNN construction. Any edges with values less
#' than or equal to this will be set to 0 and removed from the SNN graph.
#' Essentially sets the stringency of pruning (0 — no pruning, 1 — prune
#' everything).
#' @param repr Return `CsparseMatrix` or `TsparseMatrix`.
#'
#' @returns
#' A sparse matrix representing the SNN graph.
#'
#' @examples
#' nn1 <- hnswNN(pca, k = 8)
#' nn1 <- nnToGraph(nn1, use.weights = FALSE)
#' snn <- knnToSNN(nn1)
#'
#' nn2 <- hnswNN(pca, k = 8)
#' snn <- knnToSNN(nn2)  # Use the nn list directly
#'
#' @importFrom Matrix crossprod drop0
#' @importClassesFrom Matrix CsparseMatrix TsparseMatrix
#'
#' @rdname knnToSNN
#' @export
#' @method knnToSNN CsparseMatrix
knnToSNN.CsparseMatrix <- function(
    object,
    prune = 1/15,
    repr = c("C", "T"),
    ...
) {
  repr <- match.arg(repr)
  k <- round(length(object@x) / ncol(object))
  snn <- crossprod(object)
  snn@x <- snn@x / (2 * k - snn@x)
  if (prune > 0) {
    snn <- drop0(snn, tol = prune)
  }
  rownames(snn) <- colnames(snn) <- colnames(object)
  if (repr == "T") {
    return(as(snn, "TsparseMatrix"))
  }
  return(as(snn, "CsparseMatrix"))
}

#' @rdname knnToSNN
#' @export
#' @method knnToSNN list
knnToSNN.list <- function(object, prune = 1/15, repr = c("C", "T"), ...) {
  repr <- match.arg(repr)
  object <- nnToGraph.list(object, use.weights = FALSE, repr = "C", ...)
  return(knnToSNN.CsparseMatrix(object = object, prune = prune, repr = repr))
}

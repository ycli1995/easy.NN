
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S3 Methods ###################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @param repr One of "C" and "T", specifying the representation of the sparse
#' matrix result
#' @param use.weights Whether to weighted the output sparse matrix with `dist`.
#' @param self.loops Whether to allow self-loops in the output sparse graph. If
#' `FALSE`, all diagnoal values will be set to 0.
#'
#' @note
#' Don't run `r .doc_links("drop0")` on the output matrix, otherwise it will not
#' be retained as an NN list. Because the distances between each pair of NN can
#' be 0.
#'
#' @examples
#' data(pca)
#' nn <- hnswNN(pca, k = 8)
#' nn_mat <- nnToGraph(nn)
#' nn2 <- graphToNN(nn_mat)
#'
#' @importFrom Matrix sparseMatrix drop0
#'
#' @rdname Graph-to-NN
#' @export
#' @method nnToGraph list
nnToGraph.list <- function(
    object,
    repr = c("C", "T"),
    use.weights = TRUE,
    self.loops = TRUE,
    ...
) {
  repr <- match.arg(repr)
  if (!"idx" %in% names(object)) {
    stop("'nnToGraph()' needs a list containing at least 'idx' matrix.")
  }
  if (use.weights) {
    if (!"dist" %in% names(object)) {
      stop(
        "'nnToGraph()' needs a list containing 'dist' matrix ",
        "for 'use.weights = TRUE'"
      )
    }
    mat <- rcpp_knn_to_sparse_weights(
      knn_index = object$idx,
      knn_dist = object$dist,
      self_loops = self.loops
    )
  } else {
    mat <- rcpp_knn_to_sparse_idx(
      knn_index = object$idx,
      self_loops = self.loops
    )
  }
  mat <- sparseMatrix(
    i = mat$i,
    j = mat$j,
    x = mat$x,
    repr = repr,
    index1 = FALSE
  )
  colnames(mat) <- rownames(object$idx)
  rownames(mat) <- rownames(object$idx)
  return(mat)
}

#' @importFrom Matrix nnzero
#' @importClassesFrom Matrix CsparseMatrix generalMatrix
#'
#' @rdname Graph-to-NN
#' @export
#' @method graphToNN CsparseMatrix
graphToNN.CsparseMatrix <- function(object, ...) {
  if (!inherits(object, "generalMatrix")) {
    object <- as(object, "generalMatrix")
  }
  k <- round(length(object@x) / ncol(object))
  nn.idx <- matrix(object@i + 1L, ncol = k, byrow = TRUE)
  nn.dist <- matrix(object@x, ncol = k, byrow = TRUE)
  rownames(nn.idx) <- rownames(nn.dist) <- colnames(object)
  return(list(idx = nn.idx, dist = nn.dist))
}

#' @importClassesFrom Matrix TsparseMatrix CsparseMatrix
#'
#' @rdname Graph-to-NN
#' @export
#' @method graphToNN TsparseMatrix
graphToNN.TsparseMatrix <- function(object, ...) {
  object %>%
    as("CsparseMatrix") %>%
    graphToNN.CsparseMatrix()
}


#' Wrapper functions for calculating kNN
#'
#' Wrapper functions for calculating kNN with different methods. The parameters
#' of each method have been unified.
#'
#' @param ... `r .dot_param`
#' \itemize{
#' \item `nndescentNN`: Passed to either `r .doc_links("nnd_knn")` when
#' `query = NULL`, or `r .doc_links("rnnd_build")`
#' \item `annoyNN`: Passed to \code{\link{annoy_search}}
#' }
#'
#' @name knn-helpers
NULL

#' @param data A numeric matrix of observations to build the neighbor indices.
#' @param k Number of nearest neighbors to return.
#' @param query A numeric matrix of data to search for neighbors. If `NULL` as
#' default, will use `data` itself as the query observations.
#' @param metric Type of distance calculation to use.
#' @param n.threads `r .cpu_param`
#' @param seed.use `r .seed_param`
#' @param verbose `r .vb_param`
#' @param by.row `r .byrow_param`
#' @param ef.construction Size of the dynamic list used during construction.
#' Pass to `r .doc_links("hnsw_build")`
#' @param ef Size of the dynamic list used during search. Pass to
#' `r .doc_links("hnsw_search")`
#'
#' @seealso
#' \itemize{
#' \item `r .doc_links("hnsw_build")` and `r .doc_links("hnsw_search")` from
#' \pkg{RcppHNSW}
#' }
#'
#' @returns
#' A list containing `idx` matrix and `dist` matrix. Each row represents an
#' observation in NN results.
#'
#' @examples
#' nn1 <- hnswNN(pca, k = 8)
#'
#' @importFrom RcppHNSW hnsw_build hnsw_search
#' @export
#' @rdname knn-helpers
hnswNN <- function(
    data,
    k,
    query = NULL,
    metric = c("euclidean", "cosine"),
    n.threads = 1,
    seed.use = 42,
    verbose = TRUE,
    by.row = TRUE,
    ef.construction = 200,
    ef = 10
) {
  metric <- match.arg(metric)
  query <- query %||% data
  if (!is.null(seed.use)) {
    set.seed(seed.use)
  }
  ann <- hnsw_build(
    X = data,
    distance = metric,
    ef = ef.construction,
    verbose = verbose,
    n_threads = n.threads,
    byrow = by.row
  )
  if (!is.null(seed.use)) {
    set.seed(seed.use)
  }
  nn <- hnsw_search(
    X = query,
    ann = ann,
    k = k,
    ef = ef,
    verbose = verbose,
    n_threads = n.threads,
    byrow = by.row
  )
  rownames(nn$idx) <- rownames(query)
  rownames(nn$dist) <- rownames(query)
  return(nn)
}

#' @param eps Controls trade-off between accuracy and search cost. Only used in
#' `nndescentNN` when `query` is not `NULL`. Passed to `epsilon` in function
#' `r .doc_links("rnnd_query")`.
#' @param use.alt.metric Whether or not to use faster metrics that maintain the
#' ordering of distances internally. Only used in `nndescentNN`. Default is
#' `FALSE` to avoid using squared Euclidean distances when `metric` is
#' "euclidean". Passed to `use_alt_metric` in `r .doc_links("nnd_knn")`.
#'
#' @seealso
#' #' \itemize{
#' \item `r .doc_links("nnd_knn")` from \pkg{rnndescent}
#' }
#'
#' @examples
#' nn2 <- nndescentNN(pca, k = 8)
#'
#' @importFrom rnndescent nnd_knn rnnd_build rnnd_query
#' @export
#' @rdname knn-helpers
nndescentNN <- function(
    data,
    k,
    query = NULL,
    metric = c(
      'euclidean',
      'cosine',
      'manhattan',
      'hamming',
      'jaccard',
      'braycurtis',
      'canberra',
      'chebyshev',
      'correlation',
      'dice',
      'kulsinski',
      'rogerstanimoto',
      'russellrao',
      'sqeuclidean',
      'sokalmichener',
      'sokalsneath',
      'yule'
    ),
    n.threads = 1,
    seed.use = 42,
    verbose = TRUE,
    by.row = TRUE,
    eps = 0.1,
    use.alt.metric = FALSE,
    ...
) {
  metric <- match.arg(metric)
  obs <- ifelse(by.row, "R", "C")
  if (length(query) == 0) {
    query <- data
    if (!is.null(seed.use)) {
      set.seed(seed.use)
    }
    nn <- nnd_knn(
      data = query,
      k = k,
      metric = metric,
      n_threads = n.threads,
      verbose = verbose,
      use_alt_metric = use.alt.metric,
      obs = obs,
      ...
    )
  } else {
    if (!is.null(seed.use)) {
      set.seed(seed.use)
    }
    index <- rnnd_build(
      data = data,
      k = k,
      metric = metric,
      n_threads = n.threads,
      verbose = verbose,
      use_alt_metric = use.alt.metric,
      obs = obs,
      ...
    )
    if (!is.null(seed.use)) {
      set.seed(seed.use)
    }
    nn <- rnnd_query(
      index = index,
      query = query,
      k = k,
      n_threads = n.threads,
      verbose = verbose,
      epsilon = eps,
      obs = obs
    )
  }
  rownames(nn$idx) <- rownames(query)
  rownames(nn$dist) <- rownames(query)
  return(nn)
}

#' @param n.trees Passed to \code{\link{annoy_build}}.
#' @param search.k Passed to \code{\link{annoy_search}}.
#'
#' @examples
#' nn3 <- annoyNN(pca, k = 8)
#'
#' @export
#' @rdname knn-helpers
annoyNN <- function(
    data,
    k,
    query = NULL,
    metric = c("euclidean", "cosine", "manhattan", "hamming"),
    n.threads = 1,
    seed.use = 42,
    verbose = TRUE,
    by.row = TRUE,
    n.trees = 50,
    search.k = -1,
    ...
) {
  metric <- match.arg(metric)
  query <- query %||% data
  ann <- annoy_build(
    data,
    metric = metric,
    n.trees = n.trees,
    seed.use = seed.use,
    verbose = verbose
  )
  nn <- annoy_search(
    index = ann,
    query = query,
    k = k,
    search.k = search.k,
    n.threads = n.threads,
    ...
  )
  rownames(nn$idx) <- rownames(query)
  rownames(nn$dist) <- rownames(query)
  return(nn)
}

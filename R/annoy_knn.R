
#' Helper functions for RcppAnnoy
#'
#' Helper functions for building index of \pkg{RcppAnnoy} and search kNNs.
#'
#' @name annoy-knn
NULL

#' Build an Annoy index
#'
#' @param data A numeric matrix of data to build the 'annoy' index.
#' @param metric Type of distance calculation to use.
#' @param n.trees Builds a forest of `n.trees` trees. More trees gives higher
#' precision when querying.
#' @param seed.use `r .seed_param`
#' @param verbose `r .vb_param`
#' @param by.row `r .byrow_param`
#'
#' @seealso
#' \itemize{
#' \item `r .doc_links("AnnoyIndex")`
#' }
#'
#' @returns
#' \itemize{
#' \item `annoy_build`: A built `r .doc_links("AnnoyIndex")`.
#' }
#'
#' @examples
#' data(pca)
#' ann <- annoy_build(pca)
#' nn <- annoy_search(ann, pca[1:20, ], k = 8)
#'
#' @importFrom RcppAnnoy AnnoyAngular AnnoyEuclidean AnnoyHamming AnnoyManhattan
#' @export
#' @rdname annoy-knn
annoy_build <- function(
    data,
    metric = c("euclidean", "cosine", "manhattan", "hamming"),
    n.trees = 50,
    seed.use = 42,
    verbose = TRUE,
    by.row = TRUE
) {
  metric <- match.arg(metric)
  n <- ifelse(by.row, ncol(data), nrow(data))
  a <- switch(
    EXPR = metric,
    euclidean = new(AnnoyEuclidean, n),
    cosine = new(AnnoyAngular, n),
    manhattan = new(AnnoyManhattan, n),
    hamming = new(AnnoyHamming, n),
    stop("Invalid metric '", metric, "' for annoy_build().")
  )
  if (!is.null(seed.use)) {
    a$setSeed(seed.use)
  }
  for (i in seq_len(nrow(data))) {
    if (by.row) {
      a$addItem(i - 1, data[i, ])
    } else {
      a$addItem(i - 1, data[, i])
    }
  }
  verboseMsg("Build ", class(a)[1], " using n.trees = ", n.trees)
  a$build(n.trees)
  return(a)
}

#' @param index An `.doc_links("AnnoyIndex")` output by `annoy_build`.
#' @param query A numeric matrix of data to search for neighbors.
#' @param k Number of neighbors to return. Must be within the size of each item
#' in `index`.
#' @param search.k During the query, it will inspect up to `search.k` nodes
#' which defaults to `n.trees` * `k` if not provided.
#' @param include.distance Whether or not to return the distances of kNN.
#' @param n.threads `r .cpu_param`
#' @param chunk.size Used when `n.threads > 1`. How many query observations to
#' be processed in each chunk.
#'
#' @returns
#' \itemize{
#' \item `annoy_search`: A list containing `idx` matrix (and `dist` matrix if
#' `include.distance` is `TRUE`)
#' }
#'
#' @importFrom easy.utils chunkPoints
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importClassesFrom RcppAnnoy Rcpp_AnnoyAngular
#'
#' @export
#' @rdname annoy-knn
annoy_search <- function(
    index,
    query,
    k,
    search.k = -1,
    include.distance = TRUE,
    n.threads = 1,
    chunk.size = 3000L,
    by.row = TRUE
) {
  n.item <- index$getNItems()
  if (n.item == 0) {
    stop("The input AnnoyIndex 'index' doesn't contain any item.")
  }
  if (k > n.item) {
    warning(
      "'k' (", k, ") cannot be larger than 'NItems' (", n.item, "). Set k = ",
      n.item, call. = FALSE, immediate. = TRUE
    )
    k <- n.item
  }
  n <- ifelse(by.row, nrow(query), ncol(query))
  is.angular <- is(index, "Rcpp_AnnoyAngular")
  if (n.threads > 1) {
    if (any(.Platform$OS.type != "unix", .Platform$GUI == "RStudio")) {
      warning(
        "'future' parallel is not supported by 'windows' or 'rstudio', ",
        "reset n.threads to 1",
        immediate. = TRUE, call. = FALSE
      )
      n.threads <- 1
    }
  }
  verboseMsg("Search kNN from AnnoyIndex")
  if (n.threads <= 1) {
    idx <- matrix(0L, nrow = n, ncol = k)
    if (include.distance) {
      dist <- matrix(0, nrow = n, ncol = k)
    }
    for (i in seq_len(n)) {
      if (by.row) {
        tmp.res <- index$getNNsByVectorList(
          query[i, ],
          k, search.k,
          include.distance
        )
      } else {
        tmp.res <- index$getNNsByVectorList(
          query[, i],
          k, search.k,
          include.distance
        )
      }
      idx[i, ] <- tmp.res$item + 1L
      if (include.distance) {
        if (is.angular) {
          dist[i, ] <- tmp.res$distance * tmp.res$distance * 0.5
        } else {
          dist[i, ] <- tmp.res$distance
        }
      }
    }
    if (include.distance) {
      return(list(idx = idx, dist = dist))
    }
    return(list(idx = idx))
  }
  verboseMsg("Set ", n.threads, " workers for 'multicore'")
  oplan <- plan(strategy = "multicore", workers = n.threads)
  on.exit(plan(oplan), add = TRUE)
  chunks <- chunkPoints(n, chunk.size)
  res <- future_lapply(seq_len(ncol(chunks)), FUN = function(i) {
    tmp.nrow <- chunks[2, i] - chunks[1, i] + 1L
    idx <- matrix(0L, nrow = tmp.nrow, ncol = k)
    if (include.distance) {
      dist <- matrix(0, nrow = tmp.nrow, ncol = k)
    }
    for (j in seq_len(tmp.nrow)) {
      query.idx <- chunks[1, i] + j - 1L
      if (by.row) {
        tmp.res <- index$getNNsByVectorList(
          query[query.idx, ],
          k, search.k,
          include.distance
        )
      } else {
        tmp.res <- index$getNNsByVectorList(
          query[, query.idx],
          k, search.k,
          include.distance
        )
      }
      idx[j, ] <- tmp.res$item + 1L
      if (include.distance) {
        if (is.angular) {
          dist[j, ] <- tmp.res$distance * tmp.res$distance * 0.5
        } else {
          dist[j, ] <- tmp.res$distance
        }
      }
    }
    if (include.distance) {
      return(list(idx = idx, dist = dist))
    }
    return(list(idx))
  })
  idx <- list()
  dist <- list()
  for (i in seq_along(res)) {
    idx[[i]] <- res[[i]]$idx
    dist[[i]] <- res[[i]]$dist
  }
  idx <- Reduce(rbind, idx)
  if (!include.distance) {
    return(list(idx = idx))
  }
  dist <- Reduce(rbind, dist)
  return(list(idx = idx, dist = dist))
}

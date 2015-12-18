#' Factors, Redux
#'
#' @param x A vector of data.
#' @param levels An optional character vector to be used as the levels. If
#'   specified, \code{x}, \code{levels}, and additional arguments via \code{...}
#'   are passed through to \code{\link{factor}}.
#' @param logic The logical principle by which levels of \code{x} should be set.
#'   If \code{freq} (the default), the levels are in order of decreasing
#'   frequency, with ties broken alphabetically. If \code{appear}, the levels
#'   are in order of appearance.
#' @param ... Additional arguments for \code{base::factor()}, e.g.
#'   \code{ordered}.
#'
#' @return A factor.
#' @seealso \code{\link{factor}}
#' @export
#'
#' @examples
#' set.seed(1)
#' x <- sample(letters, size = 20, replace = TRUE)
#'
#' if (requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'
#'   ## refactor on-the-fly to get ggplot2 barplot sorted by frequency
#'   ggplot(data.frame(x = refactor(x)), aes(x)) + geom_bar()
#'
#'   ## or create the factor explicitly
#'   xfact <- refactor(x)
#'   ggplot(data.frame(x = xfact), aes(x)) + geom_bar()
#'
#'   ## set levels in order of appearance in the data
#'   xfact_appear <- refactor(x, logic = "appear")
#'   ggplot(data.frame(x = xfact_appear), aes(x)) + geom_bar()
#' }
refactor <- function(x, levels = NULL,
                     logic = c("freq", "appear"), ...) {

  if (!is.null(levels)) {
    return(factor(x, levels = levels, ...))
  }

  logic <- match.arg(logic)

  if (logic == "freq") {
    tabx <- table(x)
    ordx <- order(-1 * tabx, names(tabx))
    return(factor(x, levels = names(tabx)[ordx]))
  }

  if (logic == "appear")
    factor(x, levels = x[!duplicated(x)])

}

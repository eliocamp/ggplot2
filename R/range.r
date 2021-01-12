#' Mutable ranges have a two methods (`train` and `reset`), and make
#' it possible to build up complete ranges with multiple passes.
#'
#' These range objects should be instantiated with
#' [continuous_range()] and [discrete_range()].
#'
#' @noRd
Range <- ggproto("Range", NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

RangeDiscrete <- ggproto("RangeDiscrete", Range,
  train = function(self, x, drop = FALSE, na.rm = FALSE) {
    range <- try(scales::train_discrete(x, self$range, drop = drop, na.rm = na.rm), silent = TRUE)
    if (inherits(range, "try-error")) {
      stop("Continuous value supplied to discrete scale: ", self$scale_name,
           call. = FALSE)
    }
    self$range <- range
  }
)

RangeContinuous <- ggproto("RangeContinuous", Range,
  train = function(self, x) {
    self$range <- scales::train_continuous(x, self$range)
  }
)

continuous_range <- function(scale_name = NULL) {
  ggproto(NULL, RangeContinuous, scale_name = scale_name)
}

discrete_range <- function(scale_name = NULL) {
  ggproto(NULL, RangeDiscrete, scale_name = scale_name)
}

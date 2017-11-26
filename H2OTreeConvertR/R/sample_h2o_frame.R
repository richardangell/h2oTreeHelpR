
#' Sample a H2OFrame.
#'
#' @param h2o_frame \code{H2OFrame} to sample by rows.
#' @param seed seed for random number generation.
#' @param x Either a vector of one or more elements from which to choose, or
#'          a positive integer. Passed to \code{x} in \code{sample}. Cannot
#'          have negative elements or elements larger than nrow \code{H2OFrame}.
#' @param replace \code{logical} Should sampling be with replacement? Passed
#'        to \code{replace} in \code{sample}.
#' @param prob A vector of probability weights for obtaining the elements of
#'             the vector being sampled. Passed to \code{prob} in \code{sample}.
#'
#' @return Sampled \code{H2OFrame}.
#'
#' @examples
#' library(h2o)
#'
#' h2o.init()
#'
#' prostate.hex = h2o.uploadFile(path = system.file("extdata",
#'                                                  "prostate.csv",
#'                                                  package = "h2o"),
#'                               destination_frame = "prostate.hex")
#'
#' sample_h2o_frame(h2o_frame = prostate.hex, size = 100, replace = TRUE)
#'
#' @export
sample_h2o_frame <- function(h2o_frame, seed = 1, x = NULL, size, replace = FALSE, prob = NULL) {

  #---------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Perform sampling
  # Section 2. Split sample into subsamples
  # Section 3. Create H2OFrame subsets from subsamples
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (class(h2o_frame) != 'H2OFrame') {

    stop('h2o_frame should be a H2OFrame.')

  }

  if (is.null(x)) {

    x <- nrow(h2o_frame)

  } else {

    if (any(x < 1)) {

      stop('x should be greater than zero')

    } else if (any(x > nrow(h2o_frame))) {

      stop('x cannot be greater than number of rows in h2o_frame.')

    }

  }

  #---------------------------------------------------------------------------#
  # Section 1. Perform sampling ----
  #---------------------------------------------------------------------------#

  warning('sampled rows indices will be reordered')

  set.seed(seed)

  row_sample <- sample(x = x, size = size, replace = replace, prob = prob)

  #---------------------------------------------------------------------------#
  # Section 2. Split sample into subsamples ----
  #---------------------------------------------------------------------------#

  sample_table <- table(row_sample)

  sample_split <- lapply(0:(max(sample_table)-1),
                         function(x) as.numeric(names(sample_table[which(sample_table > x)])))

  #---------------------------------------------------------------------------#
  # Section 3. Create H2OFrame subsets from subsamples ----
  #---------------------------------------------------------------------------#

  h2o_frame_subsets <- lapply(sample_split, function(x) h2o_frame[x, ])

  h2o_frame_sample <- do.call(h2o.rbind, h2o_frame_subsets)

  return(h2o_frame_sample)

}



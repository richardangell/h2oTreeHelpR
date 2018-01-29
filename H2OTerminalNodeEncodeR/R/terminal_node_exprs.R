#' Get split conditions for terminal nodes
#'
#' This is a wrapper for the \code{get_terminal_node_paths} or
#' \code{get_terminal_node_directions} functions to do input checking and return
#' results in a better form (\code{data.frame})
#'
#' @param split_exprs \code{data.frame} output from \code{get_split_expressions}
#' function.
#' @param terminal_node_paths should decision paths (A > 10 & b < 3 etc) be
#' returned, or directions (LRLL etc)? Default is \code{TRUE}.
#'
#' @return \code{data.frame} containing complete split conditions for terminal
#' nodes.
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
#' prostate.hex$RACE <- as.factor(prostate.hex$RACE)
#'
#' prostate.hex$DPROS <- as.factor(prostate.hex$DPROS)
#'
#' prostate.gbm = h2o.gbm(x = c("AGE", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON"),
#'                        y = "CAPSULE",
#'                        training_frame = prostate.hex,
#'                        ntrees = 5,
#'                        max_depth = 5,
#'                        learn_rate = 0.1)
#'
#' h2o_trees <- H2OTreeConvertR::h2o_tree_convertR(prostate.gbm)
#'
#' tree_1_split_expr <- get_split_expressions(h2o_trees[[1]])
#'
#' # return paths
#' tree_1_terminal_nodes_df <- terminal_node_exprs(df = tree_1_split_expr)
#'
#' # return directions
#' tree_1_terminal_nodes_df2 <- terminal_node_exprs(df = tree_1_split_expr,
#'                                                  terminal_node_paths = FALSE)
#'
#' @export
terminal_node_exprs <- function(split_exprs, terminal_node_paths = TRUE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Run get_terminal_node_paths
  # Section 2. Transform list to data.frame to return
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  if (class(split_exprs) != 'data.frame') {

    stop('expecting split_exprs to be a data.frame (output from get_split_expressions function).')

  }

  expected_cols_expr <- c('node',
                          'left_split', 'right_split',
                          'left_split_str', 'right_split_str',
                          'NA_str', 'NA_direction')

  if (any(!colnames(split_exprs) == expected_cols_expr)) {

    stop('expecting split_exprs to be a data.frame (output from get_split_expressions function),
         some column names are not in the expected; ',
         paste(expected_cols_expr, collapse = ' '), '.')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Get either terminal node paths or directions ----
  #----------------------------------------------------------------------------#

  top_node <- split_exprs[1, 'node']

  if (terminal_node_paths) {

    # returns paths in the form of A < x & B < y etc
    terminal_node_directions_list <- get_terminal_node_paths(df = split_exprs,
                                                             node = top_node)

  } else {

    # returns directions in the form of LRLRLLL etc
    terminal_node_directions_list <- get_terminal_node_directions(df = split_exprs,
                                                                  node = top_node)

  }

  #----------------------------------------------------------------------------#
  # Section 2. Transform list to data.frame to return ----
  #----------------------------------------------------------------------------#

  terminal_node_directions_unlist <- unlist(terminal_node_directions_list)

  # this will be a concatenation of each node visited to get to the terminal
  #   node separated by '.'
  terminal_node_path <- names(terminal_node_directions_unlist)

  terminal_node_path_split <- strsplit(terminal_node_path, '\\.')

  # remove final node name as the terminal node is duplicated and also add
  #   in the top node in the tree as it has been lost
  terminal_node_path_split <- sapply(terminal_node_path_split,
                                     function(x) c(top_node,
                                                   x[1:(length(x) - 1)]))

  # paste back together again the nodes visited to reach the terminal node
  # - having removed the duplicated terminal node
  terminal_node_path <- sapply(terminal_node_path_split,
                               function(x) paste(x, collapse = '.'))

  # remove 1 from the number of nodes to get depth
  terminal_node_depth <- sapply(terminal_node_path_split, length) - 1

  # get the terminal node name
  terminal_node_names <- sapply(terminal_node_path_split,
                                function(x) x[length(x)])

  # remove names from vector of directions so they don't become rownames in the data.frame below
  names(terminal_node_directions_unlist) <- NULL

  results_df <- data.frame(terminal_node = terminal_node_names,
                           terminal_node_depth = terminal_node_depth,
                           terminal_node_path = terminal_node_path,
                           terminal_node_directions = terminal_node_directions_unlist)

  return(results_df)

}


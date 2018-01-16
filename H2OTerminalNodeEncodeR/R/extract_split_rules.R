#' Get split conditions for terminal nodes for all trees in model
#'
#' This function takes the output from \code{H2OTreeConvertR::h2o_tree_convertR}
#' and for each tree in the model determines the split conditions required to
#' reach the terminal nodes.
#'
#' @param h2o_trees_R \code{list} output from the
#' \code{H2OTreeConvertR::h2o_tree_convertR} function.
#' @param terminal_node_paths should decision paths (A > 10 & b < 3 etc) be
#' returned, or directions (LRLL etc)? Default is \code{TRUE}.
#'
#' @return \code{list} containing split conditions to reach terminal nodes for
#' each tree.
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
#' h2o_trees_terminal_node_rules <- extract_split_rules(h2o_trees)
#'
#' @export
extract_split_rules <- function(h2o_trees_R, terminal_node_paths = TRUE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Get split conditions for each edge in all trees
  # Section 2. Get complete split conditions for terminal nodes in trees
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  if (class(h2o_trees_R) != 'list') {

    stop('expecting h2o_trees_R to be a list (output from h2o_tree_model_to_R function).')

  }

  if (any(sapply(h2o_trees_R, class) != 'data.frame')) {

    stop('expecting h2o_trees_R to be a list (output from h2o_tree_model_to_R function),
         some elements are not data.frames.')

  }

  expected_col_names <- c("node",
                          "node_text",
                          "predictions",
                          "left_split",
                          "right_split",
                          "left_split_levels",
                          "right_split_levels",
                          "NA_direction",
                          "node_text_label",
                          "node_variable_type",
                          "split_column",
                          "node_split_point")

  h2o_trees_R_colnames <- sapply(h2o_trees_R, colnames, simplify = TRUE)

  colnames_check <- h2o_trees_R_colnames == expected_col_names

  if (any(!colnames_check)) {

    stop('expecting h2o_trees_R to be a list (output from h2o_tree_model_to_R function),
         some columns names are not in the expected; ',
         paste(expected_col_names, collapse = ' '), '.')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Get split conditions for each edge in all trees ----
  #----------------------------------------------------------------------------#

  trees_split_expr <- lapply(h2o_trees_R, FUN = get_split_expressions)

  #----------------------------------------------------------------------------#
  # Section 2. Get complete split conditions for terminal nodes in trees ----
  #----------------------------------------------------------------------------#

  trees_terminal_node_expr <- lapply(trees_split_expr,
                                     FUN = terminal_node_exprs,
                                     terminal_node_paths = terminal_node_paths)

  return(trees_terminal_node_expr)

}




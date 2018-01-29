#' Get temrinal node paths in terms of L/R directions and variable conditions
#'
#' This function takes the output from \code{H2OTreeConvertR::h2o_tree_convertR}
#' and for each tree returns the terminal node decision paths in terms of L/R
#' directions and variable conditions. This can be used to map the output from
#' \code{h2o::h2o.predict_leaf_node_assignment} to variable condtions.
#'
#' @param h2o_trees_R \code{list} output from the
#' \code{H2OTreeConvertR::h2o_tree_convertR} function.
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
#' h2o_terminal_node_encoding_mapping <- map_h2o_encoding(h2o_trees)
#'
#' @export
map_h2o_encoding <- function(h2o_trees_R) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Run get_terminal_node_paths
  # Section 2. Transform list to data.frame to return
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 1. Get terminal node paths ----
  #----------------------------------------------------------------------------#

  terminal_node_path <- extract_split_rules(h2o_trees_R,
                                            terminal_node_paths = TRUE)

  #----------------------------------------------------------------------------#
  # Section 2. Get terminal node directions ----
  #----------------------------------------------------------------------------#

  terminal_node_directions <- extract_split_rules(h2o_trees_R,
                                                  terminal_node_paths = FALSE)

  #----------------------------------------------------------------------------#
  # Section 3. Combine and return ----
  #----------------------------------------------------------------------------#

  for (i in 1:length(terminal_node_path)) {

    terminal_node_path[[i]]$terminal_node_directions_h2o <-
      terminal_node_directions[[i]]$terminal_node_directions

  }

  return(terminal_node_path)

}










#' Encode input data as terminal nodes for single tree.
#'
#' This function returns the results as a data.frame.
#'
#' @param data input data to encode as terminal nodes from trees. Currently only \code{data.frame}
#'        inputs are supported.
#' @param terminal_node_split_rules splitting rules for terminal nodes in a single tree,
#'        the output from \code{extract_split_rules} function.
#' @param h2o_tree_in_R single tree output from \code{h2o_tree_model_to_R} function.
#' @param tree_name the prefix to be given to column names. Columns will be named prefix followed by
#'        the name of the terminal node. Default = 'tree_'.
#'
#' @return terminal node encoding for input data. Currently only \code{data.frame} is output.
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
#' terminal_node_encoding_tree1 <- encode_terminal_nodes(data = prostate_df,
#'                                                       terminal_node_split_rules = h2o_trees_terminal_node_rules[[1]],
#'                                                       h2o_trees_in_R = h2o_trees[[1]])
#'
#' @export
encode_terminal_nodes_df <- function(data,
                                     terminal_node_split_rules,
                                     h2o_tree_in_R,
                                     tree_name = 'tree_',
                                     return_one_hot = TRUE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 1. Add input data to terminal node split expressions
  # Section 2. Get terminal node encoding for input tree
  # Section 3. Return results in requested form
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 1. Add input data to terminal node split expressions ----
  #----------------------------------------------------------------------------#

  split_columns <- unique(h2o_tree_in_R$split_column)

  split_columns <- split_columns[!is.na(split_columns)]

  terminal_node_rules_for_data <-
    terminal_node_split_rules$terminal_node_directions

  for (i in 1:length(split_columns)) {

    terminal_node_rules_for_data <-
      gsub(split_columns[i],
           paste0(deparse(substitute(data)), '$', split_columns[i]),
           terminal_node_rules_for_data)

  }

  #----------------------------------------------------------------------------#
  # Section 2. Get terminal node encoding for input tree ----
  #----------------------------------------------------------------------------#

  terminal_nodes_encoded <- sapply(terminal_node_rules_for_data,
                                   function(x) eval(parse(text = x)))

  #----------------------------------------------------------------------------#
  # Section 3. Return results in requested form ----
  #----------------------------------------------------------------------------#

  # change boolean to 0/1
  terminal_nodes_encoded <- data.frame(terminal_nodes_encoded * 1)

  if (return_one_hot) {

    # results are already one hot encoded so just add column names
    colnames(terminal_nodes_encoded) <-
      paste0(tree_name, terminal_node_split_rules$terminal_node)

  } else {

    # compress one hot encoding into single factor
    factor_encoding <-
      factor(apply(terminal_nodes_encoded, 1, function(x) which(x == 1)),
             labels = paste0(terminal_node_split_rules$terminal_node))

    # put factor in data.frame
    terminal_nodes_encoded <- data.frame(factor_encoding)

    # name of the single column is just the tree name
    colnames(terminal_nodes_encoded) <- tree_name

  }

  return(terminal_nodes_encoded)

}





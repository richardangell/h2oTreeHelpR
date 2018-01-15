#' Encode input data as terminal nodes in tree based model.
#'
#' @param data input data to encode as terminal nodes from trees. Currently only \code{data.frame}
#'        inputs are supported.
#' @param terminal_node_split_rules \code{list} of splitting rules for terminal nodes in each tree,
#'        the output from \code{extract_split_rules} function.
#' @param h2o_trees_in_R \code{list} of trees output from \code{h2o_tree_model_to_R} function.
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
#' h2o_trees <- h2o_tree_model_to_R(h2o_model = prostate.gbm,
#'                                  mojo_output_path = '/Users/UserName/Desktop',
#'                                  gv_output_path = '/Users/UserName/Desktop',
#'                                  model_ini_overwrite = TRUE,
#'                                  h2o_jar_file = 'h2o.jar')
#'
#' h2o_trees_terminal_node_rules <- extract_split_rules(h2o_trees)
#'
#' terminal_node_encoding <- encode_terminal_nodes(data = prostate_df,
#'                                                 terminal_node_split_rules = h2o_trees_terminal_node_rules,
#'                                                 h2o_trees_in_R = h2o_trees)
#'
#' @export
encode_terminal_nodes <- function(data, terminal_node_split_rules, h2o_trees_in_R) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Terminal node encoding for data.frame
  # Section 2. Terminal node encoding for data.table
  # Section 3. Terminal node encoding for H2OFrame
  # Section 4. Return results
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  data_accepted_classes <- c('data.frame', 'data.table', 'H2OFrame')

  if (!class(data) %in% data_accepted_classes) {

    stop('data should be one of the following classes; ',
         paste(data_accepted_classes, collapse = ' '),
         ' - but got; ',
         class(data))

  }

  if (class(terminal_node_split_rules) != 'list') {

    stop('expecting terminal_node_split_rules to be a list (output from extract_split_rules function).')

  }

  if (!any(sapply(terminal_node_split_rules, class) == 'data.frame')) {

    stop('expecting terminal_node_split_rules to be a list (output from extract_split_rules function),
         some elements are not data.frames.')

  }

  expected_split_rules_columns <- c("terminal_node",
                                    "terminal_node_depth",
                                    "terminal_node_path",
                                    "terminal_node_directions")

  split_rules_colnames <- sapply(terminal_node_split_rules, colnames, simplify = TRUE)

  colnames_check <- split_rules_colnames == expected_split_rules_columns

  if (any(!colnames_check)) {

    stop('expecting terminal_node_split_rules to be a list (output from extract_split_rules function),
         some columns names are not in the expected; ',
         paste(expected_split_rules_columns, collapse = ' '), '.')

  }


  if (class(h2o_trees_in_R) != 'list') {

    stop('expecting h2o_trees_in_R to be a list (output from h2o_tree_model_to_R function).')

  }

  if (any(sapply(h2o_trees_in_R, class) != 'data.frame')) {

    stop('expecting h2o_trees_in_R to be a list (output from h2o_tree_model_to_R function),
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

  h2o_trees_in_R_colnames <- sapply(h2o_trees_in_R, colnames, simplify = TRUE)

  colnames_check <- h2o_trees_in_R_colnames == expected_col_names

  if (any(!colnames_check)) {

    stop('expecting h2o_trees_in_R to be a list (output from h2o_tree_model_to_R function),
         some columns names are not in the expected; ',
         paste(expected_col_names, collapse = ' '), '.')

  }

  if (length(h2o_trees_in_R) != length(terminal_node_split_rules)) {

    stop('h2o_trees_in_R and terminal_node_split_rules should be of the same length, corresponding to
          the same model and hence number of trees / items.')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Terminal node encoding for data.frame ----
  #----------------------------------------------------------------------------#

  if (class(data) %in% 'data.frame') {

    trees_terminal_nodes_encoded_list <- mapply(function(x, y, z) encode_terminal_nodes_tree_df(data = data,
                                                                                                terminal_node_split_rules = y,
                                                                                                h2o_tree_in_R = z,
                                                                                                tree_name = paste0('tree_', x, '_')),
                                           1:length(h2o_trees_in_R),
                                           terminal_node_split_rules,
                                           h2o_trees_in_R)

    trees_terminal_nodes_encoded <- do.call(cbind, trees_terminal_nodes_encoded_list)

  #----------------------------------------------------------------------------#
  # Section 2. Terminal node encoding for data.table ----
  #----------------------------------------------------------------------------#

  } else if (class(data) %in% 'data.table') {

    stop('data.table not currently supported')

  #----------------------------------------------------------------------------#
  # Section 3. Terminal node encoding for H2OFrame ----
  #----------------------------------------------------------------------------#

  } else if (class(data) %in% 'H2OFrame') {

    stop('H2OFrame not currently supported')

  }

  #----------------------------------------------------------------------------#
  # Section 4. Return results ----
  #----------------------------------------------------------------------------#

  return(trees_terminal_nodes_encoded)

}









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
#' h2o_trees <- h2o_tree_model_to_R(h2o_model = prostate.gbm,
#'                                  mojo_output_path = '/Users/UserName/Desktop',
#'                                  gv_output_path = '/Users/UserName/Desktop',
#'                                  model_ini_overwrite = TRUE,
#'                                  h2o_jar_file = 'h2o.jar')
#'
#' h2o_trees_terminal_node_rules <- extract_split_rules(h2o_trees)
#'
#' terminal_node_encoding_tree1 <- encode_terminal_nodes(data = prostate_df,
#'                                                       terminal_node_split_rules = h2o_trees_terminal_node_rules[[1]],
#'                                                       h2o_trees_in_R = h2o_trees[[1]])
#'
#' @export
encode_terminal_nodes_tree_df <- function(data, terminal_node_split_rules, h2o_tree_in_R, tree_name = 'tree_') {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 1. Add input data to terminal node split expressions
  # Section 2. Get terminal node encoding for input tree
  # Section 3. Return results as 0/1 and data.frame
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 1. Add input data to terminal node split expressions ----
  #----------------------------------------------------------------------------#

  split_columns <- unique(h2o_tree_in_R$split_column)

  split_columns <- split_columns[!is.na(split_columns)]

  terminal_node_rules_for_data <- terminal_node_split_rules$terminal_node_directions

  for (i in 1:length(split_columns)) {

    terminal_node_rules_for_data <- gsub(split_columns[i],
                                         paste0(deparse(substitute(data)), '$', split_columns[i]),
                                         terminal_node_rules_for_data)

  }

  #----------------------------------------------------------------------------#
  # Section 2. Get terminal node encoding for input tree ----
  #----------------------------------------------------------------------------#

  terminal_nodes_encoded <- sapply(terminal_node_rules_for_data,
                                   function(x) eval(parse(text = x)))

  colnames(terminal_nodes_encoded) <- paste0(tree_name, terminal_node_split_rules$terminal_node)

  #----------------------------------------------------------------------------#
  # Section 3. Return results as 0/1 and data.frame ----
  #----------------------------------------------------------------------------#

  terminal_nodes_encoded <- data.frame(terminal_nodes_encoded * 1)

  return(terminal_nodes_encoded)

}




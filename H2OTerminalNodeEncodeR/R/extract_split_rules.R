#' Get split conditions for terminal nodes for all trees in model.
#'
#' This function takes the output from \code{h2o_tree_model_to_R} and for each
#' tree in the model determines the split conditions required to reach the
#' terminal nodes.
#'
#' @param h2o_trees_in_R \code{list} output from \code{h2o_tree_model_to_R}
#'        function.
#'
#' @return \code{list} containing split conditions to reach terminal nodes for
#'         each tree.
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
#' @export
extract_split_rules <- function(h2o_trees_in_R, terminal_node_paths = TRUE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Get split conditions for each edge in all trees
  # Section 2. Get complete split conditions for terminal nodes in all trees
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

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

  #----------------------------------------------------------------------------#
  # Section 1. Get split conditions for each edge in all trees ----
  #----------------------------------------------------------------------------#

  trees_split_expr <- lapply(h2o_trees_in_R, FUN = get_split_expressions)

  #----------------------------------------------------------------------------#
  # Section 2. Get complete split conditions for terminal nodes in all trees ----
  #----------------------------------------------------------------------------#

  trees_terminal_node_expr <- lapply(trees_split_expr,
                                     FUN = terminal_node_exprs,
                                     terminal_node_paths = terminal_node_paths)

  return(trees_terminal_node_expr)

}








#' Get split conditions for edges in tree.
#'
#' This function manipulates the output of \code{h2o_tree_model_to_R} function
#' to get the split condition for each edge in a tree.
#'
#' @param h2o_tree_in_R a sinlge \code{data.frame} output from \code{h2o_tree_model_to_R}.
#'
#' @return \code{data.frame} containing left and right split conditions for parent nodes.
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
#' tree_1_split_expr <- get_split_expressions(h2o_trees[[1]])
#'
#' @export
get_split_expressions <- function(h2o_tree_in_R) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Split edges text by \\n delimiter
  # Section 2. Determine which split NAs take
  # Section 3. Determine the split condition for numeric variables
  # Section 4. Determine the split condition for categorical variables
  # Section 5. Return info in data.frame
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  if (class(h2o_tree_in_R) != 'data.frame') {

    stop('expecting h2o_tree_in_R to be a data.frame (output from mojo_gv_to_table function).')

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

  if (any(!colnames(h2o_tree_in_R) == expected_col_names)) {

    stop('expecting h2o_tree_in_R to be a data.frame (output from mojo_gv_to_table function),
         some column names are not in the expected; ',
         paste(expected_col_names, collapse = ' '), '.')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Split edges text by | delimiter ----
  #----------------------------------------------------------------------------#

  left_split_values <- strsplit(h2o_tree_in_R$left_split_levels, '\\|')

  right_split_values <- strsplit(h2o_tree_in_R$right_split_levels, '\\|')

  #----------------------------------------------------------------------------#
  # Section 2. Determine which split NAs take ----
  #----------------------------------------------------------------------------#

  NA_str <- rep(NA, nrow(h2o_tree_in_R))

  # code to check NA for each variable
  NA_str[!is.na(h2o_tree_in_R$NA_direction)] <- paste0('is.na(',
                                                       h2o_tree_in_R$split_column[!is.na(h2o_tree_in_R$NA_direction)],
                                                       ')')

  #----------------------------------------------------------------------------#
  # Section 3. Determine the split condition for numeric variables ----
  #----------------------------------------------------------------------------#

  left_split_str <- rep(NA, nrow(h2o_tree_in_R))

  right_split_str <- rep(NA, nrow(h2o_tree_in_R))

  # remove NA from split conditions
  num_left_split_direction <- sapply(left_split_values[which(h2o_tree_in_R$node_variable_type == 'numeric')],
                                     function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x)

  num_right_split_direction <- sapply(right_split_values[which(h2o_tree_in_R$node_variable_type == 'numeric')],
                                      function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x)

  # put together code for split condition for numeric variables
  left_split_str[which(h2o_tree_in_R$node_variable_type == 'numeric')] <- paste(h2o_tree_in_R$split_column[which(h2o_tree_in_R$node_variable_type == 'numeric')],
                                                                                num_left_split_direction,
                                                                                h2o_tree_in_R$node_split_point[which(h2o_tree_in_R$node_variable_type == 'numeric')])

  right_split_str[which(h2o_tree_in_R$node_variable_type == 'numeric')] <- paste(h2o_tree_in_R$split_column[which(h2o_tree_in_R$node_variable_type == 'numeric')],
                                                                                 num_right_split_direction,
                                                                                 h2o_tree_in_R$node_split_point[which(h2o_tree_in_R$node_variable_type == 'numeric')])

  #----------------------------------------------------------------------------#
  # Section 4. Determine the split condition for categorical variables ----
  #----------------------------------------------------------------------------#

  # remove NA from split conditions
  char_left_split_direction <- sapply(left_split_values[which(h2o_tree_in_R$node_variable_type == 'categorical')],
                                      function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x,
                                      simplify = FALSE)

  char_right_split_direction <- sapply(right_split_values[which(h2o_tree_in_R$node_variable_type == 'categorical')],
                                       function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x,
                                       simplify = FALSE)

  # put together code for split condition for numeric variables
  left_split_str[which(h2o_tree_in_R$node_variable_type == 'categorical')] <- mapply(function(x, y) paste0(x, " %in% c('", paste0(y, collapse = "','"), "')"),
                                                                                     h2o_tree_in_R$split_column[which(h2o_tree_in_R$node_variable_type == 'categorical')],
                                                                                     char_left_split_direction)

  right_split_str[which(h2o_tree_in_R$node_variable_type == 'categorical')] <- mapply(function(x, y) paste0(x, " %in% c('", paste0(y, collapse = "','"), "')"),
                                                                                      h2o_tree_in_R$split_column[which(h2o_tree_in_R$node_variable_type == 'categorical')],
                                                                                      char_right_split_direction)

  #----------------------------------------------------------------------------#
  # Section 5. Return info in data.frame ----
  #----------------------------------------------------------------------------#

  split_expr_df <- data.frame(node = h2o_tree_in_R$node,
                              left_split = h2o_tree_in_R$left_split,
                              right_split = h2o_tree_in_R$right_split,
                              left_split_str = left_split_str,
                              right_split_str = right_split_str,
                              NA_str = NA_str,
                              NA_direction = h2o_tree_in_R$NA_direction,
                              stringsAsFactors = FALSE)

  return(split_expr_df)

}










#' Get split conditions for terminal nodes.
#'
#' This is a wrapper for get_terminal_node_paths to do input checking and return
#' results in a better form (\code{data.frame}).
#'
#' @param split_exprs \code{data.frame} output from get_split_expressions function.
#'
#' @return \code{data.frame} containing complete split conditions for terminal nodes.
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
#' tree_1_split_expr <- get_split_expressions(h2o_trees[[1]])
#'
#' tree_1_terminal_nodes_df <- terminal_node_exprs(df = tree_1_split_expr)
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








#' Get split conditions for terminal nodes.
#'
#' This function uses recursion to append all split conditions that lead to terminal nodes.
#'
#' @param df \code{data.frame} output from get_split_expressions function.
#' @param node name of top node in tree (to start descending from).
#' @param current_str split condition to be passed through subsequent function calls in
#'        in order to keep all conditions that lead to terminal nodes. Default is NULL.
#' @param unique_expr should unique split expressions for terminal nodes be returned?
#'        Default is \code{TRUE}.
#'
#' @return Nested \code{list} containing appeneded split conditions for terminal nodes.
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
#' tree_1_split_expr <- get_split_expressions(h2o_trees[[1]])
#'
#' tree_1_terminal_nodes_list <- get_terminal_node_paths(df = tree_1_split_expr,
#'                                                            node = tree_1_split_expr[1, 'node'],
#'                                                            current_str = NULL)
#'
#' @export
get_terminal_node_paths <- function(df, node, current_str = NULL, unique_expr = TRUE) {

  # if the current node is a terminal node then paste together all split
  #   directions and return in list
  if (is.na(df[df$node == node, 'left_split'])) {

    #print('returning TERMINAL NODE.....')

    #print(node)

    #print('........')

    #print(current_str)

    ret_list <- list()

    if (unique_expr) {

      ret_list[[node]] <- paste(unique(current_str), collapse = ' & ')

    } else {

      ret_list[[node]] <- paste(current_str, collapse = ' & ')

    }

    return(ret_list)

  } else {

    # append current split instructions with the instructions for the left node
    left_current_str <- c(current_str, df[df$node == node, 'left_split_str'])

    # append current split instructions with the instructions for the right node
    right_current_str <- c(current_str, df[df$node == node, 'right_split_str'])

    # if NAs get directed from the current node add them into the vector of split instructions
    # note, adding the NA conditions is done differently to the other split conditions
    #   the or is added at this point rather than just concatenating the expression to
    #   avoid having to sort this out later on
    if (!is.na(df[df$node == node, 'NA_direction'])) {

      if (df[df$node == node, 'NA_direction'] == 'left') {

        left_current_str[length(left_current_str)] <-
          paste0('(',
                 left_current_str[length(left_current_str)],
                 ' | ',
                 df[df$node == node, 'NA_str'],
                 ')')

      } else {

        right_current_str[length(right_current_str)] <-
          paste0('(',
                 right_current_str[length(right_current_str)],
                 ' | ',
                 df[df$node == node, 'NA_str'],
                 ')')

      }

    }

    # call function again on the left child node
    l <- get_terminal_node_paths(df, df[df$node == node, 'left_split'], left_current_str)

    # call function again on the right child node
    r <- get_terminal_node_paths(df, df[df$node == node, 'right_split'], right_current_str)

    ret_list <- list()

    ret_list[[df[df$node == node, 'left_split']]] <- l

    ret_list[[df[df$node == node, 'right_split']]] <- r

    # return results in list
    return(ret_list)

  }

}










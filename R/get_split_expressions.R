#' Get split conditions for edges in tree
#'
#' This function manipulates the output of
#' \code{H2OTreeConvertR::h2o_tree_convertR} to get the split condition for each
#' edge in a tree
#'
#' @param h2o_tree_R a sinlge \code{data.frame} output from
#' \code{H2OTreeConvertR::h2o_tree_convertR}.
#'
#' @return \code{data.frame} containing left and right split conditions for
#' parent nodes.
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
#' @export
get_split_expressions <- function(h2o_tree_R) {

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

  if (class(h2o_tree_R) != 'data.frame') {

    stop('expecting h2o_tree_R to be a data.frame (output from mojo_gv_to_table function).')

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

  if (any(!colnames(h2o_tree_R) %in% expected_col_names)) {

    stop('expecting h2o_tree_R to be a data.frame (output from mojo_gv_to_table function),
         some column names are not in the expected; ',
         paste(expected_col_names, collapse = ' '), '.')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Split edges text by | delimiter ----
  #----------------------------------------------------------------------------#

  left_split_values <- strsplit(h2o_tree_R$left_split_levels, '\\|')

  right_split_values <- strsplit(h2o_tree_R$right_split_levels, '\\|')

  #----------------------------------------------------------------------------#
  # Section 2. Determine which split NAs take ----
  #----------------------------------------------------------------------------#

  NA_str <- rep(NA, nrow(h2o_tree_R))

  # code to check NA for each variable
  NA_str[!is.na(h2o_tree_R$NA_direction)] <-
    paste0('is.na(',
           h2o_tree_R$split_column[!is.na(h2o_tree_R$NA_direction)],
           ')')

  #----------------------------------------------------------------------------#
  # Section 3. Determine the split condition for numeric variables ----
  #----------------------------------------------------------------------------#

  left_split_str <- rep(NA, nrow(h2o_tree_R))

  right_split_str <- rep(NA, nrow(h2o_tree_R))

  # remove NA from split conditions
  num_left_split_direction <-
    sapply(left_split_values[which(h2o_tree_R$node_variable_type == 'numeric')],
           function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x)

  num_right_split_direction <-
    sapply(right_split_values[which(h2o_tree_R$node_variable_type == 'numeric')],
           function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x)

  # put together code for split condition for numeric variables
  left_split_str[which(h2o_tree_R$node_variable_type == 'numeric')] <-
    paste(h2o_tree_R$split_column[which(h2o_tree_R$node_variable_type == 'numeric')],
          num_left_split_direction,
          h2o_tree_R$node_split_point[which(h2o_tree_R$node_variable_type == 'numeric')])

  right_split_str[which(h2o_tree_R$node_variable_type == 'numeric')] <-
    paste(h2o_tree_R$split_column[which(h2o_tree_R$node_variable_type == 'numeric')],
          num_right_split_direction,
          h2o_tree_R$node_split_point[which(h2o_tree_R$node_variable_type == 'numeric')])

  #----------------------------------------------------------------------------#
  # Section 4. Determine the split condition for categorical variables ----
  #----------------------------------------------------------------------------#

  # remove NA from split conditions
  char_left_split_direction <-
    sapply(left_split_values[which(h2o_tree_R$node_variable_type == 'categorical')],
           function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x,
           simplify = FALSE)

  char_right_split_direction <-
    sapply(right_split_values[which(h2o_tree_R$node_variable_type == 'categorical')],
           function(x) if (any(x == '[NA]')) x[-which(x == '[NA]')] else x,
           simplify = FALSE)

  # put together code for split condition for numeric variables
  left_split_str[which(h2o_tree_R$node_variable_type == 'categorical')] <-
    mapply(function(x, y) paste0(x, " %in% c('", paste0(y, collapse = "','"), "')"),
           h2o_tree_R$split_column[which(h2o_tree_R$node_variable_type == 'categorical')],
           char_left_split_direction)

  right_split_str[which(h2o_tree_R$node_variable_type == 'categorical')] <-
    mapply(function(x, y) paste0(x, " %in% c('", paste0(y, collapse = "','"), "')"),
           h2o_tree_R$split_column[which(h2o_tree_R$node_variable_type == 'categorical')],
           char_right_split_direction)

  #----------------------------------------------------------------------------#
  # Section 5. Return info in data.frame ----
  #----------------------------------------------------------------------------#

  split_expr_df <- data.frame(node = h2o_tree_R$node,
                              left_split = h2o_tree_R$left_split,
                              right_split = h2o_tree_R$right_split,
                              left_split_str = unlist(left_split_str),
                              right_split_str = unlist(right_split_str),
                              NA_str = NA_str,
                              NA_direction = h2o_tree_R$NA_direction,
                              stringsAsFactors = FALSE)

  return(split_expr_df)

}



#' Get split conditions for terminal nodes
#'
#' This function uses recursion to append all split conditions that lead to
#' terminal nodes.
#'
#' @param df \code{data.frame} output from get_split_expressions function.
#' @param node name of top node in tree (to start descending from).
#' @param current_str split condition to be passed through subsequent function
#' calls in order to keep all conditions that lead to terminal nodes. Default is
#' NULL.
#' @param unique_expr should unique split expressions for terminal nodes be
#' returned? Default is \code{TRUE}.
#'
#' @return Nested \code{list} containing appeneded split conditions for terminal
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
#' tree_1_terminal_nodes_list <- get_terminal_node_paths(df = tree_1_split_expr,
#'                                                       node = tree_1_split_expr[1, 'node'],
#'                                                       current_str = NULL)
#'
#' @export
get_terminal_node_paths <- function(df,
                                    node,
                                    current_str = NULL,
                                    unique_expr = TRUE) {

  # if the current node is a terminal node then paste together all split
  #   directions and return in list
  if (is.na(df[df$node == node, 'left_split'])) {

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

    # if NAs get directed from the current node add them into the vector of
    # split instructions
    # note, adding the NA conditions is done differently to the other split
    #   conditions the or is added at this point rather than just concatenating
    #   the expression to avoid having to sort this out later on
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
    l <- get_terminal_node_paths(df,
                                 df[df$node == node, 'left_split'],
                                 left_current_str,
                                 unique_expr)

    # call function again on the right child node
    r <- get_terminal_node_paths(df,
                                 df[df$node == node, 'right_split'],
                                 right_current_str,
                                 unique_expr)

    ret_list <- list()

    ret_list[[df[df$node == node, 'left_split']]] <- l

    ret_list[[df[df$node == node, 'right_split']]] <- r

    # return results in list
    return(ret_list)

  }

}





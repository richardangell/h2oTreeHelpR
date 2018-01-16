
get_terminal_node_directions <- function(df, node, current_str = NULL, unique_expr = FALSE) {

  # if the current node is a terminal node then paste together all split
  #   directions and return in list
  if (is.na(df[df$node == node, 'left_split'])) {

    #print('returning TERMINAL NODE.....')

    #print(node)

    #print('........')

    #print(current_str)

    ret_list <- list()

    if (unique_expr) {

      ret_list[[node]] <- paste(unique(current_str), collapse = '')

    } else {

      ret_list[[node]] <- paste(current_str, collapse = '')

    }

    return(ret_list)

  } else {

    # append current split instructions with the instructions for the left node
    left_current_str <- c(current_str, "L")

    # append current split instructions with the instructions for the right node
    right_current_str <- c(current_str, "R")

    # call function again on the left child node
    l <- get_terminal_node_directions(df, df[df$node == node, 'left_split'], left_current_str, unique_expr)

    # call function again on the right child node
    r <- get_terminal_node_directions(df, df[df$node == node, 'right_split'], right_current_str, unique_expr)

    ret_list <- list()

    ret_list[[df[df$node == node, 'left_split']]] <- l

    ret_list[[df[df$node == node, 'right_split']]] <- r

    # return results in list
    return(ret_list)

  }

}

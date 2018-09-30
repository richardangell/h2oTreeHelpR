#' Extract tree info from H2OTree object
#'
#' Takes a \code{H2OTree} object (output from \code{h2o.getModelTree}) and  
#' returns a \code{data.frame} representing each tree in the model in tabular 
#' structure.
#'
#' @param h2o_tree \code{H2OTree} object to extract info from.
#' @param get_internal_predictions \code{logical}, default = \code{FALSE} will 
#' return only predictions for leaf (terminal) nodes, otherwise internal node
#' predictions are also returned.
#'
#' @return returns a \code{data.frame} containing the tree structure. 
#' Tree structure \code{data.frame}s contain the following columns;
#' \itemize{
#'   \item{"node"} {name of the node in the tree}
#'   \item{"predictions"} {predicted values for terminal nodes or NA}
#'   \item{"left_split"} {left child node or NA if a terminal node}
#'   \item{"right_split"} {right child node or NA if a terminal node}
#'   \item{"left_split_levels"} {levels of the split variable that are sent to
#'   the left child node from the current node, separated by |}
#'   \item{"right_split_levels"} {levels of the split variable that are sent
#'   to the left child node from the current node, separated by |}
#'   \item{"NA_direction"} {the direction missing values are sent from the
#'   current node}
#'   \item{"node_variable_type"} {split variable type for node, either
#'   categorical or numeric}
#'   \item{"split_column"} {the name of the split column for the current node}
#'   \item{"node_split_point"} {the split value for the current node if numeric,
#'   otherwise NA}
#' }
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
#' prostate.hex["RACE"] = as.factor(prostate.hex["RACE"])
#'
#' prostate.hex["DPROS"] = as.factor(prostate.hex["DPROS"])
#'
#' expl_cols <- c("AGE", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON")
#'
#' prostate.gbm = h2o.gbm(x = expl_cols,
#'                        y = "CAPSULE",
#'                        training_frame = prostate.hex,
#'                        ntrees = 5,
#'                        max_depth = 5,
#'                        learn_rate = 0.1)
#'
#' h2o_tree <- h2o.getModelTree(prostate.gbm, 1)
#'
#' h2o_tree_df <- extract_H2OTree_info(h2o_tree)
#'
#' @export
extract_H2OTree_info <- function(h2o_tree, get_internal_predictions = FALSE) {
  
  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Get nodes
  # Section 2. Get predictions
  # Section 3. Get left split nodes
  # Section 4. Get right split nodes
  # Section 5. Get node split points
  # Section 6. Identify numeric split variables
  # Section 7. Get left split levels
  # Section 8. Get right split levels
  # Section 9. Identify categorical split variables
  # Section 10. Get NA directions
  # Section 11. Add left and right directions for numeric variables
  # Section 12. Add NAs to right_split_levels
  # Section 13. Get split columns
  # Section 14. Order columns
  # Section 15. Return tree info
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (class(h2o_tree) != 'H2OTree') {
    
    stop('h2o_tree should be H2OTree class')
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Get nodes ----
  #----------------------------------------------------------------------------#
  
  n_nodes <- length(h2o_tree@left_children)
  
  h2o_tree_df <- data.frame(node = paste0('SG_0_Node_', h2o_tree@node_ids),
                            stringsAsFactors = FALSE)
  
  #----------------------------------------------------------------------------#
  # Section 2. Get predictions ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$predictions <- h2o_tree@predictions
  
  # remove internal node predictions if get_internal_predictions is FALSE
  if (!get_internal_predictions) { 
  
    h2o_tree_df$predictions[which(h2o_tree@left_children != -1)] <- NA
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 3. Get left split nodes ----
  #----------------------------------------------------------------------------#

  # h2o_tree@left_children gives the indices of left child nodes starting at 0
  left_child_nodes <- h2o_tree@left_children + 1
  left_child_nodes[which(left_child_nodes == 0)] <- NA
  h2o_tree_df$left_split <- h2o_tree_df$node[left_child_nodes]
  
  # identify terminal nodes
  terminal_nodes <- which(h2o_tree@left_children == -1)
  
  h2o_tree_df$left_split[terminal_nodes] <- NA
  
  #----------------------------------------------------------------------------#
  # Section 4. Get right split nodes ----
  #----------------------------------------------------------------------------#
  
  # h2o_tree@right_children gives the indices of right child nodes starting at 0
  right_child_nodes <- h2o_tree@right_children + 1
  right_child_nodes[which(right_child_nodes == 0)] <- NA
  h2o_tree_df$right_split <- h2o_tree_df$node[right_child_nodes]
  
  h2o_tree_df$right_split[terminal_nodes] <- NA
  
  #----------------------------------------------------------------------------#
  # Section 5. Get node split points ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$node_split_point <- h2o_tree@thresholds
  
  #----------------------------------------------------------------------------#
  # Section 6. Identify numeric split variables ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$node_variable_type <- NA
  
  h2o_tree_df$node_variable_type[!is.na(h2o_tree@thresholds)] <- 'numeric'
  
  #----------------------------------------------------------------------------#
  # Section 7. Get left split levels ----
  #----------------------------------------------------------------------------#
  
  # h2o_tree@levels gives the levels of the child nodes for the given node that
  # travel to the current node
  child_levels_at_parent = h2o_tree@levels
  
  names(child_levels_at_parent) <- h2o_tree_df$node

  h2o_tree_df$left_split_levels <- NA
  
  left_split_levels <- child_levels_at_parent[h2o_tree_df$left_split]
  
  left_split_levels <- sapply(left_split_levels, paste, collapse = '|')
  
  left_split_levels[left_split_levels == ''] <- NA
  
  h2o_tree_df$left_split_levels <- left_split_levels
  
  #----------------------------------------------------------------------------#
  # Section 8. Get right split levels ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$right_split_levels <- NA
  
  right_split_levels <- child_levels_at_parent[h2o_tree_df$right_split]
  
  right_split_levels <- sapply(right_split_levels, paste, collapse = '|')
  
  right_split_levels[right_split_levels == ''] <- NA
  
  h2o_tree_df$right_split_levels <- right_split_levels  
  
  #----------------------------------------------------------------------------#
  # Section 9. Identify categorical split variables ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$node_variable_type[!is.na(left_split_levels)] <- 'categorical'
  
  #----------------------------------------------------------------------------#
  # Section 10. Get NA directions ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$NA_direction <- h2o_tree@nas
  
  #----------------------------------------------------------------------------#
  # Section 11. Add left and right directions for numeric variables ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$left_split_levels[h2o_tree_df$node_variable_type == 'numeric'] <-
    '<'

  h2o_tree_df$right_split_levels[h2o_tree_df$node_variable_type == 'numeric'] <-
    '>='  
    
  #----------------------------------------------------------------------------#
  # Section 11. Add NAs to left_split_levels ----
  #----------------------------------------------------------------------------#
  
  left_NAs_idx <- which(h2o_tree_df$NA_direction == 'LEFT')
  
  left_NAs = rep('[NA]|', length(left_NAs_idx))
  
  h2o_tree_df$left_split_levels[left_NAs_idx] <-
    mapply(paste0, left_NAs, h2o_tree_df$left_split_levels[left_NAs_idx])
  
  #----------------------------------------------------------------------------#
  # Section 12. Add NAs to right_split_levels ----
  #----------------------------------------------------------------------------#
  
  right_NAs_idx <- which(h2o_tree_df$NA_direction == 'RIGHT')
  
  right_NAs = rep('[NA]|', length(right_NAs_idx))
  
  h2o_tree_df$right_split_levels[right_NAs_idx] <-
    mapply(paste0, right_NAs, h2o_tree_df$right_split_levels[right_NAs_idx])  
  
  #----------------------------------------------------------------------------#
  # Section 13. Get split columns ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_df$split_column <- h2o_tree@features
 
  #----------------------------------------------------------------------------#
  # Section 14. Order columns ----
  #----------------------------------------------------------------------------#
  
  cols_order <- c("node",
                  "predictions",
                  "left_split",
                  "right_split",
                  "left_split_levels",
                  "right_split_levels",
                  "NA_direction",
                  "node_variable_type",
                  "split_column",
                  "node_split_point")
  
  h2o_tree_df <- h2o_tree_df[cols_order]
  
  #----------------------------------------------------------------------------#
  # Section 15. Return tree info ----
  #----------------------------------------------------------------------------#

  return(h2o_tree_df)

}



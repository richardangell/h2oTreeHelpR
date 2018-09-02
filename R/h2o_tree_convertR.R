#' Convert h2o gbm or drf to tabular structure
#'
#' Takes a h2o tree based model (gbm or drf) and returns a \code{list} of
#' \code{data.frame}s representing each tree in the model in tabular structure.
#'
#' @param h2o_model gbm or drf h2o model.
#' @param output_subdir directory to output intermediate files (mojo .zip, .gv
#' and model.ini files) to. Default is location is current working dir. Files
#' are put into a sudir with date-time in name to avoid conflicts.
#' @param get_internal_predictions \code{logical} default = \code{FALSE}, should
#' predictions for internal (non-terminal nodes) be extracted?
#' @param delete_intermediate_files should intermediate files output in
#'  processing be deleted? Default = \code{TRUE}.
#'
#' @return returns a \code{list} containing a \code{data.frame} for each tree
#' containing the tree structure. Tree structure \code{data.frame}s contain the
#' following columns;
#' \itemize{
#'   \item{"node"} {name of the node in the tree}
#'   \item{"node_text"} {complete text associated with the node}
#'   \item{"predictions"} {predicted values for terminal nodes or NA}
#'   \item{"left_split"} {left child node or NA if a terminal node}
#'   \item{"right_split"} {right child node or NA if a terminal node}
#'   \item{"left_split_levels"} {levels of the split variable that are sent to
#'   the left child node from the current node, separated by |}
#'   \item{"right_split_levels"} {levels of the split variable that are sent
#'   to the left child node from the current node, separated by |}
#'   \item{"NA_direction"} {the direction missing values are sent from the
#'   current node}
#'   \item{"node_text_label"} {the label arg of node_text, specifically the
#'   split condition for numeric split variables or the variable name for
#'   categorical variables}
#'   \item{"node_variable_type"} {split variable type for node, either
#'   categorical or numeric}
#'   \item{"split_column"} {the name of the split column for the current node}
#'   \item{"node_split_point"} {the split value for the current node if numeric,
#'   otherwise NA}
#' }
#'
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
#' h2o_trees <- h2o_tree_convertR(h2o_model = prostate.gbm)
#'
#' @export
h2o_tree_convertR <- function(h2o_model,
                              via_mojo = FALSE,
                              output_subdir = getwd(),
                              get_internal_predictions = FALSE,
                              delete_intermediate_files = TRUE) {
  
  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Extract tree info depending on pkg version and via_mojo arg
  # Section 2. Return data.frames containing tree structures
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (packageVersion("h2o") < '3.20.0.6') {
    
    if (via_mojo) {
      
      stop('h2o package version ', 
           packageVersion("h2o"),
           ' lt 3.20.0.6 so tree info must be extracted via mojo and .gv files',
           ' (via_mojo must be FALSE)')
      
    }

  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Extract tree info depending on pkg version and via_mojo arg ----
  #----------------------------------------------------------------------------#
  
  if (via_mojo) {
    
    trees <- h2o_tree_convertR_via_mojo(h2o_model,
                                        output_subdir,
                                        get_internal_predictions,
                                        delete_intermediate_files)
    
  } else {
    
    trees <- h2o_tree_convertR_with_h2o(h2o_model)
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 2. Return data.frames containing tree structures ----
  #----------------------------------------------------------------------------#
  
  return(trees)
  
}



#' Convert h2o gbm or drf to tabular structure with h2o
#'
#' Takes a h2o tree based model (gbm or drf) and returns a \code{list} of
#' \code{data.frame}s representing each tree in the model in tabular structure.
#' This function uses \code{h2o::h2o.getModelTree} to extract tree structures.
#'
#' @param h2o_model gbm or drf h2o model.
#' @param output_subdir directory to output intermediate files (mojo .zip, .gv
#' and model.ini files) to. Default is location is current working dir. Files
#' are put into a sudir with date-time in name to avoid conflicts.
#' @param get_internal_predictions \code{logical} default = \code{FALSE}, should
#' predictions for internal (non-terminal nodes) be extracted?
#' @param delete_intermediate_files should intermediate files output in
#'  processing be deleted? Default = \code{TRUE}.
#'
#' @return returns a \code{list} containing a \code{data.frame} for each tree
#' containing the tree structure. Tree structure \code{data.frame}s contain the
#' following columns;
#' \itemize{
#'   \item{"node"} {name of the node in the tree}
#'   \item{"node_text"} {complete text associated with the node}
#'   \item{"predictions"} {predicted values for terminal nodes or NA}
#'   \item{"left_split"} {left child node or NA if a terminal node}
#'   \item{"right_split"} {right child node or NA if a terminal node}
#'   \item{"left_split_levels"} {levels of the split variable that are sent to
#'   the left child node from the current node, separated by |}
#'   \item{"right_split_levels"} {levels of the split variable that are sent
#'   to the left child node from the current node, separated by |}
#'   \item{"NA_direction"} {the direction missing values are sent from the
#'   current node}
#'   \item{"node_text_label"} {the label arg of node_text, specifically the
#'   split condition for numeric split variables or the variable name for
#'   categorical variables}
#'   \item{"node_variable_type"} {split variable type for node, either
#'   categorical or numeric}
#'   \item{"split_column"} {the name of the split column for the current node}
#'   \item{"node_split_point"} {the split value for the current node if numeric,
#'   otherwise NA}
#' }
#'
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
#' h2o_trees <- h2o_tree_convertR_with_h2o(h2o_model = prostate.gbm)
#'
#' @export
h2o_tree_convertR_with_h2o <- function(h2o_model) {
  
  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Get tree information using h2o.getModelTree
  # Section 2. Extract info into particular format
  # Section 3. Return tree structures
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  h2o_model_classes <- c('H2ORegressionModel', 'H2OBinomialModel')
  
  if (!class(h2o_model) %in% h2o_model_classes) {
    
    stop('accepted classes for h2o_model are; ',
         h2o_model_classes,
         ', but got; ',
         class(h2o_model))
    
  }
  
  h2o_model_algos <- c('gbm', 'drf')
  
  if (!h2o_model@algorithm %in% h2o_model_algos) {
    
    stop('accepted model types for h2o_model are; ',
         h2o_model_algos,
         ', but got; ',
         h2o_model@algorithm)
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Get tree information using h2o.getModelTree ----
  #----------------------------------------------------------------------------#

  n_trees <- max(h2o_model@model$scoring_history$number_of_trees)
  
  if (n_trees == 0) {
    
    stop('no trees in model')
    
  }
  
  h2o_trees <- lapply(1:n_trees,
                      function(x) h2o.getModelTree(h2o_model, x))
  
  #----------------------------------------------------------------------------#
  # Section 2. Extract info into particular format ----
  #----------------------------------------------------------------------------#
  
  h2o_tree_info <- lapply(h2o_trees, extract_H2OTree_info)
  
  #----------------------------------------------------------------------------#
  # Section 3. Return tree structures ----
  #----------------------------------------------------------------------------#
  
  return(h2o_tree_info)
  
} 




#' Convert h2o gbm or drf to tabular structure via mojo 
#'
#' Takes a h2o tree based model (gbm or drf) and returns a \code{list} of
#' \code{data.frame}s representing each tree in the model in tabular structure.
#' This function must be used if the version of h2o is pre 3.20.0.6.
#'
#' @param h2o_model gbm or drf h2o model.
#' @param output_subdir directory to output intermediate files (mojo .zip, .gv
#' and model.ini files) to. Default is location is current working dir. Files
#' are put into a sudir with date-time in name to avoid conflicts.
#' @param get_internal_predictions \code{logical} default = \code{FALSE}, should
#' predictions for internal (non-terminal nodes) be extracted?
#' @param delete_intermediate_files should intermediate files output in
#'  processing be deleted? Default = \code{TRUE}.
#'
#' @return returns a \code{list} containing a \code{data.frame} for each tree
#' containing the tree structure. Tree structure \code{data.frame}s contain the
#' following columns;
#' \itemize{
#'   \item{"node"} {name of the node in the tree}
#'   \item{"node_text"} {complete text associated with the node}
#'   \item{"predictions"} {predicted values for terminal nodes or NA}
#'   \item{"left_split"} {left child node or NA if a terminal node}
#'   \item{"right_split"} {right child node or NA if a terminal node}
#'   \item{"left_split_levels"} {levels of the split variable that are sent to
#'   the left child node from the current node, separated by |}
#'   \item{"right_split_levels"} {levels of the split variable that are sent
#'   to the left child node from the current node, separated by |}
#'   \item{"NA_direction"} {the direction missing values are sent from the
#'   current node}
#'   \item{"node_text_label"} {the label arg of node_text, specifically the
#'   split condition for numeric split variables or the variable name for
#'   categorical variables}
#'   \item{"node_variable_type"} {split variable type for node, either
#'   categorical or numeric}
#'   \item{"split_column"} {the name of the split column for the current node}
#'   \item{"node_split_point"} {the split value for the current node if numeric,
#'   otherwise NA}
#' }
#'
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
#' h2o_trees <- h2o_tree_convertR_via_mojo(h2o_model = prostate.gbm)
#'
#' @export
h2o_tree_convertR_via_mojo <- function(h2o_model,
                                       output_subdir = getwd(),
                                       get_internal_predictions = FALSE,
                                       delete_intermediate_files = TRUE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Export h2o model to mojo
  # Section 2. Convert trees in mojo .zip to .gv files
  # Section 3. Convert .gv files to data.frame structure
  # Section 4. Optionally remove directory with intermediate files
  # Section 5. Return tree structure data.frames
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  h2o_model_classes <- c('H2ORegressionModel', 'H2OBinomialModel')

  if (!class(h2o_model) %in% h2o_model_classes) {

    stop('accepted classes for h2o_model are; ',
         h2o_model_classes,
         ', but got; ',
         class(h2o_model))

  }

  h2o_model_algos <- c('gbm', 'drf')

  if (!h2o_model@algorithm %in% h2o_model_algos) {

    stop('accepted model types for h2o_model are; ',
         h2o_model_algos,
         ', but got; ',
         h2o_model@algorithm)

  }

  output_dir <- paste0(output_subdir,
                       .Platform$file.sep,
                       Sys.Date(),
                       "_",
                       gsub(" ", "-", gsub(":", "-", format(Sys.time(), "%X"))),
                       "_H2OTreeConvertR-Intermediate")

  if (dir.exists(output_dir)) {

    stop("output directory already exists: ",
         output_dir)

  } else {

    cat("creating output directory", output_dir, "\n")

    dir.create(output_dir)

  }

  #----------------------------------------------------------------------------#
  # Section 1. Export h2o model to mojo ----
  #----------------------------------------------------------------------------#

  cat("saving model mojo to", output_dir, "\n")

  h2o::h2o.saveMojo(object = h2o_model,
                    path = output_dir)

  #----------------------------------------------------------------------------#
  # Section 2. Convert trees in mojo .zip to .gv files ----
  #----------------------------------------------------------------------------#

  cat("converting trees in mojo to .gv files", "\n")

  mojo_file = file.path(output_dir, paste0(h2o_model@model_id, ".zip"))

  # locate h2o.jar file
  h2o_jar_file = system.file("java", "h2o.jar", package = "h2o")

  output_gv_files <- trees_to_gvs(h2o_jar = h2o_jar_file,
                                  mojo_zip = mojo_file,
                                  gv_output_dir = output_dir,
                                  detail = get_internal_predictions)

  #----------------------------------------------------------------------------#
  # Section 3. Convert .gv files to data.frame structure ----
  #----------------------------------------------------------------------------#

  cat("parsing .gv structures to data.frames", "\n")

  tree_dfs <- lapply(X = output_gv_files, 
                     FUN = gv_to_table,
                     detail = get_internal_predictions)

  #----------------------------------------------------------------------------#
  # Section 4. Optionally remove directory with intermediate files ----
  #----------------------------------------------------------------------------#

  if (delete_intermediate_files) {

    cat("deleting intermediate files directory", output_dir, "\n")

    unlink(output_dir, recursive = TRUE)

  }

  #----------------------------------------------------------------------------#
  # Section 5. Return tree structure data.frames ----
  #----------------------------------------------------------------------------#

  return(tree_dfs)

}







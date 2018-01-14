#' Convert h2o tree .gv file to tabular structure.
#'
#' Reads a single .gv file containing a tree from a h2o gbm or drf and parses it
#' to a tabular structure.
#'
#' @param h2o_model gbm or drf h2o model.
#' @param mojo_output_path directory to export h2o_model's MOJO .zip to.
#' @param gv_output_path directory to output .gv file for each tree in model.
#' @param model_ini_overwrite \code{logical} when extracting the model.ini
#'        from the MOJO .zip, should an existing file be overwritten? Default
#'        = \code{TRUE}.
#' @param h2o_jar_file h2o.jar file, including path.
#'
#' @return \code{data.frame} containing tree structure.
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
#' @export
h2o_tree_model_to_R <- function(h2o_model,
                                h2o_jar_file,
                                output_subdir = getwd(),
                                delete_intermediate_files = TRUE) {

  #---------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Export h2o model to mojo
  # Section 2. Convert trees in mojo .zip to .gv files
  # Section 3. Convert .gv files to data.frame structure
  # Section 4. Optionally remove directory with intermediate files
  # Section 5. Return tree structure data.frames
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

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
                       gsub(":", "-", format(Sys.time(), "%X")),
                       "_H2OTreeConvertR-Intermediate")

  if (dir.exists(output_dir)) {

    stop("output directory already exists: ",
         output_dir)

  } else {

    cat("creating output directory", output_dir, "\n")

    dir.create(output_dir)

  }

  #---------------------------------------------------------------------------#
  # Section 1. Export h2o model to mojo ----
  #---------------------------------------------------------------------------#

  cat("saving model mojo to", output_dir, "\n")

  h2o::h2o.saveMojo(object = h2o_model,
                    path = output_dir)

  #---------------------------------------------------------------------------#
  # Section 2. Convert trees in mojo .zip to .gv files ----
  #---------------------------------------------------------------------------#

  cat("converting trees in mojo to .gv files", "\n")

  mojo_file = file.path(output_dir, paste0(h2o_model@model_id, ".zip"))

  output_gv_files <- mojo_trees_to_gvs(h2o_jar = h2o_jar_file,
                                       mojo_zip = mojo_file,
                                       gv_output_dir = output_dir)

  #---------------------------------------------------------------------------#
  # Section 3. Convert .gv files to data.frame structure ----
  #---------------------------------------------------------------------------#

  cat("parsing .gv structures to data.frames", "\n")

  tree_dfs <- lapply(output_gv_files, mojo_gv_to_table)

  #---------------------------------------------------------------------------#
  # Section 4. Optionally remove directory with intermediate files ----
  #---------------------------------------------------------------------------#

  if (delete_intermediate_files) {

    cat("deleting intermediate files directory", output_dir, "\n")

    unlink(output_dir, recursive = TRUE)

  }

  #---------------------------------------------------------------------------#
  # Section 5. Return tree structure data.frames ----
  #---------------------------------------------------------------------------#

  return(tree_dfs)

}







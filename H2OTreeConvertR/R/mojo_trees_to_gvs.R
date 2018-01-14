#' Export all trees from MOJO .zip to .gv files.
#'
#' Calls call_PrintMojo to output each tree to .gv file in the input directory.
#'
#' @param h2o_jar h2o.jar file, including path.
#' @param mojo_zip h2o tree based MOJO zip file, including path.
#' @param gv_output_dir directory to output .gv files to.
#' @param model_ini_overwrite \code{logical} when extracting the model.ini
#'        from the MOJO .zip, should an existing file be overwritten? Default
#'        = \code{TRUE}.
#'
#' @return Outputs trees from MOJO zip to .gv files.
#'
#' @examples
#' mojo_trees_to_gvs('h2o.jar',
#'                   'GBM_model_R_1488095800763_37.zip',
#'                   'gv_output/',
#'                   model_ini_overwrite = FALSE)
#'
#' @export
mojo_trees_to_gvs <- function(h2o_jar,
                              mojo_zip,
                              gv_output_dir,
                              model_ini_overwrite = TRUE) {

  #---------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Extract model.ini file
  # Section 2. Convert all trees from mojo zip file to gv files
  # Section 3. End of function
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (length(h2o_jar) != 1) {

    stop('h2o_jar must be a single file')

  }

  if (basename(h2o_jar) != 'h2o.jar') {

    stop('h2o_jar must be the full path and filename of the h2o.jar file')

  }

  if (!file.exists(h2o_jar)) {

    stop('h2o_jar file does not exist')

  }

  if (length(mojo_zip) != 1) {

    stop('mojo_zip must be a single file')

  }

  mojo_zip_split <- strsplit(mojo_zip, '\\.')[[1]]

  if (tools::file_ext(mojo_zip) != 'zip') {

    stop('mojo_zip is not a zip file - should be the model mojo exported with h2o.download_mojo')

  }

  if (!file.exists(mojo_zip)) {

    stop('mojo_zip file does not exist')

  }

  #---------------------------------------------------------------------------#
  # Section 1. Extract model.ini file  ----
  #---------------------------------------------------------------------------#

  mojo_zip_contents <- unzip(mojo_zip, list = TRUE)

  if (!'model.ini' %in% mojo_zip_contents$Name) {

    stop('no model.ini file in mojo_zip - mojo_zip should be the model mojo exported with h2o.download_mojo')

  }

  # stop the function if file exists and overwrite is FALSE
  if (!model_ini_overwrite & file.exists('model.ini')) {

    stop('model_ini_overwrite is FALSE and model.ini file already exists in working directory; ', getwd())

  }

  # extract model.ini file from zip file
  unzip(mojo_zip,
        files = 'model.ini',
        overwrite = model_ini_overwrite,
        exdir = gv_output_dir)

  model_ini <- readLines('model.ini')

  algo_line <- grep('algo = ', model_ini)

  if (length(algo_line) != 1) {

    stop('could not identify algo line in model.ini file')

  }

  algo <- gsub('algo = ', '', model_ini[algo_line])

  if (!algo %in% c('gbm', 'drf')) {

    stop('algo from model.ini file is not gbm or drf')

  }

  n_trees_line <- grep('n_trees = ', model_ini)

  if (length(n_trees_line) != 1) {

    stop('could not identify n_trees line in model.ini file')

  }

  n_trees <- as.numeric(gsub('n_trees = ', '', model_ini[n_trees_line]))

  #---------------------------------------------------------------------------#
  # Section 2. Convert all trees from mojo zip file to gv files  ----
  #---------------------------------------------------------------------------#

  # create output directory if required
  if (!dir.exists(gv_output_dir)) {

    cat('creating directory ', gv_output_dir)

    dir.create(gv_output_dir)

  }

  mojo_zip_name <- tools::file_path_sans_ext(basename(mojo_zip))

  # loop through each tree and output to .gv file
  for (i in 0:(n_trees - 1)) {

    call_PrintMojo(h2o_jar = h2o_jar,
                   tree_no = i,
                   mojo_zip = mojo_zip,
                   gv_output = file.path(gv_output_dir,
                                         paste0(mojo_zip_name, '_', i, '.gv')))

  }

  #---------------------------------------------------------------------------#
  # Section 3. End of function  ----
  #---------------------------------------------------------------------------#

  return(file.path(gv_output_dir,
                   paste0(mojo_zip_name, '_', 0:(n_trees - 1), '.gv')))

}





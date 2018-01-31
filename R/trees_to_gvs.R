#' Export all trees from a h2o mojo (.zip) to .gv files
#'
#' Calls call_PrintMojo to output each tree to .gv file in the supplied
#' directory
#'
#' @param h2o_jar h2o.jar file, including path.
#' @param mojo_zip h2o tree based mojo zip file, including path.
#' @param gv_output_dir directory to output .gv files to.
#' @param detail \code{logical} default = \code{FALSE}, should
#' printMojo be called with the '--detail' option? This results in internal
#' node predictions being printed to the .gv file, among other things.
#' 
#' @return Outputs trees from mojo zip to .gv files.
#'
#' @export
trees_to_gvs <- function(h2o_jar,
                         mojo_zip,
                         gv_output_dir,
                         detail = FALSE) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Extract model.ini file
  # Section 2. Convert all trees from mojo zip file to gv files
  # Section 3. Return .gv files created
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

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

    stop('mojo_zip is not a zip file',
         ' - should be the model mojo exported with h2o.download_mojo')

  }

  if (!file.exists(mojo_zip)) {

    stop('mojo_zip file does not exist')

  }

  #----------------------------------------------------------------------------#
  # Section 1. Extract model.ini file  ----
  #----------------------------------------------------------------------------#

  mojo_zip_contents <- unzip(mojo_zip, list = TRUE)

  if (!'model.ini' %in% mojo_zip_contents$Name) {

    stop('no model.ini file in mojo_zip',
         ' - mojo_zip should be the model mojo exported with h2o.download_mojo')

  }

  # extract model.ini file from zip file to gv_output_dir
  unzip(mojo_zip,
        files = 'model.ini',
        exdir = gv_output_dir)

  model_ini <- readLines(file.path(gv_output_dir, 'model.ini'))

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
  
  #----------------------------------------------------------------------------#
  # Section 2. Find the max no of cat levels for variables in the model ----
  #----------------------------------------------------------------------------#
  
  domains_files <- grep("domains/", mojo_zip_contents$Name)
  
  max_cat_levels <- 0
  
  if (length(domains_files) > 0) {
    
    # loop through each domain file
    for (i in domains_files) {
      
      # extract model.ini file from zip file to gv_output_dir
      unzip(mojo_zip,
            files = mojo_zip_contents$Name[i],
            exdir = gv_output_dir)
      
      # read the domain .txt file
      domain_file <- readLines(file.path(gv_output_dir, 
                                         mojo_zip_contents$Name[i]))
      
      # if the number of levels is greater than the current record the new max
      if (length(domain_file) > max_cat_levels) {
        
        max_cat_levels <- length(domain_file) 
        
      }
      
    }
    
  } 
  
  # esure max_cat_levels is at least the default value of 10
  # note, this will be passed to levels (maxLevelsToPrintPerEdge) after it is 
  # passed into PrintMojo from call_PrintMojo
  max_cat_levels <- max(10 , max_cat_levels)
  
  #----------------------------------------------------------------------------#
  # Section 3. Convert all trees from mojo zip file to gv files  ----
  #----------------------------------------------------------------------------#

  # create output directory if required
  if (!dir.exists(gv_output_dir)) {

    cat('creating directory ', gv_output_dir)

    dir.create(gv_output_dir)

  }

  mojo_zip_name <- tools::file_path_sans_ext(basename(mojo_zip))

  # loop through each tree and output to .gv file
  for (i in 0:(n_trees - 1)) {
    
    gv_file <- file.path(gv_output_dir, paste0(mojo_zip_name, '_', i, '.gv'))
    
    call_PrintMojo(h2o_jar = h2o_jar,
                   tree_no = i,
                   mojo_zip = mojo_zip,
                   output_gv = gv_file,
                   detail = detail,
                   max_levels_to_print_per_edge = max_cat_levels)

  }

  #----------------------------------------------------------------------------#
  # Section 4. Return .gv files created  ----
  #----------------------------------------------------------------------------#

  gv_files <- file.path(gv_output_dir,
                        paste0(mojo_zip_name, '_', 0:(n_trees - 1), '.gv'))
  
  return(gv_files)

}





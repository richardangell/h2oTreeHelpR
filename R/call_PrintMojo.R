#' Export single h2o tree from mojo .zip to .gv file
#'
#' Calls the h2o hex.genmodel.tools.PrintMojo function
#'
#' @param h2o_jar h2o.jar file, including path.
#' @param tree_no tree number to export.
#' @param mojo_zip h2o tree based mojo zip file, including path.
#' @param output_gv .gv file to output to, including path.
#' @param detail \code{logical} default = \code{FALSE}, should
#' printMojo be called with the '--detail' option? This results in internal
#' node predictions being printed to the .gv file, among other things.
#' @param max_levels_to_print_per_edge max number of labels to print on an
#' edge, if too small then splits from categorical variables with many levels
#' will be represented as [X levels]. Default = 10 (the default value in the 
#' Java code).
#'
#' @return Outputs tree from mojo zip to specified .gv file.
#'
#' @examples
#' # input files must be in working directory
#' call_PrintMojo('h2o.jar', 4, 'mojo.zip', 'tree_4.gv')
#'
#' @export
call_PrintMojo <- function(h2o_jar, 
                           tree_no, 
                           mojo_zip, 
                           output_gv,
                           detail = FALSE,
                           max_levels_to_print_per_edge = 10) {

  if (detail) {
    
    detail_text = "--detail"
    
  } else {
    
    detail_text = ""
    
  }
  
  # record current option 
  current_useFancyQuotes <- getOption('useFancyQuotes')
  
  # turn off useFancyQuotes so dQuote uses undirectional ASCII quote style
  options(useFancyQuotes = FALSE)
  
  java_command <- paste("java -cp",
                        dQuote(h2o_jar),
                        "hex.genmodel.tools.PrintMojo",
                        "--tree ",
                        tree_no,
                        detail_text,
                        "--levels",
                        max_levels_to_print_per_edge,
                        "-i",
                        dQuote(mojo_zip),
                        "-o",
                        dQuote(output_gv))

  system(java_command)

  # reset useFancyQuotes option 
  options(useFancyQuotes = current_useFancyQuotes)
  
}

#' Export single h2o tree from mojo .zip to .gv file
#'
#' Calls the h2o hex.genmodel.tools.PrintMojo function
#'
#' @param h2o_jar h2o.jar file, including path.
#' @param tree_no tree number to export.
#' @param mojo_zip h2o tree based mojo zip file, including path.
#' @param output_gv .gv file to output to, including path.
#'
#' @return Outputs tree from mojo zip to specified .gv file.
#'
#' @examples
#' # input files must be in working directory
#' call_PrintMojo('h2o.jar', 4, 'mojo.zip', 'tree_4.gv')
#'
#' @export
call_PrintMojo <- function(h2o_jar, tree_no, mojo_zip, output_gv) {

  java_command <- paste("java -cp",
                        h2o_jar,
                        "hex.genmodel.tools.PrintMojo",
                        "--tree ",
                        tree_no,
                        "-i",
                        mojo_zip,
                        "-o",
                        output_gv)

  system(java_command)

}

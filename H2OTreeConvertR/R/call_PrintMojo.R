
#' Export single h2o tree from MOJO .zip to .gv file.
#'
#' Calls the h2o hex.genmodel.tools.PrintMojo function.
#'
#' @param h2o_jar h2o.jar file, including path.
#' @param tree_no tree number to export.
#' @param mojo_zip h2o tree based MOJO zip file, including path.
#' @param gv_output .gv file to output to, including path.
#'
#' @return Outputs tree from MOJO zip to specified .gv file.
#'
#' @examples
#' call_PrintMojo('h2o.jar',
#'                4,
#'                'GBM_model_R_1488095800763_37.zip',
#'                'GBM_model_R_1488095800763_37_4.gv')
#'
#' @export
call_PrintMojo <- function(h2o_jar, tree_no, mojo_zip, gv_output) {

  java_command <- paste("java -cp",
                        h2o_jar,
                        "hex.genmodel.tools.PrintMojo",
                        "--tree ",
                        tree_no,
                        "-i",
                        mojo_zip,
                        "-o",
                        gv_output)

  system(java_command)

}

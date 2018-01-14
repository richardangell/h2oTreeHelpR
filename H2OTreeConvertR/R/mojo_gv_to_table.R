#' Convert h2o tree .gv file to tabular structure.
#'
#' Reads a single .gv file containing a tree from a h2o gbm or drf and parses it
#' to a tabular structure.
#'
#' @param gv_file .gv file file including path to import.
#'
#' @return \code{data.frame} containing tree structure.
#'
#' @examples
#' mojo_gv_to_table("gv_output/GBM_model_R_1488095800763_37_4.gv")
#'
#' @export
mojo_gv_to_table <- function(gv_file) {

  #---------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Import gv file
  # Section 2. Get nodes in table
  # Section 3. Get terminal node predictions
  # Section 4. Get split information
  # Section 5. Return tree in tabular structure
  #---------------------------------------------------------------------------#

  #---------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #---------------------------------------------------------------------------#

  if (tools::file_ext(gv_file) != "gv") {

    stop("gv_file does not have .gv extension")

  }

  #---------------------------------------------------------------------------#
  # Section 1. Import gv file ----
  #---------------------------------------------------------------------------#

  gv_import <- readLines(gv_file)

  n <- length(gv_import)

  #---------------------------------------------------------------------------#
  # Section 2. Get nodes in table ----
  #---------------------------------------------------------------------------#

  edges_line <- which(gv_import == "/* Edges */")

  end_edges_line <- which(gv_import[edges_line:n] == "")[1] + edges_line - 1

  nodes_line <- which(gv_import == "/* Nodes */")

  open_brackets_nodes_lines <- which(gv_import[nodes_line:edges_line] == '{') + nodes_line - 1

  close_brackets_nodes_lines <- which(gv_import[nodes_line:edges_line] == '}') + nodes_line - 1

  # extract rows from the imported file that contain node information; between '{' and  '}'
  node_levels_list <- mapply(function(x, y) gv_import[(x+1):(y-1)],
                             open_brackets_nodes_lines,
                             close_brackets_nodes_lines,
                             SIMPLIFY = FALSE)

  node_levels <- unlist(node_levels_list)

  node_levels <- gsub("\"", "", node_levels)

  node_levels_split <- strsplit(node_levels, " \\[")

  node_table <- data.frame(node = sapply(node_levels_split, "[", 1),
                           node_text = paste0("[", sapply(node_levels_split, "[", 2)),
                           stringsAsFactors = FALSE)

  #---------------------------------------------------------------------------#
  # Section 3. Get terminal node predictions ----
  #---------------------------------------------------------------------------#

  node_text_split <- strsplit(node_table$node_text, "label=")

  label_values <- sapply(node_text_split, "[", 2)

  label_values <- substr(label_values, 1, nchar(label_values) - 1)

  label_values_numeric <- suppressWarnings(as.numeric(label_values))

  # which labels contain only number and hence are terminal node predictions
  numeric_only_labels <- which(!is.na(label_values_numeric))

  # which labels contain characters
  character_labels <- which(grepl("[[:alpha:]]", label_values))

  # remove any numeric only labels that have scientific notation
  if (any(character_labels %in% numeric_only_labels)) {

    character_labels <- character_labels[-which(character_labels %in% numeric_only_labels)]

  }

  # which labels contain numerics
  numeric_labels <- which(grepl("[[:digit:]]", label_values))

  node_predictions <- rep(NA, nrow(node_table))

  node_predictions[numeric_only_labels] <- label_values_numeric[numeric_only_labels]

  node_table$predictions <- node_predictions

  #---------------------------------------------------------------------------#
  # Section 4. Get split information ----
  #---------------------------------------------------------------------------#

  # extract part of the file containing links between nodes
  edges_section <- gv_import[(edges_line + 1):(end_edges_line - 1)]

  edges_section <- gsub("\"", "", edges_section)

  edges_section_split <- strsplit(edges_section, " \\[label=")

  edges_direction <- sapply(edges_section_split, "[", 1)

  edges_label <- sapply(edges_section_split, "[", 2)

  edges_label <- substr(edges_label, 1, nchar(edges_label) - 1)

  edges_direction_split <- strsplit(edges_direction, " -> ")

  edges_direction_from <- sapply(edges_direction_split, "[", 1)

  edges_direction_to <- sapply(edges_direction_split, "[", 2)

  # assumes there are only two splits per node
  if (any(table(edges_direction_from) != 2)) {

    stop("expecting two splits per node, instead got; \n",
         paste(names(table(edges_direction_from)), collapse = " "),
         "\n",
         paste(table(edges_direction_from), collapse = " "))

  }

  node_table$left_split <- rep(NA, nrow(node_table))

  node_table$right_split <- rep(NA, nrow(node_table))

  node_table$left_split_levels <- rep(NA, nrow(node_table))

  node_table$right_split_levels <- rep(NA, nrow(node_table))

  # loop through each parent node and extract child nodes and levels for each split
  for (parent_node in unique(edges_direction_from)) {

    child_nodes <- edges_direction_to[which(edges_direction_from == parent_node)]

    node_table$left_split[node_table$node == parent_node] <- child_nodes[1]

    node_table$right_split[node_table$node == parent_node] <- child_nodes[2]

    split_levels <- edges_label[which(edges_direction_from == parent_node)]

    node_table$left_split_levels[node_table$node == parent_node] <- split_levels[1]

    node_table$right_split_levels[node_table$node == parent_node] <- split_levels[2]

  }

  node_table$node_text_label <- label_values

  node_table$node_variable_type <- rep(NA, nrow(node_table))

  # record categorical split variables
  node_table$node_variable_type[character_labels[!character_labels %in% numeric_labels]] <- "categorical"

  # record numerical split variables
  node_table$node_variable_type[character_labels[character_labels %in% numeric_labels]] <- "numeric"

  node_text_label_split <- strsplit(node_table$node_text_label, "<")

  node_text_label_split_name <- sapply(node_text_label_split, "[", 1)

  node_text_label_split_point <- sapply(node_text_label_split, "[", 2)

  # get split column
  node_table$split_column <- rep(NA, nrow(node_table))

  node_table$split_column[which(node_table$node_variable_type == 'categorical')] <- node_table$node_text_label[which(node_table$node_variable_type == 'categorical')]

  node_table$split_column[which(node_table$node_variable_type == 'numeric')] <- node_text_label_split_name[which(node_table$node_variable_type == 'numeric')]

  node_table$split_column <- gsub(' ', '', node_table$split_column)

  # get split points for numeric variables
  node_table$node_split_point <- rep(NA, nrow(node_table))

  node_table$node_split_point[which(node_table$node_variable_type == 'numeric')] <- as.numeric(node_text_label_split_point[which(node_table$node_variable_type == 'numeric')])

  #---------------------------------------------------------------------------#
  # Section 5. Return tree in tabular structure ----
  #---------------------------------------------------------------------------#

  return(node_table)

}







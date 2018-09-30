#' Convert h2o tree .gv file to tabular structure
#'
#' Reads a single .gv file containing a tree from a h2o gbm or drf and parses it
#' into a tabular structure
#'
#' @param gv_file .gv file file including path to import.
#' @param detail has the gv file been processed with the option --detail?
#' 
#' @return \code{data.frame} containing tree structure, if the gv file is of an
#' empty tree then \code{NULL} is returned.
#'
#' @export
gv_to_table <- function(gv_file, detail) {

  #----------------------------------------------------------------------------#
  # Function Layout: ----
  # Section 0. Input checking
  # Section 1. Import gv file
  # Section 2. Get nodes in table
  # Section 3. Get terminal node predictions
  # Section 4. Get split information
  # Section 5. Return tree in tabular structure
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#

  if (tools::file_ext(gv_file) != "gv") {

    stop("gv_file does not have .gv extension")

  }

  #----------------------------------------------------------------------------#
  # Section 1. Import gv file ----
  #----------------------------------------------------------------------------#

  gv_import <- readLines(gv_file)

  n <- length(gv_import)

  #----------------------------------------------------------------------------#
  # Section 2. Get nodes in table ----
  #----------------------------------------------------------------------------#
  
  # find the start of the nodes section
  nodes_line <- which(gv_import == "/* Nodes */")
  
  # find the start of the edges section
  edges_line <- which(gv_import == "/* Edges */")
  
  # if there is nothing in the .gv file return NULL
  if (nodes_line == 17 & edges_line == 24) {
    
    return(NULL)
    
  }
  
  # find the end of the edges section
  end_edges_line <- which(gv_import[edges_line:n] == "")[1] + edges_line - 1

  # find the start and end of each node level section
  open_brackets_nodes_lines <- 
    which(gv_import[nodes_line:edges_line] == '{') + nodes_line - 1

  close_brackets_nodes_lines <- 
    which(gv_import[nodes_line:edges_line] == '}') + nodes_line - 1

  # extract rows that contain node information; between '{' and  '}'
  node_levels_list <- mapply(function(x, y) gv_import[(x + 1):(y - 1)],
                             open_brackets_nodes_lines,
                             close_brackets_nodes_lines,
                             SIMPLIFY = FALSE)

  node_levels <- unlist(node_levels_list)

  # remove quotes from nodes info
  node_levels <- gsub("\"", "", node_levels)

  # split node info by [
  node_levels_split <- strsplit(node_levels, " \\[")
  
  # create data.frame to hold node information
  # put in the node text between and including the square brackets
  node_table <- 
    data.frame(node = sapply(node_levels_split, "[", 1),
               node_text = paste0("[", sapply(node_levels_split, "[", 2)),
               stringsAsFactors = FALSE)

  #----------------------------------------------------------------------------#
  # Section 3. Get terminal node predictions ----
  #----------------------------------------------------------------------------#

  node_predictions <- rep(NA, nrow(node_table))
  
  # split node text 
  node_text_split <- strsplit(node_table$node_text, "label=")

  # get the label part of the node text
  label_values <- sapply(node_text_split, "[", 2)

  # remove the trailing ] from the node text
  label_values <- substr(label_values, 1, nchar(label_values) - 1)
  
  if (detail) {
    
    # replace line breaks with spaces
    label_values_2 <- gsub("\\\\n\\\\n", " ", label_values)
    label_values_2 <- gsub("\\\\n", " ", label_values_2)
    
    node_shape_box <- grepl("shape=box", node_table$node_text)
    
    # identify non-terminal nodes from the presence of "shape=box" text
    internal_nodes <- which(node_shape_box)
    
    # get the terminal nodes
    terminal_nodes <- which(!node_shape_box)
    
    # split node label text by " "
    label_values_split <- strsplit(label_values_2, " ")
    
    # return the values after "Pred:" in the labl text for internal nodes
    internal_node_preds <- 
      sapply(label_values_split[internal_nodes],
             function(x) {
               pred_idx <- which(x == "Pred:")
               prediction <- as.numeric(x[pred_idx + 1])
               return(prediction)
             })
    
    # get the first part of the label text for terminl nodes
    terminal_node_preds <- sapply(label_values_split[terminal_nodes], "[", 1)
    
    node_predictions[internal_nodes] <- internal_node_preds
    
    node_predictions[terminal_nodes] <- terminal_node_preds
    
    # add node predictions to df
    node_table$predictions <- node_predictions
    
    node_table$node_text_label <- label_values_2
    
  } 
  
  if (detail) {
  
    # if detailed output remove everything after the first line break
    label_values <- sapply(strsplit(label_values, "\\\\n"), "[", 1)
    
  } 
  
  # convert to numeric 
  label_values_numeric <- suppressWarnings(as.numeric(label_values))
  
  # labels containing only number and hence are terminal node predictions
  numeric_only_labels <- which(!is.na(label_values_numeric))
  
  # which labels contain characters
  character_labels <- which(grepl("[[:alpha:]]", label_values))
  
  # remove any numeric only labels that have scientific notation
  if (any(character_labels %in% numeric_only_labels)) {
    
    character_labels <- 
      character_labels[-which(character_labels %in% numeric_only_labels)]
    
  }
  
  # which labels contain numerics
  numeric_labels <- which(grepl("<", label_values))
  
  if (!detail) {
    
    # record terminal node predictions
    node_predictions[numeric_only_labels] <- 
      label_values_numeric[numeric_only_labels]
    
    # add node predictions to df
    node_table$predictions <- node_predictions
    
    node_table$node_text_label <- label_values
    
  }
  

  #----------------------------------------------------------------------------#
  # Section 4. Get split information ----
  #----------------------------------------------------------------------------#

  # extract part of the file containing links between nodes
  edges_section <- gv_import[(edges_line + 1):(end_edges_line - 1)]

  # remove quotes from edges info
  edges_section <- gsub("\"", "", edges_section)

  # if processing detailed output remove the "penwidth=x," part of the label
  if (detail) {
    
    # location of open square brackets in string
    open_sq_bracket_idx <- gregexpr("\\[", edges_section)
    
    # get first open square bracket index
    open_sq_bracket_first_idx <- sapply(open_sq_bracket_idx, "[", 1)
    
    # location of commas in string
    comma_idxs <- gregexpr(",", edges_section)
    
    # get first comma index
    comma_first_idx <- sapply(comma_idxs, "[", 1)
    
    # get text before "penwidth=x," part
    before_penwidth <- substr(edges_section, 1, open_sq_bracket_first_idx)
    
    # get text after "penwidth=x," part
    after_penwidth <- substr(edges_section, 
                             comma_first_idx + 1, 
                             nchar(edges_section))
    
    # put together text before and after "penwidth=x,"
    edges_section <- paste0(before_penwidth, after_penwidth)
    
  }
  
  # split edge lines by "label="
  edges_section_split <- strsplit(edges_section, " \\label=")
  
  # get the edge routes
  edges_direction <- sapply(edges_section_split, "[", 1)

  # get the edge labels
  edges_label <- sapply(edges_section_split, "[", 2)

  # remove trailing ] from edge labels
  edges_label <- substr(edges_label, 1, nchar(edges_label) - 1)

  edges_direction_split <- strsplit(edges_direction, " -> ")
  
  # get to and form nodes for each edge
  edges_direction_from <- sapply(edges_direction_split, "[", 1)
  edges_direction_to <- sapply(edges_direction_split, "[", 2)

  # split by " " and take the first part
  edges_direction_to <- sapply(strsplit(edges_direction_to, " "), "[", 1)
  
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
    
    # get child nodes for parent
    child_nodes <- 
      edges_direction_to[which(edges_direction_from == parent_node)]

    # record left and right child nodes from parent in df
    node_table$left_split[node_table$node == parent_node] <- child_nodes[1]
    node_table$right_split[node_table$node == parent_node] <- child_nodes[2]
    
    # get the edgle label text for the 2 edges from the parent node
    split_levels <- edges_label[which(edges_direction_from == parent_node)]
    
    # record left and right edge labels in df
    node_table$left_split_levels[node_table$node == parent_node] <- 
      split_levels[1]
    
    node_table$right_split_levels[node_table$node == parent_node] <- 
      split_levels[2]

  }

  # replace the \\n separator in the split levels with |
  node_table$left_split_levels <- 
    gsub("\\\\n", "|", node_table$left_split_levels)
  
  node_table$right_split_levels <- 
    gsub("\\\\n", "|", node_table$right_split_levels)

  # remove trailing | from columns
  node_table$left_split_levels <- 
    substr(node_table$left_split_levels,
           1,
           nchar(node_table$left_split_levels) - 1)
  
  node_table$right_split_levels <- 
    substr(node_table$right_split_levels,
           1,
           nchar(node_table$right_split_levels) - 1)

  # record which direction missing values go
  node_table$NA_direction <- rep(NA, nrow(node_table))
  
  node_table$NA_direction[grepl("[NA]", 
                                node_table$left_split_levels)] <- "LEFT"
  
  node_table$NA_direction[grepl("[NA]", 
                                node_table$right_split_levels)] <- "RIGHT"

  node_table$node_variable_type <- rep(NA, nrow(node_table))

  # record categorical split variables
  cats <- character_labels[!character_labels %in% numeric_labels]
  node_table$node_variable_type[cats] <- "categorical"

  # record numerical split variables
  nums <- character_labels[character_labels %in% numeric_labels]
  node_table$node_variable_type[nums] <- "numeric"

  # extract the split variables and split points
  node_text_label_split <- strsplit(label_values, "<")
  node_text_label_split_name <- sapply(node_text_label_split, "[", 1)
  node_text_label_split_point <- sapply(node_text_label_split, "[", 2)

  # get split column for categorical and numeric variables
  node_table$split_column <- rep(NA, nrow(node_table))

  node_table$split_column[cats] <- label_values[cats]

  node_table$split_column[nums] <- node_text_label_split_name[nums]

  node_table$split_column <- gsub(' ', '', node_table$split_column)

  node_table$node_split_point <- rep(NA, nrow(node_table))
  
  # get split points for numeric variables
  node_table$node_split_point[nums] <- 
    as.numeric(node_text_label_split_point[nums])

  #----------------------------------------------------------------------------#
  # Section 5. Return tree in tabular structure ----
  #----------------------------------------------------------------------------#

  return(node_table)

}







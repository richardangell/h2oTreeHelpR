# Code-Structure
High level code structure for main functions in the package.

## h2o_tree_convertR
Convert h2o tree structures to data.frames.

* [h2o_tree_convertR](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/h2o_tree_convertR.R)
  * h2o.saveMojo
  * [trees_to_gvs](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/trees_to_gvs.R)
    * loop through all trees {[call_PrintMojo](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/call_PrintMojo.R)}
  * lapply [gv_to_table](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/gv_to_table.R) to trees_to_gvs output

## map_h2o_encoding
Create mapping between output of h2o.predict_lead_node_assignment (i.e. terminal node paths represented as L(eft) or R(ight) decisions at each node) and terminal node path represented as variable decisions (i.e. A > x & B in (y, z) etc).

* [map_h2o_encoding](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/map_h2o_encoding.R)
  * [extract_split_rules](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/extract_split_rules.R) with terminal_node_paths = TRUE
  * [extract_split_rules](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/extract_split_rules.R) with terminal_node_paths = FALSE

## extract_split_rules
Get split conditions for terminal nodes for all trees in a h2o model.

* [extract_split_rules](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/extract_split_rules.R)
  * lapply [get_split_expressions](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/get_split_expressions.R) to [h2o_tree_convertR](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/h2o_tree_convertR.R) output (function input)
  * lapply [terminal_node_exprs](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/terminal_node_exprs.R) to [get_split_expressions](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/get_split_expressions.R) output

## encode_terminal_nodes
Encode input data as terminal nodes from a h2o tree model.

* [encode_terminal_nodes](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/encode_terminal_nodes.R)
  * mapply [encode_terminal_nodes_df](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/encode_terminal_nodes_df.R) to [h2o_tree_convertR](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/h2o_tree_convertR.R) and [extract_split_rules](https://github.com/richardangell/h2oTreeHelpR/blob/master/R/extract_split_rules.R) outputs (function inputs)
    * evaluate terminal node conditions on input data

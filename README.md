# h2oTreeHelpR
Useful functions to manipulate tree based models in h2o from R. <br>
Refer to the [Code-Structure](https://github.com/richardangell/h2oTreeHelpR/blob/master/Code-Structure.md) document for an overview of the code structure. <br>
The main functions in the package are the following;

## h2o_tree_convertR
Convert h2o tree structures to data.frames.

## map_h2o_encoding
Create mapping between output of h2o.predict_lead_node_assignment (i.e. terminal node paths represented as L(eft) or R(ight) decisions at each node) and terminal node path represented as variable decisions (i.e. A > x & B in (y, z) etc).

## extract_split_rules
Get split conditions for terminal nodes for all trees in a h2o model.

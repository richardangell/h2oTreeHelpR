h2oTreeHelpR
================

##### Useful functions to manipulate tree based models in h2o from R

Refer to the [Code-Structure](https://github.com/richardangell/h2oTreeHelpR/blob/master/Code-Structure.md) document for an overview of the code structure. <br>

The main functions in the package are demonstrated below. <br>

First build a simple gbm with the following code; <br>

``` r
library(h2o)
library(h2oTreeHelpR)
h2o.no_progress()
h2o.init()
```

``` r
prostate.hex = h2o.uploadFile(path = system.file("extdata",
                                                 "prostate.csv",
                                                 package = "h2o"),
                              destination_frame = "prostate.hex")
prostate.hex["RACE"] = as.factor(prostate.hex["RACE"])
prostate.hex["DPROS"] = as.factor(prostate.hex["DPROS"])
expl_cols <- c("AGE", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON")
prostate.gbm = h2o.gbm(x = expl_cols,
                       y = "CAPSULE",
                       training_frame = prostate.hex,
                       ntrees = 3,
                       max_depth = 1,
                       learn_rate = 0.1)
```

h2o\_tree\_convertR
-------------------

##### Convert h2o tree structures to data.frames

``` r
prostate.gbm
```

    ## Model Details:
    ## ==============
    ## 
    ## H2ORegressionModel: gbm
    ## Model ID:  GBM_model_R_1538296273962_39 
    ## Model Summary: 
    ##   number_of_trees number_of_internal_trees model_size_in_bytes min_depth
    ## 1               3                        3                 260         1
    ##   max_depth mean_depth min_leaves max_leaves mean_leaves
    ## 1         1    1.00000          2          2     2.00000
    ## 
    ## 
    ## H2ORegressionMetrics: gbm
    ## ** Reported on training data. **
    ## 
    ## MSE:  0.2193848
    ## RMSE:  0.4683854
    ## MAE:  0.4565917
    ## RMSLE:  0.3295538
    ## Mean Residual Deviance :  0.2193848

``` r
h2o_tree_dfs = h2o_tree_convertR(h2o_model = prostate.gbm)
```

``` r
h2o_tree_dfs[[1]]
```

    ##          node                                     node_text predictions
    ## 1 SG_0_Node_0 [shape=box, fontsize=14, label=GLEASON < 6.5]          NA
    ## 2 SG_0_Node_3             [fontsize=14, label=-0.019210527] -0.01921053
    ## 3 SG_0_Node_4              [fontsize=14, label=0.023479532]  0.02347953
    ##   node_text_label  left_split right_split left_split_levels
    ## 1   GLEASON < 6.5 SG_0_Node_3 SG_0_Node_4            [NA]|<
    ## 2    -0.019210527        <NA>        <NA>              <NA>
    ## 3     0.023479532        <NA>        <NA>              <NA>
    ##   right_split_levels NA_direction node_variable_type split_column
    ## 1                 >=         left            numeric      GLEASON
    ## 2               <NA>         <NA>               <NA>         <NA>
    ## 3               <NA>         <NA>               <NA>         <NA>
    ##   node_split_point
    ## 1              6.5
    ## 2               NA
    ## 3               NA

extract\_split\_rules
---------------------

##### Get split conditions for terminal nodes for trees in a h2o model

``` r
terminal_node_rules <- extract_split_rules(h2o_tree_dfs)
```

``` r
terminal_node_rules[[1]]
```

    ##   terminal_node terminal_node_depth      terminal_node_path
    ## 1   SG_0_Node_3                   1 SG_0_Node_0.SG_0_Node_3
    ## 2   SG_0_Node_4                   1 SG_0_Node_0.SG_0_Node_4
    ##           terminal_node_directions
    ## 1 (GLEASON < 6.5 | is.na(GLEASON))
    ## 2                   GLEASON >= 6.5

map\_h2o\_encoding
------------------

##### Create a mapping for the output of h2o.predict\_lead\_node\_assignment in terms of variable conditions

Specifically this a mapping between terminal node paths represented as i. L(eft) or R(ight) directions at each node (output from h2o.predict\_lead\_node\_assignment) and ii. variable conditions (i.e. A &gt; x & B in (y, z) etc).

``` r
terminal_node_mapping <- map_h2o_encoding(h2o_tree_dfs)
```

``` r
terminal_node_mapping[[1]]
```

    ##   terminal_node terminal_node_depth      terminal_node_path
    ## 1   SG_0_Node_3                   1 SG_0_Node_0.SG_0_Node_3
    ## 2   SG_0_Node_4                   1 SG_0_Node_0.SG_0_Node_4
    ##           terminal_node_directions terminal_node_directions_h2o
    ## 1 (GLEASON < 6.5 | is.na(GLEASON))                            L
    ## 2                   GLEASON >= 6.5                            R

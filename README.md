# mmmactive

# Download

Need the secret token to download from private repo

    devtools::install_github("bdshaff/mmmactive", auth_token = token)

# RoadMap

-   Handling models with randomized variables

    -   Decomp

    -   Unnesting

    -   Response

-   Handling Unnested models

-   Response

-   Support for National and Regional Response Curves

-   Support for Aggregate Variable level Response generation

# Functions

## Create

`create_mod_obj`

`create_model_setup`

`create_model_spec`

## Activation

`activate_model_setup`

`activate_model_spec`

`update_model_spec`

`activate_dummy_spec`

## Data Loading

`Load_ModelData`

`Load_SpendData`

`Load_FitCurves`

`Load_FMIData` (to be removed)

`Load_MSRPData` (to be removed)

## Transform

`Transform2`

`TransformVar`

-   `AdResponse`

-   `adstockv3`

-   `AdStockPD`

`TransformSplit`

`TransformTemp`

`TransformTempJoin`

`Add_Dummy3`

## Modeling

`Run_Model`

`Run_Model3`

`my_bayes`

`Set_Date_Range`

`Var_Test_Cor`

## Decomposition

`Decomp2`

`Decomp3`

`Unnest`

## Response

`Reach_Curve_Spec`

`ReachCurveCalcl3`

`Response_Curves2`

`Reach`

`solve_abc`

`effective_grps_aggregate`

`effective_grps_window`

`generate_contribution_curve`

`generate_reach_curve`

`get_alpha_beta_gamma`

`get_channel_reach`

`get_reach_tables`

`createABG`

## Visualization

`plot_transformed`

`act_pred3`

## Logical

`is.mod_obj`

`is.activated` (to be removed)

`is.transform_ready` (to be removed)

`is.load_data_ready` (to be removed)

`is.modeled` (to be removed)

`is.data_transformed` (to be removed)

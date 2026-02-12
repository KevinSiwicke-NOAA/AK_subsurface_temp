# AK_subsurface_temp
Comparing observed subsurface temperatures with ocean model output across Alaska.

This code accompanies the submitted publication "Using modeled subsurface ocean temperature predictions in fishery science applications" submitted to Fisheries Oceanography. 

We use temperature data collected on three fishery-independent survey platforms conducted in Alaska. The most data comes from the Alaska Fisheries Science Center's (AFSC) bottom trawl surveys in the eastern Bering Sea (EBS), Aleutian Islands (AI), and Gulf of Alaska (GOA). Data from the shelf break comes from the AFSC longline survey across Alaska. Additional data came from the International Pacific Halibut Commissions longline survey.

The combined dataset was used to assess how well the Hybrid Coordinate Ocean Model (HYCOM) was able to predict subsurface temperatures in Alaska waters. We additionally explored bias-correction models (BCM) that included covariates seeking improved subsurface temperature predictions.

We then compared naive HYCOM predictions with BCM predictions in three case studies.

Data and code are available for reproducing analysis and figures. Most files have code that shows how to download data directly, as many of these files can be quite large and are not directly included here.

1) Analysis of observations is conducted in 'r/block_10fold_analysis.R', which calls on 'r/block_CV.R' to load data and spatial block design.
2) Plots of model results were created in 'r/block_CV Results_Plots.r'
3) Code to create figures in main document are located in the 'r/figures' folder.

We often use the AFSC Groundfish Assessment Program's akgfmaps package for mapping in Alaska, and this package can installed for R versions >= 4.0 using the using the following code:

```
library(remotes)

install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
```

Contact: kevin.siwicke@noaa.gov
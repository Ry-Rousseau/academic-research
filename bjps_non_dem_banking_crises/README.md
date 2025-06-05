
### Autocratic Bank Bailouts: Financialized Pensions, Mass Wealth Accumulation, and Regime Survival

Pension schemes provide autocratic regimes with an important strategic tool to curry the support of critical groups by enabling them to accumulate sizeable, deferred-access wealth. Yet, we argue, financialized pension schemes also impose political constraints on autocratic governments, incentivizing them to be more responsive to the interests of these supporters when severe banking crises put pension wealth at risk.

Employing a unique dataset of crisis policy interventions since 1800 and a state-of-the-art approach to multiple imputation, we show that autocratic regimes with financialized pension schemes provide more extensive bailouts during crises. These bailouts enhance regime resilience by limiting losses for scheme participants and reducing social unrest, reflecting the imperatives of regime survival and the implicit political bargain between the regime and its critical supporters.

### Replication of Findings

Given the use of multiple imputation, the replication process is more involved than comparable papers. 

The dataset was constructed on a country-year level to isolate the effect of policy interventions, with socioeconomic, political and demographic controls. See the codebook for an in-depth description of variables used and their sources. See appendix for an exploration of robustness check and complementary models.

Data is accessed through the structure specified in setup.R. The primary data is SBC_policy_response_in_non_democracies_january_2025.dta. The data is pre-processed. 

Utilize master.R for individual figure and table replications. 

### Multiple imputation

This paper utilizes the rMIDAS package from Lall and Robinson (2022) to robubstly and efficiently fill in missing values for the panel dataset. rMIDAS fits a autoencoder neural network with a encoder, decoder and a bottleneck layer to generatively fill in missing values based on non-linear pattern recognitions in the original data. See Lall and Robinson (2022) for an in-depth exploration of the network architecture, including usage of dropout layers to yield variation in imputed datasets for an accurate reflection of uncertainties in data generation. The neural network approach of rMIDAS is a state-of-the-art for preserving uncertainty estimates and realistic imputation of missing values at scale.    

M3/M4 models utilize imputed datasets to estimate coefficients. This is done through the application of Rubin's rules, where m regression models are estimated for m completed datasets. Applications of Rubin's rules are in step 2 of the multiple imputation scripts and in functions.R.  

Generate imputed datasets using the scripts before proceeding with the M3/M4 model estimations. 
# academic-research
Replication files and code for published research papers - reproducible analysis, modeling, and visualization


### Distributional preferences in a global pandemic: Voter attitudes towards COVID-19 economy policy interventions (forthcoming)
### European Journal of Political Research

An exploration of attitudes towards the social insurance of employment/income risk and financial interventions that supported household, corporate and financial sector balance sheets during the COVID-19 pandemic. Attitudes were assessed using a conjoint survey experiment fielded in Australia and the United Kingdom in early 2021.

Results reveal strongest support for interventions providing social insurance of employment/income risk, with preferences stable across economic, social and experiential cleavages. Support for financial interventions was mixed, revealing opposition toward support for large firms and banks. These findings are pertinent in understanding questions on public support for social insurance and redistribution during times of crisis. 

#### Replication of Findings

Open the .Rproj file to correctly configure your directory. 

To reproduce manuscript figures, a master.R script is provided, which reproduces figures in order of their appearance in publication. It also runs the setup script, performs data pre-processing, and loads main functions.

The setup.R script loads data, defines sub-directories for data and output, and creates country-specific data subsets.

It is assumed that a data folder exists in the project directory with the following: atsi_firm_data.csv, AU_final.csv, conjoint_pooled_dataset.rds, owid-covid-data.csv, uk_final.csv. These span individual level survey results for Australia and UK cases during the observation period, as well as relevant supplementary data. 

Given the proprietary nature of the data, it will be made available on the EJPR Dataverse following publication. 

#### Notes on package compatibility

This analysis utilizes the cregg package, which is no longer available on CRAN for R versions > 4.0.0. Lacking comprehensive conjoint analysis options in modern R version, the renv package is used to snapshot the project package environment, backtracking R versions. It is necessary to initialize the renv snapshot to run master.R and reproduce findings.

To initialize the package environment:
1) Run setup.R
2) When prompted to install required packages in project directory, select Y in console
3) If any issues arise, ensure that renv.lock, .Rprofile, renv/settings.json and renv/activate.R are available in the project directory
4) Following renv troubleshooting steps detailed [here](https://rstudio.github.io/renv/articles/renv.html)

### Autocratic Bank Bailouts: Financialized Pensions, Mass Wealth Accumulation, and Regime Survival
### British Journal of Political Science

Pension schemes provide autocratic regimes with an important strategic tool to curry the support of critical groups by enabling them to accumulate sizeable, deferred-access wealth. Yet, we argue, financialized pension schemes also impose political constraints on autocratic governments, incentivizing them to be more responsive to the interests of these supporters when severe banking crises put pension wealth at risk.

Employing a unique dataset of crisis policy interventions since 1800 and a state-of-the-art approach to multiple imputation, we show that autocratic regimes with financialized pension schemes provide more extensive bailouts during crises. These bailouts enhance regime resilience by limiting losses for scheme participants and reducing social unrest, reflecting the imperatives of regime survival and the implicit political bargain between the regime and its critical supporters.

#### Replication of Findings

Given the use of multiple imputation, the replication process is more involved than comparable papers. 

The dataset was constructed on a country-year level to isolate the effect of policy interventions, with socioeconomic, political and demographic controls. See the codebook for an in-depth description of variables used and their sources. See appendix for an exploration of robustness check and complementary models.

Data is accessed through the structure specified in setup.R. The primary data is SBC_policy_response_in_non_democracies_january_2025.dta. The data is pre-processed. 

Utilize master.R for individual figure and table replications. 

#### Multiple imputation

This paper utilizes the rMIDAS package from Lall and Robinson (2022) to robubstly and efficiently fill in missing values for the panel dataset. rMIDAS fits a autoencoder neural network with a encoder, decoder and a bottleneck layer to generatively fill in missing values based on non-linear pattern recognitions in the original data. See Lall and Robinson (2022) for an in-depth exploration of the network architecture, including usage of dropout layers to yield variation in imputed datasets for an accurate reflection of uncertainties in data generation. The neural network approach of rMIDAS is a state-of-the-art for preserving uncertainty estimates and realistic imputation of missing values at scale.    

M3/M4 models utilize imputed datasets to estimate coefficients. This is done through the application of Rubin's rules, where m regression models are estimated for m completed datasets. Applications of Rubin's rules are in step 2 of the multiple imputation scripts and in functions.R.  

Generate imputed datasets using the scripts before proceeding with the M3/M4 model estimations. 
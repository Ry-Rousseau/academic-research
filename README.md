# Academic Research Repository
Replication files and code for research papers in political economy - reproducible analysis, modeling, and visualization, completed as Research Officer at the LSE Department of International Relations 2024-2025.

| Paper Title | Journal | Status | Folder | Key Methods |
|--------------------------------|--------------------------|---------|---------|-------------|
| Distributional preferences in a global pandemic: Voter attitudes towards COVID-19 economy policy interventions | European Journal of Political Research | Under Review | `/ejpr_econ_preferences/` | Conjoint analysis, survey inference  |
|  Autocratic Bank Bailouts: Financialized Pensions, Mass Wealth Accumulation, and Regime Survival | British Journal of Political Science | Forthcoming | `/bjps_non_dem_banking_crises/` | multiple imputation, panel regression |

### Reproduction Workflow
1. Navigate to specific paper folder
2. Follow paper-specific README
3. Run code files in numbered order
4. Output will be generated in `/output/` folders

### Project Descriptions and Replication Instructions

#### 1) Distributional preferences in a global pandemic: Voter attitudes towards COVID-19 economy policy interventions - European Journal of Political Research

An exploration of attitudes towards the social insurance of employment/income risk and financial interventions that supported household, corporate and financial sector balance sheets during the COVID-19 pandemic. Attitudes were assessed using a conjoint survey experiment fielded in Australia and the United Kingdom in early 2021.

Results reveal strongest support for interventions providing social insurance of employment/income risk, with preferences stable across economic, social and experiential cleavages. Support for financial interventions was mixed, revealing opposition toward support for large firms and banks. These findings are pertinent in understanding questions on public support for social insurance and redistribution during times of crisis. 

#### 2) Autocratic Bank Bailouts: Financialized Pensions, Mass Wealth Accumulation, and Regime Survival - British Journal of Political Science

Pension schemes provide autocratic regimes with an important strategic tool to curry the support of critical groups by enabling them to accumulate sizeable, deferred-access wealth. Yet, we argue, financialized pension schemes also impose political constraints on autocratic governments, incentivizing them to be more responsive to the interests of these supporters when severe banking crises put pension wealth at risk.

Employing a unique dataset of crisis policy interventions since 1800 and a state-of-the-art approach to multiple imputation, we show that autocratic regimes with financialized pension schemes provide more extensive bailouts during crises. These bailouts enhance regime resilience by limiting losses for scheme participants and reducing social unrest, reflecting the imperatives of regime survival and the implicit political bargain between the regime and its critical supporters.

##### Multiple imputation

This paper utilizes the rMIDAS package from Lall and Robinson (2022) to robubstly and efficiently fill in missing values for the panel dataset. rMIDAS fits a autoencoder neural network with a encoder, decoder and a bottleneck layer to generatively fill in missing values based on non-linear pattern recognitions in the original data. See Lall and Robinson (2022) for an in-depth exploration of the network architecture, including usage of dropout layers to yield variation in imputed datasets for an accurate reflection of uncertainties in data generation. The neural network approach of rMIDAS is a state-of-the-art for preserving uncertainty estimates and realistic imputation of missing values at scale.    

M3/M4 models utilize imputed datasets to estimate coefficients. This is done through the application of Rubin's rules, where m regression models are estimated for m completed datasets. Applications of Rubin's rules are in step 2 of the multiple imputation scripts and in functions.R.  

Generate imputed datasets using the scripts before proceeding with the M3/M4 model estimations. 


### Distributional preferences in a global pandemic: Voter attitudes towards COVID-19 economy policy interventions (forthcoming)
### European Journal of Political Research

An exploration of attitudes towards the social insurance of employment/income risk and financial interventions that supported household, corporate and financial sector balance sheets during the COVID-19 pandemic. Attitudes were assessed using a conjoint survey experiment fielded in Australia and the United Kingdom in early 2021.

Results reveal strongest support for interventions providing social insurance of employment/income risk, with preferences stable across economic, social and experiential cleavages. Support for financial interventions was mixed, revealing opposition toward support for large firms and banks. These findings are pertinent in understanding questions on public support for social insurance and redistribution during times of crisis. 

#### Replication of Findings

Open the .Rproj file to correctly configure your directory. 

To reproduce manuscript figures, a master.R script is provided, which reproduces figures in order of their appearance in publication. It also runs the setup script, performs data pre-processing, and loads main functions.

The setup.R script loads data, defines sub-directories for data and output, and creates country-specific data subsets.

It is assumed that a data folder exists in the project directory with the following: atsi_firm_data.csv, AU_final.csv, conjoint_pooled_dataset.rds, owid-covid-data.csv, uk_final.csv

Given the proprietary nature of the data, it will be made available on the EJPR Dataverse following publication. 

#### Notes on package compatibility

This analysis utilizes the cregg package, which is no longer available on CRAN for R versions > 4.0.0. Lacking comprehensive conjoint analysis options in modern R version, the renv package is used to snapshot the project package environment, backtracking R versions. It is necessary to initialize the renv snapshot to run master.R and reproduce findings.

To initialize the package environment:
1) Run setup.R
2) When prompted to install required packages in project directory, select Y in console
3) If any issues arise, ensure that renv.lock, .Rprofile, renv/settings.json and renv/activate.R are available in the project directory
4) Following renv troubleshooting steps detailed [here](https://rstudio.github.io/renv/articles/renv.html)

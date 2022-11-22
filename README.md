
MMODS Elicitation 1
-----------------
[![DOI](https://zenodo.org/badge/313416712.svg)](https://zenodo.org/badge/latestdoi/313416712)

Data and code used within the [MMODS-1 elicitation](https://midasnetwork.us/mmods/) of multiple modelling groups for supporting county reopening decisions during the COVID-19 pandemic in the US.  

### Software requirements

This repository uses both R and Python.  

* R scripts within this repository require the following R packages: `tidyverse, ggplot2, RColorBrewer, gridExtra`.  
* Python scripts within this repository require the following Python modules (Python >3.6): `numpy, pandas, matplotlib`.  These requirements are listed under `requirements.txt` and can be installed using, for instance, `pip install -r requirements.txt`.  


### Usage

* Once data has been downloaded, the following command will create an Rdata file including various metadata used in visualization, `make data_round2`

* Individual figures can then be generated using, for instance: `make fig2`

### Additional commands and notes

* `make fig4_panels` will generate subpanels for Figure 4, the multi-panel figure.  
* see `Makefile` for all commands. 


### Output

Output figures are saved in `output/figures`.



For additional detail, see:

Shea, K., Borchering, R.K., Probert, W.J., Howerton, E., Bogich, T.L., Li, S., van Panhuis, W.G., Viboud, C., ... and Runge, M.C. (2020) COVID-19 reopening strategies at the county level in the face of uncertainty: Multiple Models for Outbreak Decision Support. medRxiv. [https://www.medrxiv.org/content/10.1101/2020.11.03.20225409v1.full](https://www.medrxiv.org/content/10.1101/2020.11.03.20225409v1.full)

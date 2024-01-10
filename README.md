
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

* Shea, K., Borchering, R.K., Probert, W.J.M., Howerton, E., Bogich, T.L., Li, S.-L., van Panhuis, W.G., Viboud, C., ... and Runge, M.C. (2023) Multiple models for outbreak decision support in the face of uncertainty. Proc Natl Acad Sci U S A 120, e2207537120. [https://doi.org/10.1073/pnas.2207537120](https://doi.org/10.1073/pnas.2207537120)
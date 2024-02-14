Intersectional approach of everyday geography
========================================================================

## Description

This repository contains all the material (code and data) needed to process the data, perform the analysis and produce the figures that can be found in [[1]](https://journals.sagepub.com/doi/abs/10.1177/23998083231174025). This paper explores mismatch in hourly population profiles of French districts for gender, age and educational groups from an intersectional point of view. 

The repository is composed of three folders: the folder **Data** containing the raw data and three R scripts used to process the data and extract the metrics, the folder **Analysis** containing five R scripts developed to analyze the data and produce the different figures that can be found in the paper, and, finally, the folder **Vizu** containing the source code of the [interactive web application](http://shiny.umr-tetis.fr/Intersectionality/) developed as part of this study. 

## Data

Initial datasets coming from origin-destination surveys have been previously transformed in hourly populations estimations and displayed in the [Mobiliscope](https://www.mobiliscope.cnrs.fr), an open interactive geovizualisation platform to explore cities around the clock. Derived from version v4.1 of Mobiliscope, raw data gain to be directly downloaded [here](https://doi.org/10.5281/zenodo.7738571). The archive should be decompressed in the **RAW** folder.  

The three scripts ***0_ExtractSignals.R***, ***1_ExtractClusters_L2.R*** and ***2_ExtractData.R*** should be run successively to process the data and compute the metrics (and produce the Figures S1 and S5). All the files produced during this step are stored in a **Outputs** folder. All the analysis will be conducted on the different R objects contained in the file ***Data.Rdata***. 

## Analysis

The six scripts developed to analyze the data contains in the file ***Data/Outputs/Data.Rdata*** (generated at the previous step) correspond roughly to the different analysis performed in the paper and their associated figures and tables. 

* ***1_StatSample.R*** is used to produce the Figure 1.

* ***2_Maps.R*** is used to produce the Figure 2.

* ***3_Profiles.R*** is used to produce the Figure 3 and Figures S2-S3.

* ***4_Mismatch.R*** is used to produce the Figure 4 and Figure S4.

* ***5_Cluster.R*** is used to produce the Figure 5, Table S2 and Figure S6.

* ***6_REFvsALTER.R*** is used to produce the Table 1. 

## Interactive web application

The folder **Vizu** contains all the material needed to run [this interactive web application](http://shiny.umr-tetis.fr/Intersectionality/) developed as part of this paper. 
The script ***0_ExtractData.R*** contains in **Vizu** should be run after the three 'Data' scripts and the script 
***5_Cluster.R*** to generate the data needed to run the application.

## Contributors

- [Maxime Lenormand](https://www.maximelenormand.com/)
- [Julie Vallée](https://geographie-cites.cnrs.fr/en/members/julie-vallee/)

## References

[1] Vallée J & Lenormand M (2024) [Intersectional approach of everyday geography](https://journals.sagepub.com/doi/abs/10.1177/23998083231174025). *Environment and Planning B: Urban Analytics and City Science* 51, 347-365.  
[https://doi.org/10.1177/23998083231174025](https://doi.org/10.1177/23998083231174025)  
[https://arxiv.org/abs/2106.15492](https://arxiv.org/abs/2106.15492)

## Citation

If you use this code, please cite:

Vallée J & Lenormand M (2024) [Intersectional approach of everyday geography](https://journals.sagepub.com/doi/abs/10.1177/23998083231174025). *Environment and Planning B: Urban Analytics and City Science* 51, 347-365.  

If you need help, find a bug, want to give us advice or feedback, please contact us!
You can reach us at maxime.lenormand[at]inrae.fr or julie.vallee[at]parisgeo.cnrs.fr

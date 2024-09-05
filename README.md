[![doi](https://img.shields.io/badge/10.17605/OSF.IO/JYGVK-blue)][doi]

[doi]: https://doi.org/10.17605/OSF.IO/JYGVK

# LTR-MNM-compare

## Purpose of the Repository
The repository **LTR-MNM-compare** provides `R` code to calculate the Lifetime Risk of Maternal Near Miss (LTR-MNM) from published data on MNM prevalence and information on death and birth outcomes for some countries in Asia, Africa, the Middle East, and Latin America.

## Repository Structure
The repository **LTR-MNM-compare** contains two main folders, *Journal* and *medRxiv*.

### *1. Journal*
The main folder **Journal** contains all `R` scripts necessary to replicate the information reported in the journal version of our manuscript. The analysis files are stored in the sub-folder **scripts**. We also provide the results from our literature search on MNM prevalence in the sub-folder **data**. The remaining input data need to be downloaded directly from the respective data providers and stored in the sub-folder **data** to enable full replication of our results.

#### a. *scripts*
The sub-folder **scripts** contains the `R` scripts necessary to replicate all findings reported in our main manuscript and the appendix.

##### Files *100-main.R* to *199-output.R*
The file *100-main.R* installs some `R` packages, automatically generates the sub-folders **out** and **tmp**, and automatically executes all other analysis files starting with the digit *1* to generate country-specific estimates of MNM prevalence. Specifically, the analysis file *101-preparation.R* adjusts the data from our literature search on MNM prevalence using data on stillbirths (Ref. 1), multiple births (Ref. 2), and institutional delivery (Ref. 3). The analysis file *102-meta-analysis.R*  uses the adjusted data to generate one pooled estimate of MNM prevalence per country via a random effects meta analysis. Finally, the analysis file *199-output.R*  stores the input and output data from the random effects meta analysis in the automatically generated sub-folder **out**.

##### Files *200-main.R* to *299-output.R*
Similar to the analysis file *100-main.R* , the file *200-main.R* installs some `R` packages and automatically executes all other analysis files starting with the digit *2* to generate country-specific estimates of LTR-MNM, the lifetime risk of maternal death (LTR-MD), and the lifetime risk of a severe maternal outcome (LTR-SMO). The estimation is performed for each country separately using the data from the random effects meta analysis (see above), country-specific estimates of the maternal mortality ratio (Ref. 4), and country-specific information on death and birth outcomes (Ref. 5). Country-specific data output is temporarily stored in the folder **tmp**, which is deleted automatically at the end of the analysis. The analysis file *299-output.R*  stores the estimates of LTR-MNM, LTR-MD, and LTR-SMO in the automatically generated sub-folder **out**.

#### b. *data*
The sub-folder **data** contains the results from our literature search on MNM prevalence. All other data need to be downloaded from the indicated data providers and stored in the **data** sub-folder following our naming conventions. 

### 2. *medRxiv*
The main folder **medRxiv** contains all materials associated with version 1 of our manuscript preprint, as posted on *medRxiv* (Ref. 6). This main folder is just for reference, as the analytical strategy and code have changed following the peer review of our manuscript.

## How to Use the Repository
In order to run the `R` code provided in the repository **LTR-MNM-compare**, please proceed in the following order:

1. Download the repository from `github`. If applicable, unzip the downloaded folder and place it in a location convenient for you. 
2. Download the necessary data from the respective data providers and store them in the sub-folder **data** in the **Journal** folder following our naming conventions.
3. Double click on the file `LTR-MNM-compare.Rproj` in the **Journal** folder. This should open `RStudio` on your machine.  
4. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `100-main.R` located in the **scripts** sub-folder. Run the code.
5. Within `RStudio`, click on `File`/`Open File...` and select the analysis file `200-main.R` located in the **scripts** sub-folder. Run the code.

## Disclaimer
This study uses published data by the United Nations, the World Health Organization, and Monden et al. (2021; Ref. 2). The use of these data does not imply endorsement of our work by any of these organizations or authors.

## License
This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png

## References
1. United Nations Inter-agency Group for Child Mortality Estimation (UN IGME). 2020. *A Neglected Tragedy: The global burden of stillbirths.* New York: United Nations Children&#39;s Fund.
2. Monden, Christiaan, Gilles Pison, & Jeroen Smits. 2021. *Twin Peaks: more twinning in humans than ever before.* Human Reproduction 36(6): 1666–1673.
3. World Health Organization data on institutional delivery rates are available at: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/institutional-births-(-).
4. *Trends in maternal mortality 2000 to 2020: estimates by WHO, UNICEF, UNFPA, World Bank Group and UNDESA/Population Division.* 2023. Geneva: World Health Organization.
5. United Nations, Department of Economic and Social Affairs, Population Division. 2022. *World Population Prospects*. https://population.un.org/wpp/.
6. Gazeley, Ursula, Antonino Polizzi, Julio Romero Prieto, José Manuel Aburto, Georges Reniers, Veronique Filippi. 2024. *The Lifetime Risk of Maternal Near Miss morbidity in Asia, Africa, the Middle East, and Latin America: a cross-country systematic analysis.* medRxiv. March 26. doi: 10.1101/2024.03.26.24304883.
# Msc_thesis_th
Repo contains aggregated data and R replication scripts for my Master's thesis. 

The thesis extends the Rajan and Zingales (1998) industry-level finance and growth mechanism across a panel of 41 countries from 1990 to 2024. 

**appendix:** Contains figures, and tables included in the appendix. 
**data:** Contains the aggregated final datasets.
  * `Master_data.xlsx`: Includes aggregated sheets for industry growth, external dependence indices, and country-level financial development measures.
  * `UNIDO_Establishments.xlsx`: Contains establishment counts used for the extensive/intensive margin decomposition.
* **code**: Contains all R scripts required to generate the thesis tables.
  * 00_setup.R`: Master environment file that loads all necessary R packages.
  * 01_Table4_Basic_regression.R: Executes the baseline fixed-effects regressions (Table 4).
  * 02_Table5_US_EU_ED.R: Sensitivity analysis comparing results from US and EU firm based external dependence measure (Table5).
  * 03_Table6_Decomposition_growth.R: Runs the extensive and intensive margin decomposition (Table 6).
  * 04_Table7_Globalization_test.R: Tests the globalization channel using domestic and foreign credit (Table 7).
  * 05_Table8_India_CaseS_tudy.R: Calculates average annual manufacturing growth by external dependence for the India case study (Table 8).


To comply with commercial licensing restrictions for databases such as LSEG DataStream, raw unaggregated firm-level microdata is excluded from this repository. The `data/` folder contains the final aggregated inputs required to replicate all regression outputs. 

The aggregated variables were constructed using the following sources:

**1. Firm-Level Financial Data (LSEG DataStream)**
Used to construct the industry-level External Financial Dependence index and Capital Intensity ratios.
* **Capital Expenditures.** LSEG: WC04601
* **Net Cash Flow from Operating Activities:** LSEG: WC04860
* **Changes in Working Capital:** LSEG: WC04900
* **Net Property, Plant and Equipment:** LSEG: WC02501
*(Note: External dependence is defined as `(CapEx - CFO) / CapEx`, where CFO is operating cash flow adjusted for working capital changes. Investment intensity is `CapEx / Net PPE`.)*

**2. Industry-Level Data (UNIDO)**
* **Database:** UNIDO INDSTAT4
* **Variables:** Value added, gross fixed capital formation, and establishment counts.
* **Classification:** 3-digit and 4-digit ISIC Rev.3. (NAICS to ISIC Rev.3 concordance mapping was used to align U.S. firm data with UNIDO industry codes).
* **Deflators:** Producer Price Index (PPI) or GDP deflator sourced from the World Bank and IMF.

**3. Country-Level Macro & Institutional Data**
* **Domestic Credit to Private Sector (% of GDP):** World Bank World Development Indicators 	(FS.AST.PRVT.GD.ZS).
* **Market Capitalization (% of GDP):** World Bank World Development Indicators (	CM.MKT.LCAP.GD.ZS).
* **Rule of Law Index:** World Bank Worldwide Governance Indicators (scores ranging from -2.5 to 2.5). 	(RL.EST)
* **GDP per capita (constant USD):** World Bank Development Indicators (NY.GDP.PCAP.KD)
* **Consolidated Foreign Bank Claims (% of GDP):** Bank for International Settlements (BIS) Consolidated Banking Statistics, accessed via World Bank GFDD.OI.14.


The codes were written and tested in R. The following packages must be installed prior to running the scripts:
* `readxl`
* `dplyr`
* `tidyr`
* `plm`
* `lmtest`
* `sandwich`
* `stargazer`
* `ggplot2`
* `tidyverse`
* `patchwork`

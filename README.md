# Weather, Climate, and Economic Growth: A Dynamic Panel Analysis Using Global Subnational Data

[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.eneco.2026.109411-blue)](https://doi.org/10.1016/j.eneco.2026.109411)
[![Journal](https://img.shields.io/badge/Journal-Energy%20Economics%20160%20(2026)%20109411-green)](https://www.sciencedirect.com/journal/energy-economics)

> Dong, J., Tol, R. S. J., & Wang, J. (2026). Weather, climate, and economic growth: A dynamic panel analysis using global subnational data. *Energy Economics*, *160*, 109411. https://doi.org/10.1016/j.eneco.2026.109411

This repository contains the replication code for the above paper.

---

## Overview

Using a global subnational dataset covering **over 2,800 regions in 228 countries**, this study analyzes the effects of weather (short-term) and climate (long-term) on economic output. We address two key debates in the climate-economy literature:

1. **Level vs. Growth**: Do temperature shocks permanently reduce economic growth, or do they only temporarily lower output levels?
2. **Serial correlation in growth**: Does ignoring output growth dynamics lead to biased estimates of climate damages?

**Key findings:**
- Static annual panel models **overstate** climate damages by ignoring serial correlation in output growth
- Temperature has significant negative growth effects **only in poor, hot regions** (not globally)
- Long-difference models reveal that climate effects are **persistent but not permanent** — adaptive capacity develops over time
- Precipitation shows **no robust effects** in either static or dynamic specifications
- Under 2.0°C warming, the income gap between rich and poor regions is projected to **widen**

---

## Repository Structure

```
00script/
├── LD_MakeTable1_Figure3_new.do    # Table 1: Summary statistics; Figure 3: Time trends (GDPpc, growth, temperature, precipitation)
├── LD_MakeTable2_new.do            # Table 2: Static vs. dynamic annual panel models (5 specifications: static level, level+growth, 1-lag OLS, 3-lag OLS, diff-GMM)
├── LD_MakeTable3_new.do            # Table 3: Annual panel results with poor/rich income group interaction
├── LD_MakeTable4_new.do            # Table 4: Long-difference baseline results (OLS and diff-GMM, with poor/rich interaction)
├── LD_MakeTable6_new.do            # Table 6: Robustness check using World Bank country-level GDP data
├── LD_MakeTable7_new.do            # Table 7: Future GDP loss projections under climate scenarios
├── LD_MakeTableA1.do               # Table A1: Robustness check — alternative dynamic specifications (1–3 lags)
├── LD_MakeTableA2.do               # Table A2: Robustness check using alternative climate data source
├── LD_MakeTableA3.do               # Table A3: Robustness check — alternative temperature/precipitation measures
├── LD_MakeTableA4-6.do             # Tables A4–A6: Data comparison across subnational GDP data sources
├── LD_MakeTableA7.do               # Table A7: Robustness check — additional sensitivity analysis
├── LD_Bootstrap_new.do             # Bootstrap inference (1000 reps): both annual panel (xtabond2 with 3 lags) and long-difference models (xtabond2 with 1 lag, current and lagged climate terms)
├── LD_MakeFigure4_new.do           # Figure 4: Marginal effects of temperature from annual panel model
├── LD_MakeFigure5_new.do           # Figure 5: Marginal effects of temperature from long-difference model
├── LD_Damage_Figure6_new.R         # Figure 6: Projected GDP per capita losses under SSP2-4.5 warming scenarios (2015–2100)
├── LD_Damage_Figure7_new.R         # Figure 7: Regional heterogeneity in projected climate damages under SSP1-2.6
├── LD_MakeFigure8_new.do           # Figure 8: Adaptation dynamics — time-varying climate effects across periods
01data/
├── gdppc.csv                       # Subnational GDP per capita panel data (1990–2022)
├── ...                             # Additional data and scenario files
Data_and_Result_Comparison.csv      # Comparison of results across alternative data sources
```

---

## Data

The dataset covers **2,800+ subnational regions** (e.g., states, provinces, departments) across 228 countries, providing near-complete global coverage of inhabited areas. Key variables include:

- **GDP per capita** at the subnational level
- **Temperature** (annual average, from CRU — Climate Research Unit)
- **Precipitation** (annual total, from CRU)
- Region and year fixed effects

Using subnational data avoids the aggregation bias present in country-level studies and enables more precise matching of local economic activity with local weather conditions.

---

## Software Requirements

| Software | Version | Purpose |
|----------|---------|---------|
| **Stata/MP** | 15.0+ | Primary regressions, tables, and figures (`.do` files) |
| **R** | 4.0+ | Damage projections and figures (`.R` files) |

### R Packages
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "data.table"))
```

### Stata Packages
```stata
ssc install reghdfe, replace
ssc install ftools, replace
ssc install ivreg2, replace
ssc install xtabond2, replace
ssc install estout, replace
```

---

## Replication Instructions

1. **Clone this repository:**
   ```bash
   git clone https://github.com/ClimateResearch/Outputdamage2026.git
   cd Outputdamage2026
   ```

2. **Set up the data:**
   Ensure `gdppc.csv` and other data files are in the `01data/` directory.

3. **Run Stata scripts in order:**
   ```stata
   do 00script/LD_MakeTable1_Figure3_new.do   // Summary statistics & time trends
   do 00script/LD_MakeTable2_new.do            // Static vs. dynamic panel comparison
   do 00script/LD_MakeTable3_new.do            // Income group heterogeneity
   do 00script/LD_MakeTable4_new.do            // Long-difference baseline
   do 00script/LD_MakeTable6_new.do            // World Bank data robustness
   do 00script/LD_MakeTable7_new.do            // Future projections
   do 00script/LD_Bootstrap_new.do             // Bootstrap inference (panel & long-difference)
   ```

4. **Run R scripts for damage figures:**
   ```bash
   Rscript 00script/LD_Damage_Figure6_new.R    # SSP2-4.5 projections
   Rscript 00script/LD_Damage_Figure7_new.R    # SSP1-2.6 regional heterogeneity
   ```

---

## Citation

**Copy-paste format:**

> Dong, J., Tol, R. S. J., & Wang, J. (2026). Weather, climate, and economic growth: A dynamic panel analysis using global subnational data. *Energy Economics*, *160*, 109411. https://doi.org/10.1016/j.eneco.2026.109411

**BibTeX:**

```bibtex
@article{Dong2026Weather,
  title   = {Weather, climate, and economic growth: A dynamic panel analysis using global subnational data},
  author  = {Jinchi Dong and Richard S.J. Tol and Jinnan Wang},
  journal = {Energy Economics},
  volume  = {160},
  pages   = {109411},
  year    = {2026},
  doi     = {10.1016/j.eneco.2026.109411}
}
```

---

## Contact

- **Jinchi Dong** (corresponding author): [dongjinchi@163.com](mailto:dongjinchi@163.com)
- School of Economics, Huazhong University of Science and Technology, Wuhan, China
- GitHub: [@ClimateResearch](https://github.com/ClimateResearch)

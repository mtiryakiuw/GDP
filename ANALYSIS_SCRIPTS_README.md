# R Analysis Scripts Overview

## 📊 Analysis Scripts and Their Outputs

This document provides an overview of all R scripts in the project and what they generate.

---

## 🎯 Main Analysis Scripts

### 1. **GDP.r** - Master Analysis File
**Purpose**: Main panel data analysis with three complementary models

**What it does**:
- Loads data for 2010, 2014, 2018, 2022 (4 time periods)
- Processes 3 panel datasets:
  - Panel 1: Age × Occupation
  - Panel 2: Sector × Occupation (18 detailed sectors)
  - Panel 3: Sector × Education
- Estimates Random Effects and Fixed Effects models
- Performs Hausman tests
- Calculates robust standard errors

**Outputs**:
- ✅ **Report**: `output/reports/model_summaries_4years.txt`
- 📊 **Data**: 
  - `output/data/panel1_age_occupation_4years.csv`
  - `output/data/panel2_sector_occupation_4years.csv`
  - `output/data/panel3_sector_education_4years.csv`
- 📑 **Table**: `output/tables/panel_results_4years.tex`

**Run**: `Rscript GDP.r`

---

### 2. **panel2_main_analysis.R** - Detailed Sector×Occupation Analysis
**Purpose**: Deep dive into Panel 2 with 18 NACE sectors and 9 ISCO occupations

**What it does**:
- Data quality assessment and outlier removal
- Panel structure validation (balanced vs unbalanced)
- Descriptive statistics by sector, occupation, year
- Multiple panel models (FE, RE, Interactions)
- Diagnostic tests (Breusch-Godfrey, Breusch-Pagan, structural breaks)
- Hypothesis testing (sectoral effects, occupational hierarchy, temporal convergence)
- Robustness checks (balanced panel, winsorized)

**Outputs**:
- ✅ **Reports**: 
  - `output/reports/panel2_model_summaries.txt`
  - `output/reports/panel2_analysis_full_output.txt` (comprehensive)
- 📊 **Data**:
  - `output/data/panel2_sector_occupation_cleaned.csv`
  - `output/data/panel2_detailed_sector_analysis.csv`
- 📑 **Table**: `output/tables/panel2_results.tex`

**Run**: `Rscript panel2_main_analysis.R`

---

### 3. **extended_analysis.R** - Country Groups & Convergence
**Purpose**: Analyze gender pay gap by country groups and test convergence hypotheses

**What it does**:
- Classifies countries into 7 groups (Nordic, Continental, Mediterranean, Eastern, Liberal, Balkans, Other)
- Calculates descriptive statistics by country group
- Analyzes 18 sectors for positive vs negative gaps
- Tests sigma convergence (cross-country variance reduction)
- Tests beta convergence (catch-up effect)
- Estimates models with country group dummies and interactions
- Creates 5 publication-ready figures

**Outputs**:
- ✅ **Report**: `output/reports/extended_analysis_summary.txt`
- 📊 **Data**:
  - `output/data/country_group_statistics.csv`
  - `output/data/temporal_trends_by_group.csv`
  - `output/data/sigma_convergence.csv`
  - `output/data/beta_convergence.csv`
  - `output/data/sector_detail_positive_negative_gaps.csv`
- 📑 **Table**: `output/tables/extended_country_group_analysis.tex`
- 🖼️ **Figures**:
  - `output/figures/18_sectors_detailed.png`
  - `output/figures/country_groups_comparison.png`
  - `output/figures/beta_convergence_scatter.png`
  - `output/figures/negative_gaps_by_sector.png`
  - `output/figures/temporal_trends_country_groups.png`

**Run**: `Rscript extended_analysis.R`

---

### 4. **analyze_temporal_trends.R** - Temporal Convergence Analysis
**Purpose**: Analyze 2010-2022 trends by country group

**What it does**:
- Calculates change from 2010 to 2022 for each country group
- Identifies converging vs diverging groups
- Provides detailed year-by-year trends (especially for Balkans)
- Corrects misconceptions about universal convergence

**Outputs**:
- ✅ **Report**: `output/reports/temporal_trends_analysis.txt`

**Key Findings**:
- 5/6 country groups converged (83%)
- Liberal countries: -5.05 pp (-27.4%) - fastest convergence
- Balkans: +2.38 pp (+36.2%) - diverging!

**Run**: `Rscript analyze_temporal_trends.R`

---

### 5. **diagnostic_tests.R** - Panel Regression Diagnostics
**Purpose**: Comprehensive diagnostic tests for Panel 2 models

**What it does**:
- Breusch-Godfrey test for serial correlation
- Breusch-Pagan test for heteroskedasticity
- Hausman test for FE vs RE specification
- Extracts key coefficients with cluster-robust SE

**Outputs**:
- ✅ **Report**: `output/reports/diagnostic_tests_results.txt`

**Key Findings**:
- Serial correlation detected (p < 0.001) → cluster-robust SE needed
- Heteroskedasticity detected (p < 0.001) → robust SE necessary
- Industry sector: +2.16 pp (p < 0.001)
- Public sector: -2.55 pp (p < 0.001)

**Run**: `Rscript diagnostic_tests.R`

---

### 6. **calculate_model_fit.R** - Model Comparison (AIC/BIC)
**Purpose**: Compare alternative model specifications using information criteria

**What it does**:
- Calculates AIC and BIC for 4 models:
  1. Fixed Effects (Time only)
  2. Random Effects (Sectors + Occupations)
  3. Random Effects with Interactions ⭐ BEST
  4. Random Effects with Country Groups
- Compares R² across specifications

**Outputs**:
- ✅ **Report**: `output/reports/model_fit_comparison.csv`

**Key Finding**: Interaction model has lowest AIC/BIC → best fit

**Run**: `Rscript calculate_model_fit.R`

---

## 📁 Figure Creation Scripts (`figure_creation/`)

These scripts generate publication-ready figures but **do not produce text reports**.

### 7. **create_beta_convergence_figure.R**
Scatter plot showing beta convergence (initial gap vs change)

**Output**: `output/figures/beta_convergence.png`

---

### 8. **create_country_groups_figure.R**
Bar chart comparing mean gaps by country group

**Output**: `output/figures/country_groups_barplot.png`

---

### 9. **create_scatter_figure.R**
Scatter plots for various relationships (gap vs education, gap vs sector, etc.)

**Output**: Multiple scatter plot figures

---

### 10. **create_temporal_plot.R**
Line plots showing temporal evolution of gaps

**Output**: `output/figures/temporal_trends.png`

---

### 11. **fix_figure1_sorting.R**
Fixes sorting issues in Figure 1 (country rankings)

---

## 📋 Template Scripts (`Templates/`)

### 12. **panel1_supplementary_analysis.R**
Supplementary analysis for Panel 1 (Age × Occupation)

**Outputs**:
- ✅ **Report**: `output/reports/panel1_model_summaries.txt`
- 📊 **Data**: 
  - `output/data/panel1_age_occupation_cleaned.csv`
  - `output/data/panel1_age_statistics.csv`

**Run**: `Rscript Templates/panel1_supplementary_analysis.R`

---

### 13. **paneldata_analysis.R**
Template/backup file with comprehensive analysis functions (not actively used)

---

## 🎯 Quick Reference: Which Script for What?

| Need | Script | Output |
|------|--------|--------|
| Main models (3 panels) | `GDP.r` | `model_summaries_4years.txt` |
| Detailed Panel 2 analysis | `panel2_main_analysis.R` | `panel2_model_summaries.txt` |
| Country groups & convergence | `extended_analysis.R` | `extended_analysis_summary.txt` |
| Temporal trends | `analyze_temporal_trends.R` | `temporal_trends_analysis.txt` |
| Diagnostic tests | `diagnostic_tests.R` | `diagnostic_tests_results.txt` |
| Model comparison (AIC/BIC) | `calculate_model_fit.R` | `model_fit_comparison.csv` |
| Panel 1 supplementary | `Templates/panel1_supplementary_analysis.R` | `panel1_model_summaries.txt` |

---

## 🚀 Running All Analyses

To regenerate all reports, run:

```bash
cd /Users/nemo/Desktop/GDP/GDP

# Main analyses (in order)
Rscript GDP.r
Rscript panel2_main_analysis.R
Rscript extended_analysis.R
Rscript analyze_temporal_trends.R
Rscript diagnostic_tests.R
Rscript calculate_model_fit.R

# Supplementary
Rscript Templates/panel1_supplementary_analysis.R
```

---

## 📊 Summary Statistics

- **Total R scripts**: 13 files
- **Scripts generating reports**: 7 files
- **Scripts generating figures**: 5 files
- **Total reports generated**: 7 text files + 1 CSV
- **Total figures**: 5+ publication-ready plots
- **Total data files**: 10+ CSV files
- **Total LaTeX tables**: 3 files

---

**Status**: ✅ All analysis scripts now generate reports  
**Last Updated**: November 6, 2025  
**Project**: Gender Pay Gap Analysis (2010-2022)

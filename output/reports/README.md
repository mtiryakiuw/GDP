# Gender Pay Gap Analysis - Reports Summary

## 📊 Analysis Reports Overview

This folder contains all analysis reports generated from the R scripts in the GDP project.

### Generated Reports

| Report File | Source Script | Description | Size |
|-------------|---------------|-------------|------|
| **model_summaries_4years.txt** | `GDP.r` | Main panel data models (3 panels: Age×Occupation, Sector×Occupation, Sector×Education) for 2010-2022 with 4-year intervals | 5.0 KB |
| **panel1_model_summaries.txt** | `Templates/panel1_supplementary_analysis.R` | Supplementary analysis for Panel 1 (Age × Occupation interactions) | 5.1 KB |
| **panel2_model_summaries.txt** | `panel2_main_analysis.R` | Detailed analysis for Panel 2 (18 sectors × 9 occupations) | 4.9 KB |
| **panel2_analysis_full_output.txt** | `panel2_main_analysis.R` | Complete output including diagnostics and robustness checks | 23 KB |
| **extended_analysis_summary.txt** | `extended_analysis.R` | Country groups, convergence tests (sigma & beta), sectoral detail analysis | 10 KB |
| **temporal_trends_analysis.txt** | `analyze_temporal_trends.R` | Temporal trends by country group (2010-2022), convergence/divergence patterns | 3.2 KB |
| **diagnostic_tests_results.txt** | `diagnostic_tests.R` | Breusch-Godfrey, Breusch-Pagan, and other diagnostic tests | 6.0 KB |

### Report Contents Summary

#### 1. Main Analysis (`model_summaries_4years.txt`)
- **Panel 1**: Age × Occupation (Young, Mid-Career, Senior × 9 occupations)
- **Panel 2**: Sector × Occupation (18 NACE sectors × 9 ISCO occupations)
- **Panel 3**: Sector × Education (18 sectors × Low/Medium/High education)
- Random Effects vs Fixed Effects comparison
- Robust standard errors (HC1)
- Hausman test results

#### 2. Panel-Specific Analysis
- **Panel 1** (`panel1_model_summaries.txt`): Age group effects, occupational hierarchy
- **Panel 2** (`panel2_model_summaries.txt`, `panel2_analysis_full_output.txt`): 
  - 18 detailed NACE sectors
  - Sector-occupation interactions
  - Industry vs services vs public sector
  - Managerial vs high-skill vs other occupations

#### 3. Extended Analysis (`extended_analysis_summary.txt`)
- **Country Groups**: Nordic, Continental, Mediterranean, Eastern, Liberal, Balkans
- **Convergence Tests**:
  - Sigma convergence (cross-country variance reduction)
  - Beta convergence (countries with higher initial gaps converge faster)
- **Sectoral Detail**: Positive vs negative gaps by 18 sectors
- **Country Group × Sector Interactions**

#### 4. Temporal Analysis (`temporal_trends_analysis.txt`)
- **2010-2022 Trends** by country group
- **Convergence**: 5/6 groups show gap reduction (83%)
- **Divergence**: Balkans (+2.38 pp, +36.2%)
- **Fastest Convergence**: Liberal countries (-5.05 pp, -27.4%)

#### 5. Diagnostic Tests (`diagnostic_tests_results.txt`)
- **Breusch-Godfrey Test**: Serial correlation detection (p < 0.001)
- **Breusch-Pagan Test**: Heteroskedasticity detection (p < 0.001)
- **Hausman Test**: Fixed vs Random Effects specification
- **Model Fit Statistics**: R², AIC, BIC
- **Distributional Tests**: Multimodality, Kolmogorov-Smirnov

---

## 🔄 How to Regenerate Reports

To regenerate any report, run the corresponding R script:

```bash
# Main analysis (all 3 panels)
Rscript GDP.r

# Panel 1 supplementary
Rscript Templates/panel1_supplementary_analysis.R

# Panel 2 main analysis
Rscript panel2_main_analysis.R

# Extended analysis (country groups, convergence)
Rscript extended_analysis.R

# Temporal trends
Rscript analyze_temporal_trends.R

# Diagnostic tests
Rscript diagnostic_tests.R
```

---

## 📁 Related Output Files

### Data Files (`output/data/`)
- `panel1_age_occupation_4years.csv`
- `panel2_sector_occupation_4years.csv`
- `panel3_sector_education_4years.csv`
- `country_group_statistics.csv`
- `temporal_trends_by_group.csv`
- `sigma_convergence.csv`
- `beta_convergence.csv`
- `sector_detail_positive_negative_gaps.csv`

### Tables (`output/tables/`)
- `panel_results_4years.tex` - LaTeX table for all 3 panels
- `panel2_results.tex` - LaTeX table for Panel 2
- `extended_country_group_analysis.tex` - Country groups & interactions

### Figures (`output/figures/`)
- Country group comparisons
- Temporal trend plots
- Convergence scatter plots
- Sectoral detail visualizations

---

## 📧 For Thesis Use

All reports are formatted for direct inclusion in thesis:
- ✅ Comprehensive model summaries with interpretation
- ✅ Statistical test results with p-values
- ✅ Hypothesis testing outcomes
- ✅ Robustness checks documented
- ✅ Country group classifications and trends
- ✅ Convergence evidence (sigma & beta)

---

**Last Updated**: November 6, 2025  
**Total Reports**: 7 files (57 KB total)  
**Analysis Period**: 2010-2022 (4 time points)  
**Coverage**: 28+ European countries, 18 sectors, 9 occupations

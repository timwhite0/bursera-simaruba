# bursera-simaruba

Data and code for the manuscript "Low intensity selective logging of _Bursera simaruba_ and _milpa_ may help maintain wood supply for handicraft production"

## Repository contents

- `data/data_raw.xlsx`: original dataset.
- `clean_data.Rmd`: data cleaning script; writes `data/data_stems.csv` and `data/data_plots.csv`.
- Analysis scripts:
  - `stem_density.Rmd`
  - `basal_area.Rmd`
  - `stem_density_by_size_class.Rmd`
  - `basal_area_by_size_class.Rmd`
  - `proportion_cut.Rmd`
  - `final_models.Rmd`
  - `tables.Rmd`
- Output folders:
  - `figures/` for PNG figures.
  - `pdfs/` for rendered PDF reports.

## Required packages

Use R (recommended: R 4.2+), with these packages installed:

- `rmarkdown`
- `knitr`
- `tidyverse`
- `readxl`
- `car`
- `MASS`
- `lme4`
- `janitor`

Install any missing packages with:

```r
install.packages(c(
  "rmarkdown", "knitr", "tidyverse", "readxl",
  "car", "MASS", "lme4", "janitor"
))
```

## Instructions

Run scripts from the repository root (`bursera-simaruba`).

1. Data cleaning script:

```r
rmarkdown::render("clean_data.Rmd")
```

2. Analysis scripts:

```r
rmarkdown::render("stem_density.Rmd")
rmarkdown::render("basal_area.Rmd")
rmarkdown::render("stem_density_by_size_class.Rmd")
rmarkdown::render("basal_area_by_size_class.Rmd")
rmarkdown::render("proportion_cut.Rmd")
rmarkdown::render("final_models.Rmd")
rmarkdown::render("tables.Rmd")
```

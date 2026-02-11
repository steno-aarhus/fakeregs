## Overview

What this repo (aims to) contain:

- An introduction to using Arrow and DuckDB in R
- A guide covering some good practices to consider when working with Danish register data
- A collection of references to official documentation for commonly used registers
- A loosely defined list of handy tips for junior researchers doing data analysis in R for the first time
- A walkthrough of a workflow to illustrate common tasks encountered when processing Danish register data
- Functions to generate fake register data mimicing commonly used Danish registers

What this repo does *NOT* contain:

- A general beginners intro to the basics of R.

## How to use

- To get an intro to the why's and how's of high-performance data analysis with Arrow and DuckDB, explore the vignettes
- To get familiar working with Danish register data in practice: follow along the `common_tasks_dplyr.qmd`-vignette
- To generate fake register data for other purposes:
   - Source `generate_data.R` and run the functions as needed

## Feature roadmap

What is currently covered and planned

### Vignette contents

-  `introduction.qmd`: background information
   - [x] Data storage formats
   - [x] Processing engines: getting started with Arrow and DuckDB in R via `dplyr`
- `good_practice.qmd`-vignette: tips for working with register data
   - [ ] Thinking ahead: planning analyses to increase efficiency
   - [ ] Good habits
- `tips_tricks.qmd`: tips & tricks for quality of life
   - [ ] Tips and tricks

### Fake registers supported

- [x] bef
- [ ] udda
- [ ] faik
- [ ] akm
- [ ] lmdb
- [ ] sysi/sssy
- [ ] lab
- [x] lpr2 (lpr_adm, lpr_diag)
- [x] lp3 (lpr_a_kontakt, lpr_a_diagnose)

### Data management operations in walkthrough

#### Specific tasks

- General tasks
   - [x] Read Parquet file from disk into R and convert to DuckDB
   - [ ] Join register data
   - [ ] Export results
      - Using `dput()` to convert objects to R code to facilitate exporting from DST 

- Specific tasks
  - [x] Clean `bef`
  - [ ] Clean `udda`
  - [ ] Clean `faik`
  - [ ] Clean `akm`
  - [ ] Clean `lmdb`
  - [ ] Clean `sysi`/`sssy`
  - [ ] Clean `lab`
  - Clean lpr2
     - [ ] `lpr_adm`
     - [ ] `lpr_diag`
  - Clean lpr3
     - [ ] `lpr_a_kontakt`
     - [ ] `lpr_a_diagnose`
     - To be decided: include `lpr_f`

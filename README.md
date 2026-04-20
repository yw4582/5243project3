# A/B Test of a Learning Preference Mini-Quiz Interface

**Course:** GU4243 / GR5243 Applied Data Science  
**Department:** Department of Statistics, Columbia University  
**Term:** Spring 2026  
**Team 12:** Junyang Li, Helena Li, Zhuyun Jin, Yuxuan Wu

## Overview

This repository contains the code and materials for Project 3, an A/B test of a Shiny-based learning preference mini-quiz interface.

The project compares two interface versions of the same 10-item quiz:

- **Version A (Single-page interface):** all quiz items appear on one page
- **Version B (Wizard-style interface):** users complete the same quiz step by step

The quiz content, scoring logic, and result generation are identical across conditions. The only treatment difference is the interface structure.

The goal of the experiment is to evaluate whether interface design affects:

- **completion**
- **speed**
- **progression**
- **interaction friction**

## Research Question

Does a wizard-style multi-step interface improve the success, efficiency, or smoothness of completing a short learning-preference quiz compared with a single-page interface when quiz content and scoring rules are held constant across conditions?

## Main Outcomes

- **Primary outcome:** Time to Successful Completion
- **Secondary outcomes:** Completion Rate, Abandonment Rate, Progress Depth, Revision Rate, and Friction-Event Rate
- **Supportive outcome:** Time Among Completers

## Repository Structure

```text
.
├── app.R
├── README.md
├── report/
│   └── project3_final_report.pdf
├── analysis/
│   └── figures/
└── logs/
    ├── session_events.csv
    └── sessions.csv
```

> **Note:** The `logs/` folder is generated automatically when the app is run locally.

## Requirements

This project is written in **R** and uses **Shiny**.

Install the required packages before running the app:

```r
install.packages(c("shiny", "shinyjs"))
```

For the analysis scripts, you may also need:

```r
install.packages(c("readr", "dplyr", "ggplot2", "survival", "survminer"))
```

## How to Run the App

### 1. Clone or download the repository

Clone this repository or download it as a ZIP file.

### 2. Open the project in RStudio

Open the repository folder in RStudio and make sure `app.R` is in the working directory.

You can verify this with:

```r
getwd()
list.files()
```

### 3. Optional: set a Google Analytics 4 Measurement ID

The app supports GA4 event tracking. To use your own GA4 property, set:

```r
Sys.setenv(GA_MEASUREMENT_ID = "G-XXXXXXXXXX")
```

If this is not set, the app will use the default measurement ID specified in `app.R`.

### 4. Run the app

```r
library(shiny)
runApp("app.R")
```

You can also click **Run App** in RStudio.

## Testing A/B Conditions

By default, the app randomly assigns each session to one of two conditions:

- `A` = single-page interface
- `B` = wizard-style interface

For testing, you can force a specific condition using the URL query string:

- `?group=A`
- `?group=B`

## Logging and Output Files

When the app is run locally, it automatically creates a `logs/` folder and writes two CSV files.

### `logs/session_events.csv`

This file stores **event-level logs**.

Examples include:

- A/B assignment
- quiz start
- question answered
- answer changed
- validation errors
- quiz completion
- result page view
- post-task rating submission

### `logs/sessions.csv`

This file stores **session-level summary data** and is the main file used for formal analysis.

Typical columns include:

- `group`
- `condition`
- `time_to_event_sec`
- `event_observed`
- `completion_status`
- `progress_depth`
- `revision_rate_per_answered_item`
- `friction_event_rate_per_answered_item`
- `time_among_completers_sec`

## How to Generate Local Data

1. Run the app locally.
2. Complete at least one full session, or begin a session and then end it.
3. Check the `logs/` folder for generated CSV files.

Example:

```r
list.files("logs")
dat <- read.csv("logs/sessions.csv")
head(dat)
```

## How to Reproduce the Analysis

### Step 1. Run the app and generate logs

Run `app.R` and collect session-level data in `logs/sessions.csv`.

### Step 2. Run the analysis script

Run the analysis script from the `analysis/` folder, for example:

```r
source("analysis/analysis.R")
```

This script should reproduce the main tables, figures, and statistical outputs reported in the final paper.

### Step 3. Compare outputs with the final report

The final report includes:

- experimental design
- metric definitions
- data collection logic
- statistical analysis plan
- descriptive results
- inferential results
- discussion and limitations

## Example: Read the Session-Level Data

```r
library(readr)
library(dplyr)

sessions <- read_csv("logs/sessions.csv", show_col_types = FALSE)

sessions %>%
  count(group, completion_status)

sessions %>%
  group_by(group) %>%
  summarise(
    avg_time = mean(time_to_event_sec, na.rm = TRUE),
    avg_progress = mean(progress_depth, na.rm = TRUE),
    avg_revision_rate = mean(revision_rate_per_answered_item, na.rm = TRUE),
    avg_friction_rate = mean(friction_event_rate_per_answered_item, na.rm = TRUE)
  )
```

## Statistical Analysis Summary

The final analysis uses methods matched to the structure of each outcome:

- **Primary analysis**
  - Kaplan–Meier curves
  - log-rank test
  - Cox proportional hazards model

- **Secondary and supportive analyses**
  - two-sample test for proportions
  - Wilcoxon rank-sum tests

The session is treated as the unit of analysis throughout.

## Deployed Application

The deployed Shiny app is available at:

[https://shielawu.shinyapps.io/learning-style-ab/](https://shielawu.shinyapps.io/learning-style-ab/)

## Final Report

The final report documents the full study design, data collection process, metric logic, statistical analysis plan, results, and interpretation.

## Notes

- This project evaluates **interface behavior**, not the scientific validity of learning-style theory.
- The `logs/` folder is intended for local reproducibility.
- If you change file names or folder structure, update this README accordingly.
- For full reproducibility, include both the app code and the analysis scripts in this repository.

## Contact

For questions about the project, please contact the project team through the course GitHub repository.

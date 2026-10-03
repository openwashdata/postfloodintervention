# CLAUDE.md

`postfloodintervention` is an openwashdata R data package with post-intervention monitoring data for rural water points in Mulanje District, Malawi, from the USAID Flood Response program (2019-2020).

## Package facts

- Raw data: the processing script reads `data-raw/USAID Flood Response - Post Intervention Survey.csv`, which is not in the repo.
- Processing script: `data-raw/data_processing.R`. It writes `data/postfloodintervention.rda` and the CSV and XLSX exports in `inst/extdata/`. `data-raw/postfloodintervention.R` is only the `usethis` stub.
- Data dictionary: `data-raw/dictionary.csv`.
- Branches: work and review PRs go to `dev`; `main` holds released versions.

## Reviews and releases

Reviews and releases follow the installed pkgreview skills. `/review-package` starts a review, `/review-issue` works through one review issue, `/create-release` makes a release and `/add-doi` adds the Zenodo DOI. The skills hold the steps and the current standards, so this file does not repeat them.

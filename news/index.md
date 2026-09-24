# Changelog

## postfloodintervention 0.1.2

- Metadata-only patch after the v0.1.1 Zenodo record, which was cut
  while DESCRIPTION still said 0.1.0. CITATION.cff and inst/CITATION
  carry the Zenodo concept DOI and declare the work as a dataset;
  DESCRIPTION gains keywords and the spatial and temporal coverage; the
  pkgdown site is configured per the openwashdata standard and deployed
  from gh-pages; R CMD check runs in CI. The data are unchanged.

## postfloodintervention 0.1.1

- Fix release on GitHub and first Zenodo record (2025-07-08).

## postfloodintervention 0.1.0 (2025-07-08)

- Initial release to GitHub
- Added core data processing pipeline
  ([\#3](https://github.com/openwashdata/postfloodintervention/issues/3))
- Enhanced documentation with proper README and function documentation
  ([\#4](https://github.com/openwashdata/postfloodintervention/issues/4))
- Added CI/CD workflows with R CMD check
  ([\#5](https://github.com/openwashdata/postfloodintervention/issues/5))
- Fixed data quality issues including removal of unusually low pH values
  ([\#5](https://github.com/openwashdata/postfloodintervention/issues/5))
- Implemented standard openwashdata package structure
- Added CSV and XLSX export formats in inst/extdata/

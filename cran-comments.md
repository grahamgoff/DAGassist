## Test environments
* local (macOS, R 4.x): R CMD check --as-cran
* win-builder (R-devel, Windows, 2025-11-09 r88992 ucrt): OK with 1 NOTE

## R CMD check results
❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Changes
* Updated Authors@R to reflect current authors
* Package changes listed in NEWS.md

## Notes
* The word “estimands” in DESCRIPTION is a causal inference term and is used intentionally.
* The previous submission (0.2.6) was withdrawn at my request after CRAN reported the 404; this version contains the corrected links.
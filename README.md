[![Build Status](https://travis-ci.org/TGuillerme/Inapp.svg?branch=master)](https://travis-ci.org/TGuillerme/Inapp)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/TGuillerme/Inapp/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/Inapp)
[![DOI](https://zenodo.org/badge/84838115.svg)](https://zenodo.org/badge/latestdoi/84838115)

**Inapp**: ancestral reconstruction and step counting for discrete characters with inapplicable data.

<a href="https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222). 

This package allows to run the algorithm described in [this paper](https://academic.oup.com/sysbio/advance-article/doi/10.1093/sysbio/syy083/5238046).
You can either directly run the package on your web browser (Graphical User Interface mode!), or install it in `R` as a proper package (or both!).

The `C` implementation of the algorithm is available in [morphylib](https://github.com/mbrazeau/morphylib) and the algorithm is implemented in the [TreeSearch](https://github.com/ms609/TreeSearch) `R` package.

## Running Inapp on your web browser
That's easy! In `R`, simply copy/paste the following:
```r
if(!require(devtools)) install.packages("devtools")
if(!require(shiny)) install.packages("shiny")
runGitHub("Inapp", "TGuillerme")
```
<!-- Upload the whole thing on shiny servers -->

## Running Inapp in your R console
Not much more complicated! In `R`, copy/paste the following:
```r
## Installing the App:
if(!require(devtools)) install.packages("devtools")
devtools::install_github("TGuillerme/Inapp")

## Running the App:
Inapp::runInapp()
```

Check out the package [vignette](https://tguillerme.github.io/inapp.html) for (many) more details on the GUI possibilities.


Authors
-------
[Thomas Guillerme](http://tguillerme.github.io), [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau), [Martin Smith](https://community.dur.ac.uk/martin.smith/)


Citations
-------
If you are using this package, please cite:

* Brazeau MD, Guillerme T, Smith MR. (**2018**) An algorithm for morphological phylogenetic analysis with inapplicable data. *Systematic Biology*. [doi:10.1093/sysbio/syy083](https://academic.oup.com/sysbio/advance-article/doi/10.1093/sysbio/syy083/5238046)
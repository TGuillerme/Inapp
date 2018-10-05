[![Build Status](https://travis-ci.org/TGuillerme/Inapp.svg?branch=master)](https://travis-ci.org/TGuillerme/Inapp)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/TGuillerme/Inapp/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/Inapp)
[![DOI](https://zenodo.org/badge/84838115.svg)](https://zenodo.org/badge/latestdoi/84838115)

**Inapp**: ancestral reconstruction and step counting for discrete characters with inapplicable data.

<a href="https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/Guillerme_Evolution2017_pdf/5140222). 

This package allows to run the algorithm described in [this preprint](https://www.biorxiv.org/content/early/2017/10/26/209775) (stay tuned for the accepted version in Systematic Biology comming soon!).
You can either directly run the package on your web browser (Graphical User Interface mode!), or install it in `R` as a proper package (or both!).

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

Check out the package [vignette](https://cdn.rawgit.com/TGuillerme/Inapp/2d7de88b/inst/gitbook/_book/index.html) for (many) more details on the GUI possibilities.


Authors
-------
[Thomas Guillerme](http://tguillerme.github.io), [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau), [Martin Smith](https://community.dur.ac.uk/martin.smith/)

<!-- Citation
-------
To cite the `Inapp` package, please use:
[![DOI](https://zenodo.org/badge/84838115.svg)](https://zenodo.org/badge/latestdoi/84838115)
-->



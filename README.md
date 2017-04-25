<!-- [![Build Status](https://travis-ci.org/TGuillerme/dispRity.svg?branch=release)](https://travis-ci.org/TGuillerme/dispRity) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.55646.svg)](https://doi.org/10.5281/zenodo.55646) -->

**Inapp**: ancestral reconstruction and step counting for discrete characters with inapplicable data.

<!-- 
<a href="https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546"><img src="http://tguillerme.github.io/images/logo-FS.png" height="15" widht="15"/></a> 
Check out the [presentation](https://figshare.com/articles/New_approaches_to_disparity-through-time_analysis/3437546) or the [video](https://www.youtube.com/watch?v=ZzipKw8W8KQ) of some of the package's novel features.
 -->

This package allows to run the algorithm described in @@@.
You can either directly run the package on your web browser (Graphical User Interface mode!), or install it in `R` as a proper package (or both!).

## Running Inapp on your web browser
That's easy! In `R`, simply copy/paste the following:
```r
if(!require(devtools)) install.packages("devtools")
if(!require(shiny)) install.packages("shiny")
library(devtools); library(shiny)
runGitHub("Inapp", "TGuillerme")
```
<!-- Upload the whole thing on shiny servers -->

## Running Inapp in your `R` console
Not much more complicated! In `R`, copy/paste the following:
```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/Inapp")
library(Inapp)
```

Authors
-------
[Thomas Guillerme](http://tguillerme.github.io), [Martin Brazeau](http://www.imperial.ac.uk/people/m.brazeau), [Martin Smith](https://community.dur.ac.uk/martin.smith/)

Citation
-------
<!-- 

* Guillerme, T. (**2016**). dispRity: a package for measuring disparity in R. Zenodo. 10.5281/zenodo.55646

 [BibTeX](https://zenodo.org/record/55646/export/hx), [EndNote](https://zenodo.org/record/55646/export/xe), [DataCite](https://zenodo.org/record/55646/export/dcite3), [RefWorks](https://zenodo.org/record/55646/export/xw)
 -->


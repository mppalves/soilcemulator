# Soil carbon pasture emulator creator

R package **soilcemulator**, version **2.4.0**

[![CRAN status](https://www.r-pkg.org/badges/version/soilcemulator)](https://cran.r-project.org/package=soilcemulator)   [![R build status](https://github.com/mppalves/mrscpmule/workflows/check/badge.svg)](https://github.com/mppalves/mrscpmule/actions) [![codecov](https://codecov.io/gh/mppalves/mrscpmule/branch/master/graph/badge.svg)](https://codecov.io/gh/mppalves/mrscpmule)

## Purpose and Functionality

Provides functions for the creation of a Machine learning emulator based on magpie preprocessing files.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("soilcemulator")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Alves Marcos <pedrosa@pik-potsdam.de>.

## Citation

To cite package **soilcemulator** in publications use:

Marcos A (2021). _soilcemulator: Soil carbon pasture emulator creator_. https://github.com/mppalves/mrscpmule,.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {soilcemulator: Soil carbon pasture emulator creator},
  author = {Alves Marcos},
  year = {2021},
  note = {https://github.com/mppalves/mrscpmule,},
}
```


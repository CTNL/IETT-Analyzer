# IETT Analyzer
Analyze trajectory data (GPS) and get bus route information in Istanbul, Turkey.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("CTNL/IETTAnalyzer")
```

Usage
-----

``` r
library(IETTAnalyzer)
```
Istanbul bus routes and bus stops can be fetched from iett.istanbul or trafi.com websites.
``` r
route.iett <- get_bus_route("43R")
route.trafi <- get_bus_route("43R", from="trafi")
head(route.trafi$stops.going)
```

The bus route and its bus stops can be plotted together.
``` r
plot_bus_route(route.trafi)
```

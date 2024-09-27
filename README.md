# CLIGEN Version 4 

**More recent versions of CLIGEN are found in the CLIGEN 5 repository** (https://github.com/jfrankenberger/cligen5)

Cligen (ClImate GENerator) is a stochastic weather generator which produces daily estimates of precipitation, temperature, dewpoint, wind, and solar radiation for a single geographic point, using monthly parameters (means, SD's, skewness, etc.) derived from the historic measurements. 
Unlike other climate generators, it produces individual storm parameter estimates, including time to peak, peak intensity, and storm duration, which are required to run the WEPP and the WEPS soil erosion models. 

For more information on CLIGEN see the USDA webpage at: (https://www.ars.usda.gov/midwest-area/west-lafayette-in/national-soil-erosion-research/docs/wepp/cligen/)

The orginal version was written in the 1990's by Arlin Nicks.

This repository has the version 4 releases:
- Version 4.2 - From 1997
- Version 4.3 - From 1998, allowed runs to be made using a single station file
- Version 4.2-recoded - From 1999, major restructng of the code by Chuck Meyer

For newer versions of CLIGEN see the version 5 repository. The major difference in the version 5 releases is the additon of quality control on the output values to test that the generated values are matching the statistics of the input parameter file.

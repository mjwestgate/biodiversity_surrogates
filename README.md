This repo contains the code to run a shiny app allowing users to interactively view results from the following paper:

MJ Westgate, AIT Tulloch, PS Barton, JC Pierson & DB Lindenmayer (2016) Optimal taxonomic groups for biodiversity assessment: A meta-analytic approach. Ecography - in press. doi: 10.1111/ecog.02318 
    
```
library(devtools)
install_github('mjwestgate/circleplot') # install required package
library(shiny)
runGitHub( "surrogates_metaAnalysis", "mjwestgate")  # run

```

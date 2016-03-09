This repo contains the code to run a shiny app allowing users to interactively view results from the following paper:

    MJ Westgate, AIT Tulloch, PS Barton, JC Pierson & DB Lindenmayer (2016) Optimal taxonomic groups for biodiversity assessment: A meta-analytic approach. Ecography in press. doi: 10.1111/ecog.02318 
    
```
# Load required packages
library(devtools)
install_github('mjwestgate/circleplot')
library(shiny)

# run
runGitHub( "surrogates_metaAnalysis", "mjwestgate") 

```

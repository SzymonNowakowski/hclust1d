# Local checks

## local R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
There seem to be only one reverse dependence (DMRnet). It has the same maintainer, who keeps the changes synchronized.

```{r revdep}
devtools::revdep("DMRnet")
# [1] "DMRnet"         <-------------- this package has the same maintainer
```

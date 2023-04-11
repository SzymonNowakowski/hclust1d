
# This is a 2nd resubmission of 0.0.1

- The following NOTEs from precheck **has been corrected**:

   1. Package has a VignetteBuilder field but no prebuilt vignette index.

   2. The Title field should be in title case. Current version is:
   'Hierarchical Clustering of Univariate (1D) Data'
   In title case that is:
   'Hierarchical Clustering of Univariate (1d) Data'

   3. The Description field should not start with the package name,
     'This package' or similar.
     
- The following NOTEs from manual precheck **has been corrected/answered**:
    
   1. Please always write package names, software names and API (application
    programming interface) names in single quotes in title and description.
    e.g: --> 'C++'
    Please note that package names are case sensitive.
    
  **Corrected**

  2. If there are references describing the methods in your package, please
    add these in the description field of your DESCRIPTION file in the form
    authors (year) <doi:...>
    authors (year) <arXiv:...>
    authors (year, ISBN:...)
    or if those are not available: <https:...>
    with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
    auto-linking.
    (If you want to add a title as well please put it in quotes: "Title")

  **No, at that point I don't want to add references. I plan to add references in the future**

# Local checks

## local R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

## Downstream dependencies
There seem to be no downstream dependencies:

```{r revdep}
devtools::revdep("hclust1d")
# character(0)
```

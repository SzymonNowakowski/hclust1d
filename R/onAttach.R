.onAttach=function(libname, pkgname){
  packageStartupMessage("Loaded hclust1d version ", as.character(utils::packageDescription("hclust1d")[["Version"]]), "\n")


  hclust1d_citation <- citation("hclust1d")
  packageStartupMessage(hclust1d_citation$header)
  packageStartupMessage(hclust1d_citation$textVersion)
}

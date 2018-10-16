LatexOrOther <- function(latex, other){
  if (identical(knitr:::pandoc_to(), 'latex')) 
    latex 
  else other
}

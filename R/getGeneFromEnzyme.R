getGeneFromEnzyme <-
function(enzymeList,ignoreAmbiguousEnzyme=TRUE){
      return(getGeneFromKGene(getKGeneFromEnzyme(enzymeList,ignoreAmbiguousEnzyme=ignoreAmbiguousEnzyme)))
}

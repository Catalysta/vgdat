
#' getENS_id
#'
#' This function helps clean up ENS Id's with unnecessary suffixes in them
#'
#' @param name a vector containing the list of ENS Id's the user wishes to be cleaned
#' @return a vector of searchable ENS Id's
#' @export

getENS_id<-function(name){
  result<-numeric(length(name))
  for (i in 1:length(name)){
    tmp<-as.character(name[i])
    result[i]<-unlist(strsplit(tmp,"\\."))[1]
  }
  return(result)
}


#' getVg
#'
#' Function looks up the variance estimates from VG_GTEx_v7 data for a vector of ENS IDs
#'
#' @param Vg a data frame containing the population variance estimates (default is
#' VG_GTEx_v7.rda.
#' @param colname a string for the name of the column in the Vg file containing ENS
#' IDs (these will be used to look up the desired variance estimate).
#' @param tissue the column name in Vg from which the variance estimates should be
#' obtained. Here we default to MSCLSK.
#' @param ENS_id vector containing user's list of ENS IDs for which variance estimates
#' are desired.
#' @return a dataframe containing the user inputted ENS_id column and the variance
#' estimates for those ENS IDs.
#'

getVg<-function(Vg, colname="IDs", tissue="MSCLSK",ENS_id){
  result<-numeric(length(ENS_id))
  indices<-match(ENS_id,Vg[,colname])
  #Vg[indices,"MSCLSK"]
  for (i in 1:length(ENS_id)){
    result[i]<-Vg[indices[i],"MSCLSK"]
  }
  result<-c(ENS_id,result)
  return(result)
}

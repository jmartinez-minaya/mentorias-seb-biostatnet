#' Assign probabilities based on the preferences of the mentxr
#' 
#' `preferences_mentxr` Main function to assign probabilities based on the preferences of the mentxr
#' @param info_mentxr a vector containing four elements: email, nodo, sexo_mentorandx and cat_mentorandx.
#' @param data_mentorandx data with all the information of the mentorandxs
#' @return a probabilities vector for each mentorandx of the dataset, based on the preferences of the mentxr
#'
#' @example 
#' \dontrun{
#' data_mentxr_info <- data_mentxr %>%
#'    dplyr::select(email, nodo, sexo_mentorandx, cat_mentorandx)
#' 
#' 
#' result_mentxr <- lapply(1:dim(data_mentxr)[1], 
#'                         function(x){
#'                           preferences_mentxr(data_mentxr_info[x,],
#'                                              data_mentorandx = data_mentorandx)})
#' names(result_mentxr) <- data_mentxr_info %>% dplyr::pull(email)
#' 
#' #Convirtiéndolo a matriz
#' matrix_mentxr <- plyr::join_all(result_mentxr, by='email', type='left')
#' nom_mentorandxs <- matrix_mentxr[,1]
#' nom_mentxrs <- names(result_mentxr)
#' matrix_mentxr <- matrix_mentxr[,-1]
#' rownames(matrix_mentxr) <- nom_mentorandxs
#' colnames(matrix_mentxr) <- nom_mentxrs
#' }
#' @export
#' @import dplyr tidyr
#' @author Joaquín Martínez-Minaya <\email{jomartinez@@bcamath.org}>
preferences_mentxr <- function(info_mentxr, data_mentorandx = data_mentorandx, p = c(0.1, 0.3, 0.6))
{
  email1 <- info_mentxr[1] %>% as.character()
  nodo1 <- info_mentxr[2] %>% as.character()
  sexo_mentorandx1 <- info_mentxr[3] %>% as.character()
  cat_mentorandx1 <- info_mentxr %>% dplyr::pull(cat_mentorandx) %>% as.character()

  cat("------------------------------------------------- \n")
  cat(paste0("----> email: ", email1, " \n"))
  # Chequeamos si esa persona está también en la base de datos de los mentores
  probs <- numeric(dim(data_mentorandx)[1])
  data_mentorandx1 <- data_mentorandx
  if(any(data_mentorandx1$email == email1))
  {
    data_mentorandx1 <- data_mentorandx1 %>%
      dplyr::filter(email != email1)
    pos_email <- data_mentorandx$email != email1
    probs[pos_email] <- probs[pos_email] + p[1]
  }else{
    pos_email <- rep(TRUE, length(probs))
    probs <- probs + p[1]
  }
  
  #Nodo: chequeamos que el mentor sea de diferente nodo que el mentorando
  cat(paste0("----> Nodo: ", nodo1, " \n"))
  data_mentorandx1 <- data_mentorandx1 %>%
    dplyr::filter(nodo != nodo1)
  pos_nodo <- data_mentorandx$nodo != nodo1
  probs[pos_nodo & pos_email] <- probs[pos_nodo & pos_email] + p[2]
  
  
  #Sexo
  cat(paste0("----> Sexo: ", sexo_mentorandx1, " \n"))
  #Chequeamos si el mentorando ha elegido mentor o mentora
  if(sexo_mentorandx1 == "mujer"){
    #Si ha elegido mentora, nos quedamos con las mujeres
    data_mentorandx1 %>%
      dplyr::filter(sexo == "mujer") -> data_mentorandx1
    pos_sexo <- data_mentorandx$sexo == "mujer"
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
  }else if(sexo_mentorandx1 == "hombre"){
    data_mentorandx1 %>%
      dplyr::filter(sexo == "hombre") -> data_mentorandx1
    pos_sexo <- data_mentorandx$sexo == "hombre"
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
    
  }else{
    data_mentorandx1 <- data_mentorandx1
    pos_sexo <- rep(TRUE, length(probs))
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
  }
  
  
  #probs
  result <- data_mentorandx[,c("email", "nodo", "sexo")] %>%
    cbind(., probs)
  
  pos_order <- order(result$probs, decreasing = TRUE)
  result[pos_order,]
  
  print(result[pos_order,][1:5,])
  cat("------------------------------------------------- \n")
  
  #list(result[pos_order,])
  result[,c("email", "probs")]
}







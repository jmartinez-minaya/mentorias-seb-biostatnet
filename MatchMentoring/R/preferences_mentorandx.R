#' Assign probabilities based on the preferences of the mentorandx
#' 
#' `preferences_mentorandx` Main function to assign probabilities based on the preferences of the mentorandx
#' @param info_mentorando a vector containing four elements: email, nodo, sexo_mentor1, exp_mentor1.
#' @param data_mentxr data with all the information of the mentores
#' @param p vector of probabilities for each mentorandxs info
#' @return a probabilities vector for each mentxr of the dataset, based on the preferences of the mentorandx
#'
#' @example 
#' \dontrun{
#' data_mentorandx_info <- data_mentorandx %>%
#'   dplyr::select(email, nodo, sexo_mentor, exp_mentor, ambito_mentor)
#' 
#' result_mentorandx <- lapply(1:dim(data_mentorandx)[1], 
#'                             function(x){
#'                               preferences_mentorandx(data_mentorandx_info[x,],
#'                                                      data_mentxr = data_mentxr)})
#' names(result_mentorandx) <- data_mentorandx_info %>% dplyr::pull(email)
#' 
#' #Convirtiéndolo a matriz
#' matrix_mentorandx <- plyr::join_all(result_mentorandx, by='email', type='left')
#' nom_mentores <- matrix_mentorandx[,1]
#' nom_mentorandxs <- names(result_mentorandx)
#' matrix_mentorandx <- matrix_mentorandx[,-1]
#' rownames(matrix_mentorandx) <- nom_mentores
#' colnames(matrix_mentorandx) <- nom_mentorandxs
#' }
#' @export
#' @import dplyr tidyr
#' @author Joaquín Martínez-Minaya <\email{jomartinez@@bcamath.org}>
preferences_mentorandx <- function(info_mentorando, data_mentxr = data_mentxr, p = c(0.1, 0.25, 0.5, 0.15))
{
  email1 <- info_mentorando[1] %>% as.character()
  nodo1 <- info_mentorando[2] %>% as.character()
  sexo_mentor1 <- info_mentorando[3] %>% as.character()
  exp_mentor1 <- info_mentorando %>% dplyr::pull(exp_mentor) %>% as.character()
  #ambito_mentor1 <- info_mentorando[5] %>% as.character()
  
  cat("------------------------------------------------- \n")
  cat(paste0("----> email: ", email1, " \n"))
  # Chequeamos si esa persona está también en la base de datos de los mentores
  probs <- numeric(dim(data_mentxr)[1])
  data_mentxr1 <- data_mentxr
  if(any(data_mentxr1$email == email1))
  {
    data_mentxr1 <- data_mentxr1 %>%
            dplyr::filter(email != email1)
    pos_email <- data_mentxr$email != email1
    probs[pos_email] <- probs[pos_email] + p[1]
  }else{
    pos_email <- rep(TRUE, length(probs))
    probs <- probs + p[1]
  }
  
  #Nodo: chequeamos que el mentor sea de diferente nodo que el mentorando
  cat(paste0("----> Nodo: ", nodo1, " \n"))
  data_mentxr1 <- data_mentxr1 %>%
        dplyr::filter(nodo != nodo1)
  pos_nodo <- data_mentxr$nodo != nodo1
  probs[pos_nodo & pos_email] <- probs[pos_nodo & pos_email] + p[2]
  

  #Sexo
  cat(paste0("----> Sexo: ", sexo_mentor1, " \n"))
  #Chequeamos si el mentorando ha elegido mentor o mentora
  if(sexo_mentor1 == "mujer"){
    #Si ha elegido mentora, nos quedamos con las mujeres
    data_mentxr1 %>%
        dplyr::filter(sexo == "mujer") -> data_mentxr1
    pos_sexo <- data_mentxr$sexo == "mujer"
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
  }else if(sexo_mentor1 == "hombre"){
    data_mentxr1 %>%
      dplyr::filter(sexo == "hombre") -> data_mentxr1
    pos_sexo <- data_mentxr$sexo == "hombre"
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
    
  }else{
    data_mentxr1 <- data_mentxr1
    pos_sexo <- rep(TRUE, length(probs))
    probs[pos_nodo & pos_email & pos_sexo] <- probs[pos_nodo & pos_email & pos_sexo] + p[3]
  }
  
  
  #Años experiencia
  cat(paste0("----> Experiencia: ", exp_mentor1, " \n"))

  if(exp_mentor1 == "<10")
  {
    #Si ha elegido menor o igual que 5, buscamos a alguien que tenga esa experiencia.
    data_mentxr1 %>%
      dplyr::filter((anyo_doctor == "<10")) -> data_mentxr1

    pos_exp <- data_mentxr$anyo_doctor == "<10"
    probs[pos_nodo & pos_email & pos_sexo & pos_exp] <- probs[pos_nodo & pos_email & pos_sexo & pos_exp] + p[4]
    
    #Si ha elegido menor o igual que 10, buscamos a alguien que tenga esa experiencia.
  }else if(exp_mentor1 == ">=10")
  {
    data_mentxr1 %>%
      dplyr::filter((anyo_doctor == ">=10")) -> data_mentxr1
    
    pos_exp <- data_mentxr$anyo_doctor == ">=10"
    probs[pos_nodo & pos_email & pos_sexo & pos_exp] <- probs[pos_nodo & pos_email & pos_sexo & pos_exp] + p[4]
    
  }else{
    data_mentxr1 <- data_mentxr1
    pos_exp <- rep(TRUE, length(probs))
    probs[pos_nodo & pos_email & pos_sexo & pos_exp] <- probs[pos_nodo & pos_email & pos_sexo & pos_exp] + p[4]
  }
  
  #probs
  result <- data_mentxr[,c("email", "nodo", "sexo", "anyo_doctor")] %>%
    cbind(., probs)
  
  pos_order <- order(result$probs, decreasing = TRUE)
  result[pos_order,]
  
  print(result[pos_order,][1:5,])
  cat("------------------------------------------------- \n")
  
  #list(result[pos_order,])
  result[,c("email", "probs")]
}




  
  
  
  
  
  



### --- 1. Loading libraries --- ####
library(readxl)
library(dplyr)
library(matchingR)

## Creado por nosotros
library(MatchMentoring)

### --- 2. Reading and cleaning the data --- ####

### ----- 2.1. Reading the data --- ####
data <- readxl::read_excel("data/raw/Inscritos_segunda_edicion_21-11-2022.xlsx",
                           sheet = "Respuestas de formulario 1")
colnames(data)

dim(data)
### ----- 2.2. Cleaning the data --- ####
colnames(data) <- c("fecha", #ambos
                    "nombre", #ambos
                    "DNI", #ambos
                    "email", #ambos
                    "tlf", #ambos
                    #"sexo", #ambos
                    "edad", #ambos
                    "profesion", #ambos
                    "nodo", #ambos
                    "areas", #ambos
                    "mentxr", #ambos
                    "anyo_doctor", #mentor
                    "numero_mentorandx", #mentor
                    "sexo_mentorandx", #mentor
                    "cat_mentorandx", #mentor
                    "sexo_mentor", #mentorando
                    "exp_mentor", #mentorando
                    "ambito_mentor", #mentorando
                    "difusion") #ambos

#Añadimos la columna sexo... No la hemos preguntado este año...
data$nombre
data$sexo <- rep("mujer", dim(data)[1])
data$sexo[c(1,3,4,5,8, 10, 12, 13, 15, 18, 19, 22)] <- "hombre"

data %>%
  dplyr::select(nombre, sexo) %>%
  as.data.frame(.)

#Dicotomizar la variable exp_mentor
data$exp_mentor
data$exp_mentor <- factor(data$exp_mentor,
                          levels = c("menos de 10 años de experiencia desde que ha obtenido el título de Doctor/a",   
                                     "me es indiferente"  ,                                                         
                                     "más de 10 años de experiencia desde que ha obtenido el título de Doctor/a"))
levels(data$exp_mentor) <- c("<10", "me es indiferente", ">=10")

### ----- 2.3. Split the data in two datasets --- ####
data_mentxr <- data %>%
  dplyr::filter(mentxr == "Mentor/a") %>%
  dplyr::select(-c(sexo_mentor, exp_mentor, 
                   ambito_mentor))

data_mentorandx <- data %>%
  dplyr::filter(mentxr == "Mentorando/a") %>%
  dplyr::select(-c(anyo_doctor, numero_mentorandx, 
                   sexo_mentorandx, cat_mentorandx))

data_mentxr %>%
  select(nombre)


data_mentorandx %>%
  select(nombre)

# Cálculo de los años desde que se sacó el doctorado el mentor
a <- 2022 -  data_mentxr$anyo_doctor %>% as.numeric()
data_mentxr[which(a == 0),]
data_mentxr[is.na(a),]
b <- character(length(a))
b[is.na(a)] <- "<10" #Asumimos que si no tiene doctorado es menor de 10
b[a < 10] <- "<10"
b[a >= 10] <- ">=10"
data_mentxr$anyo_doctor <- as.factor(b)


### ----- 2.4. Checking for duplicates --- ####

#In duplicates  it select the last one of the duplicates
#We just revert the data.frame and remove the first one in duplicates
data_mentxr <- data_mentxr[rev(1:dim(data_mentxr)[1]),]
data_mentorandx <- data_mentorandx[rev(1:dim(data_mentorandx)[1]),]

data_mentxr %>%
  dplyr::select(nombre) %>%
  duplicated(.) %>%
  !. -> id_mentxr

data_mentorandx %>%
  dplyr::select(nombre) %>%
  duplicated(.) %>%
  !. -> id_mentorandx

data_mentxr <- data_mentxr[id_mentxr, ]
data_mentorandx <- data_mentorandx[id_mentorandx, ]


### ----- 2.5. Random sampling of both databases --- ####
set.seed(1000)
data_mentorandx <- data_mentorandx[sample(1:dim(data_mentorandx)[1]),]

set.seed(1000)
data_mentxr <- data_mentxr[sample(1:dim(data_mentxr)[1]),]


### --- 3. Preferences for mentorandxs --- ####
data_mentorandx_info <- data_mentorandx %>%
  dplyr::select(email, nodo, sexo_mentor, exp_mentor, ambito_mentor)

result_mentorandx <- lapply(1:dim(data_mentorandx)[1], 
                            function(x){
                              preferences_mentorandx(data_mentorandx_info[x,],
                                                     data_mentxr = data_mentxr,
                                                     p = c(0.1, 0.25, 0.5, 0.15))})
names(result_mentorandx) <- data_mentorandx_info %>% dplyr::pull(email)

#Convirtiéndolo a matriz
matrix_mentorandx <- plyr::join_all(result_mentorandx, by='email', type='left')
nom_mentores <- matrix_mentorandx[,1]
nom_mentorandxs <- names(result_mentorandx)
matrix_mentorandx <- matrix_mentorandx[,-1]
rownames(matrix_mentorandx) <- nom_mentores
colnames(matrix_mentorandx) <- nom_mentorandxs



### --- 4. Preferences for mentxrs --- ####
data_mentxr_info <- data_mentxr %>%
  dplyr::select(email, nodo, sexo_mentorandx, cat_mentorandx)


result_mentxr <- lapply(1:dim(data_mentxr)[1], 
                        function(x){
                          preferences_mentxr(data_mentxr_info[x,],
                                             data_mentorandx = data_mentorandx,
                                             p = c(0.1, 0.3, 0.6))})
names(result_mentxr) <- data_mentxr_info %>% dplyr::pull(email)

#Convirtiéndolo a matriz
matrix_mentxr <- plyr::join_all(result_mentxr, by='email', type='left')
nom_mentorandxs <- matrix_mentxr[,1]
nom_mentxrs <- names(result_mentxr)
matrix_mentxr <- matrix_mentxr[,-1]
rownames(matrix_mentxr) <- nom_mentorandxs
colnames(matrix_mentxr) <- nom_mentxrs


### --- 5. Matching --- ####
results <- galeShapley.marriageMarket(matrix_mentorandx, matrix_mentxr)

galeShapley.checkStability(as.matrix(matrix_mentorandx), 
                           as.matrix(matrix_mentxr), 
                           results$proposals, results$engagements)
results$proposals[,1]
dim(matrix_mentorandx)

match_result <- data.frame(mentorandxs = rownames(matrix_mentxr), 
                           mentores    = rownames(matrix_mentorandx)[results$proposals[,1]],
                           probs_mentorandx = matrix_mentorandx[cbind(results$proposals[,1], 1:10)],
                           probs_mentxr = t(matrix_mentxr)[cbind(results$proposals[,1], 1:10)])

match_result2 <- match_result
mentorandxs_join <- data_mentorandx[,c("nombre", "email")]
colnames(mentorandxs_join) <- c("nombre", "mentorandxs")

mentores_join <- data_mentxr[,c("nombre", "email")]
colnames(mentores_join) <- c("nombre", "mentores")

match_result2 <- left_join(match_result2, 
                           mentorandxs_join,
                           by = "mentorandxs")
match_result2 <- left_join(match_result2,
                           mentores_join,
                           by = "mentores")

match_result2
#### Añadimos el nombre y el email
# data <- match_result2
# data <- data[, c("Mentorandx", "Mentor/a")] 
# colnames(data)[1] <- "nombre"
# 
# data <- data %>% 
#   dplyr::left_join(., data_mentorandx[, c("nombre", "email")], by = c("nombre"))
# data$email
# colnames(data)[1] <- c("Mentorandx")
# colnames(data)[3] <- c("email_mentorandx")
# colnames(data)[2] <- c("nombre")
# data <- data %>%
#   dplyr::left_join(., data_mentxr[, c("nombre", "email")], by = c("nombre"))
# colnames(data)[2] <- c("Mentor/a")
# colnames(data)[4] <- c("email_mentor")

data_mentxr$nombre[!(data_mentxr$nombre %in% match_result2$nombre.y)]
writexl::write_xlsx(match_result2, "matching_final.xlsx")

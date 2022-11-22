Mentorías SEB-BIOSTATNET: algoritmo utilizado para hacer el match entre mentores y mentorandxs
=======


# Introducción

Se quiere realizar un match entre lxs mentores y lxs mentorandxs. Para ello, utilizaremos lo que se conoce en la literatura como el [problema del matrimonio estable](https://dl.acm.org/doi/pdf/10.1145/362619.362631). El problema consiste en lo siguiente:

Dadas n personas de un grupo A, y n personas de un grupo B, donde cada persona del grupo A ha clasificado a todxs lxs miembrxs del grupo B en orden de preferencia, y viceversa, se trata de casar a lxs miembros del grupo A con lxs miembros del grupo B de modo que no haya dos personas de diferente grupo que prefieran tenerse el uno al otro más que a sus parejas actuales. Cuando no existen tales pares de personas, el conjunto de matrimonios se considera estable.

Una coincidencia no es estable si:
*Hay un elemento $A_x$ del conjunto A emparejado que prefiere algún elemento $B_x$ dado de B sobre el elemento con el que $A_x$ ya está emparejado, y $B_x$ también prefiere $A_x$ sobre el elemento con el que $B_x$ ya está emparejado. En otras palabras, una coincidencia es estable cuando no existe ninguna coincidencia ($A_x$, $B_x$) en la que ambos se prefieran entre sí a su pareja actual bajo la coincidencia.*

Dado que existen más mentorandxs que mentores, cuando se aplique el algoritmo, habrá una persona que quedará sin asignación. A esa persona se le ha asignado manualmente un mentor/a. Para realizar el match, nuestra implementación se ha basado en un paquete de R ya implementado [`matchingR`](https://cran.r-project.org/web/packages/matchingR/index.html).


# Aplicación al programa de mentorías
Para poder aplicar el algoritmo para la resolución del matrimonio estable en el contexto que lo queremos aplicar, se ha dividido el procedmiento en 4 bloques:

1. A cada mentorandx se le ha asignado un vector de probabilidades con respecto a cada mentor/a. Este vector indica las preferencias de cada mentorandx en base a las preguntas del formulario.

2. A cada mentor/a se le ha asignado un vector de probabilidad con respecto a cada mentorandx. Este vector indica las preferencias de cada mentor/a en base a las preguntas del formulario.

4. Dado que muchxs mentores serán asigandos con las mismas probabilidades, realizamos un muestreo aleatorio de la muestra de tal forma que el orden de inscripción no condicione el resultado final.

3. Por último, una vez se han construido la matriz de preferencias de lxs mentorandxs y la matriz de preferencias de lxs mentores, se realiza la asignación utilizando el algoritmo implementado en el paquete [`matchingR`](https://cran.r-project.org/web/packages/matchingR/index.html).



# Implementación detallada
A continuación mostramos cómo se ha realizado el match, así como su implementación en `R`. Para poder llevar a cabo todo el proceso, se han implementado funciones adicionales. Dichas funciones están en el paquete `MatchMentoring` disponible en [https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/](https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/).

Como primer paso, hemos de determinar una **lista de mentores** para cada mentorandx ordenados por prioridad. Esta lista estará basada en 4 criterios extraídos del formulario y que como comité pensamos que serían los más relevantes a la hora de realizar el match. Cada criterio ha sido cuantificado en base a la importancia que pensamos que podría tener. Así, se les ha asignando un peso. Este primer paso lo lleva a cabo la función [`preferences_mentorandx.R`](https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/MatchMentoring/R/preferences_mentorandx.R). Tanto los criterios como los pesos se describen a continuación:

1. **Email**: si el mentorandx está en la base de datos de lxs mentores, se le asignará un peso 0. Mientras que si no está, se le asignará un peso de *0.1*. Este paso tiene sentido por todas aquellas personas que se han apuntado como mentores y mentorandxs. De esta forma evitaríamos el match entre la misma persona.
        
2. **Nodos**: preferencia por alguien que esté fuera del nodo de procedencia. Se le asigna un peso de *0.25* si no pertenece al mismo nodo.
   
3. **Género**: preferencia por el género. Es uno de los factores más importantes. Se le asigna un peso de *0.5* cuando el mentor coincide con lo que busca el mentorandx.
   
4. **Años de experiencia**: preferencia por los años de experiencia. Se le asigna un peso de *0.15* cuando el/la mentor/a coincide con lo que busca el/la mentorandx. En el formulario se plantean tres posibles grupos: *0-10*, o *> 10* o *me es indiferente*. 

Procedemos de una forma similar con la base de datos de lxs mentores. Este segundo paso se ha programado en la función [`preferences_mentxr.R`](https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/MatchMentoring/R/preferences_mentxr.R). En este caso, tenemos en cuenta los siguientes criterios:

1. **Email**: de forma similar al caso anterior, asignamos un *0.1*.
        
2. **Nodos**: preferencia por alguien que esté fuera del nodo de procedencia. Se le asigna un peso de *0.3* si no pertenece al mismo nodo.
   
3. **Género**: preferencia por el género. Es uno de los factores más importantes. Se le asigna un peso de *0.6* cuando el mentor coincide con lo que busca el mentorandx.
   
Dado que muchos mentores serán asigandos con las mismas probabilidades, realizamos un muestreo aleatorio de la muestra de tal forma que el orden de inscripción no condicione el resultado final. Hasta aquí, obtendríamos dos matrices que representarían las preferencias de los mentorandxs cuantificadas usando probabilidades, y lo mismo para lxs mentores. Estas matrices las podemos ver si en el script [`1_matching.R`](https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/r/1_matching.R).

Por último, usamos la función `galeShapley.marriageMarket` del paquete `matchingR` para unir las dos bases de datos. Lo podemos ver en el script [`1_matching.R`](https://bitbucket.org/joaquin-martinez-minaya/mentorias-seb-biostatnet/src/master/r/1_matching.R).











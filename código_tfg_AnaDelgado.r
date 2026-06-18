# -------------------------------------------- #
# Trabajo de Fin de Grado - Ana Delgado García #
# ------------------ Código ------------------ #

#Instalación de paquetes necesarios.
install.packages(c("readr", "data.table", "tidyverse", "dplyr",
                    "httr", "jsonlite", "future.apply", "ggplot2",
                    "gridExtra", "corrplot", "caret", "pROC",
                    "glmnet", "randomForest", "xgboost", "shapviz",
                    "remotes", "dotenv"))
remotes::install_version("xgboost", version = "1.7.7.1")

#Lectura de paquetes
library(readr)
library(data.table)
library(tidyverse)
library(dplyr)
library(httr)
library(jsonlite)
library(future.apply)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(caret)
library(pROC)
library(glmnet)
library(randomForest)
library(remotes)
library(xgboost)
library(shapviz)
library(dotenv)
load_dot_env()

# ------------------- #
# Datos y preparación #
# ------------------- #

### Importación del primer dataset, title.basics.tsv.

#Contiene las siguientes columnas:
# tconst (string): identificador
# titleType (string): movie, short, tvseries, tvepisode, video..
# primaryTitle (string)
# originalTitle (string)
# isAdult (boolean)
# startYear (YYYY format)
# endYear (YYYY format)
# runtimeMinutes
# genres (string array): up to 3 genres associated with the title.

peliculas <- fread("IMDb\\title.basics.tsv", na.strings="\\N")
summary(peliculas)

peliculas$titleType <- as.factor(peliculas$titleType)
peliculas$isAdult <- as.factor(peliculas$isAdult)

peliculas <- peliculas[peliculas$titleType == "movie" & peliculas$isAdult == 0 & peliculas$startYear>=1980 & peliculas$startYear<=2025,]
summary(peliculas)

peliculas <- peliculas %>% select(tconst, primaryTitle, startYear, runtimeMinutes, genres) #Eliminamos las columnas titleType, originalTitle, isAdult y endYear.

dim(peliculas) # 451.665 películas, 5 variables


### Importación del segundo dataset, title.crew.tsv.

#Contiene las columnas:
# tconst (string): identificador
# directors (array of nconsts): directors of the given title
# writers (array of nconsts): writers of the given title

crew <- fread("IMDB\\title.crew.tsv", sep = "\t", na.strings ="\\N")

peliculas <- merge(peliculas, crew, by = "tconst", all.x = TRUE)

sum(is.na(peliculas$directors)) #21675 NA, un 4.7% del total
sum(is.na(peliculas$writers)) # 107.397 NA, un 23,78% del total

sum(grepl(",", peliculas$directors, fixed = TRUE), na.rm = TRUE) #Número de películas con más de un director. 44468 películas
sum(grepl(",", peliculas$writers, fixed = TRUE), na.rm = TRUE) #Número de películas con más de un escritor. 138.099 películas

table(peliculas[!is.na(directors), lengths(strsplit(directors, ","))]) #Cuántas películas tienen x directores. De 1, con 385522, hasta 88, con 1.
table(peliculas[!is.na(writers), lengths(strsplit(writers, ","))]) #Cuántas películas tienen x escritores. De 1, con 206169, hasta 69, con 1.

summary(peliculas)


### Importación del tercer dataset, title.principals.tsv

#Contiene las columnas:
# tconst (string): identificador de la película
# ordering (integer): a number to uniquely identify rows for a given titleId (orden de aparición en los créditos)
# nconst (string): identificador de la persona
# category (string): the category of job that person was in
# job (string): specific job title
# characters (string): the name of the character played if applicable

principals <- fread("IMDb\\title.principals.tsv", sep = "\t", na.strings = "\\N", quote ="",
                    select = c("tconst", "ordering", "nconst", "category"))[category %in% c("actor", "actress")]

principals <- principals[tconst %in% peliculas$tconst] #filtrar solo los tconsts de películas que estén ya en el dataset películas

#Nos quedamos con los 3 primeros actores dentro de cada película
principals[, actor_rank := rowid(tconst)]
principals <- principals[actor_rank <= 3]

head(principals)

#Pasamos a formato ancho
actors_wide <- dcast(principals, tconst ~ actor_rank, value.var = "nconst")

#Renombramos las nuevas columnas
setnames(actors_wide, c("1", "2", "3"), c("actor1", "actor2", "actor3"))

#Juntamos con el dataset películas
peliculas <- merge(peliculas, actors_wide, by = "tconst", all.x = TRUE)

#Comprobamos el número de NA en cada columna actor1, actor2 y actor3.
colMeans(is.na(peliculas[, .(actor1, actor2, actor3)]))
# 25.86% de NA en el actor 1, 31.43% en actor2 y 34.84% en actor3

table(rowSums(is.na(peliculas[, .(actor1, actor2, actor3)])))
#con 0, 294327, con 1 15364, con 2 25160 y con 3 116814 (este último significa que no tiene ningún actor registrado)

# Comprobamos cómo suelen ser las películas sin actores registrados
sin_actores <- peliculas[is.na(actor1)]

sin_actores[, .(tconst, primaryTitle, startYear, genres)]
#Tienden a ser documentales. Se tratarán post-ingesta de TMDb en función de si tienen o no las variables objetivo (recaudación y presupuesto)
rm(sin_actores)

### Importación del cuarto dataset, name.basics.tsv. 

#Contiene las siguientes columnas:
# nconst (string): identificador
# primaryName (string)
# birthYear (YYYY format)
# deathYear (YYYY format o NA)
# primaryProfession (array of strings): top 3 professions
# knownForTitles (array of tconsts): titles the person is known for.

actores <- fread("IMDb\\name.basics.tsv", na.strings ="\\N", select =c("nconst", "primaryName"))
head(actores)

#Para poder relacionar los nombres de director y escritor principales, necesitamos hacer modificaciones en estas dos variables.
#Registramos el número de directores y escritores en las variables num_directors y num_writers, respectivamente,
#y tomamos el primer director y escritor de cada película como el principal de la misma.

peliculas[, num_directors := ifelse(is.na(directors), NA, lengths(strsplit(directors, ",")))]
peliculas[, num_writers := ifelse(is.na(writers), NA, lengths(strsplit(writers, ",")))]

peliculas[, director := tstrsplit(directors, ",")[[1]]]
peliculas[, writer := tstrsplit(writers, ",")[[1]]]

peliculas[, directors := NULL]
peliculas[, writers := NULL]

#Realizamos 5 uniones consecutivas, una por cada variable que necesita identificación de nombres.

# Director. 
peliculas <- merge(peliculas, actores, by.x = "director", by.y = "nconst", all.x = TRUE)
setnames(peliculas, "primaryName", "directorName")

#Escritor.
peliculas <- merge(peliculas, actores, by.x = "writer", by.y = "nconst", all.x = TRUE)
setnames(peliculas, "primaryName", "writerName")

# Actor 1.
peliculas <- merge(peliculas, actores, by.x = "actor1", by.y = "nconst", all.x = TRUE)
setnames(peliculas, "primaryName", "actor1Name")

# Actor 2.
peliculas <- merge(peliculas, actores, by.x = "actor2", by.y = "nconst", all.x = TRUE)
setnames(peliculas, "primaryName", "actor2Name")

# Actor 3.
peliculas <- merge(peliculas, actores, by.x = "actor3", by.y = "nconst", all.x = TRUE)
setnames(peliculas, "primaryName", "actor3Name")

setkey(peliculas, NULL)


#Guardamos el dataset con los datos de IMDb.
saveRDS(peliculas, "02_datosIMDb.rds")

### Obtención de datos de TMDb a través de su API.

# Clave de acceso de la API, guardada en un entorno .env
api_key <- Sys.getenv("TMDB_API_KEY")

#Función principal para la obtención de información de TMDb.
obtener_datos_tmdb <- function(imdb_id, api_key){
  
  # Llamada 1: obtener tmdb_id a partir del imdb_id
  url_find <- paste0("https://api.themoviedb.org/3/find/", imdb_id,
                     "?api_key=", api_key,
                     "&external_source=imdb_id")
  
  respuesta_find <- tryCatch(GET(url_find, timeout(10)), error = function(e) NULL)
  
  if (is.null(respuesta_find) || status_code(respuesta_find) != 200) {
    return(data.table(tconst = imdb_id, tmdb_id = NA_integer_, budget = NA_real_,
                      revenue = NA_real_, release_date = NA_character_,
                      original_language = NA_character_, production_companies = NA_character_))
  }
  
  contenido_find <- tryCatch(fromJSON(rawToChar(respuesta_find$content)), error = function(e) NULL)
  if (is.null(contenido_find) || length(contenido_find$movie_results) == 0) {
    return(data.table(tconst = imdb_id, tmdb_id = NA_integer_, budget = NA_real_,
                      revenue = NA_real_, release_date = NA_character_,
                      original_language = NA_character_, production_companies = NA_character_))
  }
  
  # Extraer tmdb_id
  tmdb_id <- contenido_find$movie_results$id[1]
  
  # Llamada 2: obtener el resto de variables con el tmdb_id
  url_movie <- paste0("https://api.themoviedb.org/3/movie/", tmdb_id,
                      "?api_key=", api_key)
  
  respuesta_movie <- tryCatch(GET(url_movie, timeout(10)), error = function(e) NULL)
  if (is.null(respuesta_movie) || status_code(respuesta_movie) != 200) {
    return(data.table(tconst = imdb_id, tmdb_id = tmdb_id, budget = NA_real_,
                      revenue = NA_real_, release_date = NA_character_,
                      original_language = NA_character_, production_companies = NA_character_))
  }
  
  contenido_movie <- tryCatch(fromJSON(rawToChar(respuesta_movie$content)), error = function(e) NULL)
  if (is.null(contenido_movie)) {
    return(data.table(tconst = imdb_id, tmdb_id = tmdb_id, budget = NA_real_,
                      revenue = NA_real_, release_date = NA_character_,
                      original_language = NA_character_, production_companies = NA_character_))
  }
  
  Sys.sleep(0.25) #Pausa para no saturar la API
  
  # Extraer todas las variables
  return(data.table(
    tconst               = imdb_id,
    tmdb_id              = tmdb_id,
    budget               = contenido_movie$budget,
    revenue              = contenido_movie$revenue,
    release_date         = contenido_movie$release_date,
    original_language    = contenido_movie$original_language,
    production_companies = paste(contenido_movie$production_companies$name, 
                                 collapse = ",")
  ))
}

ids <- peliculas$tconst #ids de las películas del dataset
n_total <- length(ids) #número total de películas
chunk_size <- 5000

plan(multisession, workers=6) #Paralelización con 6 workers

for (chunk_start in seq(1, n_total, by=chunk_size)){
    chunk_end <- min(chunk_start + chunk_size - 1, n_total)
    archivo_parcial <- paste0("tmdb_chunk_", chunk_start, "_", chunk_end, ".rds")

    if (file.exists(archivo_parcial)){
        cat("Chunk", chunk_start, "-", chunk_end, "ya existe, saltando... \n")
        next
    }

    ids_chunk <- ids[chunk_start:chunk_end]
  
    resultados_chunk <- future_lapply(ids_chunk, function(id) {
        tryCatch(
        obtener_datos_tmdb(id, api_key),
        error = function(e) {
            data.table(tconst = id, tmdb_id = NA_integer_, budget = NA_real_,
                       revenue = NA_real_, release_date = NA_character_,
                       original_language = NA_character_, production_companies = NA_character_)
            }
        )
    }, future.seed = TRUE)
  
    chunk_dt <- rbindlist(resultados_chunk, fill = TRUE)
    saveRDS(chunk_dt, archivo_parcial)
    rm(resultados_chunk, chunk_dt)
    gc()
    cat("Completado chunk", chunk_start, "-", chunk_end, "de", n_total, "\n")
}

# Unión de todos los chunks en un único archivo final
archivos_chunks <- list.files(pattern = "tmdb_chunk_.*\\.rds")
tmdb_data <- rbindlist(lapply(archivos_chunks, readRDS), fill = TRUE)
tmdb_data <- unique(tmdb_data, by = "tconst")

#Guardamos el dataset con los datos de TMDb.
saveRDS(tmdb_data, "02_tmdb_data.rds")

cat("Total películas procesadas:", nrow(tmdb_data), "\n")

# Verificamos que no haya duplicados
sum(duplicated(tmdb_data$tconst)) # 0


### Unión de IMDb con TMDb y limpieza de datos inicial.

peliculas <- merge(peliculas, tmdb_data, by = "tconst", all.x = TRUE)

dim(peliculas)
names(peliculas)

#Guardamos el dataset inicial de la unión de los datos de IMDb y TMDb.
saveRDS(peliculas, "02_datosIMDB_y_TMDb.rds")

#En TMDb, si una película no tiene datos de revenue y budget se asigna un 0.
#Lo modificamos a NA y calculamos el porcentaje que tiene cada variable.
peliculas[budget == 0, budget := NA]
peliculas[revenue == 0, revenue := NA]

mean(is.na(peliculas$budget)) * 100 #94.20566
mean(is.na(peliculas$revenue)) * 100 #95.76146

#Creamos el dataset de las películas que contienen ambos datos económicos (neceesarios para la variable objetivo)
datos <- peliculas[!is.na(budget) & !is.na(revenue)] #Este filtro hace que mis conclusiones no vayan a ser generalizables a todo el cine
dim(datos) # 11123 películas y 23 variables

# Vemos el rango de budget y revenue
summary(datos$budget) #Mínimo de 1, mediana de 10.000.000 y máximo de 489.900.000
summary(datos$revenue) #Mínimo de 1, mediana de 1,160*10^7 y máximo de 2,924*10^9

#Hay valores extremadamente pequeños que no tienen sentido (budget y revenue de 1)
sum(datos$budget < 1000) #220 películas
sum(datos$revenue < 1000) #249 películas

#Filtramos por budget>=1000 y revenue>=1000 para quitar películas con datos económicos erróneos o no fiables
datos <- datos[budget >= 1000 & revenue >= 1000]
dim(datos) #10835 películas

saveRDS(datos, "02_peliculas_completas.rds")


### Feature engineering y gestión de NAs

datos[, tmdb_id := NULL] #Eliminamos el identificador de TMDb, que ya no es necesario

datos$decade <- (datos$startYear%/%10)*10 # Variable numérica decade a partir de startYear

table(datos$decade) # 1104 de 1980, 1677 de 1990, 2920 de 2000, 3643 de 2010 y 1491 de 2020 (década incompleta)

datos[, release_date := as.Date(release_date)] #Convertimos a fecha la variable release_date
datos[, release_month:= as.factor(month(release_date))] #Creamos la variable del mes de estreno a partir de release_date

colSums(is.na(datos)) #Ver los NAs de cada columna del dataset

datos[is.na(genres), genres:="Unknown"] #Los 9 NAs de la variable genres se recodifican como desconocido (Unknown)

#Los 114 NAs de runtimeMinutes (1,05% del total) se imputan por la mediana global, creando previamente una variable indicadora runtimeMissing de si el valor es real o imputado.
datos$runtimeMissing <- as.factor(as.integer(is.na(datos$runtimeMinutes)))
datos[,runtimeMinutes := fifelse(is.na(runtimeMinutes), median(runtimeMinutes, na.rm=TRUE), runtimeMinutes)] 

#Los 35 registros con release_date y release_month nulos (0,3% del total) se eliminan del dataset, dado que no es posible imputar dichas variables
#al ser una variable categórica sin distribución estimable.
datos <- datos[!is.na(release_date)]

#Creamos una variable que cuenta por película el número de actores conocidos que tiene: Si es 0, los tres actores son NA, etc., hasta 3 que significa que se conocen los 3 actores.
datos[, num_actores := (!is.na(actor1)) + (!is.na(actor2)) + (!is.na(actor3))]

#Recodificamos la variable de idioma original
sort(table(datos$original_language), decreasing = TRUE)[1:10] # 7748 inglés, 415 francés, 393 hindi, 241 español, 235 ruso, <200 el resto

datos[, original_language := ifelse(
  is.na(original_language), NA,
  ifelse(original_language %in% c("en", "fr", "hi", "es", "ru"),
         original_language,
         "other"))]
datos[, original_language := as.factor(original_language)]

#Variable Return On Investment (ROI).

datos[, ROI := (revenue - budget)/budget]

# distribución general del ROI
summary(datos$ROI)
#Mín -1.00, mediana 0.451, media 4.912, max 12889.387

#Dado que la distribución es realmente asimétrica, planteamos hacer una transformación logarítmica.

sum(datos$ROI < -1, na.rm = TRUE) #Hay 0 registros con ROI menor que -1, luego es posible crear la variable log(ROI+1) (no hay indeterminaciones de logaritmos negativos)

datos[, log_ROI := log(ROI+1)] #Como ROI puede ser negativo (si revenue<budget), sumamos uno para que todos
#los valores sean positivos, siempre que ROI>=-1 (que hemos visto que se cumple).

# comprobamos la distribución de log_ROI
summary(datos$log_ROI)
#min -10.84, mediana 0.37, media 0.07, max 9.46

#que el mínimo sea -10.84 significa que ROI ~ -0.9999998 (es decir, que la película casi no recaudó nada respecto a su presupuesto)
sum(datos$ROI< -0.99, na.rm=TRUE) #Hay 265 películas con ROI entre -1 y -0.99
datos[ROI< -0.99, summary(revenue)] #El revenue máximo de las 265 películas es menos de 1.5M de dólares (lo cual no suele ser normal)
datos[ROI< -0.99, .(primaryTitle, budget, revenue, ROI)][order(-budget)][1:20]

datos[ROI< -0.99, table(decade)] #De las 265 películas, 9 son de 1980, 23 de 1990, 67 de 2000, 113 de 2010 y 53 de 2020 (esta última década está incompleta)
#TMDb solo registra la recaudación en salas, y estas películas no reflejan su rendimiento económico real al haber sido distribuidas por canales alternativos (streaming en la actualidad o video, dvd anteriormente).

datos <- datos[ROI >= -0.99] #Nos quedamos con las películas con ROI superior a -0,99 (las eliminadas suponen el 2,45% del total)

#Comprobamos nuevamente la distribución de log_ROI
summary(datos$log_ROI) #El mínimo ha pasado de -10,84 a -4,6.

# Variable binaria éxito.

datos[, exito := as.factor(ifelse(ROI >= 1, 1, 0))] #la frontera de decisión es que una película más que doble su presupuesto para considerarse éxito.

table(datos$exito) # 6092 películas con 0 (fracaso) y 4443 películas con 1 (éxito) (57,83% - 42,17%)

# Variables de historial.

datos <- datos[order(release_date)]

# Director: Número de películas previas.
datos[!is.na(director), director_num_films := {
  num_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    num_prev[j] <- sum(release_date < release_date[j], na.rm=TRUE)
  }
  num_prev
}, by = director]

# Director: ROI medio de películas previas.
datos[!is.na(director), director_roi_medio := {
  roi_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    peliculas_previas <- log_ROI[release_date < release_date[j] & !is.na(release_date)]
    roi_prev[j] <- ifelse(
      length(peliculas_previas) == 0,
      NA,
      mean(peliculas_previas, na.rm = TRUE)
    )
  }
  roi_prev
}, by = director]

# Escritor: Número de películas previas.
datos[!is.na(writer), writer_num_films := {
  num_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    num_prev[j] <- sum(release_date < release_date[j], na.rm=TRUE)
  }
  num_prev
}, by = writer]

# Escritor: ROI medio de películas previas.
datos[!is.na(writer), writer_roi_medio := {
  roi_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    peliculas_previas <- log_ROI[release_date < release_date[j] & !is.na(release_date)]
    roi_prev[j] <- ifelse(
      length(peliculas_previas) == 0,
      NA,
      mean(peliculas_previas, na.rm = TRUE)
    )
  }
  roi_prev
}, by = writer]

# Actor 1: Número de películas previas.
datos[!is.na(actor1), actor1_num_films := {
  num_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    num_prev[j] <- sum(release_date < release_date[j], na.rm=TRUE)
  }
  num_prev
}, by = actor1]

# Actor 1: ROI medio de películas previas.
datos[!is.na(actor1), actor1_roi_medio := {
  roi_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    peliculas_previas <- log_ROI[release_date < release_date[j] & !is.na(release_date)]
    roi_prev[j] <- ifelse(
      length(peliculas_previas) == 0,
      NA,
      mean(peliculas_previas, na.rm = TRUE)
    )
  }
  roi_prev
}, by = actor1]

# Actor 2: Número de películas previas.
datos[!is.na(actor2), actor2_num_films := {
  num_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    num_prev[j] <- sum(release_date < release_date[j], na.rm=TRUE)
  }
  num_prev
}, by = actor2]

# Actor 2: ROI medio de películas previas.
datos[!is.na(actor2), actor2_roi_medio := {
  roi_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    peliculas_previas <- log_ROI[release_date < release_date[j] & !is.na(release_date)]
    roi_prev[j] <- ifelse(
      length(peliculas_previas) == 0,
      NA,
      mean(peliculas_previas, na.rm = TRUE)
    )
  }
  roi_prev
}, by = actor2]

# Actor 3: Número de películas previas.
datos[!is.na(actor3), actor3_num_films := {
  num_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    num_prev[j] <- sum(release_date < release_date[j], na.rm=TRUE)
  }
  num_prev
}, by = actor3]

# Actor 3: ROI medio de películas previas.
datos[!is.na(actor3), actor3_roi_medio := {
  roi_prev <- numeric(.N)
  for (j in seq_len(.N)) {
    peliculas_previas <- log_ROI[release_date < release_date[j] & !is.na(release_date)]
    roi_prev[j] <- ifelse(
      length(peliculas_previas) == 0,
      NA,
      mean(peliculas_previas, na.rm = TRUE)
    )
  }
  roi_prev
}, by = actor3]

#Comprobamos que no haya inconsistencias.
datos[is.na(director_num_films) & !is.na(director_roi_medio), .N] # 0
datos[is.na(writer_num_films) & !is.na(writer_roi_medio), .N] # 0
datos[is.na(actor1_num_films) & !is.na(actor1_roi_medio), .N] # 0
datos[is.na(actor2_num_films) & !is.na(actor2_roi_medio), .N] # 0
datos[is.na(actor3_num_films) & !is.na(actor3_roi_medio), .N] # 0

#Comprobación de que el proceso ha funcionado con un actor conocido.
sort(table(datos$actor1Name), decreasing = TRUE)[1:10] #El más repetido en el dataset es Nicolas Cage.
nicolas <- datos[actor1Name == "Nicolas Cage", .(primaryTitle, release_date, log_ROI, actor1_num_films, actor1_roi_medio)]
print(nicolas)
rm(nicolas)

# Variable binaria de productoras, is_major.

majors <- c("Warner Bros", "Universal", "Paramount", "Disney", "Columbia", "TriStar", "Screen Gems",
            "20th Century", "New Line", "Marvel Studios", "Lucasfilm", "Pixar") #Big Five (Universal, Warner Bros., Paramount, Sony y Walt Disney) y sus sellos propios

datos[, is_major := as.integer(
  Reduce(`|`, lapply(majors, function(m) grepl(m, production_companies, ignore.case = TRUE)))
)]

sapply(majors, function(m) sum(grepl(m, datos$production_companies, ignore.case = TRUE))) # Cuántas películas captura cada término

table(datos$is_major) # 7619 no major, 2916 major 

# Variables de géneros cinematográficos.

datos[, num_genres := lengths(strsplit(genres, ","))] #Creamos una variable que registre el número total de géneros de cada película

generos <- unlist(strsplit(datos$genres, ","))
sort(table(generos), decreasing=TRUE) #Frecuencia de cada género

#Drama 5563, Comedy 4008, Action 2656, Crime 1927, Adventure 1912, Romance 1638, Thriller 1589, Horror 1139,
#Mystery 964, Fantasy 799, Biography 663, Family 647, Sci-Fi 642 y Animation 616 entran dentro de géneros principales.
#History (404), Music (319), Sport (227), Documentary (189), War (169), Musical (141), Wester (44) y News (4) entran dentro de genre_other.

generos_principales <- c("Drama", "Comedy", "Action", "Crime", "Adventure", "Romance", "Thriller",
                        "Horror", "Mystery", "Fantasy", "Biography", "Family", "Sci-Fi", "Animation")

#Tomamos como géneros principales aquellos con una aparición de más de 600 registros en el dataset. Los que no sobrepasan este umbral se agrupan en la variable genre_other.

for (g in generos_principales) {
  datos[, (paste0("genre_", tolower(g))) := as.integer(grepl(g, genres))]
}

#Añadimos la variable genre_other que indica si la película no tiene ningún género de los principales
genre_cols <- paste0("genre_", tolower(generos_principales))
datos[, genre_other := as.integer(rowSums(.SD) == 0), .SDcols = genre_cols]

setnames(datos, "genre_sci-fi", "genre_scifi") # Renombramos la variable genre_scifi para mejor manejo de la misma.

# Eliminamos identificadores y variables ya transformadas y reordenamos las columnas del dataset.

names(datos)
datos[, c("director", "writer", "actor1", "actor2", "actor3", "revenue", "ROI"):=NULL]

setcolorder(datos, c("tconst", "primaryTitle", "startYear", "decade", "release_date", "release_month", "runtimeMinutes",
    "runtimeMissing", "budget", "log_ROI", "exito", "original_language", "production_companies", "is_major",
    "num_directors", "directorName", "director_num_films", "director_roi_medio", "num_writers", "writerName", "writer_num_films",
    "writer_roi_medio" , "actor1Name", "actor1_num_films", "actor1_roi_medio", "actor2Name", "actor2_num_films", "actor2_roi_medio",
    "actor3Name", "actor3_num_films", "actor3_roi_medio", "num_actores", "genres", "num_genres", "genre_drama",
    "genre_comedy", "genre_action", "genre_crime", "genre_adventure", "genre_romance", "genre_thriller", "genre_horror",
    "genre_mystery", "genre_fantasy", "genre_biography", "genre_family", "genre_scifi", "genre_animation", "genre_other"))

dim(datos) # 10535 películas, 49 variables

saveRDS(datos, "02_dataset_post_feature_engineering.rds")


# ------------------------------------ #
# Análisis Exploratorio de Datos (EDA) #
# ------------------------------------ #

#Objetivo: conocer bien los datos antes de modelar: entender la estructura, las relaciones y los patrones que hay en los datos.

dir.create("02_graficos_EDA", showWarnings = FALSE) #Carpeta para guardar los gráficos generados

### Análisis univariante.

#Estudiar cada variable por separado sin relacionarla con otras, para entender su distribución y detectar anomalías.

# 1. Distribución de log_ROI = log(ROI+1).

ggplot(datos, aes(x=log_ROI)) +
    geom_histogram(bins=50, fill="steelblue", color="white") +
    geom_vline(xintercept=0, color="firebrick", linetype = "dashed") +
    labs(title = "Distribución de log(ROI+1)", x="log(ROI+1)", y="Frecuencia")+
    theme_minimal()

ggsave("02_graficos_EDA/01_distribucion_logROI.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La línea roja vertical en 0 indica el punto donde log(ROI+1) = 0, es decir ROI = 0, que es cuando la película
#exactamente recupera su inversión. Las películas a la izquierda de esa línea pierden dinero y las de la derecha ganan.
#Es una distribución aproximadamente simétrica centrada ligeramente por encima de 0, indicando que
#la película típica recupera su inversión.


# 2. Distribución de variable objetivo binaria éxito.

ggplot(datos, aes(x=exito, fill=exito)) +
    geom_bar() +
    scale_fill_manual(values=c("0"="tomato", "1"="steelblue")) +
    labs(title = "Distribución de la variable éxito", x="Éxito (1) / Fracaso (0)", y="Nº películas") +
    theme_minimal()

ggsave("02_graficos_EDA/02_distribucion_exito.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS. Representación con un gráfico de barras de la variable binaria éxito con 6092 0 y 4443 1. 


# 3. Distribución de variable presupuesto (budget) y tomando su logaritmo (4.)

ggplot(datos, aes(x = budget)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribución del presupuesto", x = "Presupuesto (USD)", y = "Frecuencia") +
  theme_minimal()

ggsave("02_graficos_EDA/03_distribucion_budget.png", width = 8, height = 5, dpi = 300)

ggplot(datos, aes(x = log(budget))) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    labs(title = "Distribución del log(presupuesto)",
       x = "log(Presupuesto)",
       y = "Frecuencia") +
    theme_minimal()

ggsave("02_graficos_EDA/04_distribucion_logbudget.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La distribución de budget es realmente antisimétrica, a diferencia que tomando su logaritmo. Por tanto,
#transformamos la variable budget a log(budget) para que el modelado posterior sea más fácil.
datos[, log_budget := log(budget)]
datos[, budget:=NULL]


# 5. Distribución de decade

ggplot(datos, aes(x = as.factor(decade))) +
    geom_bar(fill = "steelblue", color = "white") +
    labs(title = "Número de películas por década",
        x = "Década",
        y = "Número de películas") +
    theme_minimal()

ggsave("02_graficos_EDA/05_distribucion_decade.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La década de 2010 es la que contiene más películas. La década de 2020 tiene menos películas de las que se podría esperar dado que
#la década está incompleta (solo incluye películas hasta 2025, siendo un periodo de tiempo la mitad de corto que el resto)
#El periodo con menos películas es 1980 porque había menos datos.

# 6. Distribución de release_month

ggplot(datos, aes(x = release_month)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Número de películas por mes de estreno",
       x = "Mes",
       y = "Número de películas") +
  theme_minimal()

ggsave("02_graficos_EDA/06_distribucion_release_month.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#En septiembre, octubre y diciembre es cuando más películas se estrenan. Los meses con menos estrenos son enero, mayo, junio y julio.

# 7. Distribución de original_language
ggplot(datos, aes(x = original_language)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Número de películas por idioma",
       x = "Idioma",
       y = "Número de películas") +
  theme_minimal()

ggsave("02_graficos_EDA/07_distribucion_original_language.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Como vimos al transformar la variable original_language, el idioma predominante es el inglés con más del 71% del total de películas.
#El segundo campo con más frecuencia es el de otros géneros, ya que engloba el 17% de las observaciones, repartidas en 67 idiomas distintos


### BLOQUE DE RATINGS: análisis de ratings post-estreno.

#Antes de estudiar las relaciones entre predictoras y ROI (análisis bivariante), analizamos dos variables post-estreno que, aunque no pueden
#usarse en el modelo por data leakage, ayudan a entender qué determina el éxito. Se analizan aquí únicamente para enriquecer la narrativa del TFG

ratings_eda <- fread("IMDb\\title.ratings.tsv", na.strings="\\N")
#Contiene las columnas tconst, numVotes y averageRating (son variables post-estreno)

#Juntamos ambos datasets
datos_con_ratings <- merge(datos, ratings_eda, by="tconst", all.x=FALSE) #all.x =FALSE para quedarnos solo con las películas que sí tienen ratings,
#pues no tiene sentido imputar ratings para este análisis y así los gráficos son más limpios. Sin embargo, esto puede introducir sesgo hacia películas más conocidas.

cat("Películas con rating disponible:", nrow(datos_con_ratings), "de", nrow(datos), "totales\n") #10434 de 10535
cat("Porcentaje:", round(nrow(datos_con_ratings)/nrow(datos)*100, 1), "%\n") #99%

# 8. Relación entre valoraciones y ROI.
ggplot(datos_con_ratings, aes(x=averageRating, y=log_ROI))+
    geom_point(alpha=0.3, color="steelblue") +
    geom_smooth(method="lm", color="red", se=TRUE) +
    labs(title = "Relación entre valoración media y ROI",
    subtitle = "Variable post-estreno: no utilizada en el modelo predictivo",
    x = "Valoración media (IMDb)",
    y = "log(ROI+1)") +
    theme_minimal()

ggsave("02_graficos_EDA/08_BloqueRatings_1.png", width = 8, height = 5, dpi = 300)

cor(datos_con_ratings$log_ROI, datos_con_ratings$averageRating) # 0,251

# COMENTARIOS.
#La pendiente positiva confirma que las películas mejor valoradas tienden a tener mayor ROI, con una correlación de 0.25.
#Sin embargo la nube de puntos es muy dispersa, lo que indica que la relación existe pero es débil.
#Hay además una asimetría clara: con ratings bajos (2-4) el ROI puede ser muy negativo, mientras que con ratings altos (7-9) el ROI tiene más variabilidad vertical.
#Es decir, una buena valoración no garantiza ROI alto, pero una mala valoración sí tiende a asociarse con ROI bajo.


# 9. Relación entre número de votos y ROI.
ggplot(datos_con_ratings, aes(x = log(numVotes), y = log_ROI)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre popularidad (nº votos) y ROI",
       subtitle = "Variable post-estreno: no utilizada en el modelo predictivo",
       x = "log(Número de votos)",
       y = "log(ROI + 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/09_BloqueRatings_2.png", width = 8, height = 5, dpi = 300)

cor(datos_con_ratings$log_ROI, log(datos_con_ratings$numVotes)) #0,324

# COMENTARIOS.
#La correlación es 0.324 y la pendiente es más pronunciada que en el gráfico anterior. La interpretación es que la popularidad de una película, medida por el
#número de personas que se molestan en votarla, es mejor predictor del ROI que la valoración en sí misma. Esto tiene sentido dado que una película muy vista
#genera muchos votos independientemente de si gusta mucho o poco, y una película muy vista es una película que ha recaudado mucho.
# Así, tanto el ROI como el número de votos son consecuencia del éxito comercial (reforzando que no se pueda usar en el análisis)


# 10. Relación entre valoración media y éxito de películas.

ggplot(datos_con_ratings, aes(x = exito, y = averageRating, fill = exito)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "tomato", "1" = "steelblue")) +
  labs(title = "Valoración media según éxito comercial",
       subtitle = "Variable post-estreno: no utilizada en el modelo predictivo",
       x = "Éxito (1) / Fracaso (0)",
       y = "Valoración media (IMDb)") +
  theme_minimal()

ggsave("02_graficos_EDA/10_BloqueRatings_3.png", width = 8, height = 5, dpi = 300)

datos_con_ratings[, .(mediana_rating = median(averageRating, na.rm=TRUE)), by = exito]

# COMENTARIOS.
#Para 0 (fracaso) la mediana es 6.2 y para 1 (éxito) la mediana es 6.7. Medio punto de diferencia indica que las películas exitosas tienden a estar algo
#mejor valoradas, pero la separación no es tan grande como para que la valoración sea un predictor determinante del éxito comercial por sí sola.
#A pesar de que la caja del grupo de éxito está por encima de la de fracaso, hay solapamiento entre ambas cajas, confirmando que la valoración y el
#éxito comercial están relacionados pero son conceptos distintos. 

# Limpiamos: eliminamos el dataset auxiliar para no confundirlo con el principal
rm(ratings_eda, datos_con_ratings)


### Análisis bivariante.

#Estudiar la relación entre cada variable predictora y la variable objetivo, con el fin de identificar qué variables tienen más relación con el éxito antes de modelar.

# 11. Relación entre presupuesto y ROI.
ggplot(datos, aes(x = log_budget, y = log_ROI)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre presupuesto y ROI",
       x = "log(Presupuesto)",
       y = "log(ROI + 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/11_ABivariante_logbudget.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La recta de regresión tiene una pendiente ligeramente negativa, lo que sugiere que a mayor presupuesto menor ROI. Esto puede ser porque las películas con presupuestos muy altos
#necesitan recaudar grandes cantidades de dinero únicamente para recuperar la inversión. Y, por el contrario, una película con un presupuesto de 1M que recauda 5M tiene un ROI de 4,
#lo cual es casi imposible de conseguir con un presupuesto de 200M. Es decir, una hipótesis sería que esto ocurre porque es mucho más fácil multiplicar una inversión pequeña que una grande.
#La nube de puntos es tan dispersa que aunque el presupuesto influye negativamente en el ROI, la relación entre ambas variables es débil y con mucha variabilidad.


# 12. Relación entre géneros y ROI medio.

# Primero calculamos el ROI medio por género y la tasa de éxito
generos_cols <- c("genre_drama", "genre_comedy", "genre_action", "genre_crime", "genre_adventure", "genre_romance", "genre_thriller", 
                  "genre_horror", "genre_mystery", "genre_fantasy", "genre_biography", "genre_family", "genre_scifi", "genre_animation", "genre_other")

roi_generos <- data.table(
  genero = gsub("genre_", "", generos_cols),
  roi_medio = sapply(generos_cols, function(g) {
    mean(datos[get(g) == 1, log_ROI], na.rm = TRUE)
  }),
  tasa_exito = sapply(generos_cols, function(g) {
    mean(datos[get(g) == 1, exito == 1], na.rm = TRUE) * 100
  })
)

ggplot(roi_generos, aes(x = reorder(genero, roi_medio), y = roi_medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "ROI medio por género",
       x = "Género",
       y = "log(ROI + 1) medio") +
  theme_minimal()

ggsave("02_graficos_EDA/12_ABivariante_generosROImedio.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.

#Los géneros más rentables son Horror, Family y Animation, seguidos de Adventure y Comedy.
#Horror se puede explicar porque las películas de terror suelen tener presupuestos muy bajos y pueden recaudar muchísimo. Por ejemplo,
#paranormal activity tuvo un presupuesto de 15000$ y recaudó 193M. Family y Animation tienen ROI muy alto porque atraen a toda la familia
#(audiencia familiar), multiplicando el número de espectadores por entrada vendida.

#Biography es el género menos rentable con diferencia, incluso por debajo de Drama, Crime y Otros. Esto sucede porque las películas
#biográficas suelen ser producciones de prestigio con presupuestos elevados orientadas a premios más que a taquilla masiva, lo que penaliza el ROI.
#Crime y Drama están por encima de biography pero son de los menos rentables, dado que suelen ser películas con menos atractivo comercial masivo
#y estar dirigidas frecuentemente a audiencias más nicho.

#Limitación de este gráfico: el efecto de los géneros no es aislado, es decir, si una película contiene dos géneros, esta influye en el roi medio de ambos géneros.

#13. Relación entre géneros y tasa de éxito.

ggplot(roi_generos, aes(x = reorder(genero, tasa_exito), y = tasa_exito)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Tasa de éxito por género",
       x = "Género",
       y = "% de películas con éxito (ROI > 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/13_ABivariante_generosTasaExito.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Los resultados son bastante consistentes con el gráfico de ROI medio (ambas métricas indican casi lo mismo)
#Casi el 50% de las películas con Horror son éxitos y animation y adventure tienen 47-48% tasa de éxito. Biography vuelve a ser la
#menos rentable con un poco más del 30% de sus películas siendo exitosas, seguido por Crime y other, con porcentajes de 37-38%.
#Aún así, las diferencias entre géneros son relativamente pequeñas (todas están entre el 35 y el 50%),
#lo que sugiere que el género solo no es un predictor muy potente del éxito. 

#Ambos gráficos indican que el género influye en la rentabilidad de forma moderada (ningún género garantiza ni descarta el éxito por sí solo)

# 14. Relación entre mes de estreno y ROI.

ggplot(datos, aes(x=release_month, y=log_ROI)) +
    geom_boxplot(fill="steelblue", alpha=0.7) +
    labs(title="Distribución del ROI por mes de estreno",
        x = "Mes", y ="log(ROI+1)") +
    theme_minimal()

ggsave("02_graficos_EDA/14_ABivariante_release_month.png", width = 8, height = 5, dpi = 300)

##COMENTARIOS
#Se ve que las diferencias entre meses son pequeñas en términos de mediana. Los meses con ROI más alto corresponden a junio y julio (confirmando que el verano es mejor temporada para taquilla)
#y diciembre/enero (coincidiendo con la temporada navideña), mientras que los meses con ROI más bajo son septiembre y octubre. Antes vimos que estos meses concentran muchos estrenos,
#pero este gráfico puede indicar que a pesar de que se estrenen muchas películas en esa época no necesariamente son las más rentables.
#Hay outliers extremos en prácticamente todos los meses. Esto indica que el mes de estreno por sí solo NO determina el éxito.

# 15. Relación entre majors y ROI.

ggplot(datos, aes(x = as.factor(is_major), y = log_ROI, fill = as.factor(is_major))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("0" = "tomato", "1" = "lightgreen"),
                    labels = c("0" = "No major", "1" = "Major")) +
  scale_x_discrete(labels = c("0" = "No major", "1" = "Major")) +
  labs(title = "Distribución del ROI según productora",
       x = "Tipo de productora",
       y = "log(ROI + 1)",
       fill = "Productora") +
  theme_minimal()

ggsave("02_graficos_EDA/15_ABivariante_productoras.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Las películas de majors tienen un ROI medio superior a las no majors: las majors tienen una mediana ligeramente superior,
#la caja de las majors está desplazada hacia arriba (el 50% central de los datos está más arriba en las majors). Las películas no major
#tienen mayor variabilidad en el ROI (caja más ancha) mientras que las majors tienen una distribución más concentrada, lo que sugiere
#que su ventaja no es tanto generar los ROI más espectaculares sino producir resultados más consistentes y predecibles.

# 16. Relación entre idioma y ROI.

ggplot(datos, aes(x = original_language, y = log_ROI, fill = original_language)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución del ROI por idioma",
       x = "Idioma",
       y = "log(ROI + 1)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("02_graficos_EDA/16_ABivariante_original_language.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#El hindi destaca por encima del resto de idiomas, teniendo mayor ROI medio y mediana más alta.
#Esto se podría explicar con que Bollywood produce películas con presupuestos relativamente bajos que tienen un mercado doméstico enorme, generando ROIs muy altos.
#El inglés tiene una mediana positiva y razonable a pesar de no ser el mejor idioma. Las películas en inglés incluyen tanto blockbusters con presupuestos enormes
#(que como vimos tienden a tener ROI más bajo) como películas independientes. El francés es el que tiene la mediana más baja (la única negativa de hecho).
#El cine francés tiende a ser más artístico y de autor, con menos orientación comercial y distribuida en menor medida fuera de Francia. El español, el ruso y el
#resto de idiomas se mantienen en posiciones intermedias.
#Hay que destacar también que el tamaño de cada grupo es muy distinto (+7500 películas en inglés mientras que el resto de idiomas tienen menos de 400 películas)

# 17. Relación entre década y ROI.

ggplot(datos, aes(x = as.factor(decade), y = log_ROI, fill = as.factor(decade))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución del ROI por década",
       x = "Década",
       y = "log(ROI + 1)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("02_graficos_EDA/17_ABivariante_decade.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La década de 1980 tiene la mediana más alta de todas (aunque las diferencias no son abismales). En esa época había menos estrenos anuales y no había servicios
#de streaming. Además, los presupuestos eran menores, pudiendo obtener ROIs mayores. Las siguientes 3 décadas (1990,2000 y 2010) tienen medianas similares y
#positivas (estabilidad en la rentabilidad del cine comercial). La última época, 2020, tiene la mediana más baja, aunque también tiene menos de la mitad de las
#películas de las dos décadas anteriores al estar la década incompleta (solo 5 de 10 años). La mediana baja puede explicarse con el impacto del COVID y el auge del streaming

# 18. Relación entre historial del director y ROI.

ggplot(datos[!is.na(director_roi_medio)], 
       aes(x = director_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre historial del director y ROI",
       x = "ROI medio previo del director",
       y = "log(ROI + 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/18_ABivariante_director1.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La línea roja tiene una pendiente claramente positiva, indicando que el historial previo del director es un buen predictor del ROI de su próxima película.
#Los directores con historial previo positivo (x>0) tienden a producir películas más rentables (y>0) y viceversa.
#Comparando este gráfico con el del presupuesto, el historial del director parece una variable más predictiva que el presupuesto.

# 19. Directores más frecuentes en el dataset

top_directores <- datos[!is.na(directorName), .N, by = directorName][order(-N)][1:15]

ggplot(top_directores, aes(x = reorder(directorName, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Directores con más películas en el dataset",
       x = "Director", y = "Número de películas") +
  theme_minimal()

ggsave("02_graficos_EDA/19_ABivariante_director2.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Los directores con más películas en el dataset son Woody Allen, Clint Eastwood (ambos con más de 30) y Steven Spielberg.

# 20. Directores con mayor ROI medio (que tengan al menos 5 películas en el dataset)

roi_directores <- datos[!is.na(directorName) & !is.na(log_ROI),
                        .(roi_medio = mean(log_ROI, na.rm = TRUE),
                          n_peliculas = .N),
                        by = directorName][n_peliculas >= 5][order(-roi_medio)][1:15]

ggplot(roi_directores, aes(x = reorder(directorName, roi_medio), y = roi_medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Directores más rentables (mín. 5 películas)",
       x = "Director", y = "log(ROI+1) medio") +
  theme_minimal()

ggsave("02_graficos_EDA/20_ABivariante_director3.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#A pesar de no ser de los más frecuentes, Alex Kendrick lidera el top de directores más rentables (que tengan un mínimo de 5 películas en el dataset).
#Le siguen Jeff Tremaine y David F. Sandberg.

# 21. Relación entre historial del actor principal y ROI.

ggplot(datos[!is.na(actor1_roi_medio)], 
       aes(x = actor1_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre historial del actor principal y ROI",
       x = "ROI medio previo del actor principal",
       y = "log(ROI + 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/21_ABivariante_actorprincipal.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Pendiente positiva incluso más pronunciada que la del director (el historial del actor principal es también un buen predictor del ROI).

# 22. Gráfico comparativo de la influencia de los 3 actores principales.

library(gridExtra)

p1 <- ggplot(datos[!is.na(actor1_roi_medio)], 
       aes(x = actor1_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Actor principal", x = "ROI medio previo", y = "log(ROI + 1)") +
  theme_minimal()

p2 <- ggplot(datos[!is.na(actor2_roi_medio)], 
       aes(x = actor2_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Actor secundario", x = "ROI medio previo", y = "") +
  theme_minimal()

p3 <- ggplot(datos[!is.na(actor3_roi_medio)], 
       aes(x = actor3_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Actor terciario", x = "ROI medio previo", y = "") +
  theme_minimal()

png("02_graficos_EDA/22_ABivariante_historial_actores.png", width = 2000, height = 700, res = 200)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()

# COMENTARIOS.
#Vemos cómo la pendiente se va suavizando del actor principal al terciario: La pendiente del actor principal es más pronunciada,
#la del secundario es similar pero algo menos pronunciada y la del terciario es claramente más suave que las dos anteriores.
#Aún así, todos los gráficos tienen una pendiente positiva, lo que sugiere que el reparto completo tiene cierta influencia y no solo el actor principal.

# 23. Actores principales más frecuentes

top_actores <- datos[!is.na(actor1Name), .N, by = actor1Name][order(-N)][1:15]

ggplot(top_actores, aes(x = reorder(actor1Name, N), y = N)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Actores principales más frecuentes en el dataset",
       x = "Actor", y = "Número de películas") +
  theme_minimal()

ggsave("02_graficos_EDA/23_ABivariante_actores.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Los actores más frecuentes en el dataset son Nicolas Cage (+50), Tom Hanks (casi 45), Robert De Niro y Denzel Washington (ambos con casi 40).

# 24. Actores principales con mayor ROI medio (que tengan mínimo 5 películas en el dataset)

roi_actores <- datos[!is.na(actor1Name) & !is.na(log_ROI),
                     .(roi_medio = mean(log_ROI, na.rm = TRUE),
                       n_peliculas = .N),
                     by = actor1Name][n_peliculas >= 5][order(-roi_medio)][1:15]

ggplot(roi_actores, aes(x = reorder(actor1Name, roi_medio), y = roi_medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Actores principales más rentables (mín. 5 películas)",
       x = "Actor", y = "log(ROI+1) medio") +
  theme_minimal()

ggsave("02_graficos_EDA/24_ABivariante_actores2.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#De nuevo vuelve a ocurrir que los actores que más aparecen en el dataset no son necesariamente los que tienen mayor ROI medio.
#En este caso, lideran Patrick Wilson y Rowan Atkinson.
#Es importante destacar que en estos dos últimos gráficos solo se toman los actores principales (actor1)

# 25. Relación entre historial del escritor y ROI.

ggplot(datos[!is.na(writer_roi_medio)], 
       aes(x = writer_roi_medio, y = log_ROI)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relación entre historial del escritor y ROI",
       x = "ROI medio previo del escritor",
       y = "log(ROI + 1)") +
  theme_minimal()

ggsave("02_graficos_EDA/25_ABivariante_escritor.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La pendiente de la recta es claramente positiva, indicando que el historial previo del escritor principal tiene una relación positiva
#con el ROI de la película. Aún así, la nube de puntos es muy dispersa y el intervalo de confianza crece en los extremos (es decir, que
#hay mucha variabilidad).

### Análisis de correlaciones.

#Estudiar las relaciones entre todas las variables numéricas entre sí. Matriz de correlaciones y evaluar multicolinealidad

vars_numericas <- datos[, .(log_budget, decade, runtimeMinutes, num_genres, num_directors, num_actores, is_major,
  director_num_films, director_roi_medio, writer_num_films, writer_roi_medio, actor1_num_films, actor1_roi_medio,
  actor2_num_films, actor2_roi_medio, actor3_num_films, actor3_roi_medio, genre_drama, genre_comedy, genre_action,
  genre_crime, genre_adventure, genre_romance, genre_thriller, genre_horror, genre_mystery,
  genre_fantasy, genre_biography, genre_family, genre_scifi, genre_animation, genre_other,
  log_ROI
)]

par(mfrow=c(1,1))
cor_matrix <- cor(vars_numericas, use = "pairwise.complete.obs")

library(corrplot)

png(filename = "02_graficos_EDA/26_matriz_correlaciones.png", width = 1400, height = 1400, res=170)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.6, tl.col = "black",
         addCoef.col = "black", number.cex = 0.5,
         title = "Matriz de correlaciones", mar = c(0,0,1,0))
dev.off()

# COMENTARIOS.
#La variable más correlacionada con log_ROI es writer_roi_medio, con un 0.25. Le siguen director_roi_medio (0.24), actor1_roi_medio (0.17),
#is_major (0.16) y actor2_roi_medio (0.13), indicando que el historial tanto del escritor como del director y los actores principales son
#fuertes predictores del éxito de la película.
#runtimeMinutes (0.10), actor3_roi_medio (0.09) y genre_comedy (0.06) tienen baja correlación pero positiva con log_ROI.
#log_buget tiene correlación casi nula (-0.04) confirmando que el presupuesto solo no predice el ROI
#En general, no hay multicolinealidad grave entre predictoras. El único caso preocupante es la correlación de -0.62 entre num_actores
#y genre_other (las películas con menos actores registrados suelen ser las que no tienen géneros principales). De hecho, num_actores tiene
#una correlación de 0.00 con log_ROI y genre_other de -0.02. Es decir, ninguna de las dos tiene apenas relación con la variable objetivo individualmente.
#Como hemos visto, la correlación más alta con log_ROI es de 0.25, es decir, las correlaciones en general son bajas. Esto sugiere que las relaciones
#no son lineales y que modelos como Random Forest o XGBoost captarán mejor los patrones que la regresión lineal.

#log_buget tiene correlación alta con is_major (0.42), actor1_num_films (0.36). actor1_num_films y actor2_num_films tienen una correlación de 0.33
#genre_adventure y genre_animation tienen una correlación de 0.40

#Investigamos las celdas en las que aparece un símbolo de interrogación ? (num_actores con actor3_num_films y con actor3_roi_medio)
sum(!is.na(datos$num_actores) & !is.na(datos$actor3_num_films)) #10352
sum(!is.na(datos$num_actores) & !is.na(datos$actor3_roi_medio)) #3574

var(datos$num_actores[!is.na(datos$actor3_num_films)]) # varianza = 0
table(datos$num_actores[!is.na(datos$actor3_num_films)]) #el único valor de num_actores que se toma en las 10352 películas con actor3_num_films distinto de NA es 3.

#Esto ocurre por la forma en la que se construyeron las variables: si hay datos del tercer actor, necesariamente la película tiene los tres actores registrados,
#es decir, num_actores = 3. Por tanto, no hay variabilidad posible haciendo que la varianza sea 0 y que la correlación entre las variables esté matemáticamente indefinida.

#Las celdas con ? son pares de variables donde la correlación es matemáticamente indefinida por varianza nula en el conjunto de observaciones completas.
#Concretamente, num_actores toma el valor constante 3 en todas las filas donde actor3_num_films y actor3_roi_medio están disponibles (son distintas de NA),
#que es una consecuencia directa de la construcción de ambas variables.


### Análisis de valores missing.

#Estudiar el patrón de NAs en el dataset final. Ver cuántos hay y si siguen algún patrón.

# Porcentaje de NAs por variable
na_porcentaje <- colMeans(is.na(datos)) * 100
na_porcentaje[na_porcentaje > 0]

## COMENTARIOS
#Hay claramente dos grupos de valores:
#El primer grupo son aquellas variables con un porcentaje de NA menor del 2%:
#directorName, num_directors y director_num_films con 0.057%,
#writerName, writer_num_films y num_writers con 1,15%,
#actor1Name y actor1_num_films con 1.13%,
#actor2Name y actor2_num_films con 1.47% y
#actor3Name y actor3_num_films con 1.74%.
#De estas variables, directorName, writerName, actor1Name, actor2Name y actor3Name son únicamente interpretativas y no entran en el modelado (se eliminarán al final del EDA).

#El segundo grupo, aquellas con >45% de NAs:
#director_roi_medio (48.62%), writer_roi_medio (65.03%), actor1_roi_medio (46.27%), actor2_roi_medio (57.12%) y actor3_roi_medio (66.07%).

# Cuántos NAs hay en cada variable de historial
sum(is.na(datos$director_roi_medio)) #5122 NA
sum(is.na(datos$writer_roi_medio)) # 6851 NA
sum(is.na(datos$actor1_roi_medio)) #4875 NA
sum(is.na(datos$actor2_roi_medio)) #6018 NA
sum(is.na(datos$actor3_roi_medio)) #6961 NA

#Las variables de historial previo (roi_medio de director, escritor y actores) tienen entre un 45% y 67% de NAs, lo cual es estructural y esperado: el NA aparece en todas las películas
#donde el director, escritor o actor no tiene ninguna película anterior registrada en el dataset, ya sea porque es su primera aparición o porque es la única (solo tiene una película en total).
#El alto porcentaje refleja que la mayoría de directores, escritores y actores del dataset tienen muy pocas películas registradas en él.
#El resto de variables tienen menos del 2% de NAs, lo cual es completamente asumible.

#El tratamiento de NAs en las variables de historial será diferente según el modelo.
#En regresión logística, imputaremos los NA con 0, habiendo creado previamente una variable indicadora sobre si el valor del roi medio se tenía o no.
#En Random Forest se imputarán los NA internamente y en XGBoost se dejarán sin tratar los NA, pues este modelo maneja NAs nativamente y el modelo aprende qué hacer con ellos.

### Detección de outliers.

#Identificar valores extremos que puedan distorsionar el modelo.

# Boxplots de las variables numéricas más importantes

png("02_graficos_EDA/27_boxplots_outliers.png", width = 1200, height = 500, res = 150)
par(mfrow = c(1,3))

boxplot(datos$log_ROI, main = "Outliers en log_ROI", ylab = "log(ROI+1)",col = "steelblue")

#Hay outliers en ambas direcciones pero especialmente hacia arriba. Los puntos por encima de 4 son películas con ROI extraordinariamente alto
#(blockbusters inesperados o películas con presupuesto mínimo y gran recaudación). Los outliers hacia abajo son películas que perdieron casi toda su inversión.

boxplot(datos$log_budget, main = "Outliers en log_budget", ylab = "log(budget)",col = "steelblue")

#Los outliers están únicamente hacia abajo, es decir, películas con presupuestos pequeños. Esto coincide con lo que vimos en el histograma
#del grupo pequeño separado del resto de películas independientes con presupuestos muy bajos. 

boxplot(datos$runtimeMinutes, main = "Outliers en runtimeMinutes", ylab = "Minutos",col = "steelblue")

#Hay algunos outliers hacia arriba, con duraciones de más de 250-300 minutos.

dev.off()
par(mfrow=c(1,1))

#Comprobamos cómo son esos outliers.
datos[runtimeMinutes>240, .(primaryTitle, runtimeMinutes)]

#Las películas con duración de más de 240 minutos corresponden a casos atípicos que definitivamente no representan el cine comercial.
#Sin embargo, la duración extrema es información legítima y no un error en el dato..Al no haber criterios objetivos para excluir
#estas películas y ser tan pocas (6), se mantienen como casos atípicos pero legítimos (el impacto en el modelo no será grande al ser tan pocas).

dim(datos) #10535 filas y 49 cols.
names(datos)
#Eliminamos las variables interpretativas que se mantuvieron en el EDA exclusivamente para dar
#contexto y se eliminan antes del modelado porque no pueden entrar como variables predictoras.
datos[, c("startYear", "release_date", "production_companies", "directorName", "writerName", "actor1Name", "actor2Name", "actor3Name", "genres") := NULL]


saveRDS(datos, "02_datos_post_EDA.rds")

# -------- #
# Modelado #
# -------- #

#Empleamos 3 modelos: regresión logística, Random Forest y XGBoost, en ese orden. Evaluamos los 3 modelos
#con AUC-ROC, F1 score y matriz de confusión. Haremos un train/test split aleatorio, común para los 3 modelos
#que mantenga las proporciones de la variable objetivo éxito, y hacemos validación cruzada con 5 folds en el
#conjunto de entrenamiento (en los tres modelos y con la misma configuración)

dir.create("02_graficos_modelado", showWarnings = FALSE)

# Preparaciones para los 3 modelos.

#Convertimos runtimeMissing a entero, pues necesitaremos que sea continua para los 3 modelos
datos[, runtimeMissing := as.integer(as.character(runtimeMissing))]

#Creamos una variable idéntica a éxito pero con niveles explícitos (necesaria para Regresión logística y Random Forest)
datos[, exito_factor := factor(ifelse(exito == 1, "exito", "fracaso"), levels = c("fracaso", "exito"))]

# Dataset para Random Forest y XGBoost (con NAs)
datos_rf <- datos
datos_XGB <- copy(datos)

# Dataset para regresión logística (con indicadoras y NAs reemplazados por 0)
datos_RLogistica <- copy(datos)

# Partición train/test del conjunto de datos, usando DataPartition para hacer un split estratificado, es decir,
#manteniendo la misma proporción de éxitos y fracasos en ambos grupos, de forma que el modelo sea más representativo.
set.seed(42)
train_index <- createDataPartition(datos$exito, p=0.8, list=FALSE)

# Definimos el control de cross validation (igual para los 3 modelos)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,       # necesario para calcular AUC-ROC
  summaryFunction = twoClassSummary,  # métricas para clasificación binaria, calcula AUC, sensibilidad y especificidad
  savePredictions = TRUE
)

# 1. Regresión logística.

# 1.1. Preparación previa.

#Creamos las variables indicadoras de missing en director, escritor y actores principales
#(tanto num_films como roi_medio, aunque el porcentaje de NAs en ambos conjuntos sea muy distinto)
#y después imputamos con 0 los NA (en regresión logística no puede haber NA)

for (var in c("director_roi_medio", "writer_roi_medio", "actor1_roi_medio", "actor2_roi_medio", "actor3_roi_medio")) {
  datos_RLogistica[, paste0(var, "_missing") := as.integer(is.na(get(var)))]
  datos_RLogistica[is.na(get(var)), (var) := 0]
}

for (var in c("director_num_films", "writer_num_films", "actor1_num_films", "actor2_num_films", "actor3_num_films", "num_directors", "num_writers")) {
  datos_RLogistica[, paste0(var, "_missing") := as.integer(is.na(get(var)))]
  datos_RLogistica[is.na(get(var)), (var) := 0]
}

#Eliminamos la variable genre_other por la multicolinealidad con num_actores (-0.62) y la poca influencia de esa variable a la variable objetivo (correlación casi nula)
datos_RLogistica[, genre_other:=NULL]

#Creamos variables dummies de release_month y original_language (en reg.log. las variables de tipo factor no se manejan bien)
dummies_mes <- as.data.table(model.matrix(~ release_month, data = datos_RLogistica))
dummies_mes[, `(Intercept)` := NULL]
dummies_idioma <- as.data.table(model.matrix(~ original_language, data = datos_RLogistica))
dummies_idioma[, `(Intercept)` := NULL]

datos_RLogistica <- cbind(datos_RLogistica, dummies_mes, dummies_idioma)
datos_RLogistica[, c("release_month", "original_language") := NULL]

#Conjuntos de train y test para Regresión logística
train_RLog <- datos_RLogistica[train_index, ]
test_RLog <- datos_RLogistica[-train_index, ]

# Verificamos las proporciones de éxito en train y test
prop.table(table(train_RLog$exito_factor))
prop.table(table(test_RLog$exito_factor))

# Verificamos los tamaños de cada conjunto
nrow(train_RLog) # 8429 películas, con un 57.8% de fracasos y 42.2% de éxitos
nrow(test_RLog) # 2106 películas, mismo ratio de fracasos y éxitos

#Escalamos las variables continuas únicamente para la regresión logística, pues este método es sensible a la escala de las variables
#es decir, el modelo tiende a dar más peso a variables con mayor escala simplemente por tener valores más grandes.
#Para evitar esto, escalamos las variables numéricas a media 0 y desviación típica 1.

vars_scale <- c("decade", "runtimeMinutes", "num_genres", "num_directors", "num_writers", "num_actores", "director_num_films",
                "director_roi_medio", "writer_num_films", "writer_roi_medio", "actor1_num_films", "actor1_roi_medio",
                "actor2_num_films", "actor2_roi_medio", "actor3_num_films", "actor3_roi_medio", "log_budget")
#Las variables binarias no las escalamos para mantener la interpretabilidad de los coeficientes

medias <- sapply(vars_scale, function(v) mean(train_RLog[[v]], na.rm=TRUE))
desv_tip <- sapply(vars_scale, function(v) sd(train_RLog[[v]], na.rm=TRUE))

train_RLog_scaled <- copy(train_RLog)
test_RLog_scaled <- copy(test_RLog)

for (v in vars_scale){
  train_RLog_scaled[, (v) := (get(v) - medias[v]) / desv_tip[v]]
  test_RLog_scaled[, (v) := (get(v) - medias[v]) / desv_tip[v]]
}

table(train_RLog_scaled$num_directors_missing) # 8333 ceros y 96 unos.

# Variables que NO entran como predictoras
vars_exc_RLog <- c("tconst", "primaryTitle", "log_ROI", "exito", "exito_factor",
              "num_directors_missing", "actor3_num_films_missing", "num_writers_missing")
#excluimos log_ROI porque el éxito es una función directa del ROI (por cómo la definimos, éxito := ROI>=1), luego sería data leakage
#excluimos num_directors_missing y num_writers_missing por falta de variabilidad (5 valores en 1 frente a 8424 en 0 y 96 valores en 1 frente a 8333 en 0, respectivamente)
#que hace que el modelo no pueda estimar estos coeficientes con fiabilidad (se evaluó el modelo con dichas variables en él y había singularidades)
#excluimos actor3_num_films_missing porque tiene una correlación con num_actores de un -0.957, lo que
#indica una colinealidad casi perfecta. Si se incluyen ambas variables en el modelo, aparece otra singularidad.

# Variables predictoras para regresión logística
vars_predictoras_RLog <- setdiff(names(train_RLog_scaled), vars_exc_RLog)

# 1.2. Entrenamiento del modelo con cross validation

set.seed(42)
modelo_RLog <- train(
  x = as.data.frame(train_RLog_scaled[, ..vars_predictoras_RLog]),
  y = train_RLog_scaled$exito_factor,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC" #en la cross-validation se optimiza por AUC-ROC en vez de por accuracy
)

# 1.3. Resultados

print(modelo_RLog) #Resultados de cross validation
#Modelo entrenado con 8429 observaciones y 58 predictoras. AUC-ROC en cross validation es 0.666, con sensibilidad de 0.420
#y especificidad de 0.789. Esto sugiere que el modelo identifica mucho mejor los fracasos (especificidad alta) que los éxitos (sensibilidad baja)

# Resumen del modelo
summary(modelo_RLog)
#Resultados de los coeficientes (solamente se muestran los que son significativos, es decir, p-valor<0.05)
#Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                -1.005875   0.210969  -4.768 1.86e-06 ***    
# runtimeMinutes              0.330381   0.030682  10.768  < 2e-16 ***   
# is_major                    0.611215   0.059915  10.201  < 2e-16 ***   
# director_roi_medio          0.136363   0.028535   4.779 1.76e-06 ***
# num_writers                 0.141927   0.027306   5.198 2.02e-07 ***
# writer_num_films            0.073799   0.029160   2.531 0.011380 *  
# writer_roi_medio            0.161942   0.029306   5.526 3.28e-08 ***  
# actor1_roi_medio            0.096167   0.025603   3.756 0.000173 ***  
# actor2_roi_medio            0.077972   0.025108   3.105 0.001900 ** 
# actor3_num_films            0.098777   0.033759   2.926 0.003434 ** 
# actor3_roi_medio            0.063377   0.024733   2.562 0.010393 *     
# genre_comedy                0.445966   0.087158   5.117 3.11e-07 ***
# genre_action                0.203483   0.093262   2.182 0.029121 *  
# genre_adventure             0.207297   0.096703   2.144 0.032061 *  
# genre_romance               0.212747   0.094562   2.250 0.024460 *  
# genre_thriller              0.335655   0.095111   3.529 0.000417 ***
# genre_horror                0.570221   0.107058   5.326 1.00e-07 ***
# genre_mystery               0.277523   0.107632   2.578 0.009925 **    
# genre_animation             0.350604   0.135895   2.580 0.009881 ** 
# log_budget                 -0.340532   0.036312  -9.378  < 2e-16 ***   
# actor3_roi_medio_missing    0.176318   0.074176   2.377 0.017453 *      
# release_month6              0.226013   0.123065   1.837 0.066280 .  
# release_month7              0.239864   0.123363   1.944 0.051850 .     
# release_month9             -0.235895   0.116114  -2.032 0.042196 *  
# release_month10            -0.315099   0.115522  -2.728 0.006379 **    
# original_languagefr        -0.822190   0.152836  -5.380 7.47e-08 ***
# original_languagehi        -0.298844   0.138165  -2.163 0.030546 *  
# original_languageru        -0.454254   0.180579  -2.516 0.011885 *  

#Los resultados son coherentes con lo visto en el EDA. Por ejemplo, que is_major empuja mucho hacia éxito, así como los géneros de horror,
#comedy y animation o la duración de la película. log_budget empuja hacia fracaso, así como el mes de septiembre o el idioma francés.

# Predicciones en test
pred_log_prob <- predict(modelo_RLog, newdata = as.data.frame(test_RLog_scaled[, ..vars_predictoras_RLog]), type = "prob")[, "exito"]
pred_log_clase <- predict(modelo_RLog, newdata = as.data.frame(test_RLog_scaled[, ..vars_predictoras_RLog]))

# Matriz de confusión
cm_RLog <- confusionMatrix(pred_log_clase, test_RLog_scaled$exito_factor, positive = "exito")
print(cm_RLog)
#          Reference
# Prediction fracaso exito
#    fracaso     987   512
#    exito       231   376
#Accuracy de 0.6472 con diferencia estadísticamente significativa (p-valor < 0.05)
#sensibilidad de 0.423 (el modelo solo identifica correctamente el 42% de los éxitos reales)
#especificidad de 0.81 (el modelo identifica correctamente el 81% de los fracasos)
#Esto confirma que el modelo es conservador, es decir, tiende a predecir fracaso. 

#De 1218 fracasos reales (en test), se detectan correctamente 987 y erróneamente 231.
#De 888 éxitos reales (en test), se detectan correctamente 376 y erróneamente 512.

#F1 score
precision_RLog <- cm_RLog$byClass["Pos Pred Value"]
recall_RLog <- cm_RLog$byClass["Sensitivity"]

f1_RLog <- 2 * precision_RLog * recall_RLog / (precision_RLog + recall_RLog)
cat("F1 score:", round(f1_RLog, 4), "\n")
#F1 = 0.503, esto refleja el bajo recall (la sensibilidad)

# AUC-ROC
roc_RLog <- roc(response = test_RLog_scaled$exito_factor,
              predictor = pred_log_prob,
              levels = c("fracaso", "exito"),
              direction = "<")
auc(roc_RLog)
#AUC-ROC = 0.676. Es mejor que el azar aunque tiene margen de mejora y consistente
#con el 0.666 del cross.validation, lo que indica que el modelo no está sobreajustado

# Curva ROC
png("02_graficos_modelado/01_curva_ROC_RLogistica.png", width = 800, height = 800, res = 150)
plot(roc_RLog, main = "Curva ROC - Regresión Logística", col = "steelblue")
dev.off()
#La curva está claramente por encima de la diagonal, confirmando que el modelo tiene poder predictivo real.
#La forma de la curva muestra que el modelo funciona mejor en el rango de alta especificidad (parte izquierda),
#coherente con la matriz de confusión.

## Resumen de conclusiones:
#AUC-ROC: 0.676
#F1: 0.503
# Accuracy: 0.647
#Sensibilidad: 0.423
#Especificidad: 0.810


# 2. Random Forest.

# 2.1. Preparación previa.

#Conjuntos de train y test para Random Forest
train_rf <- datos_rf[train_index, ]
test_rf <- datos_rf[-train_index, ]

#Variables predictoras
vars_exc_RF <- c("tconst", "primaryTitle", "log_ROI", "exito", "exito_factor")
vars_predictoras_RF <- setdiff(names(train_rf), vars_exc_RF)

# 2.2. Búsqueda de hiperparámetros.

p <- length(vars_predictoras_RF)
mtry_default <- round(sqrt(p))

tune_grid_rf <- expand.grid(mtry = unique(c(max(2, mtry_default - 3), max(2, mtry_default - 1),
                                        mtry_default, mtry_default + 2, mtry_default + 4)
                                        )
                            )

#Probamos distintos valores de mtry y no de ntree por la siguiente razón: ntree es un parámetro de convergencia (a partir de
#cierto número de árboles el error se estabiliza y añadir más árboles solo aumenta el tiempo de cómputo sin mejorar el modelo).
#Con 500 árboles prácticamente cualquier dataset ha convergido luego probarlo en un grid no tiene sentido porque más siempre
#es igual o mejor, nunca peor, y el único coste es tiempo. Sin embargo, mtry es un parámetro de complejidad que tiene un óptimo
#real: demasiado bajo hace que los árboles sean muy débiles y demasiado alto introduce correlación entre árboles (lo cual perjudica
#al beneficio del ensemble). Es decir, existe un óptimo y este depende de los datos, luego se necesita buscarlo.

# 2.3. Entrenamiento.

set.seed(42)
modelo_RF <- train(
  x = as.data.frame(train_rf[, ..vars_predictoras_RF]),
  y = train_rf$exito_factor,
  method = "rf",
  trControl = ctrl,
  tuneGrid = tune_grid_rf,
  metric = "ROC",
  ntree = 500,
  preProcess = "medianImpute" #Imputación interna por la mediana (los NAs no se pueden dejar como NA)
)

# 2.4. Resultados.

print(modelo_RF)
# Modelo entrenado con 8429 observaciones y 36 predictoras. El número de variables óptimas en cada split (mtry) es 3, número con el cual
#obtenemos el mejor AUC-ROC, de 0.687 (que mejora ligeramente respecto a regresión logística con un 0.666). De hecho, a mayor mtry, el AUC-ROC baja,
#lo que sugiere que el modelo generaliza mejor con pocas variables en cada split (un mytr bajo ayuda a que no siempre dominen las mismas variables en cada árbol,
#aumentando la diversidad y mejorando la generalización). Los árboles individuales son más débiles pero más diversos entre sí (cada uno ve subconjuntos
#distintos de variables), haciendo que al promediarlos el ensemble generalice mejor.

#Importancia de variables (qué variables contribuyen más a predecir exito_factor)
varImp(modelo_RF)
# COMENTARIOS.
#Overall indica la importancia relativa (a mayor valor más útil dicha variable), está escalada para que la más importante valga 100
#las variables más importantes ayudan más a separar entre éxito o fracaso, que no es lo mismo que decir que las variables causen el éxito
#Las variables más importantes son release_month, log_budget y runtimeMinutes, seguidas de las variables de historial del director, escritor y actores.
#No se puede asumir que estas 3 primeras variables sean las que mejor predicen el éxito, sino que son las más consistentemente disponibles
#(las variables de historial tienen una cantidad demasiado grande de NA, y aunque el modelo los trata internamente, asigna menos importancia
#a estas variables porque al estar imputadas se pierde variabilidad, es decir, muchos valores son iguales).
#Aún así, los resultados de validación muestran que este modelo está capturando mejor las relaciones reales entre las variables y el éxito.

#Graficamos la importancia del modelo
ggplot(varImp(modelo_RF)$importance %>% 
         tibble::rownames_to_column("variable") %>%
         arrange(desc(Overall)) %>%
         head(20),
       aes(x = reorder(variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de variables - Random Forest",
       x = "Variable", y = "Importancia") +
  theme_minimal()

ggsave("02_graficos_modelado/02_importancia_variables_RF.png", width = 8, height = 5, dpi = 300)

#Predicciones en test
pred_RF_prob <- predict(modelo_RF, newdata = as.data.frame(test_rf[, ..vars_predictoras_RF]), type = "prob")[, "exito"]
pred_RF_clase <- predict(modelo_RF, newdata = as.data.frame(test_rf[, ..vars_predictoras_RF]))

#Matriz de confusión y F1
cm_RF <- confusionMatrix(pred_RF_clase, test_rf$exito_factor, positive = "exito")
print(cm_RF)
#           Reference
# Prediction fracaso exito
#    fracaso    1004   502
#    exito       214   386
#Accuracy de 0.66 (mejor que en regresión con 0.647).
#Sensibilidad de 0.435 vs 0.423 de regresión.
#Especificidad de 0.824 vs 0.810 de regresión.
#Se repite el patrón de que se identifican mejor los fracasos que los éxitos (1004 de 1218 fracasos bien identificados frente a 386 de 888 éxitos bien identificados)

precision_RF <- cm_RF$byClass["Pos Pred Value"]
recall_RF <- cm_RF$byClass["Sensitivity"]
f1_RF <- 2 * precision_RF * recall_RF / (precision_RF + recall_RF)
cat("F1 score Random Forest:", round(f1_RF, 4), "\n")
#F1-score de 0.519, mejor que el 0.503 de regresión logística.

#AUC-ROC
roc_RF <- roc(response = test_rf$exito_factor, predictor = pred_RF_prob,
              levels = c("fracaso", "exito"), direction = "<")
auc(roc_RF)
#AUC-ROC de 0.696, mejor que el 0.676 de regresión logística.

#Curva ROC
png("02_graficos_modelado/04_curva_ROC_RF.png", width = 800, height = 800, res = 150)
plot(roc_RF, main = "Curva ROC - Random Forest", col = "steelblue")
dev.off()
#Prácticamente la misma forma que la de regresión logística aunque ligeramente mejor (del 0.676 al 0.696)

#Visualizamos el error OOB (estimación del error de generalización que Random Forest calcula internamente sin necesidad de cross-validation)
RF_final <- modelo_RF$finalModel

oob_data <- data.frame(
  Trees = rep(1:nrow(RF_final$err.rate), times=3),
  Type = rep(c("OOB", "exito", "fracaso"), each =nrow(RF_final$err.rate)),
  Error = c(RF_final$err.rate[,"OOB"], RF_final$err.rate[, "exito"], RF_final$err.rate[, "fracaso"])
)

ggplot(oob_data, aes(x = Trees, y= Error, color =Type)) +
  geom_line() +
  scale_color_manual(values = c("OOB" = "steelblue", "exito" = "seagreen", "fracaso" = "tomato")) +
  labs(title = "Error OOB según número de árboles - Random Forest",
      x = "Número de árboles",
      y = "Error OOB",
      color = "Clase") +
  theme_minimal()

ggsave("02_graficos_modelado/05_oob_error_RF.png", width = 8, height = 5, dpi = 300)
#Podemos observar el claro desequilibrio entre ambas clases: el error al clasificar éxitos es muy alto (entre 0.55 y 0.6),
#es decir, el modelo falla en más de la mitad de los éxitos reales y, sin embargo, el error al clasificar fracasos es muy
#bajo, menos de 0.2, es decir, el modelo identifica bien los fracasos. A su vez, el error global del modelo (OOB) se
#estabiliza por debajo de 0.35 tras unos 200 árboles, confirmando que 500 árboles es más que suficiente.
#El patrón de clasificación de fracasos y éxitos visto en los valores de especificidad y sensibilidad se corroboran con este gráfico.

## Resumen de conclusiones:
#AUC-ROC: 0.696
#F1: 0.519
#Accuracy: 0.66
#Sensibilidad: 0.435
#Especificidad: 0.824

# 3. XGBoost.

# 3.1. Preparación previa.

datos_XGB[, release_month := as.integer(release_month)] #Convertimos release_month en variable numérica

#Creamos dummies del idioma como en regresión logística
dummies_idioma_XGB <- as.data.table(model.matrix(~ original_language, data = datos_XGB))
dummies_idioma_XGB[, `(Intercept)` := NULL]

datos_XGB <- cbind(datos_XGB, dummies_idioma_XGB)
datos_XGB[, "original_language" := NULL]

#Split train test con los índices train_index
train_XGB <- datos_XGB[train_index, ]
test_XGB  <- datos_XGB[-train_index, ]

#Variables predictoras
vars_exc_XGB <- c("tconst", "primaryTitle", "log_ROI", "exito", "exito_factor") #igual que vars_exc_RF
vars_predictoras_XGB <- setdiff(names(train_XGB), vars_exc_XGB)

# 3.2. Búsqueda de hiperparámetros. (1)

tune_grid_XGB <- expand.grid(
  nrounds          = c(100, 200, 300),  # número de árboles
  max_depth        = c(3, 5, 7),        # profundidad máxima de cada árbol
  eta              = c(0.01, 0.05, 0.1),# learning rate
  gamma            = 0,                 # umbral mínimo de ganancia para hacer un split (lo fijamos en 0)
  colsample_bytree = c(0.6, 0.8),       # fracción de variables por árbol, introduce aleatoriedad (como mtry en RF) reduciendo la correlación entre árboles y mejorando la generalización
  min_child_weight = 1,                 # mínimo de observaciones en nodo hoja (lo fijamos en 1)
  subsample        = c(0.7, 0.9)        # fracción de observaciones por árbol, igual que colsample_bytree pero por filas
)

nrow(tune_grid_XGB) # Para saber cuántas combinaciones estamos probando, 3 × 3 × 3 × 1 × 2 × 1 × 2 = 108 combinaciones × 5 folds = 540 modelos

# 3.3. Entrenamiento (1)

# Usamos el mismo ctrl que en los modelos anteriores
set.seed(42)
modelo_XGB <- train(
  x = as.data.frame(train_XGB[, ..vars_predictoras_XGB]),
  y = train_XGB$exito_factor,
  method    = "xgbTree",
  trControl = ctrl,           
  tuneGrid  = tune_grid_XGB,
  metric    = "ROC",
  verbosity = 0          # evita que XGBoost imprima mensajes en cada árbol
)

# Hiperparámetros óptimos y resultados de CV
print(modelo_XGB)
#Los hiperparámetros óptimos son nrounds=300, max_depth = 7, eta = 0.01, colsample_bytree = 0.6 y subsample = 0.7. Con dichos hiperparámetros óptimos se tiene un AUC-ROC de 0.6873.
#Al tener óptimos en los extremos de los valores nrounds y max_depth, reentrenamos buscando alrededor de dichos valores puesto que el modelo podría seguir mejorando en esas
#direcciones y explorar más allá podría dar mejores resultados. La segunda búsqueda nos asegura encontrar el verdadero óptimo en vez de simplemente el mejor valor dentro de un rango insuficiente.
#Mantenemos los óptimos obtenidos para eta (valores más altos empeoran el rendimiento, el modelo aprende mejor de forma conservadora, es decir dando pequeños pasos en cada árbol) y subsample

png("02_graficos_modelado/06_modelo_XGB.png", width = 1200, height = 1000, res = 150)
plot(modelo_XGB) # visualiza cómo varía el AUC-ROC según los hiperparámetros
dev.off()

# 3.2. Búsqueda de hiperparámetros (2)
tune_grid_XGB_v2 <- expand.grid(
  nrounds          = c(300, 500, 700),
  max_depth        = c(7, 9),
  eta              = 0.01,
  gamma            = 0,
  colsample_bytree = c(0.4, 0.6),
  min_child_weight = 1,
  subsample        = c(0.5, 0.7)
)

nrow(tune_grid_XGB_v2) # 24 combinaciones x 5 folds = 120 modelos

# 3.3. Entrenamiento (2)
set.seed(42)
modelo_XGB <- train(
  x = as.data.frame(train_XGB[, ..vars_predictoras_XGB]),
  y = train_XGB$exito_factor,
  method    = "xgbTree",
  trControl = ctrl,         
  tuneGrid  = tune_grid_XGB_v2,
  metric    = "ROC",
  verbosity = 0
)

# Hiperparámetros óptimos y resultados de CV
print(modelo_XGB)
#El óptimo se alcanza en nrounds=500, max_depth=7, colsample_bytree=0.6 y subsample=0.5, con un AUC-ROC de 0.691. Sin embargo, si se fijan todos
#los hiperparámetros salvo subsample y se busca el óptimo de este, se obtiene subsample=0.7, con una diferencia de AUC-ROC de 0.001. Esto significa
#que realmente no hay diferencias entre ambos valores para este problema, luego tomamos subsample = 0.7 para mantener la aleatoriedad (se entrena cada
#árbol con el 70% de observaciones en lugar del 50%). Se confirman los óptimos encontrados y se utilizan estos mismos.

png("02_graficos_modelado/07_modelo_XGB_v2.png", width = 800, height = 800, res = 150)
plot(modelo_XGB) # visualiza cómo varía el AUC-ROC según los hiperparámetros
dev.off()

# 3.4. Modelo final
tune_grid_XGB_final <- expand.grid(nrounds = 500, max_depth = 7, eta = 0.01, gamma = 0,
                                colsample_bytree = 0.6, min_child_weight = 1, subsample = 0.7)

set.seed(42)
modelo_XGB <- train(
  x = as.data.frame(train_XGB[, ..vars_predictoras_XGB]),
  y = train_XGB$exito_factor,
  method    = "xgbTree",
  trControl = ctrl,         
  tuneGrid  = tune_grid_XGB_final,
  metric    = "ROC",
  verbosity = 0
)

# 3.5. Resultados

print(modelo_XGB)
#Efectivamente, se mantiene un ROC de 0.691 con los parámetros óptimos.

# Predicciones
pred_XGB_prob  <- predict(modelo_XGB, newdata = as.data.frame(test_XGB[, ..vars_predictoras_XGB]), type = "prob")[, "exito"] #probabilidad de éxito
pred_XGB_clase <- predict(modelo_XGB, newdata = as.data.frame(test_XGB[, ..vars_predictoras_XGB]))

# Matriz de confusión
cm_XGB <- confusionMatrix(pred_XGB_clase, test_XGB$exito_factor, positive = "exito")
print(cm_XGB)
#          Reference
# Prediction fracaso exito
#    fracaso    1001   494
#    exito       217   394

#Accuracy de 0.662, con sensibilidad de 0.444 y especificidad de 0.822
#De 1218 fracasos, 1001 se identifican correctamente. De 888 éxitos, 394 se identifican correctamente.

# F1 score
precision_XGB <- cm_XGB$byClass["Pos Pred Value"]
recall_XGB    <- cm_XGB$byClass["Sensitivity"]
f1_XGB        <- 2 * precision_XGB * recall_XGB / (precision_XGB + recall_XGB)
cat("F1 score XGBoost:", round(f1_XGB, 4), "\n")
#f1 de 0.5257

# AUC-ROC
roc_XGB <- roc(response    = test_XGB$exito_factor,
               predictor   = pred_XGB_prob,
               levels      = c("fracaso", "exito"),
               direction   = "<")
auc(roc_XGB)
#AUC-ROC de 0.7029

# Curva ROC
png("02_graficos_modelado/08_curva_ROC_XGB.png", width = 800, height = 800, res = 150)
plot(roc_XGB, main = "Curva ROC - XGBoost", col = "steelblue")
dev.off()

## Resumen de conclusiones:
#AUC-ROC: 0.703
#F1: 0.526
#Accuracy: 0.662
#Sensibilidad: 0.444
#Especificidad: 0.822

### 
#XGBoost tiene el mejor AUC-ROC en CV y en test, aunque los resultados con RF no difieren mucho, lo que sugiere que
#ambos modelos son equivalentes en capacidad predictiva real. El patrón de sesgo hacia fracasos se mantiene en los 3
#modelos (especificidad mayor o igual que 0.80 en los 3 modelos y sensibilidad entre 0.42 y 0.45).


# 3.6. Importancia de variables nativa de XGBoost

xgb_booster <- modelo_XGB$finalModel #Extraemos el modelo interno xgb.Booster

# XGBoost tiene tres métricas de importancia distintas:
# - Gain: reducción media del error que aporta cada variable (la más informativa) Mide cuánto reduce el error cada variable en los splits del árbol
# - Cover: número de observaciones afectadas por los splits de esa variable
# - Frequency: número de veces que aparece la variable en los árboles

importancia_XGB <- xgb.importance(model = xgb_booster)
print(importancia_XGB)
##El orden de importancia de las variables predictoras coincide más con lo esperado: las variables más importantes son log_budget seguida de
#runtimeMinutes, actor1_roi_medio, director_roi_medio, y writer_roi_medio. Las dos siguientes son actor2_roi_medio y actor3_roi_medio.
#Que las variables de historial sean más importantes es mucho más intuitivo y coherente con el EDA (que en RF) y además refuerza la hipótesis
#de que la importancia de variables de RF está influenciada por la variabilidad disponible de cada variable (que disminuye si las variables están
#imputadas en su gran mayoría). Los NA de las variables de historial hacen que se reduzca artificialmente su importancia. En cambio, XGBoost asigna
#importancia basándose en la ganancia real de información de cada split, independientemente de cuántas observaciones haya disponibles, proporcionando un resultado más justo.

#El reparto completo aporta información predictiva, aunque evidentemente con peso decreciente del actor principal al terciario (como se ve en los scatterplot del EDA).

#Las variables release_month, decade e is_major ocupan los puestos 7, 8 y 9 respectivamente, con una importancia moderada pero clara. 

#En cuanto a los géneros más importantes, lo son comedy, drama y horror. Su importancia es baja pero no nula.

#Las variables con menor importancia (prácticamente nula) son num_actores, runtimeMissing y genre_other. Probablemente eliminarlas del modelo no supondría una pérdida en el rendimiento.


# Gráfico de importancia (top 20 variables por Gain)
xgb_imp_plot <- importancia_XGB[1:20]

ggplot(xgb_imp_plot, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de variables - XGBoost (Gain)",
       x = "Variable", y = "Gain medio") +
  theme_minimal()

ggsave("02_graficos_modelado/09_importancia_variables_XGB.png", width = 8, height = 8, dpi = 300)

# 3.7. SHAP values

#Los SHAP values te dicen cuánto contribuye cada variable a cada predicción individual, con signo positivo si empuja hacia éxito y negativo si empuja hacia fracaso.
#Miden cuánto desplaza cada variable la predicción final de cada observación individualmente.

#Para poder calcularlos correctamente, necesitamos reentrenar el booster nativo de XGBoost pues el booster interno predice probabilidad de fracaso
#mientras que pred_XGB_prob predice probabilidad de éxito, invirtiendo los valores de SHAP (se calculan respecto a fracaso y no respecto a éxito)
X_train_matrix <- as.matrix(train_XGB[, ..vars_predictoras_XGB])
X_test_matrix  <- as.matrix(test_XGB[, ..vars_predictoras_XGB])

y_train_xgb <- as.integer(train_XGB$exito == 1)
y_test_xgb <- as.integer(test_XGB$exito == 1)

train_xgb_matrix <- xgb.DMatrix(data = X_train_matrix, label = y_train_xgb)
test_xgb_matrix <- xgb.DMatrix(data = X_test_matrix, label = y_test_xgb)

#Utilizamos los parámetros óptimos del grid search
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = modelo_XGB$bestTune$max_depth,
  eta = modelo_XGB$bestTune$eta,
  gamma = modelo_XGB$bestTune$gamma,
  colsample_bytree = modelo_XGB$bestTune$colsample_bytree,
  min_child_weight = modelo_XGB$bestTune$min_child_weight,
  subsample = modelo_XGB$bestTune$subsample
)

set.seed(42)
xgb_booster_for_SHAP <- xgb.train(
  params = params,
  data = train_xgb_matrix,
  nrounds = modelo_XGB$bestTune$nrounds,
  verbose = 0
)

# Calculamos SHAP values sobre el conjunto de test
shp <- shapviz(xgb_booster_for_SHAP, X_pred = X_test_matrix, X = X_test_matrix)
#Los SHAP values se expresan en escala de log-odds (es la transformación logarítimica del cociente entre la probabilidad de éxito y la de fracaso)
#log-odds = log( P(exito)/P(fracaso) ). La relación entre log-odds y probabilidad es: log-odds = 0 -> probabilidad = 0.5
#log-odds > 0 -> probabilidad > 0.5 (más probable éxito) y log-odds < 0 -> probabilidad < 0.5 (más probable fracaso). Luego en los siguientes gráficos
#SHAP positivo significa que esa variable empuja la predicción hacia probabilidades mayores de 0.5 (es decir hacia éxito), SHAP negativo significa que
#esa variable empuja hacia probabilidades menores de 0.5 (es decir hacia fracaso) y SHAP = 0 significa que esa variable no cambia la predicción respecto a la media

# Gráfico 1: importancia global por SHAP.

sv_importance(shp, kind = "bar", max_display = 20)
ggsave("02_graficos_modelado/11_shap_importancia_global.png", width = 8, height = 6, dpi = 300)

# COMENTARIOS.
#Las variables con mayor importancia por SHAP son log_budget, is_major y runtime_Minutes. Esto quiere decir que estas variables son las que
#producen los desplazamientos más grandes en la predicción de cada película concreta, (log_budget era la que más reduce el error en los splits según el gain).


# Gráfico 2: beeswarm plot.
# Muestra para cada variable tanto su importancia como la dirección del efecto. Cada punto es una observación de test. El color
#indica el valor de la variable (amarillo = valor alto, morado = valor bajo) y la posición en x indica si empuja hacia éxito (positivo) o fracaso (negativo).

sv_importance(shp, kind = "beeswarm", max_display = 20)
ggsave("02_graficos_modelado/12_shap_beeswarm.png", width = 8, height = 6, dpi = 300)

# COMENTARIOS.
#log_budget: presupuestos altos (amarillos) tienden a empujar menos hacia éxito que los bajos (morados)
#is_major: puntos morados (is_major=0) a la izquierda, amarillos (is_major=1) a la derecha. Clara separación entre ambos valores dividido por SHAP=0. Ser major empuja hacia éxito de forma clara.
#runtimeMinutes: puntos morados (películas cortas) a la izquierda, amarillos (largas) a la derecha. Mayor duración empuja hacia éxito.

#writer_roi_medio, director_roi_medio y actor1_roi_medio: historial positivo (amarillos) a la derecha, negativo (morados) a la izquierda. Mejor historial empuja hacia éxito. Hay una gran
#concentración de NAs (zonas grises) alrededor de 0. actor2 y actor3 siguen este patrón aunque con menos impacto.

# decade: últimas décadas (amarillos) a la izquierda pero con mucha dispersión y décadas más antiguas a la derecha (morados). Décadas más antiguas empujan hacia éxito y más recientes
#hacia fracaso. Coherente con el EDA (el impacto del COVID y del streaming, además de la última década incompleta)

#genre_comedy y genre_drama: tienen efectos contrarios. comedia tira al éxito (amarillos derecha morados izquierda) y drama al contrario. Separaciones muy claras en estas dos variables.
#además, genre_horror empuja mucho hacia éxito (muy clara separación también), coherente con el EDA.


# Gráfico 3: Dependencia parcial de la variable más importante.
# Muestra cómo varía el SHAP value de log_budget según su valor, coloreado por la segunda variable más importante para ver interacciones

sv_dependence(shp, v = "log_budget", color_var = "auto")
ggsave("02_graficos_modelado/13_shap_dependencia_log_budget.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#Los presupuestos bajos (log_budget<12, es decir, menos de 160 mil dolares) tienen un SHAP positivo alto. Las películas con presupuesto muy bajo que
#llegan a tener datos en TMDb consiguieron recaudar suficiente respecto a su coste. Los presupuestos medios-altos (entre 12 y 17) tienen un SHAP que
#baja progresivamente hacia 0 y negativo. Los presupuestos muy altos (más de 17, es decir, más de 34 millones de dólares) tienen un SHAP que sube
#ligeramente de nuevo, especialmente para majors (puntos amarillos).


# Gráfico 4: explicación de una predicción individual
# Muestra qué variables empujaron hacia éxito o fracaso para una película concreta

sv_waterfall(shp, row_id = 1)
ggsave("02_graficos_modelado/14_shap_waterfall_pelicula1.png", width = 8, height = 5, dpi = 300)

# COMENTARIOS.
#La predicción base del modelo es E[f(x)] = -0.323, que corresponde a una probabilidad de 1/(1+exp(0.323)) = 0.42, es decir el modelo parte de una probabilidad base
#del 42% de éxito para cualquier película. La predicción final para dicha película es f(x) = 0.516, que corresponde a una probabilidad de 1/(1+exp(-0.516)) = 0.626,
#es decir, el modelo predice un 62,6% de éxito para esta película en concreto.

#Las variables que más empujan hacia el éxito son log_budget, decade y genre_comedy, que valen 11.4, 1980 y 1 respectivamente (en este caso concreto)
#Las variables que más empujan hacia el fracaso son runtimeMinutes e is_major, que en este caso valen 85 y 0 respectivamente.
#En este caso las contribuciones positivas superan a las negativas proporcionando una predicción final positiva que se traduce en una probabilidad de éxito del 62,6%.


# 4. Comparación de modelos.

comparacion <- data.frame(
  Modelo = c("Regresión Logística", "Random Forest", "XGBoost"),
  AUC_ROC = c(as.numeric(auc(roc_RLog)), as.numeric(auc(roc_RF)), as.numeric(auc(roc_XGB))),
  F1_score = c(f1_RLog, f1_RF, f1_XGB),
  Accuracy = c(cm_RLog$overall["Accuracy"], cm_RF$overall["Accuracy"], cm_XGB$overall["Accuracy"]),
  Sensibilidad = c(cm_RLog$byClass["Sensitivity"], cm_RF$byClass["Sensitivity"], cm_XGB$byClass["Sensitivity"]),
  Especificidad = c(cm_RLog$byClass["Specificity"], cm_RF$byClass["Specificity"], cm_XGB$byClass["Specificity"])
)

comparacion[, 2:6] <- round(comparacion[, 2:6], 4)

print(comparacion)

#Gráfico comparativo de AUC-ROC
ggplot(comparacion, aes(x=reorder(Modelo, AUC_ROC), y=AUC_ROC, fill=Modelo)) +
  geom_bar(stat = "identity", width=0.5) +
  geom_text(aes(label=round(AUC_ROC, 3)), hjust=-0.1, size=4) +
  coord_flip() +
  scale_y_continuous(limits= c(0,0.8)) +
  labs(title = "Comparación de modelos por AUC-ROC",
       x = "", y = "AUC-ROC") +
  theme_minimal() +
  theme(legend.position ="none")
#Podemos ver perfectamente que a pesar de que RF y XGB superan a RL, las diferencias son pequeñas (rango de menos de 0.03)
ggsave("02_graficos_modelado/16_comparacion_modelos.png", width = 8, height = 6, dpi = 300)

# Curvas ROC superpuestas
png("02_graficos_modelado/17_curvas_ROC_comparacion.png", width = 800, height = 800, res = 150)
plot(roc_RLog, col = "tomato", main = "Comparación curvas ROC")
lines(roc_RF, col = "steelblue")
lines(roc_XGB, col = "seagreen")
legend("bottomright", 
       legend = c(paste("Reg. Logística (AUC =", round(auc(roc_RLog), 3), ")"),
                  paste("Random Forest (AUC =", round(auc(roc_RF), 3), ")"),
                  paste("XGBoost (AUC =", round(auc(roc_XGB), 3), ")")),
       col = c("tomato", "steelblue", "seagreen"),
       lwd = 2)
dev.off()

# COMENTARIOS COMPARACIÓN MODELOS (TABLA Y GRÁFICOS).

#RF y XGB difieren en 0.0066 en AUC, en la práctica ambos modelos son equivalentes en campacidad predictiva.
#En efecto, en las curvas ROC superpuestas se observa que las correspondientes a estos dos métodos son prácticamente iguales.

#La regresión logística queda claramente por detrás, confirmando que las relaciones entre variables predictoras y éxito
#no son lineales y los modelos basados en árboles capturan mejor los patrones subyacentes.

#El patrón de sesgo hacia fracasos se mantiene en los 3 modelos (todos tienen especificidad alta y sensibilidad baja), lo que significa que todos los modelos
#identifican bien los fracasos pero fallan en más de la mitad de los éxitos reales (el éxito comercial de una película es realmente difícil de predecir antes del estreno)


#Test de DeLong (compara si dos AUCs son significativamente distintos entre sí)
roc.test(roc_RF, roc_RLog)   # RF vs Logística. p-valor de 0.01763 < 0.05 --> Diferencia de AUCs estadísticamente significativa
roc.test(roc_XGB, roc_RLog)  # XGBoost vs Logística. p-valor de  0.0008046 < 0.05 --> Diferencia de AUCs estadísticamente significativa
roc.test(roc_RF, roc_XGB)   # RF vs XGBoost. p-valor de 0.228 > 0.05 --> Diferencia de AUCs no estadísticamente significativa (modelos equivalentes en capacidad predictiva real)

#Los modelos basados en árboles (RF y XGB) superan significativamente a la regresión logística según el test de DeLong (p < 0.05 en ambos casos), mientras que la
#diferencia entre RF y XGB no es estadísticamente significativa (p=0.228), confirmando que ambos modelos son equivalentes en capacidad predictiva para este problema.
library(shiny)
library(geosphere)
library(leaflet)
library(dplyr)
library(ggmap)
library(purrr)
library(devtools)
library(jsonlite)
library(dplyr)
library(tidyr)
library(usethis)


setwd("/Users/noa/Desktop/NavigaNantes/")
arrets_tan <- jsonlite::fromJSON("https://open.tan.fr/ewp/arrets.json")
arrets_tan <- arrets_tan[,-3]

arrets_tan_filtre <- arrets_tan %>% unnest(ligne, keep_empty = TRUE)

#' @title is_station
#' @description The function takes an alphabetic code as input and returns a logical value indicating whether the station exists.
#' @param code Character string representing the station code.
#' @return A logical value indicating whether the station exists.
#' @examples
#' is_station("ABCH")
#' is_station("MONERO")

is_station <- function(code) {
  exists <- code %in% arrets_tan$codeLieu
  return(exists)
}

#' @title find_closest_name
#' @description The function takes two inputs, input and list_stations. It calculates the Levenshtein distance between the input and all elements of list_stations. The station with the smallest distance is considered the closest station and is returned. If the distance to the nearest station is 0, it means that the input is exactly equal to the station name and the station name is returned. If the distance is less than or equal to the length of the entry, the nearest station is returned. Otherwise, NA is returned.
#' @import stringdist
#' @param input A string representing the input value.
#' @param list_stations A vector of strings representing station names.
#' @return A string or NA, representing the name of the nearest station or NA if no close match is found.

find_closest_name <- function(input, list_stations) {
  library(stringdist)
  closest_stations <- stringdist(input, list_stations, method = "lv")
  closest_station_index <- which.min(closest_stations)
  closest_station_name <- list_stations[closest_station_index]
  if (closest_stations[closest_station_index] == 0) {
    closest_station_name
  } else if (closest_stations[closest_station_index] <= nchar(input)) {
    closest_station_name
  } else {
    NA
  }
}

#' @title get_waiting_time
#' @description Retrieves information about the waiting time of the next trams
#' @param locationcode The station code for which the waiting time for the next trams is required
#' @return A dataframe with information on the terminus and the time of the next trams
get_waiting_time <- function(codeLieu) {
  library(jsonlite)
  api_url <- paste0("https://open.tan.fr/ewp/tempsattente.json/", codeLieu)
  api_response <- fromJSON(api_url)
  
  api_response_subset <- api_response[c("terminus", "temps")]
  return(api_response_subset)
}

#' @title get_next_transport at Nantes
#' @description This function allows you to obtain information on the schedules of the next trams
#' @param input A character string representing the full station name, the "libelle"
#' @examples get_next_transport("Bourgeonnière")
#' 1      Orvault Grand Val   2mn
#' @return A data frame of the next passages for all lines passing through the station



get_next_transport <- function(arret) {
  library(jsonlite)
  arrets_tan <- jsonlite::fromJSON("https://open.tan.fr/ewp/arrets.json")
  arrets_tan <- arrets_tan[,-3]
  station_libelle <- find_closest_name(arret, arrets_tan$libelle)
  print(paste0("La station que vous cherchez semble être : ", station_libelle))
  output <- as.character(unlist(station_libelle))
  output2 <- subset(arrets_tan, libelle == output, select = c(codeLieu))
  output3 <- lapply(output2, function(x) x[1])
  output4 <- as.character(unlist(output3))
  next_passages <- get_waiting_time(output4)
  print('Voici la liste des prochains passages : ')
  return(head(next_passages, 5))
}

#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Chargement des BDD----
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

## Chargement de la base de données----
df <- read.csv("data/df.csv", sep = ";")
df1 <- select(df, 2, 15, 16)
df1 <- df1 %>% rename(nom = 1, latitude = 2, longitude = 3)
df1$info <- sub("^(\\w+).*", "\\1", df1$nom)

## Chargement de la BDD des musées----
musee = read.csv2("data/musees.csv")
musee = select(musee, 1,2,13,14)
musee <- musee %>% rename(nom = 1, info = 2, latitude = 3, longitude = 4)
musee <- musee[, c(1, 3, 4, 2)]

## Chargement de la BDD des resto----
resto = read.csv2("data/resto.csv")
resto = select(resto, 1,2,12,13)
resto <- resto %>% rename(nom = 1, info = 2, latitude = 3, longitude = 4)
resto <- resto[, c(1, 3, 4, 2)]
resto <- resto[resto$info != "", ]

## Chargement de la BDD des parcs----
parc = read.csv2("data/parcs.csv")
parc = select(parc, 2,3,7)
parc$latitude <- as.numeric(sub(",.*", "", parc$Géolocalisation))
parc$longitude <- as.numeric(sub(".*, ", "", parc$Géolocalisation))
parc$Géolocalisation <- NULL
parc <- parc %>% rename(nom = 1, info = 2, latitude = 3, longitude = 4)
parc <- parc[, c(1, 3, 4, 2)]

## Chargement de la BDD des velos----
velos = read.csv2("data/velos.csv")
velos <- velos %>%
  mutate(latitude = as.numeric(stringr::str_extract(position, "\\d+\\.\\d+")),
         longitude = as.numeric(stringr::str_extract(position, "-\\d+\\.\\d+")),
         info = "data/Station_vélo") %>%
  select(name, latitude, longitude, info)
velos <- velos %>% rename(nom = name)

## Chargement de la BDD des arrêtes tan----
Arrets = read.csv2("data/tan.csv")
Arrets <- Arrets %>%
  mutate(latitude = as.numeric(stringr::str_extract(Coordinates, "\\d+\\.\\d+")),
         longitude = as.numeric(stringr::str_extract(Coordinates, "-\\d+\\.\\d+")),
         info = "Arrets") %>%
  select(Name, latitude, longitude, info)
Arrets <- Arrets %>% rename(nom = Name)

## Chargement de la BDD des toilettes----
wc = read.csv2("data/wc.csv")
wc = select(wc, 3,20)
wc$info <- rep("Toilettes", nrow(wc))
wc$longitude <- as.numeric(sub(",.*", "", gsub(".*\\[", "", wc$Géométrie)))
wc$latitude <- as.numeric(sub(".*,", "", gsub("\\].*", "", wc$Géométrie)))
wc$Géométrie <- NULL
wc <- wc %>% rename(nom = 1)
wc <- wc[, c(1, 4, 3, 2)]

## Création de la BDD finale----
base <- rbind(df1, parc, resto, velos, wc, Arrets)
base <- base %>% mutate(info = ifelse(info == "section", "Section", info))
base$latitude = as.numeric(base$latitude)
base$longitude = as.numeric(base$longitude)
#long_inc <- which(base$longitude < -180 | base$longitude > 180 | is.na(base$longitude))
#base <- base[-long_inc,]

base <- filter(base, !info %in% c("Ferme auberge", "Section", "Restaurant pour groupes exclusivement", "Etablissement"))
base$info <- gsub("Hôtel restaurant", "Hôtel", base$info)



ui <- fluidPage(
  tags$head(tags$style("#titre{margin-top: 30px; font-size: 16px; font-weight: bold;}")),
  titlePanel("Nantes informations"),
  sidebarLayout(
    sidebarPanel(
      textInput("adresse", "Adresse :"),
      actionButton("trouver_point", "Chercher"),
      tableOutput("resultat_adresse")
    ),
    mainPanel(
      leafletOutput("carte"),
      tableOutput("titre"),
      DT::dataTableOutput("resultat_tan") # Modification ici
    )
  )
)


# Définition du serveur
server <- function(input, output) {
  
  register_google(key="AIzaSyDyW3CiYMeSztdMle6BgREfMthOrC8T60Q")
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 47.218371, lng = -1.553621, zoom = 12) # Centré sur Nantes
  })
  
  
  
  categories <- c("Collège", "Parc", "Restaurant", "Lycée","Ecole","Station_vélo","Toilettes","Hôtel","Arrets")
  
  trouver_point_proche_par_categorie <- function(coord, dataframe, categorie) {
    dataframe_filtre <- filter(dataframe, info == categorie)
    distances <- apply(dataframe_filtre[, c("longitude", "latitude")], 1, function(x) distHaversine(coord, x))
    indice_plus_proche <- which.min(distances)
    point_plus_proche <- dataframe_filtre[indice_plus_proche, ]
    distance_metres <- distHaversine(coord, c(point_plus_proche$longitude, point_plus_proche$latitude))
    return(list(point_plus_proche, distance_metres))
  }
  
  observeEvent(input$trouver_point, {
    if (!is.null(input$adresse)) {
      adresse <- input$adresse
      coord <- geocode(adresse)
      dataframe_gps <- base
      
      
      # Créer une liste de points plus proches pour chaque catégorie
      points_plus_proches <- lapply(categories, function(cat) {
        trouver_point_proche_par_categorie(coord, dataframe_gps, cat)
      })
      
      resultats_table <- data.frame(Catégorie = sapply(points_plus_proches, function(ppp) ppp[[1]]$info), Distance = sapply(points_plus_proches, function(ppp) ppp[[2]]))
      output$resultat_adresse <- renderTable(resultats_table)
      
      
      adresse_maker <- data.frame(latitude = coord$lat, longitude = coord$lon)
      
      # Convertir la liste points_plus_proches en un data.frame
      df_points <- purrr::map_df(points_plus_proches, function(ppp) {
        data.frame(nom = ppp[[1]]$nom, 
                   latitude = ppp[[1]]$latitude, 
                   longitude = ppp[[1]]$longitude, 
                   info = ppp[[1]]$info)
      })
      
      library(DT)
      
      output$resultat_tan <- DT::renderDataTable({
        if(sum(df_points$info == "Arrets") > 0){
          texte_a_recuperer <- df_points$nom[df_points$info == "Arrets"]
          resultat <- get_next_transport(paste0('"', texte_a_recuperer, '"'))
          DT::datatable(resultat, options = list(pageLength = 10))
        }
      })
      
      output$titre <- renderText({
        if(sum(df_points$info == "Arrets") > 0){
          texte_a_recuperer <- df_points$nom[df_points$info == "Arrets"]
          nom_arret <- paste0("<h2>Les horaires pour l'arrêt \"", texte_a_recuperer, "\"</h2>")
          return(HTML(nom_arret))
        }
      })
      
      output$carte <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = df_points, lng = ~longitude, lat = ~latitude, 
                     popup = ~paste(nom, "<br>", info),
                     icon = iconList(Lycée = makeIcon("icons/university_2.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40), 
                                     Collège = makeIcon("icons/lycee.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40), 
                                     Parc = makeIcon("icons/tree.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Ecole = makeIcon("icons/school_2.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Hôtel = makeIcon("icons/hostel.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Station_vélo = makeIcon("icons/velos.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Toilettes = makeIcon("icons/toilet.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Arrets = makeIcon("icons/tan.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Restaurant = makeIcon("icons/resto.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40))[df_points$info]) %>%
          addCircleMarkers(data = adresse_maker, lng = ~longitude, lat = ~latitude, popup = "Adresse recherchée")
      })
    }
  })
  
  observeEvent(input$carte_click, {
    clicked <- input$carte_click
    if (!is.null(clicked)) {
      longitude <- clicked$lng
      latitude <- clicked$lat
      point_reference <- c(longitude, latitude)
      dataframe_gps <- base
      
      # Créer une liste de points plus proches pour chaque catégorie
      points_plus_proches <- lapply(categories, function(cat) {
        trouver_point_proche_par_categorie(point_reference, dataframe_gps, cat)
      })
      
      resultats_table <- data.frame(Catégorie = sapply(points_plus_proches, function(ppp) ppp[[1]]$info), Distance = sapply(points_plus_proches, function(ppp) ppp[[2]]))
      output$resultat_adresse <- renderTable(resultats_table)
      
      
      adresse_maker <- data.frame(latitude = latitude, longitude = longitude)
      
      # Convertir la liste points_plus_proches en un data.frame
      df_points <- purrr::map_df(points_plus_proches, function(ppp) {
        data.frame(nom = ppp[[1]]$nom, 
                   latitude = ppp[[1]]$latitude, 
                   longitude = ppp[[1]]$longitude, 
                   info = ppp[[1]]$info)
      })
      
      output$resultat_tan <- DT::renderDataTable({
        if(sum(df_points$info == "Arrets") > 0){
          texte_a_recuperer <- df_points$nom[df_points$info == "Arrets"]
          resultat <- get_next_transport(paste0('"', texte_a_recuperer, '"'))
          DT::datatable(resultat, options = list(pageLength = 10))
        }
      })
      
      output$titre <- renderText({
        if(sum(df_points$info == "Arrets") > 0){
          texte_a_recuperer <- df_points$nom[df_points$info == "Arrets"]
          nom_arret <- paste0("<h2>Les horaires pour l'arrêt \"", texte_a_recuperer, "\"</h2>")
          return(HTML(nom_arret))
        }
      })
      
      output$carte <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = df_points, lng = ~longitude, lat = ~latitude, 
                     popup = ~paste(nom, "<br>", info),
                     icon = iconList(Lycée = makeIcon("icons/university_2.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40), 
                                     Collège = makeIcon("icons/lycee.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40), 
                                     Parc = makeIcon("icons/tree.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Ecole = makeIcon("icons/school_2.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Hôtel = makeIcon("icons/hostel.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Station_vélo = makeIcon("icons/velos.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Toilettes = makeIcon("icons/toilet.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Arrets = makeIcon("icons/tan.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40),
                                     Restaurant = makeIcon("icons/resto.png", iconWidth = 30, iconHeight = 40, iconAnchorX = 15, iconAnchorY = 40))[df_points$info]) %>%
          addCircleMarkers(data = adresse_maker, lng = ~longitude, lat = ~latitude, popup = "Adresse recherchée")
      })
    }
  })
}

# Lancer l'application Shinyy
shinyApp(ui, server)
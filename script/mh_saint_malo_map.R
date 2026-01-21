#install.packages("leaflet")
#install.packages("sp")
#install.packages("sf")
#install.packages("RColorBrewer")
#install.packages("leaflet.extras")
#install.packages("leaflet.minicharts")
#install.packages("htmlwidgets")
#install.packages("raster")
#install.packages("mapview")
#install.packages("leafem")
#install.packages("leafpop")
#install.packages("htmltools")

## librairies dont a besoin le projet
library(leaflet)
library(sp)
library(sf)
library(RColorBrewer)
library(leaflet.extras)
library(leaflet.minicharts)
library(htmlwidgets)
library(raster)
library(mapview)
library(leafem)
library(leafpop)
library(htmltools)

## PARTIE 1 - Lire les fichiers et associer les attributs aux éléments

## Lire le shapefile concernant le polygone département
departement <- st_read('data/shp/departement.shp')

## Lire le CSV concernant le data des MH
data <- read.csv("data/csv/tableau pour constituer la carte.csv")

types <- sort(unique(data$type))


## Créer les pop up pour les markers des MH
content <- paste(sep = "<br/>",
                 paste0("<div class='leaflet-popup-scrolled' style='max-width:300px;max-height:200px'>"),
                 paste0("<b>", data$name, "</b>"),
                 paste0("type de bâtiment : <b>", data$type, "</b>"),
                 paste0("siècle de construction : <b>",data$siecle_de_construction,"</b>"),
                 ifelse(!is.na(data$Personnes_liees_a_l_edifice) & data$Personnes_liees_a_l_edifice != "",
                   paste0("personne liée à l'édifice : <b>", data$Personnes_liees_a_l_edifice,"</b>"),
                   ""),
                 paste0("date de la protection : <b>", data$date_de_la_protection, "</b>"),
                 paste0("<img src='", data$image_url, "' width='200'><br>"),
                 paste0("<a href='", data$lien_vers_la_notice,"' target='_blank'>➡ Voir la notice du monument</a>"),
                 paste0("</div>"))

palette_data <- colorNumeric("YlOrRd", data$date_de_la_protection)


## PARTIE 2 - Le code ajoute les éléments comme le polygone et les points.

## Initialiser le fond de carte
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  
  ## Ajouter un bouton pour pouvoir zoomer et dézoomer la carte
  addResetMapButton() %>%
  ## Ajouter une minicarte
  addMiniMap("bottomleft") %>%
  ## Ajouter les coordonnées quand on déplace le pointeur
  leafem::addMouseCoordinates() %>%
  ## Ajouter la vue initiale sur la carte
  setView(lng = -2.017477503564932, 
          lat = 48.64934133689417, 
          zoom = 10.5 ) %>%
  
  ## Ajouter le polygone du département avec la couleur et les options
  addPolygons(data = departement,
              weight = 0.1,
              color = "#BDFFB8",
              dashArray = "3",
              opacity = 0.7,
              stroke = FALSE,
              fillOpacity = 0.5,
              smoothFactor = 0.5) %>%
  addControl(
    "Source : data.culture.gouv",
    position = "bottomright"
  )%>%
  
  
  ## Ajouter les markers des MH avec l'option cluster
  addAwesomeMarkers(
    data = data,
    lng = data$long,
    lat = data$lat,
    popup = c(content),
    group = "Monuments historiques",
    options = popupOptions(maxWidth = 100, maxHeight = 150),
    clusterOptions = markerClusterOptions(),
    popupOptions = popupOptions(autoClose = FALSE,
                                closeOnClick = FALSE))%>%
 
  
  ## Ajouter des markers en forme de cercle qui permet de voir les MH en fonction de leur date de protection 
  addCircleMarkers(data = data, 
                   lng = ~long,
                   lat = ~lat,
                   fillColor = ~palette_data(date_de_la_protection),
                   color = "black",
                   weight = 1,
                   radius = ~sqrt(data$date_de_la_protection) * 0.2,
                   stroke = TRUE,
                   fillOpacity = 0.5,
                   group = "Date de la protection",
                   label = ~paste("Le monument historique ", data$name, 
                                  " a été protégé en ",
                                  data$date_de_la_protection))%>%
  
  ## Ajouter une légende pour les markers en forme de cercle concernant la date de protection
  addLegend("bottomleft", 
            pal = palette_data, 
            values = data$date_de_la_protection,
            title = "Date de la protection",
            labFormat = labelFormat(suffix = " date"),
            opacity = 1,
            group = "Date de la protection")
  
## Ajouter des markers en forme de cercle qui permet de voir les MH en fonction de leur type
  for (t in types) {
    
    map <- map %>%
      addCircleMarkers(
        data = data[data$type == t, ],
        lng = ~long,
        lat = ~lat,
        popup = content[data$type == t],
        radius = 8,
        stroke = TRUE,
        color = ~color,
        fillColor = ~color,
        fillOpacity = 0.8,
        group = t,
        label = ~type
      )
  }


  ## Ajouter une légende
map <- map %>%
  addLegend("topright", 
            colors = c("transparent"),
            labels = c("Albane d'Acremont"),
            title = "Les Monuments historiques classés et inscrits à Saint Malo : ") %>%
  
  ## PARTIE 3 - Le code gère le sélecteur de couches
  
  ## Ajouter le selecteur de couches pour les options de navigation
  addLayersControl(baseGroups = c("Monuments historiques", "Empty layer"),
                   overlayGroups = c(types,
                                     "Date de la protection"),
                   options = layersControlOptions(collapsed = FALSE)) %>%

  ## Cacher les couches que l'on ne veut pas voir à l'initialisation de la carte
  hideGroup(c("Empty layer",
              types,
              "Date de la protection"))
## Montrer la carte  
map

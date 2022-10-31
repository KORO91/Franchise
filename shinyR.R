library(shiny)
library(readxl)
library(tidyverse)
library(rnaturalearth)
library(openxlsx)
library(rvest)
library(mapview)
library(tmap)

setwd("~/Desktop")
franchise = read_excel("Franchise.xlsx")
View(franchise)
#  Rajout des 0 pour les département sans 0 a gauche:
# EX: 1 -> 01

franchise$Départements = case_when(
  str_count(franchise$Départements) == 1 ~ str_c("0", franchise$Départements),
  TRUE ~ as.character(franchise$Départements)
)

# on recupere le nom de toutes les franchises présente dans le jeu de données
liste.franchise = franchise$Franchises %>% table() %>% names()


#table(franchise$Départements)


#as.numeric(franchise$Départements) %>% table(useNA = "ifany") %>% addmargins()

## L'objectif est de remplacer (ex: 75 par Paris)
## On scrappe donc sur internet un tableau contenant ces informations

url = "https://www.axl.cefan.ulaval.ca/europe/france_departements.htm#:~:text=Les%20d%C3%A9partements%20m%C3%A9tropolitains%20%C3%A9taient%20num%C3%A9rot%C3%A9s,%2Dde%2DFrance%20en%201968."
xopen::xopen(url)

a = read_html(url) %>%
  html_table()

dep_code = a[[3]]


## On ecrit qui remplace les code de département par le nom complet

find_dep = function(code, tib = dep_code){
  out = tib %>%
    filter(`N°` == code)
  res = out[, 2] %>% pull()

  if(length(res) == 0){
    res = code}
  return(res)
}

## Application
find_dep("75")

## On vectorize la fonction pour l'utiliser sur tout les département de notre jeu de données
Find_dep = Vectorize(find_dep, vectorize.args = "code")

# Application sur un sous vecteur

Find_dep(c("01", "75", "9345"))

# Application sur les données
franchise$Departement = Find_dep(franchise$Départements)

# on supprime les ligne contenant des valeur manquante
franchise = na.omit(franchise)


# Il reste des département d'outre mer qu'on retire du jeu de donnée
franchise = franchise %>%
  filter( !Departement %in% c("20", "971", "972" ,"973", "974"))

# On calcul le nombre de chaque franchise dans chaque département

base_final = franchise %>%
  group_by(Franchises, Departement) %>%
  summarise(Effectif = n()) %>% ungroup()


## On récupère la franchise la plus représenté dans chaque département

frequfranch = base_final %>%
  group_by(Departement) %>%
  summarise(maxp = Franchises[which.max(Effectif)],
            Fréquence = 100*max(Effectif)/sum(Effectif))

## un peu de nettoyage dans les noms

frequfranch$Departement = tolower(frequfranch$Departement) %>% #miniscules
  str_replace(" ", "-") %>% # remplacer les espace par les traits d'unions
  iconv(to='ASCII//TRANSLIT') # remplacer les accents par les lettres simples

# On importe un fichier qui contier les logitude et latitude de chque départment
# Le nettoyage précédent avait pour objectif de faire correspondre les nom de dpartement
# avec ceux de ce fichier
dep <- read.csv("exo5_dep.csv")

# on fait donc une fusion entre les deux jeu de données afin de faire la map
# geodata= merge(dep, frequfranch, by.x = "dep", by.y = "Departement", all = TRUE)

#write.xlsx(geodata, "geodata.xlsx")
geodata = read_excel("geodata.xlsx")

library(rnaturalearth)
france <- ne_states(country = "France", returnclass = "sf") %>%
  filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))

mapp = function(franchise = "BODY MINUTE"){ # Par défaut on choit body minute arbitrairement
  # petit nettoyage
  base_final$Departement = tolower(base_final$Departement) %>%
    str_replace(" ", "-") %>%
    iconv(to='ASCII//TRANSLIT')

  # on recupere les données sur la franchise qui nous intéresse
  bm = base_final %>%
    filter(Franchises == franchise)

  #  netoyage
  france$name = tolower(france$name) %>%
    str_replace(" ", "-") %>%
    iconv(to='ASCII//TRANSLIT')
  # fusion des données pour la map
  nb = merge(france, bm, by.x = "name", by.y = "Departement", all.x = TRUE)
  nb$Effectif[is.na(nb$Effectif)] = 0
  # names(nb)[85] = franchise
  # map
  tmap_mode("view")
  tm_shape(nb) +
    tm_polygons(col = "Effectif")
}

nom_fran <- levels(factor(franchise$Franchises))
nom_fran2 <- list()
for(i in 2:length(nom_fran)){
  nom_fran2[[i]] <- nom_fran[i]
}
names(nom_fran2) <- nom_fran

nom_reg <- levels(factor(france$woe_name))
nom_reg2 <- list()
for(i in 2:length(nom_reg)){
  nom_reg2[[i]] <- nom_reg[i]
}
names(nom_reg2) <- nom_reg



ui <- fluidPage(
  titlePanel("Franchise"),

 sidebarPanel(
    selectInput("select", h3("Choisis ta franchise"),
                choices = nom_fran2,selected = nom_fran2[1]),
    selectInput("selects", h3("Choisis ta région"),
                choices = nom_reg2,selected = nom_reg2[1]),
    width = 3
  ),
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"),
                  tmapOutput("tmapp",height = 350, width = 475),
                  tmapOutput("tmapp2",height = 350, width = 475))
    )
  )
)


# Define server logic ----
server <- function(input, output) {

  test = reactiveVal()
  testt = reactiveVal()


  test2 = eventReactive( input$select, {
    x = input$select
    mapp(x)

  })

  testt2 = eventReactive( input$selects, {
    x = input$selects
    tm_shape(france[france$woe_name==x,]) +
      tm_polygons()

  })

  observeEvent(input$select, {
    test(test2())

  })

  observeEvent(input$selects, {
    testt(testt2())

  })

  output$tmapp = renderTmap({
    test()
  })

  output$tmapp2 = renderTmap({
    testt()
  })


}

# Run the app ----
shinyApp(ui = ui, server = server)


# Load packages
library(shiny)
library(shinyWidgets)
library(leaflet)

# Load data
load("Data.Rdata")

# Normalize data
profiles$total <- 100 * profiles$total / sum(profiles$total)

# Source text for the "About" panel
tabPanelAbout <- source("About.R")$value

# UI
shinyUI(navbarPage(
  title = HTML('<span style="font-size:120%;color:white;font-weight:bold;">Intersectionality&nbsp;&nbsp;</span></a>'),
  windowTitle = "Intersectionality",

  ##### Profiles #####################################################################################
  tabPanel(
    HTML('<span style="font-size:100%;color:white;font-weight:bold;">Profile</span></a>'),

    # Include custom CSS & logo
    tags$head(
      includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "logo.png")
    ),
    div(
      class = "outer",

      # Map
      leafletOutput("mappro", width = "100%", height = "100%"),

      # Panel
      absolutePanel(
        id = "control1", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",
        h2("Profiles' explorer"),
        selectInput(
          inputId = "CSpro",
          label = strong("City region"),
          choices = list(
            "All" = "ALL", "Metropolitan France" = "METRO",
            "Albi" = "ALBI",
            "Alençon" = "ALENCON",
            "Amiens" = "AMIENS",
            "Angers" = "ANGERS",
            "Angoulême" = "ANGOULEME",
            "Annecy" = "ANNECY",
            "Annemasse" = "ANNEMASSE",
            "Bayonne" = "BAYONNE",
            "Besançon" = "BESANCON",
            "Béziers" = "BEZIERS",
            "Bordeaux" = "BORDEAUX",
            "Brest" = "BREST",
            "Caen" = "CAEN",
            "Carcassonne" = "CARCASSONNE",
            "Cherbourg" = "CHERBOURG",
            "Clermont-Ferrand" = "CLERMONT FERRAND",
            "Creil" = "CREIL",
            "Dijon" = "DIJON",
            "Douai" = "DOUAI",
            "Dunkerque" = "DUNKERQUE",
            "Fort-de-France" = "FORT DE FRANCE",
            "Grenoble" = "GRENOBLE",
            "La Rochelle" = "LA ROCHELLE",
            "Le Havre" = "LE HAVRE",
            "Lille" = "LILLE",
            "Longwy" = "LONGWY",
            "Lyon" = "LYON",
            "Marseille" = "MARSEILLE",
            "Metz" = "METZ",
            "Montpellier" = "MONTPELLIER",
            "Nancy" = "NANCY",
            "Nantes" = "NANTES",
            "Nice" = "NICE",
            "Nîmes" = "NIMES",
            "Niort" = "NIORT",
            "Paris" = "PARIS",
            "Poitiers" = "POITIERS",
            "Quimper" = "QUIMPER",
            "Rennes" = "RENNES",
            "Rouen" = "ROUEN",
            "Saint-Brieuc" = "SAINT BRIEUC",
            "St-Denis (La Réunion)" = "SAINT DENIS",
            "Saint-Etienne" = "SAINT ETIENNE",
            "Strasbourg" = "STRASBOURG",
            "Thionville" = "THIONVILLE",
            "Toulouse" = "TOULOUSE",
            "Tours" = "TOURS",
            "Valence" = "VALENCE",
            "Valenciennes" = "VALENCIENNES"
          ),
          selected = "MONTPELLIER"
        ),
        selectInput(
          inputId = "CATpro",
          label = strong("Category"),
          choices = list(
            "All" = "tot",
            "Men" = "sex1",
            "Women" = "sex2",
            "16-24 years" = "age1",
            "25-34 years" = "age2",
            "35-64 years" = "age3",
            "65 and more" = "age4",
            "Low educ." = "edu1",
            "Middle-low educ." = "edu2",
            "Middle-high educ." = "edu3",
            "High educ." = "edu4"
          ),
          selected = "tot"
        ),

        # Barplot
        HTML('<div style="font-weight:bold;"align="justified">Percentage of profile by social group</div>'),
        plotOutput("barplotpro", height = 300),

        # Details on the popup
        HTML('<div align="justified">Click on a district to display the profile for each social group.</div>')
      ),

      # Legend
      absolutePanel(
        id = "control2", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = "auto", left = 10, right = "auto", bottom = 0,
        width = 310, height = "auto",
        HTML('<div style="font-weight:bold;"align="justified">Profiles</div>'),
        plotOutput("plotpro", height = 200)
      )
    )
  ),

  ##### Mismatch #####################################################################################
  tabPanel(
    HTML('<span style="font-size:100%;color:white;font-weight:bold;">Mismatch</span></a>'),

    # Include custom CSS & logo
    tags$head(
      includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "logo.png")
    ),
    div(
      class = "outer",

      # Map
      leafletOutput("mapmis", width = "100%", height = "100%"),

      # Panel
      absolutePanel(
        id = "control1", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",
        h2("Mismatches' explorer"),
        selectInput(
          inputId = "CSmis",
          label = strong("City region"),
          choices = list(
            "All" = "ALL", "Metropolitan France" = "METRO",
            "Albi" = "ALBI",
            "Alençon" = "ALENCON",
            "Amiens" = "AMIENS",
            "Angers" = "ANGERS",
            "Angoulême" = "ANGOULEME",
            "Annecy" = "ANNECY",
            "Annemasse" = "ANNEMASSE",
            "Bayonne" = "BAYONNE",
            "Besançon" = "BESANCON",
            "Béziers" = "BEZIERS",
            "Bordeaux" = "BORDEAUX",
            "Brest" = "BREST",
            "Caen" = "CAEN",
            "Carcassonne" = "CARCASSONNE",
            "Cherbourg" = "CHERBOURG",
            "Clermont-Ferrand" = "CLERMONT FERRAND",
            "Creil" = "CREIL",
            "Dijon" = "DIJON",
            "Douai" = "DOUAI",
            "Dunkerque" = "DUNKERQUE",
            "Fort-de-France" = "FORT DE FRANCE",
            "Grenoble" = "GRENOBLE",
            "La Rochelle" = "LA ROCHELLE",
            "Le Havre" = "LE HAVRE",
            "Lille" = "LILLE",
            "Longwy" = "LONGWY",
            "Lyon" = "LYON",
            "Marseille" = "MARSEILLE",
            "Metz" = "METZ",
            "Montpellier" = "MONTPELLIER",
            "Nancy" = "NANCY",
            "Nantes" = "NANTES",
            "Nice" = "NICE",
            "Nîmes" = "NIMES",
            "Niort" = "NIORT",
            "Paris" = "PARIS",
            "Poitiers" = "POITIERS",
            "Quimper" = "QUIMPER",
            "Rennes" = "RENNES",
            "Rouen" = "ROUEN",
            "Saint-Brieuc" = "SAINT BRIEUC",
            "St-Denis (La Réunion)" = "SAINT DENIS",
            "Saint-Etienne" = "SAINT ETIENNE",
            "Strasbourg" = "STRASBOURG",
            "Thionville" = "THIONVILLE",
            "Toulouse" = "TOULOUSE",
            "Tours" = "TOURS",
            "Valence" = "VALENCE",
            "Valenciennes" = "VALENCIENNES"
          ),
          selected = "MONTPELLIER"
        ),
        selectInput(
          inputId = "CATmis1",
          label = strong("Category"),
          choices = list(
            "All" = "tot",
            "Men" = "sex1",
            "Women" = "sex2",
            "16-24 years" = "age1",
            "25-34 years" = "age2",
            "35-64 years" = "age3",
            "65 and more" = "age4",
            "Low educ." = "edu1",
            "Middle-low educ." = "edu2",
            "Middle-high educ." = "edu3",
            "High educ." = "edu4"
          ),
          selected = "sex1"
        ),
        selectInput(
          inputId = "CATmis2",
          label = strong("Category"),
          choices = list(
            "All" = "tot",
            "Men" = "sex1",
            "Women" = "sex2",
            "16-24 years" = "age1",
            "25-34 years" = "age2",
            "35-64 years" = "age3",
            "65 and more" = "age4",
            "Low educ." = "edu1",
            "Middle-low educ." = "edu2",
            "Middle-high educ." = "edu3",
            "High educ." = "edu4"
          ),
          selected = "sex2"
        ),

        # Details on the popup
        HTML('<div align="justified">Click on a district to display the profile for each social group.</div>')
      )
    )
  ),

  ##### Cluster #####################################################################################
  tabPanel(
    HTML('<span style="font-size:100%;color:white;font-weight:bold;">Cluster</span></a>'),

    # Include custom CSS & logo
    tags$head(
      includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "logo.png")
    ),
    div(
      class = "outer",

      # Map
      leafletOutput("mapclu", width = "100%", height = "100%"),

      # Panel
      absolutePanel(
        id = "control1", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 80, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",
        h2("Clusters' explorer"),
        selectInput(
          inputId = "CSclu",
          label = strong("City region"),
          choices = list(
            "All" = "ALL", "Metropolitan France" = "METRO",
            "Albi" = "ALBI",
            "Alençon" = "ALENCON",
            "Amiens" = "AMIENS",
            "Angers" = "ANGERS",
            "Angoulême" = "ANGOULEME",
            "Annecy" = "ANNECY",
            "Annemasse" = "ANNEMASSE",
            "Bayonne" = "BAYONNE",
            "Besançon" = "BESANCON",
            "Béziers" = "BEZIERS",
            "Bordeaux" = "BORDEAUX",
            "Brest" = "BREST",
            "Caen" = "CAEN",
            "Carcassonne" = "CARCASSONNE",
            "Cherbourg" = "CHERBOURG",
            "Clermont-Ferrand" = "CLERMONT FERRAND",
            "Creil" = "CREIL",
            "Dijon" = "DIJON",
            "Douai" = "DOUAI",
            "Dunkerque" = "DUNKERQUE",
            "Fort-de-France" = "FORT DE FRANCE",
            "Grenoble" = "GRENOBLE",
            "La Rochelle" = "LA ROCHELLE",
            "Le Havre" = "LE HAVRE",
            "Lille" = "LILLE",
            "Longwy" = "LONGWY",
            "Lyon" = "LYON",
            "Marseille" = "MARSEILLE",
            "Metz" = "METZ",
            "Montpellier" = "MONTPELLIER",
            "Nancy" = "NANCY",
            "Nantes" = "NANTES",
            "Nice" = "NICE",
            "Nîmes" = "NIMES",
            "Niort" = "NIORT",
            "Paris" = "PARIS",
            "Poitiers" = "POITIERS",
            "Quimper" = "QUIMPER",
            "Rennes" = "RENNES",
            "Rouen" = "ROUEN",
            "Saint-Brieuc" = "SAINT BRIEUC",
            "St-Denis (La Réunion)" = "SAINT DENIS",
            "Saint-Etienne" = "SAINT ETIENNE",
            "Strasbourg" = "STRASBOURG",
            "Thionville" = "THIONVILLE",
            "Toulouse" = "TOULOUSE",
            "Tours" = "TOURS",
            "Valence" = "VALENCE",
            "Valenciennes" = "VALENCIENNES"
          ),
          selected = "MONTPELLIER"
        ),


        # Details on the popup
        HTML('<div align="justified">The V-test is proportional to the difference between the average mismatch in a given cluster and the average mismatch in the whole region.</div>')
      ),

      # Legend
      absolutePanel(
        id = "control2", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = "auto", left = 10, right = "auto", bottom = 0,
        width = 500, height = "auto",
        HTML('<div style="font-weight:bold;"align="justified">Clusters</div>'),
        plotOutput("plotclu", height = 300)
      )
    )
  ),

  ## About ----------------------------------------------------------------------
  tabPanelAbout()
))

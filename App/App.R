library(shiny)
library(tidyverse)

cost_inputs <- readRDS("data/Eoles_cost_inputs.rds")
results <- readRDS("data/Eoles_outputs.rds")
LCOE <- readRDS("data/LCOE.rds")
cost_shares <- readRDS("data/cost_shares.rds")

production_technologies <- c("offshore", "onshore", "pv", "biogas")
storage_technologies <- c("battery", "methanation", "phs")

color_scheme <- tibble(
  technology = c("onshore", "offshore", "pv", "methanation", "biogas", "phs", "battery"),
  color = c("light green", "dark green", "#FFF633", "grey", "brown", "#487FDF", "purple")
) %>%
  arrange(technology)

#Get the unique cost values used as inputs
cost_inputs %>% 
  select(-scen) %>% 
  gather(key = cost_item, value = cost) %>% 
  unique()
  
#Capex
onshore_capex_values <- cost_inputs$onshore_capex %>% unique()
offshore_capex_values <- cost_inputs$offshore_capex %>% unique()
pv_capex_values <- cost_inputs$pv_capex %>% unique()
battery_capex_values <- cost_inputs$battery_capex %>% unique()
methanation_capex_values <- cost_inputs$methanation_capex %>% unique()

#Opex
onshore_opex_values <- cost_inputs$onshore_opex %>% unique()
offshore_opex_values <- cost_inputs$offshore_opex %>% unique()
pv_opex_values <- cost_inputs$pv_opex %>% unique()
methanation_opex_values <- cost_inputs$methanation_opex %>% unique()


# UI ----
ui <- fluidPage(
  img(src='logo_cired.png', align = "right", height = '80px', width = '80px'),
  titlePanel("Le modèle Eoles"),

  
      h3("A vous de jouer !"),
      p("Quel serait le mix optimal de production et de stockage d'électricité 100% renouvelable en France en 2050 ? "),
      p("Ce site présente graphiquement les résultats du modèle d'optimisation du système électrique EOLES_Res, publié dans la revue à comité de lecture The Energy Journal*. Ce modèle sélectionne l'investissement dans les différentes technologies de production et de stockage, et le fonctionnement de ces équipements de production et de stockage, qui satisfont la demande au moindre coût. Ce mix optimal est calculé en se basant sur l'année météo de 2006, heure par heure. Nous avons choisi cette année après avoir montré qu'elle fournit les résultats les plus représentatifs de la période 2000 à 2017. Tous les mix obtenus satisfont la demande d'électricité pour chacune des 8760 heures de l'année, sans recourir aux importations ou à la flexibilité de la demande."),
      p("Sélectionnez les coûts d'investissement (CAPEX) dans les différentes technologies et observez le mix optimal, en production et en capacité. Les valeurs centrales des CAPEX sont les projections du JRC (le centre de recherche de la Commission européenne) pour 2050. Pour les autres hypothèses (en particulier sur le taux d'actualisation, la demande d'électricité heure par heure, les coûts de fonctionnement, les capacités maximales dans les différentes technologies et la production maximale d'hydraulique et de biogaz), voir l'article*. Toutes les équations du modèle sont disponibles en accès libre", tags$a(href = "https://github.com/BehrangShirizadeh/EOLES_elecRES", "ici.")),
      
      helpText("* Shirizadeh B., Q. Perrier, P. Quirion, 2020. How sensitive are optimal fully renewable power systems to technology cost uncertainty? Energy Journal, accepté pour publication."),
  br(),
  wellPanel(
    p("Choisissez ici les coûts des technologies :"),
    h3("CAPEX"),
    fluidRow(
      column(6,
             selectInput("onshore_capex_selection", p("Eolien terrestre (euro/kW)"), 
                         choices = onshore_capex_values, selected = onshore_capex_values[2]),
             
             selectInput("offshore_capex_selection", p("Eolien en mer (euro/kW)"), 
                         choices = offshore_capex_values, selected = offshore_capex_values[2]),
             
             selectInput("pv_capex_selection", p("PV (euro/kW)"), 
                         choices = pv_capex_values, selected = pv_capex_values[3])
             ),
    column(6,
           selectInput("battery_capex_selection", p("Batteries (euro/kWh**)"), 
                       choices = battery_capex_values, selected = battery_capex_values[2]),
           helpText("**Pour les batteries, la part est proportionnelle à l'énergie (euro/kWh) et une part du CAPEX est proportionnelle à la puissance (140 euros/kW)"),
           
           selectInput("methanation_capex_selection", p("Méthanation (euro/kW électrique)"), 
                       choices = methanation_capex_values, selected = methanation_capex_values[2])
           )
    ),
    
    helpText("Ces hypothèses de coût reviennent à sélectionner l'un de nos scénarios."),
    textOutput("selected_scenario")
  ),  
  br(),
  p("A partir de vos hypothèses de coût, le mix électrique optimal à partir d'énergies renouvelables est le suivant  pour la France :"),
  h3("Production"),
      fluidRow(
        column(4,
               plotOutput("LCOE_production"),
               p("Le LCOE (Levelized cost of electricity) représente le coût moyen de production. Plus précisément, il s'agit de la somme des coûts de production actualisés divisée par la somme de la production actualisée, pendant la durée de vie de l'équipement.")),
        column(4, 
               plotOutput("plotGene")),
        column(4,
               plotOutput("plotCapa"))
      ),
  h3("Stockage"),
  fluidRow(
    column(4,
           plotOutput("LCOE_storage"),
           p("Le LCOS (Levelized cost of storage) représente le coût moyen de stockage.")),
    column(4, 
           plotOutput("plotGeneStorage")),
    column(4,
           plotOutput("plotStorageVolume"))
  ),
  br(),
  h3("Données du système électrique"),
  textOutput("total_cost"),
  br(),
  textOutput("curtailment"),
  br(),
  textOutput("storage_loss"),
  br(),
  textOutput("avg_LCOE"),
  br(),
  textOutput("cost_share_storage"),
  br(),
  hr(),
  p("Pour toute question, vous pouvez contacter", tags$a(href = "http://www2.centre-cired.fr/Equipe-27/Chercheurs/QUIRION-Philippe/", "Philippe Quirion"),".")
)


# SERVER ----
server <- function(input, output) {
  
  selected_scenario <- reactive({
    if(input$onshore_capex_selection == onshore_capex_values[1] & input$offshore_capex_selection == offshore_capex_values[3]) {
      0
    } else if(input$onshore_capex_selection == onshore_capex_values[3] & input$offshore_capex_selection == offshore_capex_values[1]) {
      0
    }
    else {
      cost_inputs %>%
        filter(onshore_capex == input$onshore_capex_selection,
               offshore_capex ==  input$offshore_capex_selection,
               pv_capex ==  input$pv_capex_selection,
               battery_capex == input$battery_capex_selection,
               methanation_capex == input$methanation_capex_selection) %>% 
        .$scen
    }

  })

  output$selected_scenario <- renderText({
    if(selected_scenario() == 0) {
      paste("Attention ! Vos hypothèses de coût ne correspondent à aucun des scénarios modélisés. Veillez à mettre des hypothèses cohérentes pour l'éolien terrestre et en mer.")
    } else {
      paste("Vous avez sélectionné le scénario ", selected_scenario())
    }
   
  })
  
  
# Production plots --------------------------------------------------------

  output$LCOE_production <- renderPlot({
    data <- LCOE %>%
      filter(scen == selected_scenario()) %>%
      select(-scen) %>%
      gather(technology, value) %>%
      mutate(technology = tolower(technology)) %>% 
      filter(technology %in% production_technologies)
    
    color_gene <- color_scheme %>%
      filter(technology %in% data$technology)
    
    ggplot(data, aes(x = technology, y = value, fill = technology)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      scale_fill_manual(values = color_gene$color) +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      labs(x = "", y = "", title = "Coût moyen de production \n(LCOE, en euro/MWh)", fill = "Technology") +
      scale_x_discrete(labels=c("offshore" = "Eolien\nen mer", 
                                  "onshore" = "Eolien\nterrestre",
                                  "pv" = "PV"))
  })
  
  output$plotGene <- renderPlot({
    generation <- results %>%
      filter(scen == selected_scenario()) %>%
      select(ends_with("gene")) %>%
      gather(technology, generation) %>%
      mutate(technology = str_remove_all(technology, pattern = "_gene"),
             technology = tolower(technology)) %>% 
      filter(technology %in% production_technologies) %>% 
      add_row(technology = "biogas", generation = 15)

    color_gene <- color_scheme %>%
      filter(technology %in% generation$technology)

    ggplot(generation, aes(x = technology, y = generation, fill = technology)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      scale_fill_manual(values = color_gene$color) +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      labs(x = "", y = "", title = "Génération \n(TWh/an)", fill = "Technology") +
      scale_x_discrete(labels=c("offshore" = "Eolien\nen mer", 
                                "onshore" = "Eolien\nterrestre",
                                "pv" = "PV",
                                "biogas" = "Biogaz"))
  })

  output$plotCapa <- renderPlot({
    capa <- results %>%
      filter(scen == selected_scenario()) %>%
      select(ends_with("cap")) %>%
      gather(technology, capacity) %>%
      mutate(technology = str_remove_all(technology, pattern = "_cap"),
             technology = tolower(technology)) %>% 
      filter(technology %in% production_technologies)

    color_cap <- color_scheme %>%
      filter(technology %in% capa$technology)

    ggplot(capa, aes(x = technology, y = capacity, fill = technology)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = color_cap$color) +
      labs(x = "", y = "", title = "Capacité \n(GW)", fill = "Technology") +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      scale_x_discrete(labels=c("offshore" = "Eolien\nen mer", 
                                "onshore" = "Eolien\nterrestre",
                                "pv" = "PV",
                                "biogas" = "Biogaz"))
  })
  

# Storage plots -----------------------------------------------------------

  output$LCOE_storage <- renderPlot({
    data <- LCOE %>%
      filter(scen == selected_scenario()) %>%
      select(-scen) %>%
      gather(technology, value) %>%
      mutate(technology = tolower(technology)) %>% 
      filter(technology %in% storage_technologies)
    
    color_gene <- color_scheme %>%
      filter(technology %in% data$technology)
    
    ggplot(data, aes(x = technology, y = value, fill = technology)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      scale_fill_manual(values = color_gene$color) +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      labs(x = "", y = "", title = "Coût du stockage \n(LCOS, en euro/MWh)", fill = "Technology") +
      scale_x_discrete(labels=c("battery" = "Batteries", 
                                "methanation" = "Méthanation"))
  })
  
  output$plotGeneStorage <- renderPlot({
    generation <- results %>%
      filter(scen == selected_scenario()) %>%
      select(ends_with("gene")) %>%
      gather(technology, generation) %>%
      mutate(technology = str_remove_all(technology, pattern = "_gene"),
             technology = tolower(technology)) %>% 
      filter(technology %in% storage_technologies)
    
    color_gene <- color_scheme %>%
      filter(technology %in% generation$technology)
    
    ggplot(generation, aes(x = technology, y = generation, fill = technology)) +
      geom_bar(stat="identity", show.legend = FALSE) +
      scale_fill_manual(values = color_gene$color) +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      labs(x = "", y = "", title = "Production annuelle \n(TWh/an)", fill = "Technology") +
      scale_x_discrete(labels=c("battery" = "Batteries", 
                                "methanation" = "Méthanation",
                                "phs" = "STEP"))
  })
  
  output$plotStorageVolume <- renderPlot({
    data <- results %>%
      filter(scen == selected_scenario()) %>%
      select(ends_with("_v")) %>%
      gather(technology, value) %>%
      mutate(technology = str_remove_all(technology, pattern = "_v"),
             technology = tolower(technology)) %>% 
      filter(technology %in% storage_technologies) %>% 
      add_row(technology = "phs", value = 180) %>% 
      mutate(value = ifelse(technology == "methanation", value / 1000, value))
    
    color_cap <- color_scheme %>%
      filter(technology %in% data$technology)
    
    ggplot(data, aes(x = technology, y = value, fill = technology)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = color_cap$color) +
      labs(x = "", y = "", title = "Capacité en énergie \n(GWh ou TWh)", fill = "Technology") +
      theme_bw() +
      theme(text = element_text(size = 15)) +
      scale_x_discrete(labels=c("battery" = "Batteries \n(GWh)", 
                                "methanation" = "Méthanation \n(TWh)",
                                "phs" = "STEP \n(GWh)"))
  })
  

# System values -----------------------------------------------------------

  output$total_cost <- renderText({
    paste(" > Le coût total de ce scénario est de ", results %>% filter(scen == selected_scenario()) %>% .$cost, "milliards d'euros par an, en incluant les coûts d'investissement, de raccordement au réseau électrique et de production (mais pas les coûts de transport et distribution d'électricité).")
  })
  
  output$curtailment <- renderText({
    paste(" > L'écrêtement est de ", results %>% filter(scen == selected_scenario()) %>% .$LC, "% de la production annuelle.")
  })

  output$storage_loss <- renderText({
    paste(" > Les pertes de stockage représentent ", results %>% filter(scen == selected_scenario()) %>% .$str_loss, "% de la production annuelle.")
  })

  output$avg_LCOE <- renderText({
    paste(" > Le coût moyen de production pour l'ensemble du système est de ", results %>% filter(scen == selected_scenario()) %>% .$LCOE2, "euro/MWh.")
  })
  
  output$cost_share_storage <- renderText({
    paste(" > Le stockage représente ", cost_shares %>% filter(scen == selected_scenario()) %>% .$Storage, "% du coût total.")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
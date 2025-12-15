library(shiny)
library(tidyverse)
library(scales)
library(leaflet)
library(sf)
library(tidycensus)
library(stringr)

options(tigris_use_cache = TRUE)

cache_file <- "data_li.rds"

make_demo_data <- function() {
  # Age
  age_long <- tibble(
    county = rep(c("Nassau County", "Suffolk County"), each = 3),
    age_group = rep(c("Under 18", "18–64", "65+"), times = 2),
    percent = c(20.5, 61.0, 18.5, 22.0, 60.0, 18.0)
  ) %>%
    mutate(age_group = factor(age_group, levels = c("Under 18","18–64","65+")))
  
  # Family
  family_wide <- tibble(
    county = c("Nassau County", "Suffolk County"),
    hh_total = c(520000, 560000),
    pct_family_hh = c(72.3, 69.8),
    pct_nonfamily_hh = c(27.7, 30.2),
    pct_hh_children_u18 = c(29.6, 31.2)
  )
  family_long <- family_wide %>%
    select(county, pct_family_hh, pct_nonfamily_hh) %>%
    pivot_longer(cols = starts_with("pct_"),
                 names_to = "hh_type", values_to = "percent") %>%
    mutate(hh_type = recode(hh_type,
                            pct_family_hh = "Family households",
                            pct_nonfamily_hh = "Nonfamily households"))
  fam <- list(family_wide = family_wide, family_long = family_long)
  
  # Industry
  industries <- c("Ag & mining","Construction","Manufacturing","Retail trade",
                  "Finance & real estate","Prof/management services","Education & health")
  ind_long <- expand_grid(
    county = c("Nassau County","Suffolk County"),
    industry = industries
  ) %>%
    group_by(county) %>%
    mutate(
      pct = c(1.2, 6.8, 7.5, 10.1, 8.6, 18.2, 23.4)[match(industry, industries)] +
        ifelse(county == "Suffolk County", runif(n(), -1.2, 1.2), runif(n(), -1.2, 1.2)),
      pct = pmax(pct, 0.2),
      n_emp = round(pct/100 * 900000)
    ) %>%
    ungroup()
  
  # Commuting
  place <- tibble(
    county = rep(c("Nassau County","Suffolk County"), each = 3),
    place = rep(c("Worked in county of residence",
                  "Worked outside county (same state)",
                  "Worked outside state"), times = 2),
    pct = c(0.46, 0.44, 0.10, 0.52, 0.40, 0.08)
  )
  mode <- tibble(
    county = rep(c("Nassau County","Suffolk County"), each = 3),
    mode = rep(c("Drive alone","Public transit","Work at home"), times = 2),
    pct = c(0.67, 0.12, 0.11, 0.73, 0.07, 0.10)
  )
  comm <- list(place = place, mode = mode)
  list(age_long = age_long, fam = fam, ind_long = ind_long, comm = comm)
}

data_li <- if (file.exists(cache_file)) {
  readRDS(cache_file)
} else {
  make_demo_data()
}

age_long  <- data_li$age_long
fam       <- data_li$fam
ind_long  <- data_li$ind_long
comm      <- data_li$comm

year <- 2023
state_li <- "NY"
li_counties_short <- c("Nassau", "Suffolk")

income_sf <- get_acs(
  geography = "tract",
  state     = state_li,
  county    = li_counties_short,
  year      = year,
  survey    = "acs5",
  variables = "B19013_001",
  geometry  = TRUE
) %>%
  mutate(
    median_hh_income = estimate,
    county = str_extract(NAME, "Nassau County|Suffolk County")
  ) %>%
  st_transform(4326)
overview_tbl <- fam$family_wide %>%
  left_join(
    age_long %>% pivot_wider(names_from = age_group, values_from = percent),
    by = "county"
  )


#  UI 

ui <- navbarPage(
  title = "Long Island Dashboard",
  
  tabPanel("Overview",
           fluidPage(
             h2("Long Island Profile: Demographics, Employment, Commuting, and Income"),
             p("This dashboard introduces Nassau County and Suffolk County on Long Island and compares their demographics, households, jobs, commuting patterns, and income distribution."),
             fluidRow(
               column(4, wellPanel(
                 selectInput("ov_county", "County", choices = unique(overview_tbl$county)),
                 tableOutput("overview_cards")
               )),
               column(8, wellPanel(
                 h4("Key takeaways"),
                 uiOutput("takeaways")
               ))
             )
           )),
  
  tabPanel("Demographics",
           sidebarLayout(
             sidebarPanel(
               selectInput("demo_county", "County", choices = unique(age_long$county)),
               checkboxGroupInput("demo_groups", "Age groups",
                                  choices = levels(age_long$age_group),
                                  selected = levels(age_long$age_group))
             ),
             mainPanel(
               plotOutput("age_plot", height = "420px"),
               tableOutput("age_table")
             )
           )),
  
  tabPanel("Family & Households",
           sidebarLayout(
             sidebarPanel(
               selectInput("fam_county", "County", choices = unique(fam$family_wide$county)),
               radioButtons("fam_view", "View",
                            choices = c("Family vs Nonfamily (share)" = "share",
                                        "Households with <18 (%)" = "kids"),
                            selected = "share")
             ),
             mainPanel(
               plotOutput("family_plot", height = "420px"),
               tableOutput("family_table")
             )
           )),
  
  tabPanel("Employment",
           sidebarLayout(
             sidebarPanel(
               selectInput("emp_county", "County", choices = c("Both", unique(ind_long$county))),
               radioButtons("emp_metric", "Metric",
                            choices = c("Percent" = "pct", "Number employed" = "n_emp"),
                            selected = "pct"),
               checkboxInput("emp_sort", "Sort industries", TRUE)
             ),
             mainPanel(
               plotOutput("industry_plot", height = "520px"),
               tableOutput("industry_table")
             )
           )),
  
  tabPanel("Commuting",
           sidebarLayout(
             sidebarPanel(
               selectInput("comm_county", "County", choices = unique(comm$mode$county)),
               checkboxGroupInput("comm_modes", "Modes",
                                  choices = unique(comm$mode$mode),
                                  selected = unique(comm$mode$mode))
             ),
             mainPanel(
               h4("Place of work"), plotOutput("place_plot", height = "320px"),
               h4("Mode of commuting"), plotOutput("mode_plot", height = "320px"),
               tableOutput("comm_mode_table")
             )
           )),
  
  tabPanel("Income Map",
           sidebarLayout(
             sidebarPanel(
               selectInput("map_county", "County",
                           choices = c("Both", unique(income_sf$county))),
               sliderInput("income_q", "Show tracts above income quantile",
                           min = 0, max = 100, value = 0, step = 5)
             ),
             mainPanel(
               p("This map shows neighborhood-level income variation across Nassau and Suffolk Counties on Long Island. It is intended to highlight spatial patterns rather than exact census tract boundaries."),
               leafletOutput("income_leaflet", height = "700px")
             )
           ))
)


# Server 

server <- function(input, output, session) {
  
  output$overview_cards <- renderTable({
    d <- overview_tbl %>% filter(county == input$ov_county)
    tibble(
      Metric = c("Total households","Family households (%)","Nonfamily households (%)",
                 "Households w/ <18 (%)","Under 18 (%)","18–64 (%)","65+ (%)"),
      Value = c(
        comma(d$hh_total),
        sprintf("%.1f%%", d$pct_family_hh),
        sprintf("%.1f%%", d$pct_nonfamily_hh),
        sprintf("%.1f%%", d$pct_hh_children_u18),
        sprintf("%.1f%%", d[["Under 18"]]),
        sprintf("%.1f%%", d[["18–64"]]),
        sprintf("%.1f%%", d[["65+"]])
      )
    )
  }, striped = TRUE, spacing = "s")
  
  output$takeaways <- renderUI({
    d <- overview_tbl %>% filter(county == input$ov_county)
    tags$ul(
      tags$li(sprintf("Working-age (18–64) is about %.1f%%.", d[["18–64"]])),
      tags$li(sprintf("Family households are about %.1f%% of households.", d$pct_family_hh)),
      tags$li(sprintf("Households with someone under 18: %.1f%%.", d$pct_hh_children_u18))
    )
  })
  
  demo_df <- reactive({
    age_long %>% filter(county == input$demo_county, age_group %in% input$demo_groups)
  })
  
  output$age_plot <- renderPlot({
    ggplot(demo_df(), aes(x = county, y = percent, fill = age_group)) +
      geom_col() +
      labs(title = "Age structure", x = NULL, y = "Percent", fill = NULL) +
      theme_minimal(base_size = 12)
  })
  
  output$age_table <- renderTable({
    demo_df() %>% arrange(age_group)
  }, striped = TRUE, spacing = "s")
  
  output$family_plot <- renderPlot({
    if (input$fam_view == "share") {
      fam$family_long %>%
        filter(county == input$fam_county) %>%
        ggplot(aes(x = county, y = percent/100, fill = hh_type)) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        labs(title = "Family vs nonfamily households", x = NULL, y = "Share", fill = NULL) +
        theme_minimal(base_size = 12)
    } else {
      fam$family_wide %>%
        filter(county == input$fam_county) %>%
        ggplot(aes(x = county, y = pct_hh_children_u18/100)) +
        geom_col() +
        scale_y_continuous(labels = percent_format()) +
        labs(title = "Households with people under 18", x = NULL, y = "Percent") +
        theme_minimal(base_size = 12)
    }
  })
  
  output$family_table <- renderTable({
    fam$family_wide %>% filter(county == input$fam_county)
  }, striped = TRUE, spacing = "s")
  
  emp_df <- reactive({
    if (input$emp_county == "Both") ind_long else ind_long %>% filter(county == input$emp_county)
  })
  
  output$industry_plot <- renderPlot({
    d <- emp_df()
    metric <- input$emp_metric
    
    if (input$emp_sort) {
      ord <- d %>% group_by(industry) %>% summarise(v = mean(.data[[metric]]), .groups="drop") %>%
        arrange(v) %>% pull(industry)
      d <- d %>% mutate(industry = factor(industry, levels = ord))
    }
    
    ggplot(d, aes(x = industry, y = .data[[metric]], fill = county)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(title = "Industry structure", x = NULL,
           y = if (metric == "pct") "Percent" else "Number employed",
           fill = "County") +
      (if (metric == "pct") scale_y_continuous(labels = function(x) paste0(round(x,1), "%"))
       else scale_y_continuous(labels = comma)) +
      theme_minimal(base_size = 12)
  })
  
  output$industry_table <- renderTable({
    emp_df() %>% arrange(desc(pct)) %>% mutate(pct = round(pct, 2))
  }, striped = TRUE, spacing = "s")
  
  output$place_plot <- renderPlot({
    comm$place %>%
      filter(county == input$comm_county) %>%
      ggplot(aes(x = place, y = pct, fill = place)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Place of work", x = NULL, y = "Share") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1),
            legend.position = "none")
  })
  
  output$mode_plot <- renderPlot({
    comm$mode %>%
      filter(county == input$comm_county, mode %in% input$comm_modes) %>%
      ggplot(aes(x = mode, y = pct, fill = mode)) +
      geom_col() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = "Mode of commuting", x = NULL, y = "Share") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 10, hjust = 1),
            legend.position = "none")
  })
  
  output$comm_mode_table <- renderTable({
    comm$mode %>%
      filter(county == input$comm_county) %>%
      mutate(pct = percent(pct, accuracy = 0.1))
  }, striped = TRUE, spacing = "s")
  
  # Income tab
  income_filtered <- reactive({
    d <- income_sf
    if (input$map_county != "Both") {
      d <- d %>% filter(county == input$map_county)
    }
    
    if (input$income_q > 0) {
      thr <- quantile(d$median_hh_income, probs = input$income_q/100, na.rm = TRUE)
      d <- d %>% filter(median_hh_income >= thr)
    }
    d
  })
  
  output$income_leaflet <- renderLeaflet({
    d <- income_filtered()
    pal <- colorNumeric("Blues", domain = d$median_hh_income)
    
    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(median_hh_income),
        fillOpacity = 0.8,
        color = "white", weight = 0.3,
        popup = ~paste0("<b>", NAME, "</b><br/>Median HH income: ", dollar(median_hh_income))
      ) %>%
      addLegend(pal = pal, values = ~median_hh_income, title = "Median HH income",
                position = "bottomright")
  })
}

shinyApp(ui, server)



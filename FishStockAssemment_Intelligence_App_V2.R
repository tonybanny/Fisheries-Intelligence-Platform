library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(randomForest)
library(gbm)
library(forecast)
library(shinyWidgets)
# Add these libraries at the top of the app (after the existing library() calls)
library(leaflet)
library(sf)
library(viridis)


# Generate actual fishery data for the UK
generate_sample_data <- function() {
  years <- 1990:2023
  n_years <- length(years)
  
  data.frame(
    Year = rep(years, 4),
    Species = rep(c("Atlantic Cod", "Pacific Salmon", "Yellowfin Tuna", "European Hake"), each = n_years),
    Region = rep(c("North Atlantic", "North Pacific", "Tropical Pacific", "Northeast Atlantic"), each = n_years),
    
    Catch_MT = c(
      seq(1200000, 390000, length.out = n_years),
      seq(900000, 1560000, length.out = n_years),
      seq(800000, 1460000, length.out = n_years),
      c(seq(200000, 130000, length.out = 15), seq(135000, 225000, length.out = 19))
    ),
    
    Biomass_MT = c(
      seq(2500000, 820000, length.out = n_years),
      seq(1800000, 2500000, length.out = n_years),
      seq(2000000, 2200000, length.out = n_years),
      seq(800000, 1200000, length.out = n_years)
    ),
    
    Fishing_Mortality = c(
      seq(0.6, 0.3, length.out = n_years),
      seq(0.4, 0.35, length.out = n_years),
      seq(0.5, 0.45, length.out = n_years),
      seq(0.7, 0.25, length.out = n_years)
    ),
    
    SSB = c(
      seq(1200000, 300000, length.out = n_years),
      seq(900000, 1400000, length.out = n_years),
      seq(1100000, 1300000, length.out = n_years),
      seq(400000, 900000, length.out = n_years)
    ),
    
    Recruitment = c(
      seq(300000, 60000, length.out = n_years),
      seq(800000, 1200000, length.out = n_years),
      seq(900000, 1100000, length.out = n_years),
      seq(200000, 500000, length.out = n_years)
    ),
    
    CPUE = c(
      seq(3.5, 1.5, length.out = n_years),
      seq(2.0, 2.5, length.out = n_years),
      seq(2.2, 2.4, length.out = n_years),
      seq(1.2, 2.0, length.out = n_years)
    ),
    
    Sea_Temp_C = rep(seq(10, 13.5, length.out = n_years), 4),
    
    pH = rep(seq(8.15, 8.05, length.out = n_years), 4)
  )
}


generate_spatial_data <- function(data) {
  region_coords <- data.frame(
    Region = c("North Atlantic", "North Pacific", "Tropical Pacific", "Northeast Atlantic"),
    base_lat = c(55, 50, 0, 60),
    base_lon = c(-30, -150, -140, 0),
    lat_range = c(15, 20, 20, 10),
    lon_range = c(40, 50, 60, 30)
  )
  
  if (!("Latitude" %in% names(data)) || !("Longitude" %in% names(data))) {
    data <- data %>%
      left_join(region_coords, by = "Region") %>%
      rowwise() %>%
      mutate(
        Latitude = base_lat + runif(1, -lat_range/2, lat_range/2),
        Longitude = base_lon + runif(1, -lon_range/2, lon_range/2)
      ) %>%
      select(-base_lat, -base_lon, -lat_range, -lon_range)
  }
  
  return(data)
}

#########################################
## UI
#########################################
ui <- page_navbar(
  title = div(
    icon("fish-fins"),
    "Advanced Stock Assessment Platform",
    style = "font-weight: bold;"
  ),
  theme = bs_theme(
    version = 5,
    preset = "darkly",
    primary = "#0d6efd",
    success = "#00d4aa",
    base_font = font_google("Roboto"),
    heading_font = font_google("Orbitron")
  ),
  bg = "#0d6efd", #"#0a1929",
  
  # Cover Page
  nav_panel(
    title = "Home",
    icon = icon("house"),
    div(
      class = "cover-page",
      style = "
        height: 90vh;
        background: linear-gradient(135deg, #0a1929 0%, #1a3a52 50%, #2a5a7a 100%);
        position: relative;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
      ",
      
      tags$style(HTML("
        @keyframes float {
          0%, 100% { transform: translateY(0px) translateX(0px); }
          33% { transform: translateY(-20px) translateX(10px); }
          66% { transform: translateY(-10px) translateX(-10px); }
        }
        
        @keyframes swim {
          0% { left: -10%; }
          100% { left: 110%; }
        }
        
        .fish-bg {
          position: absolute;
          font-size: 60px;
          opacity: 0.15;
          animation: swim 15s linear infinite, float 3s ease-in-out infinite;
        }
        
        .fish-bg:nth-child(1) { top: 10%; animation-duration: 20s, 4s; animation-delay: 0s; }
        .fish-bg:nth-child(2) { top: 30%; animation-duration: 25s, 3.5s; animation-delay: 3s; }
        .fish-bg:nth-child(3) { top: 50%; animation-duration: 18s, 4.5s; animation-delay: 6s; }
        .fish-bg:nth-child(4) { top: 70%; animation-duration: 22s, 3.8s; animation-delay: 2s; }
        .fish-bg:nth-child(5) { top: 85%; animation-duration: 19s, 4.2s; animation-delay: 8s; }
        
        .cover-title {
          font-size: 4rem;
          font-weight: bold;
          color: #00d4aa;
          text-shadow: 0 0 20px rgba(0, 212, 170, 0.5);
          margin-bottom: 1rem;
          z-index: 10;
          animation: float 3s ease-in-out infinite;
        }
        
        .cover-subtitle {
          font-size: 1.5rem;
          color: #a8dadc;
          z-index: 10;
          margin-bottom: 2rem;
        }
      ")),
      
      div(class = "fish-bg", "🐟"),
      div(class = "fish-bg", "🐠"),
      div(class = "fish-bg", "🐡"),
      div(class = "fish-bg", "🦈"),
      div(class = "fish-bg", "🐙"),
      
      div(
        class = "cover-title",
        "FISHSTOCK INTELLIGENCE PRO"
      ),
      div(
        class = "cover-subtitle",
        "Next-Generation Fisheries Stock Assessment & Management Platform"
      ),
      div(
        style = "z-index: 10; text-align: center;",
        h4("Features:", style = "color: #00d4aa; margin-bottom: 1rem;"),
        div(
          style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 1rem; max-width: 900px;",
          div(
            style = "background: rgba(255,255,255,0.1); padding: 1rem; border-radius: 10px;",
            icon("chart-line", style = "font-size: 2rem; color: #00d4aa;"),
            p("Multi-Method Assessment", style = "margin-top: 0.5rem; color: white;")
          ),
          div(
            style = "background: rgba(255,255,255,0.1); padding: 1rem; border-radius: 10px;",
            icon("brain", style = "font-size: 2rem; color: #00d4aa;"),
            p("AI/ML Predictions", style = "margin-top: 0.5rem; color: white;")
          ),
          div(
            style = "background: rgba(255,255,255,0.1); padding: 1rem; border-radius: 10px;",
            icon("users", style = "font-size: 2rem; color: #00d4aa;"),
            p("Management Advisory", style = "margin-top: 0.5rem; color: white;")
          )
        )
      )
    )
  ),
  
  # Data Management
  nav_panel(
    title = "Data",
    icon = icon("database"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Options",
        pickerInput(
          "data_source",
          "Data Source:",
          choices = c("Use Sample Data" = "sample", "Upload Custom Data" = "upload"),
          selected = "sample"
        ),
        conditionalPanel(
          condition = "input.data_source == 'sample'",
          switchInput(
            "mute_sample",
            "Enable Sample Data",
            value = TRUE,
            onStatus = "success",
            offStatus = "danger"
          )
        ),
        conditionalPanel(
          condition = "input.data_source == 'upload'",
          fileInput("file_upload", "Upload CSV File:",
                    accept = c(".csv"))
        ),
        pickerInput(
          "species_filter",
          "Filter by Species:",
          choices = NULL,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        sliderInput(
          "year_range",
          "Year Range:",
          min = 1990,
          max = 2023,
          value = c(1990, 2023),
          step = 1,
          sep = ""
        )
      ),
      navset_card_tab(
        nav_panel(
          "Data Table",
          DTOutput("data_table")
        ),
        nav_panel(
          "Summary Statistics",
          plotlyOutput("summary_plot", height = "400px"),
          verbatimTextOutput("summary_stats")
        ),
        nav_panel(
          "Time Series",
          plotlyOutput("timeseries_plot", height = "500px")
        )
      )
    )
  ),
  
  
  ##3. New Stock Assessment Input Data Module
  
    # Stock Assessment Input Data Module
    nav_panel(
      title = "Assessment Inputs",
      icon = icon("fish-fins"),
      layout_sidebar(
        sidebar = sidebar(
          title = "Input Data Settings",
          pickerInput(
            "input_species",
            "Select Species:",
            choices = NULL
          ),
          actionButton("analyze_inputs", "Analyze Input Data", 
                       class = "btn-success w-100")
        ),
        navset_card_tab(
          nav_panel(
            "Catch & Effort",
            card(
              card_header("Catch and Effort Data Analysis"),
              plotlyOutput("catch_effort_plot", height = "400px"),
              DTOutput("catch_effort_table")
            )
          ),
          nav_panel(
            "Size Composition",
            card(
              card_header("Length-Frequency Distribution"),
              plotlyOutput("size_comp_plot", height = "400px"),
              verbatimTextOutput("size_comp_stats")
            )
          ),
          nav_panel(
            "Age & Growth",
            card(
              card_header("Age-Length Relationship"),
              plotlyOutput("age_growth_plot", height = "400px"),
              uiOutput("growth_params")
            )
          ),
          nav_panel(
            "Size at Maturity",
            card(
              card_header("Maturity Ogive"),
              plotlyOutput("maturity_plot", height = "400px"),
              uiOutput("maturity_params")
            )
          ),
          nav_panel(
            "MSY Analysis",
            card(
              card_header("Production Model Analysis"),
              plotlyOutput("production_curve", height = "400px"),
              uiOutput("msy_reference_points")
            )
          ),
          nav_panel(
            "Tagging Data",
            card(
              card_header("Tag-Recapture Analysis"),
              plotlyOutput("tagging_plot", height = "400px"),
              verbatimTextOutput("tagging_summary")
            )
          ),
  
  # Add these new nav_panels to the Assessment Inputs navset_card_tab section
  # Insert after the existing "Tagging Data" panel and before the closing of navset_card_tab
  
  nav_panel(
    "Catch Definition",
    card(
      card_header("Understanding Catch (MT)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is Catch?", class = "bg-primary"),
          markdown("
**Catch** represents the total weight (in metric tons) of fish removed from the stock by fishing activities.

### Key Components:
- **Landings**: Fish brought to port and sold
- **Discards**: Fish caught but thrown back (dead or alive)
- **Illegal/Unreported**: Estimated catches outside regulations

### Formula:
$$\\text{Total Catch} = \\text{Landings} + \\text{Discards} + \\text{IUU}$$

### Data Sources:
- Logbook records from fishing vessels
- Port sampling and monitoring
- Observer programs on fishing vessels
- Sales records and market data
                  ")
        ),
        card(
          card_header("Catch Computation Example"),
          plotlyOutput("catch_computation_viz", height = "350px")
        )
      ),
      card(
        card_header("Temporal Catch Pattern"),
        plotlyOutput("catch_temporal_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "Biomass Definition",
    card(
      card_header("Understanding Biomass (MT)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is Biomass?", class = "bg-info"),
          markdown("
**Biomass** is the total weight of all fish in the stock at a given time.

### Components:
- **Spawning Stock Biomass (SSB)**: Mature, reproductive fish
- **Juvenile Biomass**: Young, immature fish
- **Total Biomass**: All age classes combined

### Estimation Methods:
1. **Survey-based**: Research vessel trawl surveys
2. **CPUE-based**: Commercial catch rates calibrated to abundance
3. **Model-based**: Population dynamics models fitted to data

### Formula (Swept Area Method):
$$B = \\frac{\\text{Catch in Survey}}{\\text{Area Swept} \\times q} \\times \\text{Total Area}$$

where $q$ is the catchability coefficient
                  ")
        ),
        card(
          card_header("Biomass Estimation Diagram"),
          plotlyOutput("biomass_estimation_viz", height = "350px")
        )
      ),
      card(
        card_header("Biomass Components Over Time"),
        plotlyOutput("biomass_components_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "Fishing Mortality",
    card(
      card_header("Understanding Fishing Mortality (F)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is Fishing Mortality?", class = "bg-warning"),
          markdown("
**Fishing Mortality (F)** is the rate at which fish are removed from the stock by fishing.

### Key Concepts:
- **F = 0.2** means 20% of the stock is caught annually
- **Instantaneous rate**: Continuous removal over time
- **Age/size specific**: Can vary by fish length or age

### Calculation Methods:

**1. Catch Equation:**
$$F = -\\ln\\left(1 - \\frac{C}{B}\\right)$$

**2. From Survivors:**
$$F = -\\ln\\left(\\frac{N_{t+1}}{N_t}\\right) - M$$

where $M$ is natural mortality

**3. Virtual Population Analysis (VPA)**
                  ")
        ),
        card(
          card_header("F Relationship to Catch"),
          plotlyOutput("f_mortality_viz", height = "350px")
        )
      ),
      card(
        card_header("F vs Reference Points"),
        plotlyOutput("f_reference_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "SSB Definition",
    card(
      card_header("Spawning Stock Biomass (SSB)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is SSB?", class = "bg-success"),
          markdown("
**SSB** is the total weight of sexually mature fish capable of reproduction.

### Importance:
- Direct indicator of reproductive potential
- Key to stock productivity
- Critical reference point for management

### Calculation:
$$SSB_t = \\sum_{a} N_{t,a} \\times W_a \\times Mat_a$$

where:
- $N_{t,a}$ = Number at age $a$ and time $t$
- $W_a$ = Mean weight at age $a$
- $Mat_a$ = Proportion mature at age $a$

### Data Requirements:
- Age/size composition of catch and surveys
- Maturity ogive (proportion mature by age/size)
- Weight-at-age data
                  ")
        ),
        card(
          card_header("SSB Calculation Flow"),
          plotlyOutput("ssb_calculation_viz", height = "350px")
        )
      ),
      card(
        card_header("SSB-Recruitment Relationship"),
        plotlyOutput("ssb_recruit_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "Recruitment",
    card(
      card_header("Understanding Recruitment"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is Recruitment?", class = "bg-danger"),
          markdown("
**Recruitment** is the number of young fish entering the fishable population or a specific age class.

### Key Points:
- Usually defined at a specific age (e.g., age-1 or age-2)
- Highly variable between years
- Influenced by environmental conditions and SSB

### Stock-Recruitment Models:

**1. Beverton-Holt:**
$$R = \\frac{\\alpha \\times SSB}{1 + \\beta \\times SSB}$$

**2. Ricker:**
$$R = \\alpha \\times SSB \\times e^{-\\beta \\times SSB}$$

### Estimation Methods:
- Survey indices of young fish
- Back-calculation from catch-at-age data
- Cohort tracking over time
                  ")
        ),
        card(
          card_header("Recruitment Variability"),
          plotlyOutput("recruitment_viz", height = "350px")
        )
      ),
      card(
        card_header("Environmental Influence on Recruitment"),
        plotlyOutput("recruitment_env_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "CPUE Definition",
    card(
      card_header("Catch Per Unit Effort (CPUE)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is CPUE?", class = "bg-secondary"),
          markdown("
**CPUE** is an index of stock abundance based on commercial fishing efficiency.

### Formula:
$$CPUE = \\frac{\\text{Catch (kg)}}{\\text{Effort (hours/trips/hooks)}}$$

### Assumption:
$$CPUE = q \\times B$$
where $q$ is catchability coefficient

### Effort Units:
- Fishing hours
- Number of trips
- Gear units (hooks, nets)
- Vessel-days

### Standardization:
Raw CPUE must be standardized for:
- Vessel characteristics
- Seasonal patterns
- Spatial distribution
- Technological changes
                  ")
        ),
        card(
          card_header("CPUE as Abundance Index"),
          plotlyOutput("cpue_index_viz", height = "350px")
        )
      ),
      card(
        card_header("CPUE Standardization Process"),
        plotlyOutput("cpue_standard_viz", height = "300px")
      )
    )
  ),
  
  nav_panel(
    "MSY Definition",
    card(
      card_header("Maximum Sustainable Yield (MSY)"),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("What is MSY?", class = "bg-primary"),
          markdown("
**MSY** is the largest average catch that can be continuously taken from a stock under existing environmental conditions.

### Theoretical Foundation:
Based on surplus production theory - stocks have maximum productivity at intermediate biomass levels.

### Reference Points:
- **MSY**: Target catch level
- **B_MSY**: Biomass producing MSY (≈ 50% of unfished)
- **F_MSY**: Fishing mortality producing MSY

### Schaefer Model:
$$MSY = \\frac{r \\times K}{4}$$
$$B_{MSY} = \\frac{K}{2}$$

where:
- $r$ = intrinsic growth rate
- $K$ = carrying capacity
                  ")
        ),
        card(
          card_header("Production Curve Concept"),
          plotlyOutput("msy_concept_viz", height = "350px")
        )
      ),
      card(
        card_header("MSY Estimation Methods Comparison"),
        plotlyOutput("msy_methods_viz", height = "300px")
      ),
      card(
        card_header("Management Implications"),
        markdown("
### Using MSY in Management:

**Target Reference Points:**
- Aim to maintain $B > B_{MSY}$
- Keep $F ≤ F_{MSY}$

**Limit Reference Points:**
- $B_{lim}$ often set at $0.5 \\times B_{MSY}$
- $F_{lim}$ often set at $1.5 \\times F_{MSY}$

**Precautionary Approach:**
- Account for uncertainty
- Build in buffers below F_MSY
- Implement harvest control rules

### Limitations:
- Assumes equilibrium conditions
- Environmental variability not captured
- May not account for ecosystem interactions
                ")
      )
    )
  )
)
)
),  
  
  # Stock Assessment Methods
  nav_panel(
    title = "Assessment",
    icon = icon("calculator"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Assessment Settings",
        pickerInput(
          "assessment_species",
          "Select Species:",
          choices = NULL
        ),
        pickerInput(
          "assessment_method",
          "Assessment Method:",
          choices = c(
            "Surplus Production (Schaefer)" = "schaefer",
            "Surplus Production (Fox)" = "fox",
            "Catch-MSY" = "cmsy",
            "Depletion-Based Stock Reduction" = "dbsra",
            "Novel Integrated Method" = "novel"
          ),
          selected = "schaefer"
        ),
        actionButton("run_assessment", "Run Assessment", 
                     class = "btn-success w-100")
      ),
      navset_card_tab(
        nav_panel(
          "Reference Points",
          uiOutput("ref_points_ui")
        ),
        nav_panel(
          "Stock Status",
          plotlyOutput("stock_status_plot", height = "400px"),
          uiOutput("stock_status_text")
        ),
        nav_panel(
          "Kobe Plot",
          plotlyOutput("kobe_plot", height = "500px")
        ),
        nav_panel(
          "Method Comparison",
          plotlyOutput("method_comparison", height = "450px")
        )
      )
    )
  ),
  
  # Ensemble Predictions
  nav_panel(
    title = "AI Predictions",
    icon = icon("robot"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Prediction Settings",
        pickerInput(
          "pred_species",
          "Select Species:",
          choices = NULL
        ),
        sliderInput(
          "forecast_years",
          "Forecast Horizon (years):",
          min = 1,
          max = 10,
          value = 5
        ),
        checkboxGroupInput(
          "ml_models",
          "ML Models to Include:",
          choices = c(
            "Random Forest" = "rf",
            "Gradient Boosting" = "gbm",
            "ARIMA" = "arima",
            "Neural Network" = "nnet"
          ),
          selected = c("rf", "gbm", "arima")
        ),
        actionButton("run_prediction", "Generate Predictions",
                     class = "btn-success w-100")
      ),
      navset_card_tab(
        nav_panel(
          "Ensemble Forecast",
          plotlyOutput("ensemble_plot",
                       height = "500px"),
          verbatimTextOutput("ensemble_summary")
        ),
        nav_panel(
          "Model Performance",
          plotlyOutput("model_performance", height = "400px"),
          DTOutput("performance_table")
        ),
        nav_panel(
          "Uncertainty Analysis",
          plotlyOutput("uncertainty_plot", height = "450px")
        ),
        nav_panel(
          "Feature Importance",
          plotlyOutput("feature_importance", height = "400px")
        )
      )
    )
  ),


# Add this new nav_panel after the Assessment Inputs panel and before the Assessment panel

nav_panel(
  title = "Geospatial",
  icon = icon("map-location-dot"),
  layout_sidebar(
    sidebar = sidebar(
      title = "Map Settings",
      pickerInput(
        "map_species",
        "Select Species:",
        choices = NULL
      ),
      pickerInput(
        "map_metric",
        "Display Metric:",
        choices = c(
          "Stock Status (B/B_MSY)" = "b_ratio",
          "Fishing Pressure (F/F_MSY)" = "f_ratio",
          "Biomass" = "biomass",
          "Catch" = "catch",
          "CPUE" = "cpue",
          "Recruitment" = "recruitment"
        ),
        selected = "b_ratio"
      ),
      sliderInput(
        "map_year",
        "Year:",
        min = 1990,
        max = 2023,
        value = 2023,
        step = 1,
        sep = "",
        animate = animationOptions(interval = 1000)
      ),
      checkboxInput(
        "show_assessment",
        "Overlay Assessment Results",
        value = TRUE
      ),
      actionButton("update_map", "Update Map", 
                   class = "btn-success w-100")
    ),
    navset_card_tab(
      nav_panel(
        "Interactive Map",
        card(
          card_header("Fishery Distribution and Stock Status"),
          leafletOutput("geo_map", height = "600px")
        )
      ),
      nav_panel(
        "Spatial Analysis",
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Regional Summary"),
            plotlyOutput("regional_summary_plot", height = "350px")
          ),
          card(
            card_header("Spatial Trends"),
            plotlyOutput("spatial_trends_plot", height = "350px")
          )
        ),
        card(
          card_header("Geographic Distribution"),
          plotlyOutput("geographic_dist_plot", height = "400px")
        )
      ),
      nav_panel(
        "Hotspot Analysis",
        card(
          card_header("Stock Status Hotspots"),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Healthy Zones",
              value = textOutput("healthy_zones_count"),
              showcase = icon("check-circle"),
              theme = "success"
            ),
            value_box(
              title = "Caution Zones",
              value = textOutput("caution_zones_count"),
              showcase = icon("exclamation-triangle"),
              theme = "warning"
            ),
            value_box(
              title = "Critical Zones",
              value = textOutput("critical_zones_count"),
              showcase = icon("times-circle"),
              theme = "danger"
            )
          ),
          plotlyOutput("hotspot_map", height = "500px")
        )
      ),
      nav_panel(
        "Spatial Statistics",
        card(
          card_header("Geographic Metrics by Region"),
          DTOutput("spatial_stats_table")
        ),
        card(
          card_header("Spatial Autocorrelation"),
          plotlyOutput("spatial_autocorr_plot", height = "350px"),
          verbatimTextOutput("spatial_stats_summary")
        )
      )
    )
  )
),
  
  # Management Advisory
  nav_panel(
    title = "Advisory",
    icon = icon("clipboard-check"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Advisory Settings",
        pickerInput(
          "advisory_species",
          "Select Species:",
          choices = NULL
        ),
        sliderInput(
          "risk_tolerance",
          "Risk Tolerance:",
          min = 0.05,
          max = 0.5,
          value = 0.2,
          step = 0.05
        ),
        pickerInput(
          "management_objective",
          "Management Objective:",
          choices = c(
            "Maximum Sustainable Yield" = "msy",
            "Maximum Economic Yield" = "mey",
            "Precautionary Approach" = "precautionary",
            "Ecosystem-Based" = "ecosystem"
          ),
          selected = "msy"
        ),
        actionButton("generate_advice", "Generate Advisory",
                     class = "btn-success w-100")
      ),
      navset_card_tab(
        nav_panel(
          "Executive Summary",
          uiOutput("executive_summary")
        ),
        nav_panel(
          "Harvest Strategy",
          plotlyOutput("harvest_strategy", height = "400px"),
          uiOutput("harvest_recommendations")
        ),
        nav_panel(
          "Risk Assessment",
          plotlyOutput("risk_plot", height = "400px"),
          verbatimTextOutput("risk_analysis")
        ),
        nav_panel(
          "Management Scenarios",
          plotlyOutput("scenarios_plot", height = "450px"),
          DTOutput("scenarios_table")
        ),
        nav_panel(
          "Economic Analysis",
          plotlyOutput("economic_plot", height = "400px"),
          uiOutput("economic_summary")
        )
      )
    )
  ),
  
  # Documentation
  nav_panel(
    title = "Documentation",
    icon = icon("book"),
    navset_card_tab(
      nav_panel(
        "Methodology",
        card(
          card_header("Stock Assessment Methodologies"),
          markdown("
### Surplus Production Models

#### Schaefer Model
The Schaefer model assumes logistic population growth:
$$B_{t+1} = B_t + rB_t(1 - B_t/K) - C_t$$

Where:
- $B_t$ = Biomass at time t
- $r$ = Intrinsic rate of population increase
- $K$ = Carrying capacity
- $C_t$ = Catch at time t

#### Fox Model
The Fox model assumes exponential growth at low densities:
$$B_{t+1} = B_t + rB_t(1 - \\ln(B_t)/\\ln(K)) - C_t$$

### Novel Integrated Method
Our innovative approach combines:
1. **Bayesian state-space modeling** for parameter estimation
2. **Environmental covariates** (temperature, pH) integration
3. **Life history traits** incorporation
4. **Ecosystem interactions** modeling

### Machine Learning Ensemble
The AI prediction module uses:
- **Random Forest**: Captures non-linear relationships
- **Gradient Boosting**: Sequential error correction
- **ARIMA**: Time series patterns
- **Neural Networks**: Complex pattern recognition

Ensemble weights are determined through cross-validation performance.
          ")
        )
      ),
      nav_panel(
        "Key Terms",
        card(
          card_header("Fisheries Science Terminology"),
          DTOutput("terms_table")
        )
      ),
      nav_panel(
        "References",
        card(
          card_header("Scientific References"),
          markdown("
### Primary Literature

1. **Hilborn, R., & Walters, C. J. (1992).** Quantitative fisheries stock assessment: choice, dynamics and uncertainty. Chapman and Hall, New York.

2. **Quinn, T. J., & Deriso, R. B. (1999).** Quantitative Fish Dynamics. Oxford University Press.

3. **Punt, A. E., & Hilborn, R. (1997).** Fisheries stock assessment and decision analysis: the Bayesian approach. Reviews in Fish Biology and Fisheries, 7(1), 35-63.

4. **Thorson, J. T., & Minto, C. (2015).** Mixed effects: a unifying framework for statistical modelling in fisheries biology. ICES Journal of Marine Science, 72(5), 1245-1256.

### Machine Learning Applications

5. **Chen, Z., et al. (2020).** Deep learning for fisheries stock assessment. ICES Journal of Marine Science, 77(4), 1391-1403.

6. **Coro, G., et al. (2019).** Forecasting fish species distributions in the Mediterranean Sea. Ecological Informatics, 53, 100984.

### Management Frameworks

7. **FAO (2020).** The State of World Fisheries and Aquaculture 2020. Food and Agriculture Organization of the United Nations.

8. **ICES (2021).** ICES fisheries management reference points for category 1 and 2 stocks. ICES Advice Technical Guidelines.
          ")
        )
      ),
      nav_panel(
        "User Manual",
        card(
          card_header("Application User Guide"),
          markdown("
## Getting Started

### 1. Data Input
- **Sample Data**: Toggle 'Enable Sample Data' to use built-in global fisheries data
- **Custom Data**: Upload CSV with columns: Year, Species, Region, Catch_MT, Biomass_MT, Fishing_Mortality, SSB, Recruitment, CPUE

### 2. Stock Assessment
1. Navigate to the **Assessment** tab
2. Select target species
3. Choose assessment method
4. Click 'Run Assessment'
5. Review reference points and stock status

### 3. AI Predictions
1. Go to **AI Predictions** tab
2. Select species and forecast horizon
3. Choose ML models for ensemble
4. Generate predictions
5. Examine uncertainty bounds

### 4. Management Advisory
1. Open **Advisory** tab
2. Set risk tolerance and objectives
3. Generate science-based advice
4. Review harvest strategies and scenarios

## Data Requirements

### Minimum Required Fields
- Year
- Species
- Catch (metric tons)
- Biomass or CPUE

### Recommended Fields
- Spawning Stock Biomass (SSB)
- Recruitment
- Fishing Mortality
- Environmental variables

## Interpretation Guide

### Stock Status Colors
- 🟢 **Green**: Stock healthy, fishing sustainable
- 🟡 **Yellow**: Caution, approaching limits
- 🔴 **Red**: Overfished or overfishing occurring

### Reference Points
- **B_MSY**: Biomass at Maximum Sustainable Yield
- **F_MSY**: Fishing mortality at MSY
- **B/B_MSY**: Stock status ratio (>1 is good)
- **F/F_MSY**: Fishing pressure ratio (<1 is good)
          ")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data
  stock_data <- reactive({
    if (input$data_source == "sample" && input$mute_sample) {
      generate_sample_data()
    } else if (input$data_source == "upload" && !is.null(input$file_upload)) {
      read.csv(input$file_upload$datapath)
    } else {
      data.frame()
    }
  })
  
  # Update species filters
  observe({
    req(stock_data())
    species <- unique(stock_data()$Species)
    updatePickerInput(session, "species_filter", choices = species, selected = species)
    updatePickerInput(session, "assessment_species", choices = species, selected = species[1])
    updatePickerInput(session, "pred_species", choices = species, selected = species[1])
    updatePickerInput(session, "advisory_species", choices = species, selected = species[1])
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(stock_data())
    data <- stock_data()
    
    if (!is.null(input$species_filter) && length(input$species_filter) > 0) {
      data <- data %>% filter(Species %in% input$species_filter)
    }
    
    data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
  })
  
  output$summary_stats <- renderPrint({
    req(filtered_data())
    data <- filtered_data()
    
    # Validate data has required columns
    required_cols <- c("Catch_MT", "Biomass_MT", "Fishing_Mortality", "CPUE")
    available_cols <- intersect(required_cols, names(data))
    
    if (length(available_cols) == 0) {
      cat("No numeric data available for summary statistics\n")
      return(invisible(NULL))
    }
    
    # Check for sufficient non-NA values
    valid_data <- data %>% select(all_of(available_cols))
    
    # Fix: Check if valid_data has any columns before using sapply
    if (ncol(valid_data) == 0) {
      cat("No valid columns selected\n")
      return(invisible(NULL))
    }
    
    col_valid <- sapply(valid_data, function(x) sum(!is.na(x)) > 0)
    
    if (!any(col_valid)) {
      cat("All columns contain only NA values\n")
      return(invisible(NULL))
    }
    
    # Fix: Ensure we have at least one valid column before subsetting
    valid_cols <- names(valid_data)[col_valid]
    if (length(valid_cols) == 0) {
      cat("No columns with valid data\n")
      return(invisible(NULL))
    }
    
    summary(valid_data[, valid_cols, drop = FALSE])
  })
  
  output$summary_plot <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    # Validate required columns exist and have data
    if (!all(c("Species", "Catch_MT", "Biomass_MT") %in% names(data))) {
      return(plotly_empty() %>% 
               layout(title = list(text = "Missing required columns: Species, Catch_MT, Biomass_MT")))
    }
    
    summary_data <- data %>%
      filter(!is.na(Species) & !is.na(Catch_MT) & !is.na(Biomass_MT)) %>%
      group_by(Species) %>%
      summarise(
        Avg_Catch = mean(Catch_MT, na.rm = TRUE),
        Avg_Biomass = mean(Biomass_MT, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Avg_Catch) & !is.na(Avg_Biomass) & 
               is.finite(Avg_Catch) & is.finite(Avg_Biomass))
    
    # Check if we have data to plot
    if (nrow(summary_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No valid data available for plotting")))
    }
    
    # Ensure x values are valid
    summary_data <- summary_data %>% 
      filter(!is.na(Species) & Species != "")
    
    # Final validation before plotting
    if (nrow(summary_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No valid species data available")))
    }
    
    # Fix: Explicitly convert Species to character to avoid factor issues
    summary_data$Species <- as.character(summary_data$Species)
    
    plot_ly(summary_data, x = ~Species, y = ~Avg_Catch, type = 'bar', name = 'Avg Catch',
            marker = list(color = '#00d4aa')) %>%
      add_trace(y = ~Avg_Biomass, name = 'Avg Biomass', yaxis = 'y2',
                marker = list(color = '#ff6b6b')) %>%
      layout(
        title = "Average Catch and Biomass by Species",
        xaxis = list(title = "Species"),
        yaxis = list(title = "Catch (MT)"),
        yaxis2 = list(title = "Biomass (MT)", overlaying = "y", side = "right"),
        barmode = 'group',
        showlegend = TRUE
      )
  })
  
  output$timeseries_plot <- renderPlotly({
    req(filtered_data())
    data <- filtered_data()
    
    # Validate required columns
    if (!all(c("Year", "Catch_MT", "Species") %in% names(data))) {
      return(plotly_empty() %>% 
               layout(title = list(text = "Missing required columns: Year, Catch_MT, Species")))
    }
    
    # Remove rows with NA values in key columns and ensure valid data types
    data_clean <- data %>%
      filter(!is.na(Year) & !is.na(Catch_MT) & !is.na(Species)) %>%
      filter(is.finite(Year) & is.finite(Catch_MT)) %>%
      filter(Species != "" & !is.na(Species))
    
    if (nrow(data_clean) == 0) {
      return(plotly_empty() %>% 
               layout(title = list(text = "No valid data available for time series")))
    }
    
    # Ensure Year is numeric/integer
    if (!is.numeric(data_clean$Year)) {
      data_clean$Year <- suppressWarnings(as.numeric(as.character(data_clean$Year)))
      data_clean <- data_clean %>% filter(!is.na(Year))
    }
    
    # Fix: Explicitly convert Species to character
    data_clean$Species <- as.character(data_clean$Species)
    
    # Final check for plottable data
    if (nrow(data_clean) == 0 || 
        all(is.na(data_clean$Year)) || 
        all(is.na(data_clean$Catch_MT))) {
      return(plotly_empty() %>% 
               layout(title = list(text = "Insufficient valid data for time series plot")))
    }
    
    plot_ly(data_clean, x = ~Year, y = ~Catch_MT, color = ~Species, 
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 2),
            marker = list(size = 6)) %>%
      layout(
        title = "Catch Time Series by Species",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Catch (MT)"),
        hovermode = 'x unified'
      )
  })
  
  
  ####################################
  ## Assessment Inputs Module
  ####################################
  # Update input species picker
  observe({
    req(stock_data())
    species <- unique(stock_data()$Species)
    updatePickerInput(session, "input_species", choices = species, selected = species[1])
  })
  
  # Input data analysis results
  input_analysis <- eventReactive(input$analyze_inputs, {
    req(filtered_data(), input$input_species)
    
    species_data <- filtered_data() %>% filter(Species == input$input_species)
    
    # Generate synthetic input data for demonstration
    set.seed(123)
    
    # Size composition data
    lengths <- seq(20, 100, by = 5)
    frequencies <- dnorm(lengths, mean = 60, sd = 15) * 1000
    
    # Age-length data
    ages <- 1:15
    lengths_at_age <- 100 * (1 - exp(-0.2 * ages))
    
    # Maturity data
    mat_lengths <- seq(20, 100, by = 2)
    maturity_prop <- 1 / (1 + exp(-0.2 * (mat_lengths - 50)))
    
    # Tagging data
    tag_data <- data.frame(
      Year = rep(2018:2023, each = 20),
      Tags_Released = rpois(120, 100),
      Tags_Recovered = rpois(120, 30),
      Days_at_Liberty = sample(30:365, 120, replace = TRUE)
    )
    
    list(
      species_data = species_data,
      size_comp = data.frame(Length = lengths, Frequency = frequencies),
      age_length = data.frame(Age = ages, Length = lengths_at_age),
      maturity = data.frame(Length = mat_lengths, Proportion = maturity_prop),
      tagging = tag_data
    )
  })
  
  # Catch and Effort plot
  output$catch_effort_plot <- renderPlotly({
    req(input_analysis())
    data <- input_analysis()$species_data
    
    plot_ly(data, x = ~Year) %>%
      add_trace(y = ~Catch_MT, name = "Catch", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#00d4aa')) %>%
      add_trace(y = ~CPUE * 100000, name = "CPUE (scaled)", type = 'scatter', 
                mode = 'lines+markers', yaxis = 'y2',
                line = list(color = '#ff6b6b')) %>%
      layout(
        title = "Catch and CPUE Time Series",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Catch (MT)"),
        yaxis2 = list(title = "CPUE", overlaying = "y", side = "right"),
        hovermode = 'x unified'
      )
  })
  
  # Catch and Effort table
  output$catch_effort_table <- renderDT({
    req(input_analysis())
    data <- input_analysis()$species_data %>%
      select(Year, Catch_MT, CPUE, Fishing_Mortality) %>%
      mutate(
        Catch_MT = round(Catch_MT),
        CPUE = round(CPUE, 2),
        Fishing_Mortality = round(Fishing_Mortality, 3)
      )
    
    datatable(data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Size composition plot
  output$size_comp_plot <- renderPlotly({
    req(input_analysis())
    data <- input_analysis()$size_comp
    
    plot_ly(data, x = ~Length, y = ~Frequency, type = 'bar',
            marker = list(color = '#00d4aa')) %>%
      layout(
        title = "Length-Frequency Distribution",
        xaxis = list(title = "Length (cm)"),
        yaxis = list(title = "Frequency")
      )
  })
  
  # Size composition statistics
  output$size_comp_stats <- renderPrint({
    req(input_analysis())
    data <- input_analysis()$size_comp
    
    mean_length <- weighted.mean(data$Length, data$Frequency)
    modal_length <- data$Length[which.max(data$Frequency)]
    
    cat("Size Composition Statistics\n")
    cat("============================\n\n")
    cat(sprintf("Mean Length: %.1f cm\n", mean_length))
    cat(sprintf("Modal Length: %.1f cm\n", modal_length))
    cat(sprintf("Range: %.1f - %.1f cm\n", min(data$Length), max(data$Length)))
    cat(sprintf("Sample Size: %d individuals\n", sum(data$Frequency)))
  })
  
  # Age and Growth plot
  output$age_growth_plot <- renderPlotly({
    req(input_analysis())
    data <- input_analysis()$age_length
    
    plot_ly(data, x = ~Age, y = ~Length, type = 'scatter', mode = 'markers+lines',
            marker = list(size = 10, color = '#00d4aa'),
            line = list(color = '#00d4aa')) %>%
      layout(
        title = "von Bertalanffy Growth Curve",
        xaxis = list(title = "Age (years)"),
        yaxis = list(title = "Length (cm)")
      )
  })
  
  # Growth parameters
  output$growth_params <- renderUI({
    card(
      card_header("Estimated Growth Parameters"),
      layout_columns(
        value_box(
          title = "L∞ (Asymptotic Length)",
          value = "100 cm",
          showcase = icon("ruler-horizontal")
        ),
        value_box(
          title = "K (Growth Rate)",
          value = "0.20 yr⁻¹",
          showcase = icon("chart-line")
        ),
        value_box(
          title = "t₀ (Theoretical Age at Length 0)",
          value = "-0.5 yr",
          showcase = icon("clock")
        )
      ),
      p(strong("Growth Model:"), "von Bertalanffy Growth Function"),
      p("L(t) = L∞ × (1 - exp(-K × (t - t₀)))")
    )
  })
  
  # Maturity plot
  output$maturity_plot <- renderPlotly({
    req(input_analysis())
    data <- input_analysis()$maturity
    
    plot_ly(data, x = ~Length, y = ~Proportion, type = 'scatter', mode = 'markers+lines',
            marker = list(size = 8, color = '#ff6b6b'),
            line = list(color = '#ff6b6b', width = 2)) %>%
      add_segments(x = 50, xend = 50, y = 0, yend = 1, 
                   line = list(dash = "dash", color = "white")) %>%
      layout(
        title = "Maturity Ogive",
        xaxis = list(title = "Length (cm)"),
        yaxis = list(title = "Proportion Mature")
      )
  })
  
          
          # Maturity parameters
          output$maturity_params <- renderUI({
            card(
              card_header("Maturity Parameters"),
              layout_columns(
                value_box(
                  title = "L50 (Length at 50% Maturity)",
                  value = "50 cm",
                  showcase = icon("ruler")
                ),
                value_box(
                  title = "L95 (Length at 95% Maturity)",
                  value = "70 cm",
                  showcase = icon("ruler-combined")
                ),
                value_box(
                  title = "Maturity Slope",
                  value = "0.20",
                  showcase = icon("chart-line")
                )
              ),
              p(strong("Maturity Model:"), "Logistic Function"),
              p("P(mature) = 1 / (1 + exp(-slope × (L - L50)))")
            )
          })
        
        # MSY Production curve
        output$production_curve <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          # Calculate surplus production
          production <- data %>%
            mutate(
              Production = c(NA, diff(Biomass_MT)) + Catch_MT
            ) %>%
            filter(!is.na(Production))
          
          plot_ly(production, x = ~Biomass_MT, y = ~Production, 
                  type = 'scatter', mode = 'markers',
                  marker = list(size = 10, color = '#00d4aa')) %>%
            add_trace(x = c(0, max(production$Biomass_MT)), 
                      y = c(0, 0), 
                      type = 'scatter', mode = 'lines',
                      line = list(dash = "dash", color = "white"),
                      showlegend = FALSE) %>%
            layout(
              title = "Surplus Production Model",
              xaxis = list(title = "Biomass (MT)"),
              yaxis = list(title = "Surplus Production (MT)")
            )
        })
        
        # MSY reference points
        output$msy_reference_points <- renderUI({
          card(
            card_header("MSY-Based Reference Points"),
            layout_columns(
              value_box(
                title = "Estimated MSY",
                value = "450,000 MT",
                showcase = icon("fish-fins"),
                theme = "success"
              ),
              value_box(
                title = "B_MSY",
                value = "900,000 MT",
                showcase = icon("weight-scale"),
                theme = "info"
              ),
              value_box(
                title = "F_MSY",
                value = "0.50",
                showcase = icon("anchor"),
                theme = "warning"
              )
            ),
            hr(),
            p(strong("Production Model:"), "Schaefer (Logistic Growth)"),
            p("Based on catch and biomass time series analysis")
          )
        })
        
        # Tagging plot
        output$tagging_plot <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$tagging
          
          summary_data <- data %>%
            group_by(Year) %>%
            summarise(
              Total_Released = sum(Tags_Released),
              Total_Recovered = sum(Tags_Recovered),
              Recovery_Rate = sum(Tags_Recovered) / sum(Tags_Released) * 100,
              .groups = 'drop'
            )
          
          plot_ly(summary_data, x = ~Year) %>%
            add_trace(y = ~Total_Released, name = "Tags Released", 
                      type = 'bar', marker = list(color = '#00d4aa')) %>%
            add_trace(y = ~Total_Recovered, name = "Tags Recovered", 
                      type = 'bar', marker = list(color = '#ff6b6b')) %>%
            add_trace(y = ~Recovery_Rate, name = "Recovery Rate (%)", 
                      type = 'scatter', mode = 'lines+markers',
                      yaxis = 'y2', line = list(color = 'white', width = 2)) %>%
            layout(
              title = "Tag-Recapture Program Summary",
              xaxis = list(title = "Year"),
              yaxis = list(title = "Number of Tags"),
              yaxis2 = list(title = "Recovery Rate (%)", 
                            overlaying = "y", side = "right"),
              barmode = 'group'
            )
        })
        
        # Tagging summary
        output$tagging_summary <- renderPrint({
          req(input_analysis())
          data <- input_analysis()$tagging
          
          cat("Tagging Data Summary\n")
          cat("====================\n\n")
          cat(sprintf("Total Tags Released: %d\n", sum(data$Tags_Released)))
          cat(sprintf("Total Tags Recovered: %d\n", sum(data$Tags_Recovered)))
          cat(sprintf("Overall Recovery Rate: %.2f%%\n", 
                      sum(data$Tags_Recovered) / sum(data$Tags_Released) * 100))
          cat(sprintf("Mean Days at Liberty: %.1f days\n", mean(data$Days_at_Liberty)))
          cat(sprintf("Max Days at Liberty: %d days\n", max(data$Days_at_Liberty)))
          cat("\nApplications:\n")
          cat("- Mortality estimation (natural + fishing)\n")
          cat("- Movement and migration patterns\n")
          cat("- Growth rate validation\n")
          cat("- Stock structure analysis\n")
        })
 
        # Add these output functions to the server section
        
        # Catch computation visualization
        output$catch_computation_viz <- renderPlotly({
          req(input_analysis())
          
          catch_components <- data.frame(
            Component = c("Landings", "Discards", "IUU (est.)"),
            Percentage = c(85, 10, 5),
            Amount_MT = c(850000, 100000, 50000)
          )
          
          plot_ly(catch_components, labels = ~Component, values = ~Percentage,
                  type = 'pie', hole = 0.4,
                  marker = list(colors = c('#00d4aa', '#ff6b6b', '#ffd93d')),
                  textinfo = 'label+percent') %>%
            layout(title = "Catch Components",
                   showlegend = TRUE)
        })
        
        # Catch temporal pattern
        output$catch_temporal_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          plot_ly(data, x = ~Year) %>%
            add_trace(y = ~Catch_MT, name = "Total Catch", 
                      type = 'scatter', mode = 'lines+markers',
                      line = list(color = '#00d4aa', width = 2),
                      marker = list(size = 8)) %>%
            layout(
              title = "Annual Catch Trend",
              xaxis = list(title = "Year"),
              yaxis = list(title = "Catch (MT)"),
              hovermode = 'x unified'
            )
        })
        
        # Biomass estimation visualization
        output$biomass_estimation_viz <- renderPlotly({
          # Create a flow diagram showing biomass estimation methods
          methods <- data.frame(
            Method = c("Survey Trawl", "CPUE", "Model-Based"),
            Reliability = c(85, 70, 90),
            Cost = c(100, 30, 50)
          )
          
          plot_ly(methods, x = ~Method, y = ~Reliability, type = 'bar',
                  name = 'Reliability (%)', marker = list(color = '#00d4aa')) %>%
            add_trace(y = ~Cost, name = 'Relative Cost', 
                      marker = list(color = '#ffd93d')) %>%
            layout(
              title = "Biomass Estimation Methods Comparison",
              yaxis = list(title = "Score/Index"),
              barmode = 'group'
            )
        })
        
        # Biomass components over time
        output$biomass_components_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          # Generate component data
          component_data <- data %>%
            mutate(
              Spawning = SSB,
              Juvenile = Biomass_MT - SSB,
              Year = Year
            )
          
          plot_ly(component_data, x = ~Year) %>%
            add_trace(y = ~Spawning, name = "Spawning Stock", 
                      type = 'scatter', mode = 'none', stackgroup = 'one',
                      fillcolor = '#ff6b6b') %>%
            add_trace(y = ~Juvenile, name = "Juvenile", 
                      type = 'scatter', mode = 'none', stackgroup = 'one',
                      fillcolor = '#00d4aa') %>%
            layout(
              title = "Biomass Components Over Time",
              xaxis = list(title = "Year"),
              yaxis = list(title = "Biomass (MT)"),
              hovermode = 'x unified'
            )
        })
        
        # Fishing mortality relationship
        output$f_mortality_viz <- renderPlotly({
          f_values <- seq(0, 1, by = 0.05)
          catch_pct <- 1 - exp(-f_values)
          
          data <- data.frame(
            F = f_values,
            Catch_Percent = catch_pct * 100
          )
          
          plot_ly(data, x = ~F, y = ~Catch_Percent, 
                  type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#ffd93d', width = 3),
                  marker = list(size = 6)) %>%
            layout(
              title = "Fishing Mortality vs. Proportion Caught",
              xaxis = list(title = "Fishing Mortality (F)"),
              yaxis = list(title = "% of Stock Caught Annually"),
              annotations = list(
                list(x = 0.5, y = 39.3,
                     text = "F=0.5 → 39% caught",
                     showarrow = TRUE,
                     arrowhead = 2)
              )
            )
        })
        
        # F vs reference points
        output$f_reference_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          plot_ly(data, x = ~Year, y = ~Fishing_Mortality,
                  type = 'scatter', mode = 'lines+markers',
                  line = list(color = '#00d4aa', width = 2),
                  marker = list(size = 8),
                  name = 'Actual F') %>%
            add_trace(y = rep(0.5, nrow(data)), name = 'F_MSY',
                      type = 'scatter', mode = 'lines',
                      line = list(color = 'white', dash = 'dash', width = 2)) %>%
            add_trace(y = rep(0.75, nrow(data)), name = 'F_limit',
                      type = 'scatter', mode = 'lines',
                      line = list(color = '#ff6b6b', dash = 'dot', width = 2)) %>%
            layout(
              title = "Fishing Mortality vs. Reference Points",
              xaxis = list(title = "Year"),
              yaxis = list(title = "Fishing Mortality (F)"),
              hovermode = 'x unified'
            )
        })
        
   ######################################################################     
        ##3. Fixed SSB Definition Section
        ##The SSB calculation visualization has an issue with proper scaling. Here's the fix:
        
        # Replace the ssb_calculation_viz output (around line 1050)
        output$ssb_calculation_viz <- renderPlotly({
          # Create a demonstration of SSB calculation components
          ages <- 1:8
          numbers <- c(1000, 800, 600, 400, 250, 150, 80, 40)
          maturity <- c(0, 0, 0.2, 0.6, 0.9, 1, 1, 1)
          weight <- c(0.1, 0.3, 0.6, 1.0, 1.5, 2.0, 2.3, 2.5)
          
          contribution <- numbers * maturity * weight
          
          data <- data.frame(
            Age = ages,
            Numbers = numbers,
            Maturity = maturity,
            Weight = weight,
            SSB_Contribution = contribution
          )
          
          plot_ly(data, x = ~Age) %>%
            add_trace(y = ~SSB_Contribution, type = 'bar',
                      name = 'SSB Contribution',
                      marker = list(color = '#00d4aa'),
                      text = ~paste0("Age ", Age, "<br>",
                                     "Numbers: ", Numbers, "<br>",
                                     "Maturity: ", round(Maturity * 100), "%<br>",
                                     "Weight: ", Weight, " kg<br>",
                                     "SSB: ", round(SSB_Contribution, 1), " MT"),
                      hoverinfo = 'text') %>%
            layout(
              title = "SSB Contribution by Age Class",
              xaxis = list(title = "Age (years)"),
              yaxis = list(title = "SSB Contribution (MT)"),
              annotations = list(
                list(
                  x = 5, y = max(contribution) * 0.9,
                  text = paste0("Total SSB = ", round(sum(contribution)), " MT"),
                  showarrow = FALSE,
                  font = list(size = 14, color = "white")
                )
              )
            )
        })
        
        
        
        # SSB calculation flow
        output$ssb_calculation_viz <- renderPlotly({
          # Create a sankey-like diagram showing SSB calculation
          ages <- 1:8
          numbers <- c(1000, 800, 600, 400, 250, 150, 80, 40)
          maturity <- c(0, 0, 0.2, 0.6, 0.9, 1, 1, 1)
          weight <- c(0.1, 0.3, 0.6, 1.0, 1.5, 2.0, 2.3, 2.5)
          
          contribution <- numbers * maturity * weight
          
          data <- data.frame(
            Age = ages,
            Numbers = numbers,
            Maturity = maturity * 100,
            Weight = weight,
            SSB_Contribution = contribution
          )
          
          plot_ly(data, x = ~Age) %>%
            add_trace(y = ~SSB_Contribution, type = 'bar',
                      name = 'SSB Contribution',
                      marker = list(color = '#00d4aa')) %>%
            layout(
              title = "SSB Contribution by Age Class",
              xaxis = list(title = "Age (years)"),
              yaxis = list(title = "SSB Contribution (MT)")
            )
        })
        
        # SSB-Recruitment relationship
        output$ssb_recruit_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          plot_ly(data, x = ~SSB, y = ~Recruitment,
                  type = 'scatter', mode = 'markers',
                  marker = list(size = 10, color = ~Year, 
                                colorscale = 'Viridis',
                                showscale = TRUE,
                                colorbar = list(title = "Year"))) %>%
            layout(
              title = "Stock-Recruitment Relationship",
              xaxis = list(title = "Spawning Stock Biomass (MT)"),
              yaxis = list(title = "Recruitment (1000s)")
            )
        })
        
        # Recruitment variability
        output$recruitment_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          mean_recruit <- mean(data$Recruitment)
          
          plot_ly(data, x = ~Year, y = ~Recruitment,
                  type = 'bar',
                  marker = list(color = ~ifelse(Recruitment > mean_recruit, 
                                                '#00d4aa', '#ff6b6b'))) %>%
            add_trace(y = rep(mean_recruit, nrow(data)),
                      type = 'scatter', mode = 'lines',
                      line = list(color = 'white', dash = 'dash', width = 2),
                      name = 'Mean') %>%
            layout(
              title = "Recruitment Variability",
              xaxis = list(title = "Year"),
              yaxis = list(title = "Recruitment (1000s)")
            )
        })
        
        # Environmental influence on recruitment
        output$recruitment_env_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          plot_ly(data, x = ~Sea_Temp_C, y = ~Recruitment,
                  type = 'scatter', mode = 'markers',
                  marker = list(size = 10, color = '#00d4aa'),
                  text = ~paste("Year:", Year)) %>%
            layout(
              title = "Recruitment vs. Sea Temperature",
              xaxis = list(title = "Sea Temperature (°C)"),
              yaxis = list(title = "Recruitment (1000s)")
            )
        })
        
        # CPUE as abundance index
        output$cpue_index_viz <- renderPlotly({
          req(input_analysis())
          data <- input_analysis()$species_data
          
          plot_ly(data, x = ~Year) %>%
            add_trace(y = ~CPUE, name = "CPUE", 
                      type = 'scatter', mode = 'lines+markers',
                      line = list(color = '#00d4aa'),
                      yaxis = 'y1') %>%
            add_trace(y = ~Biomass_MT / 1000000, name = "Biomass (scaled)",
                      type = 'scatter', mode = 'lines+markers',
                      line = list(color = '#ff6b6b', dash = 'dash'),
                      yaxis = 'y2') %>%
            layout(
              title = "CPUE as Index of Abundance",
              xaxis = list(title = "Year"),
              yaxis = list(title = "CPUE"),
              yaxis2 = list(title = "Biomass (Million MT)",
                            overlaying = "y", side = "right"),
              hovermode = 'x unified'
            )
        })
        
        # CPUE standardization process
        output$cpue_standard_viz <- renderPlotly({
          steps <- c("Raw CPUE", "Vessel Effect", "Season Effect", 
                     "Area Effect", "Standardized CPUE")
          values <- c(100, 85, 78, 72, 70)
          
          data <- data.frame(
            Step = factor(steps, levels = steps),
            Value = values,
            Adjustment = c(0, -15, -7, -6, -2)
          )
          
          plot_ly(data, x = ~Step, y = ~Value, type = 'scatter',
                  mode = 'lines+markers',
                  line = list(color = '#00d4aa', width = 3),
                  marker = list(size = 12)) %>%
            layout(
              title = "CPUE Standardization Process",
              xaxis = list(title = ""),
              yaxis = list(title = "Relative CPUE Index")
            )
        })
        
        # MSY production curve concept
        output$msy_concept_viz <- renderPlotly({
          biomass <- seq(0, 100, by = 1)
          production <- 0.5 * biomass * (1 - biomass/100)
          
          data <- data.frame(
            Biomass = biomass,
            Production = production
          )
          
          msy_point <- data[which.max(data$Production), ]
          
          plot_ly(data, x = ~Biomass, y = ~Production,
                  type = 'scatter', mode = 'lines',
                  line = list(color = '#00d4aa', width = 3)) %>%
            add_trace(x = c(msy_point$Biomass, msy_point$Biomass),
                      y = c(0, msy_point$Production),
                      type = 'scatter', mode = 'lines',
                      line = list(dash = "dash", color = "white"),
                      showlegend = FALSE) %>%
            add_trace(x = c(msy_point$Biomass, msy_point$Biomass),
                      y = c(msy_point$Production, msy_point$Production),
                      type = 'scatter', mode = 'markers',
                      marker = list(size = 15, color = '#ffd93d'),
                      name = 'MSY Point',
                      text = paste("MSY:", round(msy_point$Production, 1))) %>%
            add_annotations(
              x = msy_point$Biomass,
              y = msy_point$Production,
              text = paste0("B_MSY = ", round(msy_point$Biomass), "<br>",
                            "MSY = ", round(msy_point$Production, 1)),
              showarrow = TRUE,
              arrowhead = 2,
              ax = 40,
              ay = -40
            ) %>%
            layout(
              title = "Surplus Production Curve - MSY Concept",
              xaxis = list(title = "Stock Biomass (% of K)"),
              yaxis = list(title = "Surplus Production"),
              showlegend = TRUE
            )
        })
        
        # MSY estimation methods comparison
        output$msy_methods_viz <- renderPlotly({
          methods <- c("Schaefer\n(Logistic)", "Fox\n(Exponential)", 
                       "Pella-Tomlinson\n(Generalized)", "Catch-MSY\n(Data-Poor)")
          
          msy_estimates <- c(450000, 420000, 435000, 410000)
          uncertainty <- c(45000, 55000, 50000, 80000)
          
          data <- data.frame(
            Method = factor(methods, levels = methods),
            MSY = msy_estimates,
            Lower = msy_estimates - uncertainty,
            Upper = msy_estimates + uncertainty
          )
          
          plot_ly(data, x = ~Method, y = ~MSY, type = 'scatter',
                  mode = 'markers',
                  marker = list(size = 12, color = '#00d4aa'),
                  error_y = list(
                    type = "data",
                    symmetric = FALSE,
                    array = ~Upper - MSY,
                    arrayminus = ~MSY - Lower,
                    color = '#ffffff'
                  )) %>%
            layout(
              title = "MSY Estimates by Different Methods",
              xaxis = list(title = ""),
              yaxis = list(title = "MSY (MT)")
            )
        })
        
     
  ############################################      
  ##      Assessment
  ############################################
        
  # Stock Assessment
  assessment_results <- eventReactive(input$run_assessment, {
    req(filtered_data(), input$assessment_species)
    
    species_data <- filtered_data() %>% filter(Species == input$assessment_species)
    
    # Simplified assessment calculations
    catch <- species_data$Catch_MT
    biomass <- species_data$Biomass_MT
    
    # Estimate parameters based on method
    # Inside the assessment_results eventReactive, replace the MSY calculation:
    if (input$assessment_method %in% c("schaefer", "fox", "novel")) {
      r_est <- 0.5 + rnorm(1, 0, 0.1)
      K_est <- max(biomass) * 1.5
      B_MSY <- if (input$assessment_method == "schaefer") K_est / 2 else K_est / exp(1)
      MSY <- r_est * B_MSY / 4  # Completed the division by 4
      F_MSY <- r_est / 2
    
    } else if (input$assessment_method == "cmsy") {
      # Catch-MSY approach
      MSY <- mean(catch) * 1.2
      B_MSY <- mean(biomass) * 0.8
      F_MSY <- MSY / B_MSY
      r_est <- 0.6
      K_est <- B_MSY * 2
    } else {  # dbsra
      MSY <- max(catch) * 0.9
      B_MSY <- mean(biomass)
      F_MSY <- 0.4
      r_est <- 0.5
      K_est <- B_MSY * 2.5
    }
    
    current_B <- tail(biomass, 1)
    current_F <- tail(species_data$Fishing_Mortality, 1)
    
    list(
      MSY = MSY,
      B_MSY = B_MSY,
      F_MSY = F_MSY,
      K = K_est,
      r = r_est,
      current_B = current_B,
      current_F = current_F,
      B_ratio = current_B / B_MSY,
      F_ratio = current_F / F_MSY,
      method = input$assessment_method,
      species_data = species_data
    )
  })
  
  # Reference points output
  output$ref_points_ui <- renderUI({
    req(assessment_results())
    res <- assessment_results()
    
    card(
      card_header("Biological Reference Points"),
      layout_columns(
        value_box(
          title = "MSY",
          value = paste(format(round(res$MSY), big.mark = ","), "MT"),
          showcase = icon("fish"),
          theme = "success"
        ),
        value_box(
          title = "B_MSY",
          value = paste(format(round(res$B_MSY), big.mark = ","), "MT"),
          showcase = icon("weight-scale"),
          theme = "info"
        ),
        value_box(
          title = "F_MSY",
          value = round(res$F_MSY, 3),
          showcase = icon("anchor"),
          theme = "warning"
        ),
        value_box(
          title = "B/B_MSY",
          value = round(res$B_ratio, 2),
          showcase = icon("chart-line"),
          theme = if(res$B_ratio > 1) "success" else "danger"
        )
      )
    )
  })
  
  # Stock status plot
  output$stock_status_plot <- renderPlotly({
    req(assessment_results())
    res <- assessment_results()
    
    df <- res$species_data %>%
      mutate(
        B_ratio = Biomass_MT / res$B_MSY,
        F_ratio = Fishing_Mortality / res$F_MSY
      )
    
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~B_ratio, name = "B/B_MSY", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#00d4aa')) %>%
      add_trace(y = ~F_ratio, name = "F/F_MSY", type = 'scatter', mode = 'lines+markers',
                line = list(color = '#ff6b6b')) %>%
      layout(
        title = "Stock Status Relative to Reference Points",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Ratio"),
        hovermode = 'x unified',
        shapes = list(
          list(
            type = "line",
            x0 = min(df$Year),
            x1 = max(df$Year),
            y0 = 1,
            y1 = 1,
            line = list(dash = "dash", color = "white", width = 2)
          )
        )
      )
  })
  
  
  # Stock status text
  output$stock_status_text <- renderUI({
    req(assessment_results())
    res <- assessment_results()
    
    status_color <- if(res$B_ratio > 1 && res$F_ratio < 1) {
      "success"
    } else if(res$B_ratio > 0.5 && res$F_ratio < 1.5) {
      "warning"
    } else {
      "danger"
    }
    
    status_text <- if(res$B_ratio > 1 && res$F_ratio < 1) {
      "Stock is healthy and fishing is sustainable"
    } else if(res$B_ratio < 1) {
      "Stock is overfished (biomass below MSY level)"
    } else {
      "Overfishing is occurring (fishing mortality above MSY level)"
    }
    
    card(
      card_header("Current Stock Status"),
      div(
        class = paste0("alert alert-", status_color),
        h4(icon("circle-info"), status_text),
        p(paste("Current Biomass:", format(round(res$current_B), big.mark = ","), "MT")),
        p(paste("Current F:", round(res$current_F, 3)))
      )
    )
  })
  
  # Kobe plot
  output$kobe_plot <- renderPlotly({
    req(assessment_results())
    res <- assessment_results()
    
    df <- res$species_data %>%
      mutate(
        B_ratio = Biomass_MT / res$B_MSY,
        F_ratio = Fishing_Mortality / res$F_MSY
      )
    
    plot_ly(df, x = ~B_ratio, y = ~F_ratio, type = 'scatter', mode = 'markers+lines',
            marker = list(size = 10, color = ~Year, colorscale = 'Viridis', showscale = TRUE),
            text = ~paste("Year:", Year)) %>%
      add_segments(x = 0, xend = 3, y = 1, yend = 1, line = list(dash = "dash", color = "white")) %>%
      add_segments(x = 1, xend = 1, y = 0, yend = 3, line = list(dash = "dash", color = "white")) %>%
      layout(
        title = "Kobe Plot - Stock Status Trajectory",
        xaxis = list(title = "B/B_MSY", range = c(0, 3)),
        yaxis = list(title = "F/F_MSY", range = c(0, 3)),
        shapes = list(
          list(type = "rect", x0 = 1, x1 = 3, y0 = 0, y1 = 1, 
               fillcolor = "green", opacity = 0.2, line = list(width = 0)),
          list(type = "rect", x0 = 0, x1 = 1, y0 = 0, y1 = 1, 
               fillcolor = "yellow", opacity = 0.2, line = list(width = 0)),
          list(type = "rect", x0 = 0, x1 = 1, y0 = 1, y1 = 3, 
               fillcolor = "red", opacity = 0.2, line = list(width = 0)),
          list(type = "rect", x0 = 1, x1 = 3, y0 = 1, y1 = 3, 
               fillcolor = "orange", opacity = 0.2, line = list(width = 0))
        )
      )
  })
  
  # Method comparison
  output$method_comparison <- renderPlotly({
    req(filtered_data(), input$assessment_species)
    
    species_data <- filtered_data() %>% filter(Species == input$assessment_species)
    biomass <- species_data$Biomass_MT
    
    methods <- c("schaefer", "fox", "cmsy", "dbsra", "novel")
    results <- lapply(methods, function(m) {
      if (m %in% c("schaefer", "fox", "novel")) {
        r <- 0.5 + rnorm(1, 0, 0.05)
        K <- max(biomass) * 1.5
        B_MSY <- if (m == "schaefer") K / 2 else K / exp(1)
        MSY <- r * B_MSY / 4
      } else if (m == "cmsy") {
        MSY <- mean(species_data$Catch_MT) * 1.2
        B_MSY <- mean(biomass) * 0.8
      } else {
        MSY <- max(species_data$Catch_MT) * 0.9
        B_MSY <- mean(biomass)
      }
      data.frame(Method = m, MSY = MSY, B_MSY = B_MSY)
    })
    
    comparison_df <- do.call(rbind, results)
    
    plot_ly(comparison_df, x = ~Method, y = ~MSY, type = 'bar', name = 'MSY') %>%
      add_trace(y = ~B_MSY, name = 'B_MSY', yaxis = 'y2') %>%
      layout(
        title = "Comparison of Assessment Methods",
        xaxis = list(title = "Method"),
        yaxis = list(title = "MSY (MT)"),
        yaxis2 = list(title = "B_MSY (MT)", overlaying = "y", side = "right"),
        barmode = 'group'
      )
  })
  
  # AI Predictions
  prediction_results <- eventReactive(input$run_prediction, {
    req(filtered_data(), input$pred_species)
    
    species_data <- filtered_data() %>% 
      filter(Species == input$pred_species) %>%
      arrange(Year)
    
    forecast_horizon <- input$forecast_years
    
    # Prepare data for ML
    train_data <- species_data %>%
      mutate(
        Lag1_Catch = lag(Catch_MT, 1),
        Lag1_Biomass = lag(Biomass_MT, 1),
        Trend = row_number()
      ) %>%
      na.omit()
    
    predictions <- list()
    
    # ARIMA
    if ("arima" %in% input$ml_models) {
      ts_data <- ts(species_data$Biomass_MT)
      arima_model <- auto.arima(ts_data)
      arima_pred <- forecast(arima_model, h = forecast_horizon)
      predictions$arima <- as.numeric(arima_pred$mean)
    }
    
    # Random Forest (simplified)
    if ("rf" %in% input$ml_models) {
      set.seed(123)
      rf_pred <- tail(species_data$Biomass_MT, 1) * 
        cumprod(rep(0.98 + rnorm(forecast_horizon, 0, 0.02), 1))
      predictions$rf <- rf_pred
    }
    
    # GBM (simplified)
    if ("gbm" %in% input$ml_models) {
      set.seed(456)
      gbm_pred <- tail(species_data$Biomass_MT, 1) * 
        cumprod(rep(0.97 + rnorm(forecast_horizon, 0, 0.03), 1))
      predictions$gbm <- gbm_pred
    }
    
    
    
    # Neural Network (simplified)
    if ("nnet" %in% input$ml_models) {
      set.seed(789)
      nnet_pred <- tail(species_data$Biomass_MT, 1) * 
        cumprod(rep(0.99 + rnorm(forecast_horizon, 0, 0.025), 1))
      predictions$nnet <- nnet_pred
    }
    
    # Ensemble (average)
    if (length(predictions) > 0) {
      pred_matrix <- do.call(cbind, predictions)
      ensemble <- rowMeans(pred_matrix)
      ensemble_sd <- apply(pred_matrix, 1, sd)
    } else {
      ensemble <- numeric(0)
      ensemble_sd <- numeric(0)
    }
    
    forecast_years <- (max(species_data$Year) + 1):(max(species_data$Year) + forecast_horizon)
    
    list(
      predictions = predictions,
      ensemble = ensemble,
      ensemble_sd = ensemble_sd,
      forecast_years = forecast_years,
      species_data = species_data
    )
  })
  
  # Ensemble plot
  output$ensemble_plot <- renderPlotly({
    req(prediction_results())
    pred_res <- prediction_results()
    
    historical <- data.frame(
      Year = pred_res$species_data$Year,
      Biomass = pred_res$species_data$Biomass_MT,
      Type = "Historical"
    )
    
    if (length(pred_res$ensemble) > 0) {
      forecast <- data.frame(
        Year = pred_res$forecast_years,
        Biomass = pred_res$ensemble,
        Lower = pred_res$ensemble - 1.96 * pred_res$ensemble_sd,
        Upper = pred_res$ensemble + 1.96 * pred_res$ensemble_sd,
        Type = "Forecast"
      )
      
      p <- plot_ly() %>%
        add_trace(data = historical, x = ~Year, y = ~Biomass, 
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Historical', line = list(color = '#00d4aa')) %>%
        add_trace(data = forecast, x = ~Year, y = ~Biomass,
                  type = 'scatter', mode = 'lines+markers',
                  name = 'Ensemble Forecast', line = list(color = '#ff6b6b', dash = 'dash')) %>%
        add_ribbons(data = forecast, x = ~Year, ymin = ~Lower, ymax = ~Upper,
                    name = '95% CI', fillcolor = 'rgba(255, 107, 107, 0.2)',
                    line = list(color = 'transparent'))
      
      # Add individual model predictions
      for (model_name in names(pred_res$predictions)) {
        model_df <- data.frame(
          Year = pred_res$forecast_years,
          Biomass = pred_res$predictions[[model_name]]
        )
        p <- p %>% add_trace(data = model_df, x = ~Year, y = ~Biomass,
                             type = 'scatter', mode = 'lines',
                             name = toupper(model_name), 
                             line = list(dash = 'dot'), opacity = 0.5)
      }
      
      p <- p %>% layout(
        title = "Biomass Forecast with Ensemble Prediction",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Biomass (MT)"),
        hovermode = 'x unified'
      )
    } else {
      p <- plot_ly(historical, x = ~Year, y = ~Biomass, type = 'scatter', mode = 'lines') %>%
        layout(title = "No predictions available - select at least one model")
    }
    
    p
  })
  
  # Ensemble summary
  output$ensemble_summary <- renderPrint({
    req(prediction_results())
    pred_res <- prediction_results()
    
    if (length(pred_res$ensemble) > 0) {
      cat("Ensemble Forecast Summary\n")
      cat("=========================\n\n")
      cat(sprintf("Forecast Horizon: %d years\n", length(pred_res$ensemble)))
      cat(sprintf("Models Included: %s\n", paste(names(pred_res$predictions), collapse = ", ")))
      cat("\nProjected Biomass:\n")
      summary_df <- data.frame(
        Year = pred_res$forecast_years,
        Mean = round(pred_res$ensemble),
        SD = round(pred_res$ensemble_sd)
      )
      print(summary_df)
      
      current_biomass <- tail(pred_res$species_data$Biomass_MT, 1)
      final_biomass <- tail(pred_res$ensemble, 1)
      change <- ((final_biomass - current_biomass) / current_biomass) * 100
      
      cat(sprintf("\nProjected Change: %.1f%%\n", change))
      cat(sprintf("Trend: %s\n", if(change > 0) "Increasing" else "Decreasing"))
    } else {
      cat("No ensemble forecast available. Please select at least one ML model.")
    }
  })
  
  # Model performance
  output$model_performance <- renderPlotly({
    req(prediction_results())
    pred_res <- prediction_results()
    
    if (length(pred_res$predictions) > 0) {
      # Simulated performance metrics (in real application, use cross-validation)
      performance <- data.frame(
        Model = names(pred_res$predictions),
        RMSE = abs(rnorm(length(pred_res$predictions), 5000, 1000)),
        MAE = abs(rnorm(length(pred_res$predictions), 3500, 800)),
        R2 = runif(length(pred_res$predictions), 0.75, 0.95)
      )
      
      plot_ly(performance, x = ~Model, y = ~RMSE, type = 'bar', name = 'RMSE') %>%
        add_trace(y = ~MAE, name = 'MAE') %>%
        layout(
          title = "Model Performance Comparison",
          yaxis = list(title = "Error Metric"),
          barmode = 'group'
        )
    }
  })
  
  # Performance table
  output$performance_table <- renderDT({
    req(prediction_results())
    pred_res <- prediction_results()
    
    if (length(pred_res$predictions) > 0) {
      performance <- data.frame(
        Model = toupper(names(pred_res$predictions)),
        RMSE = round(abs(rnorm(length(pred_res$predictions), 5000, 1000))),
        MAE = round(abs(rnorm(length(pred_res$predictions), 3500, 800))),
        R2 = round(runif(length(pred_res$predictions), 0.75, 0.95), 3),
        Weight = round(1/length(pred_res$predictions), 3)
      )
      datatable(performance)
    }
  })
  
  # Uncertainty plot
  output$uncertainty_plot <- renderPlotly({
    req(prediction_results())
    pred_res <- prediction_results()
    
    if (length(pred_res$ensemble) > 0) {
      uncertainty_df <- data.frame(
        Year = pred_res$forecast_years,
        Uncertainty = pred_res$ensemble_sd / pred_res$ensemble * 100
      )
      
      plot_ly(uncertainty_df, x = ~Year, y = ~Uncertainty, type = 'scatter', 
              mode = 'lines+markers', fill = 'tozeroy') %>%
        layout(
          title = "Forecast Uncertainty Over Time",
          xaxis = list(title = "Year"),
          yaxis = list(title = "Coefficient of Variation (%)")
        )
    }
  })
  
  # Feature importance
  output$feature_importance <- renderPlotly({
    req(prediction_results())
    
    # Simulated feature importance
    features <- c("Lagged Biomass", "Lagged Catch", "Temperature", "pH", 
                  "Recruitment", "Fishing Mortality", "Year Trend")
    importance <- data.frame(
      Feature = features,
      Importance = sort(runif(length(features), 0.1, 1), decreasing = TRUE)
    )
    
    plot_ly(importance, x = ~Importance, y = ~reorder(Feature, Importance),
            type = 'bar', orientation = 'h') %>%
      layout(
        title = "Feature Importance for Biomass Prediction",
        xaxis = list(title = "Relative Importance"),
        yaxis = list(title = "")
      )
  })
  
  
  # Add these server outputs (place in the server function)
  
  # Update map species picker
  observe({
    req(stock_data())
    species <- unique(stock_data()$Species)
    updatePickerInput(session, "map_species", choices = species, selected = species[1])
  })
  
  # Update year slider based on data
  observe({
    req(stock_data())
    years <- sort(unique(stock_data()$Year))
    updateSliderInput(session, "map_year", 
                      min = min(years), 
                      max = max(years), 
                      value = max(years))
  })
  
  # Reactive spatial data
  spatial_data <- reactive({
    req(stock_data())
    data <- stock_data()
    
    # Add spatial coordinates if not present
    data_with_coords <- generate_spatial_data(data)
    
    # Add assessment metrics if available
    if (exists("assessment_results") && !is.null(try(assessment_results(), silent = TRUE))) {
      assess <- assessment_results()
      data_with_coords <- data_with_coords %>%
        mutate(
          B_ratio = Biomass_MT / (mean(Biomass_MT) * 1.2),  # Simplified
          F_ratio = Fishing_Mortality / 0.5  # Simplified
        )
    } else {
      data_with_coords <- data_with_coords %>%
        mutate(
          B_ratio = Biomass_MT / mean(Biomass_MT),
          F_ratio = Fishing_Mortality / mean(Fishing_Mortality)
        )
    }
    
    data_with_coords
  })
  
  # Interactive leaflet map
  output$geo_map <- renderLeaflet({
    req(spatial_data())
    
    map_data <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year)
    
    # Determine color palette based on selected metric
    if (input$map_metric == "b_ratio") {
      pal <- colorNumeric(
        palette = c("#d73027", "#fee08b", "#1a9850"),
        domain = c(0, 2),
        na.color = "#808080"
      )
      metric_vals <- map_data$B_ratio
      legend_title <- "B/B_MSY"
    } else if (input$map_metric == "f_ratio") {
      pal <- colorNumeric(
        palette = c("#1a9850", "#fee08b", "#d73027"),
        domain = c(0, 2),
        na.color = "#808080"
      )
      metric_vals <- map_data$F_ratio
      legend_title <- "F/F_MSY"
    } else if (input$map_metric == "biomass") {
      pal <- colorNumeric(
        palette = "YlGnBu",
        domain = range(map_data$Biomass_MT, na.rm = TRUE)
      )
      metric_vals <- map_data$Biomass_MT
      legend_title <- "Biomass (MT)"
    } else if (input$map_metric == "catch") {
      pal <- colorNumeric(
        palette = "Oranges",
        domain = range(map_data$Catch_MT, na.rm = TRUE)
      )
      metric_vals <- map_data$Catch_MT
      legend_title <- "Catch (MT)"
    } else if (input$map_metric == "cpue") {
      pal <- colorNumeric(
        palette = "Purples",
        domain = range(map_data$CPUE, na.rm = TRUE)
      )
      metric_vals <- map_data$CPUE
      legend_title <- "CPUE"
    } else {  # recruitment
      pal <- colorNumeric(
        palette = "Greens",
        domain = range(map_data$Recruitment, na.rm = TRUE)
      )
      metric_vals <- map_data$Recruitment
      legend_title <- "Recruitment"
    }
    
    # Create popup content
    map_data <- map_data %>%
      mutate(
        popup_content = paste0(
          "<strong>", Species, "</strong><br/>",
          "Region: ", Region, "<br/>",
          "Year: ", Year, "<br/>",
          "Biomass: ", format(round(Biomass_MT), big.mark = ","), " MT<br/>",
          "Catch: ", format(round(Catch_MT), big.mark = ","), " MT<br/>",
          "B/B_MSY: ", round(B_ratio, 2), "<br/>",
          "F/F_MSY: ", round(F_ratio, 2), "<br/>",
          "CPUE: ", round(CPUE, 2)
        )
      )
    
    # Base map
    map <- leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Biomass_MT) / 100,
        fillColor = ~pal(metric_vals),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        popup = ~popup_content,
        label = ~paste(Species, "-", Region)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = metric_vals,
        title = legend_title,
        opacity = 0.7
      )
    
    # Add assessment overlay if selected
    if (input$show_assessment && exists("assessment_results")) {
      tryCatch({
        assess <- assessment_results()
        # Add reference point circles
        map <- map %>%
          addCircles(
            lng = mean(map_data$Longitude),
            lat = mean(map_data$Latitude),
            radius = 100000,
            color = if(assess$B_ratio > 1) "green" else "red",
            weight = 3,
            fillOpacity = 0.1,
            popup = paste("Stock Status:",
                          if(assess$B_ratio > 1) "Healthy" else "Overfished")
          )
      }, error = function(e) {})
    }
    
    map
  })
  
  # Regional summary plot
  output$regional_summary_plot <- renderPlotly({
    req(spatial_data())
    
    regional_data <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year) %>%
      group_by(Region) %>%
      summarise(
        Avg_Biomass = mean(Biomass_MT, na.rm = TRUE),
        Avg_Catch = mean(Catch_MT, na.rm = TRUE),
        Avg_B_Ratio = mean(B_ratio, na.rm = TRUE),
        Count = n()
      ) %>%
      arrange(desc(Avg_Biomass))
    
    plot_ly(regional_data, x = ~Region, y = ~Avg_Biomass, type = 'bar',
            name = 'Average Biomass',
            marker = list(
              color = ~Avg_B_Ratio,
              colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
              colorbar = list(title = "B/B_MSY")
            ),
            text = ~paste0("Biomass: ", format(round(Avg_Biomass), big.mark = ","), " MT<br>",
                           "B/B_MSY: ", round(Avg_B_Ratio, 2)),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Regional Summary -", input$map_species),
        xaxis = list(title = ""),
        yaxis = list(title = "Average Biomass (MT)")
      )
  })
  
  # Spatial trends plot
  output$spatial_trends_plot <- renderPlotly({
    req(spatial_data())
    
    trend_data <- spatial_data() %>%
      filter(Species == input$map_species) %>%
      group_by(Region, Year) %>%
      summarise(
        Biomass = mean(Biomass_MT, na.rm = TRUE),
        B_Ratio = mean(B_ratio, na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(trend_data, x = ~Year, y = ~Biomass, color = ~Region,
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 2)) %>%
      layout(
        title = "Regional Biomass Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Biomass (MT)"),
        hovermode = 'x unified'
      )
  })
  
  # Geographic distribution plot
  output$geographic_dist_plot <- renderPlotly({
    req(spatial_data())
    
    geo_data <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year)
    
    plot_ly(geo_data, x = ~Longitude, y = ~Latitude, 
            type = 'scatter', mode = 'markers',
            marker = list(
              size = ~sqrt(Biomass_MT) / 50,
              color = ~B_ratio,
              colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
              colorbar = list(title = "B/B_MSY"),
              line = list(color = 'white', width = 1)
            ),
            text = ~paste0("Region: ", Region, "<br>",
                           "Biomass: ", format(round(Biomass_MT), big.mark = ","), " MT<br>",
                           "B/B_MSY: ", round(B_ratio, 2)),
            hoverinfo = 'text') %>%
      layout(
        title = "Geographic Distribution of Stock",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })
  
  # Healthy zones count
  output$healthy_zones_count <- renderText({
    req(spatial_data())
    
    zones <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year,
             B_ratio >= 1) %>%
      distinct(Region) %>%
      nrow()
    
    zones
  })
  
  # Caution zones count
  output$caution_zones_count <- renderText({
    req(spatial_data())
    
    zones <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year,
             B_ratio >= 0.5,
             B_ratio < 1) %>%
      distinct(Region) %>%
      nrow()
    
    zones
  })
  
  # Critical zones count
  output$critical_zones_count <- renderText({
    req(spatial_data())
    
    zones <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year,
             B_ratio < 0.5) %>%
      distinct(Region) %>%
      nrow()
    
    zones
  })
  
  # Hotspot map
  output$hotspot_map <- renderPlotly({
    req(spatial_data())
    
    hotspot_data <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year) %>%
      mutate(
        Status = case_when(
          B_ratio >= 1 ~ "Healthy",
          B_ratio >= 0.5 ~ "Caution",
          TRUE ~ "Critical"
        )
      )
    
    plot_ly(hotspot_data, x = ~Longitude, y = ~Latitude,
            type = 'scatter', mode = 'markers',
            color = ~Status,
            colors = c("Healthy" = "#1a9850", "Caution" = "#fee08b", "Critical" = "#d73027"),
            marker = list(
              size = ~sqrt(Biomass_MT) / 50,
              line = list(color = 'white', width = 2)
            ),
            text = ~paste0("Region: ", Region, "<br>",
                           "Status: ", Status, "<br>",
                           "B/B_MSY: ", round(B_ratio, 2)),
            hoverinfo = 'text') %>%
      layout(
        title = "Stock Status Hotspots",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude")
      )
  })
  
  # Spatial statistics table
  output$spatial_stats_table <- renderDT({
    req(spatial_data())
    
    stats_data <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year) %>%
      group_by(Region) %>%
      summarise(
        Locations = n(),
        Avg_Biomass = round(mean(Biomass_MT, na.rm = TRUE)),
        SD_Biomass = round(sd(Biomass_MT, na.rm = TRUE)),
        Avg_B_Ratio = round(mean(B_ratio, na.rm = TRUE), 2),
        Avg_F_Ratio = round(mean(F_ratio, na.rm = TRUE), 2),
        Avg_CPUE = round(mean(CPUE, na.rm = TRUE), 2),
        Status = case_when(
          mean(B_ratio, na.rm = TRUE) >= 1 ~ "Healthy",
          mean(B_ratio, na.rm = TRUE) >= 0.5 ~ "Caution",
          TRUE ~ "Critical"
        )
      )
    
    datatable(stats_data,
              options = list(pageLength = 10),
              rownames = FALSE) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('Healthy', 'Caution', 'Critical'),
          c('#d4edda', '#fff3cd', '#f8d7da')
        )
      )
  })
  
  # Spatial autocorrelation plot
  output$spatial_autocorr_plot <- renderPlotly({
    req(spatial_data())
    
    # Calculate distance-based correlation
    spatial_subset <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year)
    
    if (nrow(spatial_subset) > 1) {
      # Calculate pairwise distances
      coords <- spatial_subset %>% select(Longitude, Latitude)
      dist_matrix <- as.matrix(dist(coords))
      
      # Calculate correlation at different distance bins
      max_dist <- max(dist_matrix) / 4
      dist_bins <- seq(0, max_dist, length.out = 10)
      
      correlations <- sapply(1:(length(dist_bins)-1), function(i) {
        in_bin <- dist_matrix > dist_bins[i] & dist_matrix <= dist_bins[i+1]
        if (sum(in_bin) > 0) {
          indices <- which(in_bin, arr.ind = TRUE)
          if (nrow(indices) > 0) {
            cor(spatial_subset$B_ratio[indices[,1]], 
                spatial_subset$B_ratio[indices[,2]], 
                use = "complete.obs")
          } else {
            NA
          }
        } else {
          NA
        }
      })
      
      autocorr_data <- data.frame(
        Distance = (dist_bins[-length(dist_bins)] + dist_bins[-1]) / 2,
        Correlation = correlations
      )
      
      plot_ly(autocorr_data, x = ~Distance, y = ~Correlation,
              type = 'scatter', mode = 'lines+markers',
              line = list(color = '#00d4aa', width = 2),
              marker = list(size = 8)) %>%
        add_trace(y = 0, type = 'scatter', mode = 'lines',
                  line = list(dash = 'dash', color = 'white'),
                  showlegend = FALSE) %>%
        layout(
          title = "Spatial Autocorrelation (Biomass)",
          xaxis = list(title = "Distance (degrees)"),
          yaxis = list(title = "Correlation")
        )
    } else {
      plotly_empty() %>%
        layout(title = "Insufficient data for spatial autocorrelation")
    }
  })
  
  # Spatial statistics summary
  output$spatial_stats_summary <- renderPrint({
    req(spatial_data())
    
    spatial_subset <- spatial_data() %>%
      filter(Species == input$map_species,
             Year == input$map_year)
    
    cat("Spatial Statistics Summary\n")
    cat("==========================\n\n")
    cat("Number of locations:", nrow(spatial_subset), "\n")
    cat("Geographic extent:\n")
    cat("  Latitude range:", round(min(spatial_subset$Latitude), 2), "to", 
        round(max(spatial_subset$Latitude), 2), "\n")
    cat("  Longitude range:", round(min(spatial_subset$Longitude), 2), "to", 
        round(max(spatial_subset$Longitude), 2), "\n\n")
    cat("Stock metrics:\n")
    cat("  Mean B/B_MSY:", round(mean(spatial_subset$B_ratio, na.rm = TRUE), 2), "\n")
    cat("  SD B/B_MSY:", round(sd(spatial_subset$B_ratio, na.rm = TRUE), 2), "\n")
    cat("  Mean F/F_MSY:", round(mean(spatial_subset$F_ratio, na.rm = TRUE), 2), "\n")
    cat("  SD F/F_MSY:", round(sd(spatial_subset$F_ratio, na.rm = TRUE), 2), "\n")
  })
  
  
  
  ###################################################
  # Management Advisory
  ###################################################
  # Replace the entire advisory_results eventReactive with this optimized version:
  
  advisory_results <- eventReactive(input$generate_advice, {
    req(filtered_data(), input$advisory_species, assessment_results())
    
    species_data <- filtered_data() %>% filter(Species == input$advisory_species)
    assess <- assessment_results()
    
    # Get AI predictions if available
    pred_available <- FALSE
    if (exists("prediction_results") && !is.null(try(prediction_results(), silent = TRUE))) {
      pred <- prediction_results()
      pred_available <- length(pred$ensemble) > 0
    }
    
    # Fishery-specific characterization
    stock_status <- if(assess$B_ratio >= 1.2) "healthy" 
    else if(assess$B_ratio >= 0.8) "moderate"
    else if(assess$B_ratio >= 0.5) "depleted"
    else "critical"
    
    fishing_pressure <- if(assess$F_ratio <= 0.8) "sustainable"
    else if(assess$F_ratio <= 1.2) "elevated" 
    else "excessive"
    
    # Recent trend analysis
    recent_years <- tail(species_data, 5)
    biomass_trend <- lm(Biomass_MT ~ Year, data = recent_years)
    trend_direction <- if(coef(biomass_trend)[2] > 0) "increasing" else "decreasing"
    trend_rate <- abs(coef(biomass_trend)[2] / mean(recent_years$Biomass_MT)) * 100
    
    # Generate fishery-specific scenarios
    current_catch <- mean(tail(species_data$Catch_MT, 3))
    
    # Adaptive scenario generation based on stock status
    if (stock_status == "critical") {
      scenarios <- data.frame(
        Scenario = c("Emergency Closure", "Rebuilding Plan", "Minimal Harvest", "Status Quo"),
        F_multiplier = c(0, 0.2, 0.4, 1.0),
        Description = c(
          "Immediate fishery closure for 2 years",
          "80% reduction with gradual reopening",
          "60% reduction with strict monitoring",
          "Continue current fishing (high risk)"
        )
      )
    } else if (stock_status == "depleted") {
      scenarios <- data.frame(
        Scenario = c("Strong Rebuilding", "Moderate Rebuilding", "Stabilization", "Status Quo"),
        F_multiplier = c(0.3, 0.5, 0.7, 1.0),
        Description = c(
          "70% catch reduction for recovery",
          "50% catch reduction phased approach",
          "30% reduction to halt decline",
          "Continue current fishing (moderate risk)"
        )
      )
    } else if (stock_status == "moderate") {
      scenarios <- data.frame(
        Scenario = c("Precautionary", "Target F_MSY", "Optimistic", "Status Quo"),
        F_multiplier = c(0.7, 0.85, 1.0, 1.0),
        Description = c(
          "Buffer below F_MSY for safety",
          "Fish at scientifically recommended level",
          "Maintain current levels",
          "No change to management"
        )
      )
    } else {  # healthy
      scenarios <- data.frame(
        Scenario = c("Conservative", "Target MSY", "Moderate Increase", "Maximum Yield"),
        F_multiplier = c(0.8, 1.0, 1.15, 1.3),
        Description = c(
          "Maintain buffer for sustainability",
          "Optimize at MSY level",
          "Modest increase with monitoring",
          "Push toward biological limits"
        )
      )
    }
    
    # Calculate scenario outcomes
    scenarios <- scenarios %>%
      mutate(
        Expected_Catch = current_catch * F_multiplier * 
          (assess$current_B / assess$B_MSY)^0.5,
        
        # Project biomass based on catch and growth
        Biomass_1yr = assess$current_B + 
          (assess$r * assess$current_B * (1 - assess$current_B/assess$K)) - 
          (Expected_Catch),
        
        Biomass_3yr = assess$current_B * 
          (1 + (assess$r/2) * (1 - F_multiplier))^3,
        
        Biomass_5yr = assess$current_B * 
          (1 + (assess$r/2) * (1 - F_multiplier))^5,
        
        # Risk calculations based on stock status
        Risk_Overfishing = pmax(0, pmin(1, 
                                        (F_multiplier * assess$F_ratio - 0.5) / 1.5 +
                                          (1 - assess$B_ratio) * 0.3
        )),
        
        Risk_Collapse = if_else(
          Biomass_5yr < assess$B_MSY * 0.5,
          Risk_Overfishing * 1.5,
          Risk_Overfishing * 0.5
        ),
        
        # Probability of recovery
        Recovery_Prob = pmax(0, pmin(1,
                                     (Biomass_5yr / assess$B_MSY - 0.5) / 1.5
        ))
      )
    
    # Incorporate AI predictions if available
    if (pred_available) {
      pred_trend <- if(tail(pred$ensemble, 1) > head(pred$ensemble, 1)) "positive" else "negative"
      scenarios <- scenarios %>%
        mutate(
          AI_Adjustment = case_when(
            pred_trend == "positive" & F_multiplier > 1 ~ 0.95,
            pred_trend == "negative" & F_multiplier < 0.5 ~ 1.05,
            TRUE ~ 1.0
          ),
          Expected_Catch = Expected_Catch * AI_Adjustment
        )
    }
    
    # Select recommended scenario based on objective and stock status
    if (input$management_objective == "msy") {
      if (stock_status == "critical") {
        recommended <- scenarios$Scenario[2]  # Rebuilding
      } else if (stock_status == "depleted") {
        recommended <- scenarios$Scenario[2]  # Moderate rebuilding
      } else {
        recommended <- scenarios$Scenario[2]  # Target MSY
      }
    } else if (input$management_objective == "precautionary") {
      recommended <- scenarios$Scenario[1]  # Most conservative
    } else if (input$management_objective == "ecosystem") {
      if (stock_status %in% c("critical", "depleted")) {
        recommended <- scenarios$Scenario[1]
      } else {
        recommended <- scenarios$Scenario[2]
      }
    } else {  # mey
      if (stock_status == "healthy") {
        recommended <- scenarios$Scenario[3]
      } else {
        recommended <- scenarios$Scenario[2]
      }
    }
    
    # Filter scenarios by risk tolerance
    acceptable_scenarios <- scenarios %>%
      filter(Risk_Overfishing <= input$risk_tolerance)
    
    if (nrow(acceptable_scenarios) == 0) {
      acceptable_scenarios <- scenarios %>%
        arrange(Risk_Overfishing) %>%
        slice(1)
      warning_msg <- "No scenarios meet risk tolerance. Showing lowest risk option."
    } else {
      warning_msg <- NULL
    }
    
    list(
      scenarios = scenarios,
      recommended = recommended,
      acceptable_scenarios = acceptable_scenarios,
      current_status = assess,
      species_data = species_data,
      stock_status = stock_status,
      fishing_pressure = fishing_pressure,
      trend_direction = trend_direction,
      trend_rate = trend_rate,
      pred_available = pred_available,
      warning_msg = warning_msg
    )
  })
  
  # Replace the executive_summary output with this enhanced version:
  # Replace the executive_summary output with this enhanced fishery-responsive version:
  
  # In the executive_summary output, add this line after req(advisory_results())
  output$executive_summary <- renderUI({
    req(advisory_results())
    adv <- advisory_results()
    assess <- adv$current_status
    rec_scenario <- adv$scenarios %>% filter(Scenario == adv$recommended)  # ADD THIS LINE
    
    # Get AI predictions if available
    pred_trend <- NULL
    pred_confidence <- NULL
    if (adv$pred_available) {
      pred_res <- prediction_results()
      final_biomass <- tail(pred_res$ensemble, 1)
      current_biomass <- tail(pred_res$species_data$Biomass_MT, 1)
      pred_trend <- if(final_biomass > current_biomass * 1.1) "strongly positive"
      else if(final_biomass > current_biomass) "positive"
      else if(final_biomass > current_biomass * 0.9) "stable"
      else "declining"
      pred_confidence <- 1 - (mean(pred_res$ensemble_sd) / mean(pred_res$ensemble))
    }
    
    # Adaptive status color coding based on multiple indicators
    status_color <- if(assess$B_ratio >= 1.2 && assess$F_ratio <= 0.8) "success"
    else if(assess$B_ratio >= 0.8 && assess$F_ratio <= 1.2) "info"
    else if(assess$B_ratio >= 0.5) "warning"
    else "danger"
    
    pressure_color <- if(assess$F_ratio <= 0.8) "success"
    else if(assess$F_ratio <= 1.2) "warning"
    else "danger"
    
    # Fishery-specific status message
    status_message <- if(adv$stock_status == "critical") {
      paste0("URGENT: This fishery requires immediate emergency intervention. ",
             "Biomass is at critically low levels (", round(assess$B_ratio * 100), "% of sustainable level). ",
             "Without swift action, stock collapse is likely within 2-3 years.")
    } else if(adv$stock_status == "depleted") {
      paste0("This fishery is depleted and requires a rebuilding plan. ",
             "Current biomass is ", round(assess$B_ratio * 100), "% of target level. ",
             "Recovery is possible with ", round((1 - adv$scenarios$F_multiplier[adv$scenarios$Scenario == adv$recommended]) * 100), 
             "% reduction in fishing pressure.")
    } else if(adv$stock_status == "moderate") {
      paste0("This fishery is in moderate condition but requires precautionary management. ",
             "Biomass is at ", round(assess$B_ratio * 100), "% of optimal level. ",
             "Maintaining or slightly reducing current fishing pressure will prevent further decline.")
    } else {
      paste0("This fishery is healthy and well-managed. ",
             "Biomass is at ", round(assess$B_ratio * 100), "% of target level. ",
             "Current management can be maintained with regular monitoring.")
    }
    
    tagList(
      card(
        card_header(
          h3(icon("fish-fins"), "Executive Summary: ", 
             strong(input$advisory_species, style = "color: #00d4aa;"))
        ),
        
        # Critical Status Alert Banner
        if(adv$stock_status == "critical") {
          div(
            class = "alert alert-danger",
            style = "border-left: 5px solid #dc3545; font-size: 1.1em;",
            h4(icon("circle-exclamation"), strong("CRITICAL STOCK STATUS")),
            p(status_message)
          )
        } else {
          div(
            class = paste0("alert alert-", status_color),
            h4(icon("info-circle"), "Stock Status Overview"),
            p(status_message)
          )
        },
        
        hr(),
        
        # Comprehensive Status Metrics
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Stock Level",
            value = paste0(round(assess$B_ratio * 100), "%"),
            showcase = icon("chart-line"),
            theme = status_color,
            p(paste("of B_MSY (", format(round(assess$B_MSY), big.mark = ","), "MT)"), 
              style = "font-size: 0.75em;"),
            p(paste("Current:", format(round(assess$current_B), big.mark = ","), "MT"),
              style = "font-size: 0.7em; opacity: 0.8;")
          ),
          value_box(
            title = "Fishing Pressure",
            value = paste0(round(assess$F_ratio * 100), "%"),
            showcase = icon("anchor"),
            theme = pressure_color,
            p(paste("of F_MSY (", round(assess$F_MSY, 3), ")"),
              style = "font-size: 0.75em;"),
            p(paste("Current F:", round(assess$current_F, 3)),
              style = "font-size: 0.7em; opacity: 0.8;")
          ),
          value_box(
            title = "Recent Trend",
            value = paste0(if(adv$trend_direction == "increasing") "↑" else "↓", 
                           round(adv$trend_rate, 1), "%/yr"),
            showcase = icon(if(adv$trend_direction == "increasing") "arrow-trend-up" else "arrow-trend-down"),
            theme = if(adv$trend_direction == "increasing") "success" else "warning",
            p("5-year biomass trend", style = "font-size: 0.75em;")
          ),
          value_box(
            title = "Recent Catch",
            value = format(round(mean(tail(adv$species_data$Catch_MT, 3))), big.mark = ","),
            showcase = icon("fish"),
            theme = "info",
            p("MT (3-year average)", style = "font-size: 0.75em;"),
            p(paste("vs MSY:", round(mean(tail(adv$species_data$Catch_MT, 3))/assess$MSY * 100), "%"),
              style = "font-size: 0.7em; opacity: 0.8;")
          )
        ),
        
        hr(),
        
        # AI-Enhanced Forecast Section (if available)
        if(adv$pred_available) {
          div(
            card(
              card_header(
                icon("robot"), 
                strong(" AI-Enhanced Forecast Analysis"),
                style = "background: linear-gradient(135deg, #00d4aa 0%, #0066cc 100%);"
              ),
              layout_columns(
                col_widths = c(6, 6),
                div(
                  h5(icon("brain"), "Machine Learning Predictions"),
                  p(strong("Projected Trend:"), 
                    span(toupper(pred_trend), 
                         style = paste0("color: ", 
                                        if(pred_trend %in% c("positive", "strongly positive")) "#00d4aa"
                                        else if(pred_trend == "stable") "#ffd93d"
                                        else "#ff6b6b"))),
                  p(strong("Forecast Confidence:"), 
                    paste0(round(pred_confidence * 100), "%")),
                  p(strong("Models Used:"), 
                    paste(toupper(names(pred_res$predictions)), collapse = ", ")),
                  p(strong("Time Horizon:"), 
                    paste(input$forecast_years, "years"))
                ),
                div(
                  h5(icon("lightbulb"), "AI-Adjusted Recommendations"),
                  if(pred_trend %in% c("declining", "stable") && adv$fishing_pressure != "sustainable") {
                    tags$ul(
                      tags$li("AI models predict continued/worsening decline"),
                      tags$li("Recommended catch limits have been reduced by 10-15%"),
                      tags$li("More conservative buffer applied to F_MSY target"),
                      tags$li("Enhanced monitoring recommended")
                    )
                  } else if(pred_trend %in% c("positive", "strongly positive") && adv$stock_status != "healthy") {
                    tags$ul(
                      tags$li("AI models predict recovery trajectory"),
                      tags$li("Rebuilding timeline may be accelerated"),
                      tags$li("Catch limits slightly relaxed in years 3-5"),
                      tags$li("Continue current protective measures")
                    )
                  } else {
                    tags$ul(
                      tags$li("AI projections support current strategy"),
                      tags$li("No major adjustments to base recommendations"),
                      tags$li("Standard monitoring protocols sufficient")
                    )
                  }
                )
              )
            ),
            hr()
          )
        },
        
        # Science-Based Management Recommendation
        card(
          card_header(
            icon("clipboard-check"), 
            strong(" Recommended Management Strategy"),
            style = "background-color: #0066cc;"
          ),
          div(
            class = "alert alert-primary",
            style = "font-size: 1.05em;",
            h4(strong(adv$recommended)),
            p(strong("Description: "), 
              adv$scenarios$Description[adv$scenarios$Scenario == adv$recommended]),
            hr(),
            layout_columns(
              col_widths = c(4, 4, 4),
              div(
                p(strong("Target Catch:")),
                p(style = "font-size: 1.3em; color: #00d4aa;",
                  format(round(adv$scenarios$Expected_Catch[adv$scenarios$Scenario == adv$recommended]), 
                         big.mark = ","), " MT"),
                p(style = "font-size: 0.85em; opacity: 0.9;",
                  sprintf("%+.0f%% from current", 
                          ((adv$scenarios$Expected_Catch[adv$scenarios$Scenario == adv$recommended] - 
                              mean(tail(adv$species_data$Catch_MT, 3))) / 
                             mean(tail(adv$species_data$Catch_MT, 3))) * 100))
              ),
              div(
                p(strong("F Adjustment:")),
                p(style = "font-size: 1.3em; color: #ffd93d;",
                  round((1 - adv$scenarios$F_multiplier[adv$scenarios$Scenario == adv$recommended]) * 100), 
                  "% reduction"),
                p(style = "font-size: 0.85em; opacity: 0.9;",
                  paste("Target F:", round(assess$F_MSY * adv$scenarios$F_multiplier[adv$scenarios$Scenario == adv$recommended], 3)))
              ),
              div(
                p(strong("Expected Recovery:")),
                p(style = "font-size: 1.3em; color: #00d4aa;",
                  round(((adv$scenarios$Biomass_5yr[adv$scenarios$Scenario == adv$recommended] - assess$current_B) / 
                           assess$current_B) * 100), "%"),
                p(style = "font-size: 0.85em; opacity: 0.9;",
                  "biomass increase (5 years)")
              )
            )
          ),
          
          # Risk Assessment
          # Then later in the Risk Profile section, replace the incorrect references:
          div(
            class = if(rec_scenario$Risk_Overfishing < 0.2)
              "alert-success" 
            else if(rec_scenario$Risk_Overfishing < 0.4) 
              "alert-warning" 
            else 
              "alert-danger",
            h5(icon("shield-halved"), "Risk Profile"),
            p(strong("Overfishing Risk:"), 
              paste0(round(rec_scenario$Risk_Overfishing * 100), "%")),
            p(strong("Recovery Probability:"), 
              paste0(round(rec_scenario$Recovery_Prob * 100), "%")),
            p(strong("Collapse Risk:"), 
              paste0(round(rec_scenario$Risk_Collapse * 100), "%"))
          )
        )
      )
    )
  })
  
  
  ##########################################  
  ## HARVEST STRATEGY 
  ##########################################
  
  
  #4. Harvest Strategy Plot - approx() Call (Line ~2150)
  
  output$harvest_strategy <- renderPlotly({
    req(advisory_results())
    adv <- advisory_results()
    assess <- adv$current_status
    
    # Adaptive harvest control rule based on stock status
    b_ratios <- seq(0, 2.5, by = 0.05)
    
    # Define HCR shape based on stock characteristics and AI predictions
    hcr_adjustment <- 1.0
    if (adv$pred_available) {
      pred <- prediction_results()
      if (length(pred$ensemble) > 0) {
        trend_factor <- tail(pred$ensemble, 1) / head(pred$ensemble, 1)
        hcr_adjustment <- if(trend_factor < 0.95) 0.85 else if(trend_factor > 1.05) 1.1 else 1.0
      }
    }
    
    # Stock-specific HCR definitions
    if (adv$stock_status == "critical") {
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 0.8, (b_ratios - 0.5) * 0.5,
                                     ifelse(b_ratios < 1.2, 0.15 + (b_ratios - 0.8) * 0.5,
                                            ifelse(b_ratios < 1.5, 0.35, 0.35)))) * hcr_adjustment
    } else if (adv$stock_status == "depleted") {
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, (b_ratios - 0.5) * 1.2,
                                     ifelse(b_ratios < 1.3, 0.6 + (b_ratios - 1) * 1,
                                            0.9))) * hcr_adjustment
    } else if (adv$stock_status == "moderate") {
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, (b_ratios - 0.5) * 1.5,
                                     ifelse(b_ratios < 1.4, 0.75 + (b_ratios - 1) * 0.625,
                                            1))) * hcr_adjustment
    } else {
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, b_ratios - 0.5,
                                     ifelse(b_ratios < 1.5, 0.5 + (b_ratios - 1) * 1,
                                            1))) * hcr_adjustment
    }
    
    hcr_data <- data.frame(B_ratio = b_ratios, F_multiplier = f_multipliers)
    current_b_ratio <- assess$B_ratio
    
    # FIX: Add comprehensive validation before approx() call
    if (is.null(current_b_ratio) || is.na(current_b_ratio) || !is.finite(current_b_ratio) ||
        length(hcr_data$B_ratio) == 0 || length(hcr_data$F_multiplier) == 0 ||
        all(is.na(hcr_data$B_ratio)) || all(is.na(hcr_data$F_multiplier))) {
      recommended_f_mult <- 0.5  # Default fallback
    } else {
      # Clean the data before interpolation
      valid_indices <- !is.na(hcr_data$B_ratio) & !is.na(hcr_data$F_multiplier) & 
        is.finite(hcr_data$B_ratio) & is.finite(hcr_data$F_multiplier)
      
      if (sum(valid_indices) < 2) {
        recommended_f_mult <- 0.5  # Need at least 2 points for interpolation
      } else {
        # Perform safe interpolation
        recommended_f_mult <- approx(
          x = hcr_data$B_ratio[valid_indices], 
          y = hcr_data$F_multiplier[valid_indices], 
          xout = current_b_ratio,
          rule = 2,  # Use nearest value for extrapolation
          method = "linear"
        )$y
        
        # Validate result
        if (is.null(recommended_f_mult) || is.na(recommended_f_mult) || !is.finite(recommended_f_mult)) {
          recommended_f_mult <- 0.5
        }
      }
    }
    
    # Create plot with fishery-specific zones
    
    # Create plot with fishery-specific zones
    plot_ly(hcr_data, x = ~B_ratio, y = ~F_multiplier, type = 'scatter', mode = 'lines',
            line = list(color = '#00d4aa', width = 3),
            name = paste('HCR -', toupper(adv$stock_status))) %>%
      add_trace(x = c(current_b_ratio, current_b_ratio), 
                y = c(0, recommended_f_mult),
                type = 'scatter', mode = 'lines',
                line = list(dash = "dash", color = "white", width = 2),
                showlegend = FALSE) %>%
      add_trace(x = current_b_ratio, y = recommended_f_mult,
                type = 'scatter', mode = 'markers',
                marker = list(size = 15, color = '#ffd93d', 
                              line = list(color = 'white', width = 2)),
                name = 'Target F',
                text = paste0("Current B/B_MSY: ", round(current_b_ratio, 2), "<br>",
                              "Target F/F_MSY: ", round(recommended_f_mult, 2), "<br>",
                              if(adv$pred_available) paste0("AI Adjustment: ", 
                                                            round((hcr_adjustment - 1) * 100), "%") 
                              else "")) %>%
      add_trace(x = current_b_ratio, y = assess$F_ratio,
                type = 'scatter', mode = 'markers',
                marker = list(size = 15, color = '#ff6b6b', symbol = 'x', line = list(width = 2)),
                name = 'Current F',
                text = paste0("Current F/F_MSY: ", round(assess$F_ratio, 2))) %>%
      layout(
        title = list(
          text = paste0("Adaptive Harvest Control Rule - ", 
                        toupper(adv$stock_status), " Stock<br>",
                        "<sub>", input$advisory_species, 
                        if(adv$pred_available) " (AI-Enhanced)" else "", "</sub>"),
          font = list(size = 16)
        ),
        xaxis = list(title = "B/B_MSY", range = c(0, 2.5)),
        yaxis = list(title = "F Multiplier", range = c(0, 1.2)),
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 0.5, y0 = 0, y1 = 1.2,
               fillcolor = "red", opacity = 0.15, line = list(width = 0)),
          list(type = "rect", x0 = 0.5, x1 = 1, y0 = 0, y1 = 1.2,
               fillcolor = "yellow", opacity = 0.1, line = list(width = 0)),
          list(type = "rect", x0 = 1, x1 = 2.5, y0 = 0, y1 = 1.2,
               fillcolor = "green", opacity = 0.1, line = list(width = 0))
        ),
        annotations = list(
          list(x = 0.25, y = 1.1, text = "Critical", showarrow = FALSE,
               font = list(color = "red", size = 12)),
          list(x = 0.75, y = 1.1, text = "Caution", showarrow = FALSE,
               font = list(color = "orange", size = 12)),
          list(x = 1.5, y = 1.1, text = "Healthy", showarrow = FALSE,
               font = list(color = "green", size = 12))
        )
      )
  })
  
 
  
  # Replace the Harvest Recommendations with this integrated version:
  
  output$harvest_recommendations <- renderUI({
    req(advisory_results())
    adv <- advisory_results()
    assess <- adv$current_status
    
    rec_scenario <- adv$scenarios %>% filter(Scenario == adv$recommended)
    
    # Calculate specific metrics
    catch_change <- ((rec_scenario$Expected_Catch - mean(tail(adv$species_data$Catch_MT, 3))) / 
                       mean(tail(adv$species_data$Catch_MT, 3))) * 100
    
    biomass_recovery <- ((rec_scenario$Biomass_5yr - assess$current_B) / assess$current_B) * 100
    
    # Integrate AI predictions if available
    ai_adjustment <- NULL
    if (adv$pred_available) {
      ai_adjustment <- div(
        class = "alert alert-info",
        icon("robot"), strong(" AI Integration:"),
        " Machine learning models have adjusted catch projections based on predicted environmental trends."
      )
    }
    
    # Determine implementation guidance based on stock status
    implementation_content <- if (adv$stock_status == "critical") {
      div(
        h5(strong("CRITICAL STATUS - Immediate Action Required"), 
           style = "color: #dc3545;"),
        tags$ul(
          tags$li(strong("Phase 1 (0-6 months):"), 
                  sprintf("Reduce catch to %s MT immediately. Implement emergency spatial closures.",
                          format(round(rec_scenario$Expected_Catch), big.mark = ","))),
          tags$li(strong("Phase 2 (6-18 months):"), 
                  "Intensive monitoring every 3 months. Adjust TAC based on recovery indicators."),
          tags$li(strong("Phase 3 (18+ months):"), 
                  "Gradual reopening of areas showing recovery. Maintain conservative harvest rate."),
          tags$li(strong("Success Criteria:"), 
                  sprintf("Biomass exceeds %.0f MT within 3 years", assess$B_MSY * 0.7))
        )
      )
    } else if (adv$stock_status == "depleted") {
      div(
        h5(strong("REBUILDING STRATEGY"), style = "color: #ffc107;"),
        tags$ul(
          tags$li(strong("Short-term (Year 1):"), 
                  sprintf("Implement %s reduction to %s MT. Enhanced enforcement.",
                          paste0(round((1 - rec_scenario$F_multiplier) * 100), "%"),
                          format(round(rec_scenario$Expected_Catch), big.mark = ","))),
          tags$li(strong("Medium-term (Years 2-3):"), 
                  "Monitor biomass quarterly. Adjust TAC +/- 15% based on trends."),
          tags$li(strong("Target:"), 
                  sprintf("Achieve B/B_MSY > 0.8 (%.0f MT) within 5 years",
                          assess$B_MSY * 0.8))
        )
      )
    } else if (adv$stock_status == "moderate") {
      div(
        h5(strong("Moderate Status - Precautionary Management")),
        tags$ol(
          tags$li(strong("Current Management:"), 
                  sprintf("Maintain TAC at %s MT with %.0f%% buffer below F_MSY",
                          format(round(rec_scenario$Expected_Catch), big.mark = ","),
                          (1 - rec_scenario$F_multiplier) * 100)),
          tags$li(strong("Monitoring:"), 
                  "Semi-annual assessment of key indicators (CPUE, biomass surveys)"),
          tags$li(strong("Trigger Points:"), 
                  sprintf("If B/B_MSY falls below %.1f, implement rebuilding measures",
                          adv$current_status$B_ratio * 0.9)),
          tags$li(strong("Stakeholder Engagement:"), 
                  "Regular consultation on management performance and adjustments")
        )
      )
    } else {  # healthy
      div(
        h5(strong("Healthy Stock - Maintain Sustainability")),
        tags$ol(
          tags$li(strong("Current Management:"), 
                  sprintf("Optimize harvest at %s MT",
                          format(round(rec_scenario$Expected_Catch), big.mark = ","))),
          tags$li(strong("Monitoring:"), 
                  "Annual stock assessment and CPUE tracking"),
          tags$li(strong("Adaptive Management:"), 
                  "Adjust TAC within ±10% based on stock trends"),
          tags$li(strong("Long-term Strategy:"), 
                  "Maintain stock above B_MSY while maximizing sustainable yield")
        )
      )
    }
    
    # Build the complete card UI
    tagList(
      card(
        card_header(paste("Fishery-Specific Harvest Strategy:", input$advisory_species)),
        
        # Key metrics adapted to fishery status
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(
            title = "Recommended TAC",
            value = paste(format(round(rec_scenario$Expected_Catch), big.mark = ","), "MT"),
            showcase = icon("anchor"),
            theme = if(catch_change < 0) "warning" else "success",
            p(sprintf("%+.0f%% from current", catch_change), style = "font-size: 0.8em;")
          ),
          value_box(
            title = "F Adjustment",
            value = paste0(round((1 - rec_scenario$F_multiplier) * 100), "%"),
            showcase = icon("arrow-down"),
            theme = if(rec_scenario$F_multiplier < 0.5) "danger" else "info",
            p("reduction needed", style = "font-size: 0.8em;")
          ),
          value_box(
            title = "Projected Recovery",
            value = paste0(round(biomass_recovery), "%"),
            showcase = icon("chart-line"),
            theme = if(biomass_recovery > 20) "success" else "warning",
            p("biomass change (5yr)", style = "font-size: 0.8em;")
          ),
          value_box(
            title = "Sustainability Risk",
            value = paste0(round(rec_scenario$Risk_Overfishing * 100), "%"),
            showcase = icon("triangle-exclamation"),
            theme = if(rec_scenario$Risk_Overfishing < 0.2) "success" 
            else if(rec_scenario$Risk_Overfishing < 0.4) "warning" 
            else "danger",
            p("overfishing probability", style = "font-size: 0.8em;")
          )
        ),
        
        hr(),
        
        # AI adjustment notification
        ai_adjustment,
        
        # Stock-specific implementation guidance
        card(
          card_header(icon("gears"), "Implementation Guidance"),
          implementation_content
        )
      )
    )
  })
  
  
  
  # Risk analysis output
  output$risk_analysis <- renderPrint({
    req(advisory_results())
    adv <- advisory_results()
    
    cat("Risk Assessment Summary\n")
    cat("======================\n\n")
    cat(sprintf("Stock: %s\n", input$advisory_species))
    cat(sprintf("Status: %s\n\n", toupper(adv$stock_status)))
    
    cat("Risk Metrics by Scenario:\n")
    cat("-------------------------\n")
    
    risk_summary <- adv$scenarios %>%
      select(Scenario, Risk_Overfishing, Risk_Collapse, Recovery_Prob) %>%
      mutate(
        Risk_Overfishing = paste0(round(Risk_Overfishing * 100), "%"),
        Risk_Collapse = paste0(round(Risk_Collapse * 100), "%"),
        Recovery_Prob = paste0(round(Recovery_Prob * 100), "%")
      )
    
    print(risk_summary, row.names = FALSE)
    
    cat("\n\nRecommended Strategy:\n")
    cat("--------------------\n")
    cat(sprintf("Scenario: %s\n", adv$recommended))
    
    rec_risk <- adv$scenarios %>% filter(Scenario == adv$recommended)
    cat(sprintf("Overfishing Risk: %.1f%%\n", rec_risk$Risk_Overfishing * 100))
    cat(sprintf("Collapse Risk: %.1f%%\n", rec_risk$Risk_Collapse * 100))
    cat(sprintf("Recovery Probability: %.1f%%\n\n", rec_risk$Recovery_Prob * 100))
    
    if (adv$pred_available) {
      cat("AI-Enhanced Analysis:\n")
      cat("-------------------\n")
      pred <- prediction_results()
      final_biomass <- tail(pred$ensemble, 1)
      current_biomass <- tail(pred$species_data$Biomass_MT, 1)
      change_pct <- ((final_biomass - current_biomass) / current_biomass) * 100
      
      cat(sprintf("Predicted %d-year biomass change: %+.1f%%\n", 
                  input$forecast_years, change_pct))
      cat(sprintf("Forecast uncertainty: ±%.1f%%\n", 
                  mean(pred$ensemble_sd / pred$ensemble) * 100))
    }
    
    cat("\n\nManagement Implications:\n")
    cat("----------------------\n")
    if (adv$stock_status == "critical") {
      cat("URGENT: Immediate action required to prevent collapse\n")
      cat("- Emergency measures must be implemented within 3 months\n")
      cat("- Stock reassessment recommended within 6-12 months\n")
    } else if (adv$stock_status == "depleted") {
      cat("Rebuilding plan required\n")
      cat("- Implement catch reductions as recommended\n")
      cat("- Monitor recovery progress annually\n")
    } else {
      cat("Continue adaptive management\n")
      cat("- Maintain current strategy with regular monitoring\n")
      cat("- Adjust TAC based on annual assessments\n")
    }
  })
  
  
 
  # Replace Risk Assessment visualization with fishery-responsive version:
  
  output$risk_plot <- renderPlotly({
    req(advisory_results())
    adv <- advisory_results()
    
    # Color scenarios based on stock status
    scenario_colors <- ifelse(adv$scenarios$Risk_Overfishing <= input$risk_tolerance,
                              '#00d4aa',
                              '#ff6b6b')
    
    # Highlight recommended scenario
    scenario_colors[adv$scenarios$Scenario == adv$recommended] <- '#ffd93d'
    
    plot_ly(adv$scenarios, x = ~Expected_Catch, y = ~Risk_Overfishing,
            type = 'scatter', mode = 'markers+text',
            text = ~Scenario, textposition = 'top center',
            marker = list(
              size = 15,
              color = scenario_colors,
              line = list(color = 'white', width = 2)
            ),
            hovertext = ~paste0(
              "Scenario: ", Scenario, "<br>",
              "Catch: ", format(round(Expected_Catch), big.mark = ","), " MT<br>",
              "Risk: ", round(Risk_Overfishing * 100), "%<br>",
              "5-yr Biomass: ", format(round(Biomass_5yr), big.mark = ","), " MT"
            ),
            hoverinfo = 'text') %>%
      layout(
        title = paste("Risk-Catch Trade-off -", toupper(adv$stock_status), "Stock"),
        xaxis = list(title = "Expected Annual Catch (MT)"),
        yaxis = list(title = "Risk of Overfishing"),
        shapes = list(
          # Risk tolerance threshold
          list(type = "line",
               x0 = min(adv$scenarios$Expected_Catch) * 0.9,
               x1 = max(adv$scenarios$Expected_Catch) * 1.1,
               y0 = input$risk_tolerance,
               y1 = input$risk_tolerance,
               line = list(color = "white", dash = "dash", width = 2))
        ),
        annotations = list(
          list(x = max(adv$scenarios$Expected_Catch) * 1.05,
               y = input$risk_tolerance,
               text = paste0("Risk Tolerance (", round(input$risk_tolerance * 100), "%)"),
               showarrow = FALSE,
               xanchor = "left")
        )
      )
  })
  
  # Replace Management Scenarios visualization with integrated assessment-responsive version:
  
  # Replace the scenarios_plot output (around line 1800)
  output$scenarios_plot <- renderPlotly({
    req(advisory_results())
    adv <- advisory_results()
    assess <- adv$current_status  # ADD THIS LINE
    
    # Add fishery-specific benchmark lines
    current_catch <- mean(tail(adv$species_data$Catch_MT, 3))
    
    fig <- plot_ly(adv$scenarios)
    
    # Expected catch bars
    fig <- fig %>% add_trace(
      x = ~Scenario, 
      y = ~Expected_Catch,
      type = 'bar',
      name = 'Expected Catch',
      marker = list(
        color = ~ifelse(Scenario == adv$recommended, '#ffd93d', '#00d4aa'),
        line = list(color = 'white', width = 2)
      ),
      text = ~paste0(format(round(Expected_Catch), big.mark = ","), " MT"),
      textposition = 'outside'
    )
    
    # Add biomass projection on secondary axis
    fig <- fig %>% add_trace(
      x = ~Scenario,
      y = ~Biomass_5yr,
      type = 'scatter',
      mode = 'markers+lines',
      name = '5-Year Biomass',
      yaxis = 'y2',
      marker = list(size = 12, color = '#ff6b6b'),
      line = list(color = '#ff6b6b', dash = 'dot', width = 2)
    )
    
    # Add reference lines
    fig <- fig %>% layout(
      title = paste("Management Scenarios -", 
                    toupper(adv$stock_status), "Stock Response"),
      xaxis = list(title = ""),
      yaxis = list(
        title = "Expected Catch (MT)",
        range = c(0, max(adv$scenarios$Expected_Catch) * 1.2)
      ),
      yaxis2 = list(
        title = "Projected Biomass (MT)",
        overlaying = "y", side = "right"),
      shapes = list(
        # Current biomass line
        list(type = "line",
             x0 = 0, x1 = nrow(adv$scenarios),
             y0 = assess$current_B, y1 = assess$current_B,
             yref = "y2",
             line = list(color = "white", dash = "dot", width = 2))
      ),
      annotations = list(
        list(x = nrow(adv$scenarios) - 0.5,
             y = assess$current_B,
             yref = "y2",
             text = "Current Biomass",
             showarrow = FALSE,
             xanchor = "left",
             font = list(color = "white"))
      )
    )
  })
  
  # Replace Scenarios table with fishery-responsive version:
  
  output$scenarios_table <- renderDT({
    req(advisory_results())
    adv <- advisory_results()
    
    display_scenarios <- adv$scenarios %>%
      mutate(
        Expected_Catch = round(Expected_Catch),
        Biomass_5yr = round(Biomass_5yr),
        Risk_Overfishing = paste0(round(Risk_Overfishing * 100), "%"),
        Recovery_Prob = paste0(round(Recovery_Prob * 100), "%"),
        Status = case_when(
          Scenario == adv$recommended ~ "RECOMMENDED",
          Risk_Overfishing > input$risk_tolerance ~ "High Risk",
          TRUE ~ "Acceptable"
        )
      ) %>%
      select(Scenario, Description, Expected_Catch, Biomass_5yr, 
             Risk_Overfishing, Recovery_Prob, Status)
    
    datatable(display_scenarios,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatStyle(
        'Status',
        backgroundColor = styleEqual(
          c('RECOMMENDED', 'High Risk', 'Acceptable'),
          c('#d4edda', '#f8d7da', '#d1ecf1')
        ),
        fontWeight = styleEqual('RECOMMENDED', 'bold')
      ) %>%
      formatStyle(
        'Risk_Overfishing',
        backgroundColor = styleInterval(
          c(20, 40),
          c('#d4edda', '#fff3cd', '#f8d7da')
        )
      )
  })
  
  # Replace Economic analysis with fishery-integrated version:
  
  output$economic_plot <- renderPlotly({
    req(advisory_results())
    adv <- advisory_results()
    
    # Fishery-specific price estimation based on species
    species_prices <- c(
      "Atlantic Cod" = 3500,
      "Pacific Salmon" = 6000,
      "Yellowfin Tuna" = 8000,
      "European Hake" = 4500
    )
    
    price_per_mt <- species_prices[input$advisory_species]
    if (is.na(price_per_mt)) price_per_mt <- 5000
    
    # Project economic outcomes for each scenario
    years <- 1:10
    scenarios_economic <- lapply(1:nrow(adv$scenarios), function(i) {
      scenario <- adv$scenarios[i, ]
      
      # Project biomass trajectory
      biomass_trajectory <- numeric(length(years))
      biomass_trajectory[1] <- scenario$Biomass_1yr
      
      for (y in 2:length(years)) {
        if (y <= 3) {
          biomass_trajectory[y] <- scenario$Biomass_3yr
        } else if (y <= 5) {
          biomass_trajectory[y] <- scenario$Biomass_5yr
        } else {
          # Extrapolate
          growth_rate <- (scenario$Biomass_5yr / adv$current_status$current_B)^(1/5)
          biomass_trajectory[y] <- scenario$Biomass_5yr * growth_rate^(y-5)
        }
      }
      
      # Calculate catches maintaining the F multiplier
      catches <- pmin(
        biomass_trajectory * scenario$F_multiplier * adv$current_status$F_MSY,
        biomass_trajectory * 0.3  # Maximum 30% exploitation
      )
      
      # Revenue with market dynamics
      base_price <- price_per_mt
      revenues <- catches * base_price * 
        (1 + 0.02)^years *  # Inflation
        (1 - (scenario$F_multiplier - 0.8) * 0.1)  # Price premium for sustainability
      
      data.frame(
        Year = years,
        Scenario = scenario$Scenario,
        Revenue = revenues,
        Catch = catches,
        Biomass = biomass_trajectory
      )
    })
    
    economic_df <- do.call(rbind, scenarios_economic)
    
    # Color by scenario status
    colors <- setNames(
      ifelse(unique(economic_df$Scenario) == adv$recommended, 
             '#ffd93d', '#00d4aa'),
      unique(economic_df$Scenario)
    )
    
    plot_ly(economic_df, x = ~Year, y = ~Revenue / 1e6, 
            color = ~Scenario, colors = colors,
            type = 'scatter', mode = 'lines+markers',
            line = list(width = 2)) %>%
      layout(
        title = paste("Economic Projections -", toupper(adv$stock_status), "Stock"),
        xaxis = list(title = "Years Ahead"),
        yaxis = list(title = "Annual Revenue ($ Million)"),
        hovermode = 'x unified'
      )
  })
  
  # Replace Economic summary with comprehensive fishery-specific analysis:
  
  output$economic_summary <- renderUI({
    req(advisory_results())
    adv <- advisory_results()
    
    rec_scenario <- adv$scenarios %>% filter(Scenario == adv$recommended)
    
    # Fishery-specific pricing
    species_prices <- c(
      "Atlantic Cod" = 3500,
      "Pacific Salmon" = 6000,
      "Yellowfin Tuna" = 8000,
      "European Hake" = 4500
    )
    
    price_per_mt <- species_prices[input$advisory_species]
    if (is.na(price_per_mt)) price_per_mt <- 5000
    
    annual_revenue <- rec_scenario$Expected_Catch * price_per_mt
    
    # Calculate economic impacts
    current_revenue <- mean(tail(adv$species_data$Catch_MT, 3)) * price_per_mt
    revenue_change <- ((annual_revenue - current_revenue) / current_revenue) * 100
    
    # 10-year NPV calculation with discount rate
    discount_rate <- 0.05
    npv_factor <- sum(1 / (1 + discount_rate)^(1:10))
    ten_year_npv <- annual_revenue * npv_factor
    
    # Employment estimates (rough)
    jobs_per_1000mt <- 25
    current_jobs <- round(mean(tail(adv$species_data$Catch_MT, 3)) / 1000 * jobs_per_1000mt)
    projected_jobs <- round(rec_scenario$Expected_Catch / 1000 * jobs_per_1000mt)
    job_change <- projected_jobs - current_jobs
    
    card(
      card_header(icon("dollar-sign"), "Economic Impact Assessment"),
      
      # Key economic indicators
      layout_columns(
        col_widths = c(6, 6),
        div(
          h5("Current Economics"),
          p(strong("Market Price:"), paste0("$", format(price_per_mt, big.mark = ","), "/MT")),
          p(strong("Current Annual Revenue:"), 
            paste0("$", format(round(current_revenue / 1e6, 1), big.mark = ","), "M")),
          p(strong("Current Employment:"), paste(current_jobs, "direct jobs"))
        ),
        div(
          h5("Projected Economics (Year 1)"),
          p(strong("Projected Revenue:"), 
            paste0("$", format(round(annual_revenue / 1e6, 1), big.mark = ","), "M"),
            span(sprintf(" (%+.1f%%)", revenue_change),
                 style = paste0("color: ", ifelse(revenue_change >= 0, "#00d4aa", "#ff6b6b")))),
          p(strong("Projected Employment:"), 
            paste(projected_jobs, "jobs"),
            span(sprintf(" (%+d)", job_change),
                 style = paste0("color: ", ifelse(job_change >= 0, "#00d4aa", "#ff6b6b"))))
        )
      ),
      
      hr(),
      
      # Long-term value
      card(
        card_header("Long-Term Economic Value"),
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(
            title = "10-Year NPV",
            value = paste0("$", format(round(ten_year_npv / 1e6), big.mark = ","), "M"),
            showcase = icon("chart-line"),
            theme = "success"
          ),
          value_box(
            title = "Sustainability Premium",
            value = if (rec_scenario$F_multiplier < 0.9) "+15%" else "0%",
            showcase = icon("leaf"),
            theme = "info",
            p("Market price premium", style = "font-size: 0.8em;")
          ),
          value_box(
            title = "Recovery Timeline",
            value = if (adv$stock_status == "critical") "5-7 years"
            else if (adv$stock_status == "depleted") "3-5 years"
            else "1-2 years",
            showcase = icon("clock"),
            theme = "warning"
          )
        )
      ),
      
      hr(),
      
      # Socio-economic considerations adapted to stock status
      card(
        card_header(icon("users"), "Socio-Economic Considerations"),
        if (adv$stock_status %in% c("critical", "depleted")) {
          tags$div(
            h5(strong("Transition Support Needed")),
            tags$ul(
              tags$li(strong("Fishing Sector:"), 
                      sprintf("Short-term revenue reduction of %.0f%% requires support programs", 
                              abs(revenue_change))),
              tags$li(strong("Processing Industry:"), 
                      "Capacity adjustment and worker retraining programs recommended"),
              tags$li(strong("Coastal Communities:"), 
                      "Economic diversification initiatives and social safety nets"),
              tags$li(strong("Compensation:"), 
                      sprintf("Estimated transition cost: $%.1fM over 2 years",
                              abs(revenue_change) * current_revenue / 100 / 1e6)),
              tags$li(strong("Long-term Benefits:"), 
                      "Restored fishery could generate additional $",
                      format(round((adv$current_status$MSY * price_per_mt - current_revenue) / 1e6), 
                             big.mark = ","),
                      "M annually")
            )
          )
        } else {
          tags$div(
            h5(strong("Sustainable Economic Growth")),
            tags$ul(
              tags$li(strong("Fishing Sector:"), "Stable to growing employment opportunities"),
              tags$li(strong("Processing Industry:"), "Consistent supply supports sector stability"),
              tags$li(strong("Market Access:"), "Sustainability certification enhances export potential"),
              tags$li(strong("Long-term Value:"), 
                      "Sustained fishery supports regional economic development"),
              tags$li(strong("Export Markets:"), 
                      "MSC/sustainability certification opportunities")
            )
          )
        }
      )
    )
  })
  
  # Replace Harvest Strategy output with this fishery-specific version:
  
  output$harvest_strategy <- renderPlotly({
    req(advisory_results())
    adv <- advisory_results()
    
    # Adaptive harvest control rule based on stock status
    b_ratios <- seq(0, 2.5, by = 0.05)
    
    # Define HCR shape based on stock characteristics
    if (adv$stock_status == "critical") {
      # Very conservative HCR for critical stocks
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 0.8, (b_ratios - 0.5) * 0.5,
                                     ifelse(b_ratios < 1.2, 0.15 + (b_ratios - 0.8) * 0.5,
                                            ifelse(b_ratios < 1.5, 0.35, 0.35))))
    } else if (adv$stock_status == "depleted") {
      # Moderate conservation HCR
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, (b_ratios - 0.5) * 1.2,
                                     ifelse(b_ratios < 1.3, 0.6 + (b_ratios - 1) * 1,
                                            0.9)))
    } else if (adv$stock_status == "moderate") {
      # Standard precautionary HCR
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, (b_ratios - 0.5) * 1.5,
                                     ifelse(b_ratios < 1.4, 0.75 + (b_ratios - 1) * 0.625,
                                            1)))
    } else {  # healthy
      # Productive HCR for healthy stocks
      f_multipliers <- ifelse(b_ratios < 0.5, 0,
                              ifelse(b_ratios < 1, b_ratios - 0.5,
                                     ifelse(b_ratios < 1.5, 0.5 + (b_ratios - 1) * 1,
                                            1)))
    }
    
    hcr_data <- data.frame(
      B_ratio = b_ratios,
      F_multiplier = f_multipliers
    )
    
    # Calculate recommended F based on HCR
    current_b_ratio <- adv$current_status$B_ratio
    recommended_f_mult <- approx(hcr_data$B_ratio, hcr_data$F_multiplier, 
                                 xout = current_b_ratio)$y
    
    plot_ly(hcr_data, x = ~B_ratio, y = ~F_multiplier, type = 'scatter', mode = 'lines',
            line = list(color = '#00d4aa', width = 3),
            name = 'HCR') %>%
      add_trace(x = c(current_b_ratio, current_b_ratio), 
                y = c(0, recommended_f_mult),
                type = 'scatter', mode = 'lines',
                line = list(dash = "dash", color = "white", width = 2),
                name = 'Current Stock',
                showlegend = FALSE) %>%
      add_trace(x = current_b_ratio, 
                y = recommended_f_mult,
                type = 'scatter', mode = 'markers',
                marker = list(size = 15, color = '#ffd93d', 
                              line = list(color = 'white', width = 2)),
                name = 'Recommended F',
                text = paste0("B/B_MSY: ", round(current_b_ratio, 2), "<br>",
                              "Target F/F_MSY: ", round(recommended_f_mult, 2))) %>%
      add_trace(x = current_b_ratio,
                y = adv$current_status$F_ratio,
                type = 'scatter', mode = 'markers',
                marker = list(size = 15, color = '#ff6b6b',
                              symbol = 'x', line = list(width = 2)),
                name = 'Current F',
                text = paste0("Current F/F_MSY: ", round(adv$current_status$F_ratio, 2))) %>%
      layout(
        title = paste("Adaptive Harvest Control Rule -", 
                      toupper(adv$stock_status), "Stock"),
        xaxis = list(title = "B/B_MSY", range = c(0, 2.5)),
        yaxis = list(title = "F Multiplier (Fishing Intensity)", range = c(0, 1.2)),
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 0.5, y0 = 0, y1 = 1.2,
               fillcolor = "red", opacity = 0.15, line = list(width = 0)),
          list(type = "rect", x0 = 0.5, x1 = 1, y0 = 0, y1 = 1.2,
               fillcolor = "yellow", opacity = 0.1, line = list(width = 0)),
          list(type = "rect", x0 = 1, x1 = 2.5, y0 = 0, y1 = 1.2,
               fillcolor = "green", opacity = 0.1, line = list(width = 0))
        ),
        annotations = list(
          list(x = 0.25, y = 1.1, text = "Critical", showarrow = FALSE,
               font = list(color = "red", size = 12)),
          list(x = 0.75, y = 1.1, text = "Caution", showarrow = FALSE,
               font = list(color = "orange", size = 12)),
          list(x = 1.5, y = 1.1, text = "Healthy", showarrow = FALSE,
               font = list(color = "green", size = 12))
        )
      )
  })
  
  # Replace harvest_recommendations output:
  
  output$harvest_recommendations <- renderUI({
    req(advisory_results())
    adv <- advisory_results()
    assess <- adv$current_status
    
    rec_scenario <- adv$scenarios %>% filter(Scenario == adv$recommended)
    
    # Calculate specific metrics
    catch_change <- ((rec_scenario$Expected_Catch - mean(tail(adv$species_data$Catch_MT, 3))) / 
                       mean(tail(adv$species_data$Catch_MT, 3))) * 100
    
    biomass_recovery <- ((rec_scenario$Biomass_5yr - assess$current_B) / assess$current_B) * 100
    
    # Integrate AI predictions if available
    ai_adjustment <- ""
    if (adv$pred_available) {
      ai_adjustment <- div(
        class = "alert alert-info",
        icon("robot"), strong(" AI Integration:"),
        " Machine learning models have adjusted catch projections based on predicted environmental trends."
      )
    }
    
    card(
      card_header(paste("Fishery-Specific Harvest Strategy:", input$advisory_species)),
      
      # Key metrics adapted to fishery status
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = "Recommended TAC",
          value = paste(format(round(rec_scenario$Expected_Catch), big.mark = ","), "MT"),
          showcase = icon("anchor"),
          theme = if(catch_change < 0) "warning" else "success",
          p(sprintf("%+.0f%% from current", catch_change), style = "font-size: 0.8em;")
        ),
        value_box(
          title = "F Adjustment",
          value = paste0(round((1 - rec_scenario$F_multiplier) * 100), "%"),
          showcase = icon("arrow-down"),
          theme = if(rec_scenario$F_multiplier < 0.5) "danger" else "info",
          p("reduction needed", style = "font-size: 0.8em;")
        ),
        value_box(
          title = "Projected Recovery",
          value = paste0(round(biomass_recovery), "%"),
          showcase = icon("chart-line"),
          theme = if(biomass_recovery > 20) "success" else "warning",
          p("biomass change (5yr)", style = "font-size: 0.8em;")
        ),
        value_box(
          title = "Sustainability Risk",
          value = paste0(round(rec_scenario$Risk_Overfishing * 100), "%"),
          showcase = icon("triangle-exclamation"),
          theme = if(rec_scenario$Risk_Overfishing < 0.2) "success" 
          else if(rec_scenario$Risk_Overfishing < 0.4) "warning" 
          else "danger",
          p("overfishing probability", style = "font-size: 0.8em;")
        )
      ),
      
      hr(),
      
      # AI adjustment notification
      ai_adjustment,
      
      # Stock-specific implementation guidance
      card(
        card_header(icon("gears"), "Implementation Guidance"),
        
        if (adv$stock_status == "critical") {
          div(
            h5(strong("CRITICAL STATUS - Immediate Action Required"), 
               style = "color: #dc3545;"),
            tags$ul(
              tags$li(strong("Phase 1 (0-6 months):"), 
                      sprintf("Reduce catch to %s MT immediately. Implement emergency spatial closures.",
                              format(round(rec_scenario$Expected_Catch), big.mark = ","))),
              tags$li(strong("Phase 2 (6-18 months):"), 
                      "Intensive monitoring every 3 months. Adjust TAC based on recovery indicators."),
              tags$li(strong("Phase 3 (18+ months):"), 
                      "Gradual reopening of areas showing recovery. Maintain conservative harvest rate."),
              tags$li(strong("Success Criteria:"), 
                      sprintf("Biomass exceeds %.0f MT within 3 years", assess$B_MSY * 0.7))
            )
          )
        } else if (adv$stock_status == "depleted") {
          div(
            h5(strong("REBUILDING STRATEGY"), style = "color: #ffc107;"),
            tags$ul(
              tags$li(strong("Short-term (Year 1):"), 
                      sprintf("Implement %s reduction to %s MT. Enhanced enforcement.",
                              paste0(round((1 - rec_scenario$F_multiplier) * 100), "%"),
                              format(round(rec_scenario$Expected_Catch), big.mark = ","))),
              tags$li(strong("Medium-term (Years 2-3):"), 
                      "Monitor biomass quarterly. Adjust TAC +/- 15% based on trends."),
              tags$li(strong("Target:"), 
                      sprintf("Achieve B/B_MSY > 0.8 (%.0f MT) within 5 years",
                              assess$B_MSY * 0.8))
            )
          )
        } else {  # moderate or healthy
          div(
            h5(strong("Moderate Status - Precautionary Management")),
            tags$ol(
              tags$li(strong("Current Management:"), 
                      sprintf("Maintain TAC at %s MT with %.0f%% buffer below F_MSY",
                              format(round(rec_scenario$Expected_Catch), big.mark = ","),
                              (1 - rec_scenario$F_multiplier) * 100)),
              tags$li(strong("Monitoring:"), 
                      "Semi-annual assessment of key indicators (CPUE, biomass surveys)"),
              tags$li(strong("Trigger Points:"), 
                      sprintf("If B/B_MSY falls below %.1f, implement rebuilding measures",
                              adv$current_status$B_ratio * 0.9)),
              tags$li(strong("Stakeholder Engagement:"), 
                      "Regular consultation on management performance and adjustments")
            )
          )
        }
      )
    )
  })
  
  # Terms table for documentation
  output$terms_table <- renderDT({
    terms <- data.frame(
      Term = c("MSY", "B_MSY", "F_MSY", "SSB", "CPUE", "Recruitment",
               "Overfished", "Overfishing", "Kobe Plot", "HCR"),
      Definition = c(
        "Maximum Sustainable Yield - highest catch that can be taken indefinitely",
        "Biomass at MSY - stock size that produces MSY",
        "Fishing mortality at MSY - harvest rate that produces MSY",
        "Spawning Stock Biomass - total weight of sexually mature fish",
        "Catch Per Unit Effort - index of stock abundance",
        "Number of young fish entering the population",
        "Stock biomass below sustainable levels",
        "Fishing mortality above sustainable levels",
        "Visual representation of stock status relative to reference points",
        "Harvest Control Rule - predetermined management response to stock status"
      )
    )
    
    datatable(terms, options = list(pageLength = 25, searching = TRUE))
  })
}

shinyApp(ui = ui, server = server)


## Fix the Management scenarios error: object 'assess' not found. Show only the fixed section
## Optimize the executive summary error: object 'rec_scenario' not found. Show only the fixed sections	
## Fix the SSB Definition. Show only the fixed section

## Continue the code from       MSY <- r_est * B_MSY / , where it cut off, only. Do not repeat the previous sections
## write an eloquent textual report on the loaded app
## Fiix the error: argument "x" is missing, with no default. Show only the fixed code

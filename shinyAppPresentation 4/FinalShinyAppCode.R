# shinyapp.R
# Improving Aircraft Safety: Fatalities & Settlement Costs

# Libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)
library(viridis)
library(metR)   # for geom_contour_fill, geom_text_contour, label_placer_n
library(bslib)  # for modern Bootstrap themes
library(dplyr)

rm(list = ls())

## ===================== LOAD DATA =====================
master_raw <- readr::read_csv("FinalizeData.csv", show_col_types = FALSE)
master <- master_raw %>%
  rename_with(~ str_replace_all(., " ", "_")) %>%
  mutate(
    Manufacturer      = trimws(Manufacturer),
    Causes            = trimws(Causes),
    Fatalities        = as.numeric(Fatalities),
    Everyone_Involved = as.numeric(Everyone_Involved),
    Date_dmy = suppressWarnings(dmy(Date)),
    Date_mdy = suppressWarnings(mdy(Date)),
    Date_use = coalesce(Date_dmy, Date_mdy),
    Year  = year(Date_use),
    Month_cal = month(Date_use),
    MonthIndex = case_when(
      Year == 2021 ~ Month_cal,
      Year == 2022 ~ Month_cal + 12,
      Year == 2023 ~ Month_cal + 24,
      Year == 2024 ~ Month_cal + 36,
      TRUE ~ NA_real_
    ),
    Settlement_Cost = Fatalities * 1445086.71
  ) %>%
  filter(!is.na(Year), Year >= 2021, Year <= 2024) %>%
  dplyr::select(MonthIndex, Month_cal, Year, Manufacturer, Causes,
                Fatalities, Everyone_Involved, Settlement_Cost)

# --------- EXACT MATCH TO YOUR WORKING SCRIPT ---------
# Step 1: Group by Month + Manufacturer + Causes
grouped_data <- master %>%
  group_by(MonthIndex, Manufacturer, Causes) %>%
  summarise(
    Fatalities        = sum(Fatalities,        na.rm = TRUE),
    Everyone_Involved = sum(Everyone_Involved, na.rm = TRUE),
    Settlement_Cost   = sum(Settlement_Cost,   na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Count distinct months per manufacturer
threshold <- 24
manu_months <- grouped_data %>%
  group_by(Manufacturer) %>%
  summarise(month_count = n_distinct(MonthIndex), .groups = "drop")

# Step 3: Manufacturers with ≥ 24 months
valid_manufacturers <- manu_months %>%
  filter(month_count >= threshold) %>%
  pull(Manufacturer)

# Step 4: Filter grouped dataset
data_filtered <- grouped_data %>%
  filter(Manufacturer %in% valid_manufacturers)

# Step 5: Create vector for UI + Top 6 used in charts
manufacturer_choices <- sort(valid_manufacturers)

top6_manufacturers <- data_filtered %>%
  group_by(Manufacturer) %>%
  summarise(total_fatalities = sum(Fatalities), .groups = "drop") %>%
  arrange(desc(total_fatalities)) %>%
  slice_head(n = 6) %>%
  pull(Manufacturer)

# Year range for filters
year_min <- min(master$Year, na.rm = TRUE)
year_max <- max(master$Year, na.rm = TRUE)

# Sidebar filter function (unchanged)
filter_data <- function(data, manufacturer, year_range) {
  df <- data %>% filter(Year >= year_range[1], Year <= year_range[2])
  if (!is.null(manufacturer) && manufacturer != "All manufacturers") {
    df <- df %>% filter(Manufacturer == manufacturer)
  }
  df
}

## ========================= UI =========================

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  h1("Improving Aircraft Safety: Fatalities & Settlement Costs",
     style = "text-align:center;"),
  
  tags$head(tags$style(HTML("
      .shiny-image-output img {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 100%;
        height: auto;
      }
      /* Make FMEA image a bit smaller than full width */
      #fmea_img img {
        max-width: 90%;
        height: auto;
      }
      .well {
        background-color: #f9f9f9;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }
      .kpi-box {
        border-radius: 12px;
        padding: 15px 20px;
        color: #ffffff;
        margin-bottom: 15px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.12);
      }
      .kpi-blue  { background: linear-gradient(135deg, #1E88E5, #42A5F5); }
      .kpi-red   { background: linear-gradient(135deg, #C62828, #EF5350); }
      .kpi-green { background: linear-gradient(135deg, #2E7D32, #66BB6A); }
      .kpi-title {
        font-size: 14px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        opacity: 0.9;
      }
      .kpi-value {
        font-size: 26px;
        font-weight: 700;
        margin-top: 5px;
      }
      .kpi-sub {
        font-size: 13px;
        opacity: 0.9;
        margin-top: 2px;
      }
      .nav-tabs > li > a {
        font-weight: 500;
      }
  "))),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("manufacturer", "Manufacturer",
                  choices = c("All manufacturers", manufacturer_choices)),
      sliderInput("year_range", "Year Range",
                  min = year_min, max = year_max,
                  value = c(year_min, year_max), step = 1),
      tags$hr(),
      h4("Bootstrapping"),
      numericInput("boot_iters", "Bootstrap Iterations", value = 2000, min = 100),
      tags$hr(),
      downloadButton("download_data", "Download Filtered Dataset"),
      tags$hr(),
      actionButton("update", "Update dashboard", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        
        # ========= OVERVIEW ==========
        tabPanel(
          "Overview",
          h3("Process Overview"),
          p("Context for the Six Sigma project: SIPOC, process flow, VOC, and project assumptions."),
          tags$hr(),
          
          h3("Research Question"),
          wellPanel(HTML("
  <div style='font-size:16px; line-height:1.6;'>
    <p><b>Primary Research Question:</b></p>
    <p style='margin-left: 20px; font-size: 18px;'>
      <i>What Are the Main Causes of Aviation Accidents Based on Fatal Aircraft Accident Data From 2021 to 2024?</i>
    </p>
  </div>
")),
          
          tags$hr(),
          h3("Data Description"),
          wellPanel(HTML("
  <div style='font-size:16px; line-height:1.6;'>
    <p>
      The dataset used for this project is sourced from the 
      <b>Aviation Safety Network</b>, an exclusive service of the 
      <b>Flight Safety Foundation</b>, which provides detailed reports of aviation accidents and incidents.
      The data extracted for this analysis covers the years <b>2021–2024</b> (Ranter, 2025).
    </p>

    <p>
      Each row in the dataset represents a single airplane accident and includes:
    </p>

    <ul>
      <li><b>Date</b> of the accident</li>
      <li><b>Aircraft type</b></li>
      <li><b>Aircraft registration</b></li>
      <li><b>Operator or airline</b> involved</li>
      <li><b>Number of fatalities</b> in the accident</li>
      <li><b>Location</b> of the accident</li>
      <li><b>Level of aircraft damage</b> (e.g., destroyed, substantial)</li>
    </ul>

<h4><b>Table: Variable Definitions and Context</b></h4>

<table style='width:100%; border-collapse:collapse;'>
  <tr style='background-color:#f2f2f2;'>
    <th style='padding:10px; border:1px solid #ccc;'>Variable</th>
    <th style='padding:10px; border:1px solid #ccc;'>Description</th>
    <th style='padding:10px; border:1px solid #ccc;'>Context</th>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Fatalities</td>
    <td style='padding:10px; border:1px solid #ccc;'>Number of people who died in an aircraft incident</td>
    <td style='padding:10px; border:1px solid #ccc;'>Min reported: 0 &nbsp;&nbsp; Max reported: 179</td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Settlement Cost</td>
    <td style='padding:10px; border:1px solid #ccc;'>Average payout by manufacturer per fatality</td>
    <td style='padding:10px; border:1px solid #ccc;'>Linear model: <b>Settlement = Fatalities × $1,445,086.71</b></td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Date / Month</td>
    <td style='padding:10px; border:1px solid #ccc;'>Time variable used to observe trends</td>
    <td style='padding:10px; border:1px solid #ccc;'>Jan 2021 to Dec 2024</td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Aircraft Manufacturer</td>
    <td style='padding:10px; border:1px solid #ccc;'>Aircraft-producing company</td>
    <td style='padding:10px; border:1px solid #ccc;'>
      Used to compare fatality severity across manufacturers.<br><br>
      Examples:<br>
     <b>Airbus</b>, <b>Boeing</b>, <b>Embraer</b>, <b>Antonov</b>, <b>Cessna</b>, <b>Beechcraft</b>,  Lockheed, Bombardier, de Havilland, ATR, Let, Ilyushin, Shaanxi, Viking, Gulfstream, Dornier, Learjet, Cirrus, CASA, Hawker, Dassault, Sukhoi, North, Saab, Rockwell, Pilatus, Tupolev, Fairchild, BAE, and others.
    </td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Fatality Rate</td>
    <td style='padding:10px; border:1px solid #ccc;'>Fatalities / Total people involved</td>
    <td style='padding:10px; border:1px solid #ccc;'>
      Example monthly means:<br>
      Month 1: 21 fatalities<br>
      Month 2: 1.4 fatalities<br>
      Month 3: 1.83 fatalities<br>
      Month 4: 0 fatalities<br>
      Month 5: 4.5 fatalities<br>
      Month 6: 2.4 fatalities<br>
    </td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Causes</td>
    <td style='padding:10px; border:1px solid #ccc;'>Categorical cause assigned to each accident</td>
    <td style='padding:10px; border:1px solid #ccc;'>
      Bird Strike, Pilot Error, Mechanical Failure, Maintenance, Fire, Runway, Turbulence,
      ATC Miscommunication, Undetermined, Miscellaneous
    </td>
  </tr>

  <tr>
    <td style='padding:10px; border:1px solid #ccc;'>Cause Frequency</td>
    <td style='padding:10px; border:1px solid #ccc;'>Count of accidents per cause type</td>
    <td style='padding:10px; border:1px solid #ccc;'>
      Example: <b>201 accidents</b> attributed to Pilot Error
    </td>
  </tr>

</table>

</div>
")),
          
          tags$hr(),
          h3("Project Assumptions & Data Limitations"),
          wellPanel(HTML("
  <div style='font-size:16px; line-height:1.6;'>

    <h4><b>Project Assumptions & Data Limitations</b></h4>

    <ul>
      <li>
        Manufacturer payout assumption: Boeing paid <b>$500M</b> for 
        <b>346 fatalities</b>, implying approximately <b>$1,445,086.71 per passenger</b>.
      </li>

      <li>
        Data are sourced primarily from a <b>single website</b>, which may lead to
        missing or inaccurate accident reports.
      </li>

      <li>
        Only manufacturers with accidents in <b>at least 24 of the 48 months</b>
        were included.
      </li>

      <li>
        The six qualifying manufacturers are:
        <b>Airbus, Antonov, Beechcraft, Boeing, Cessna, Embraer</b>.
      </li>

      <li>
        Many accidents fall under <b>Miscellaneous</b>; future work will break these
        into detailed subcategories.
      </li>
    </ul>

    <h4><b>Linear Regression Model Used in Analysis</b></h4>

    <p>
      The analysis uses a multivariate linear regression model to examine how
      aircraft manufacturer category and accident characteristics influence the 
      number of fatalities. The dependent variable is <b>Fatalities</b>, while 
      independent variables include <b>Everyone_Involved</b>, <b>Month</b>, 
      manufacturer categories, and accident causes.
    </p>

    <p><b>
      y = β₀ + β₁(Everyone_Involved) + β₂(Month)
      + β₃(Antonov) + β₄(Beechcraft) + β₅(Boeing)
      + β₆(Cessna) + β₇(Embraer)
      + β₈(Bird Strike) + β₉(Fire) + β₁₀(Maintenance)
      + β₁₁(Mechanical Failure) + β₁₂(Miscellaneous)
      + β₁₃(Pilot Error) + β₁₄(Runway)
      + β₁₅(Turbulence) + β₁₆(Undetermined)
    </b></p>

  </div>
")),
          h3("Methods"),
          wellPanel(HTML("
  <div style='font-size:16px; line-height:1.6;'>
    
    <p><b>Analytical Methods Used in This Project</b></p>

    <ul>
      <li>
        <b>Regression Modeling:</b> Between the number of casualties, type of plane 
        manufacturers, causes, and time to examine their relationships.
      </li>

      <li>
        <b>Statistical Process Control (SPC):</b> To find causes of variation in the 
        data using statistical bounds and rules.
      </li>

      <li>
        <b>Failure Modes and Effects Analysis (FMEA):</b> To determine severity, 
        occurrence, and detection, and to identify areas where reducing the RPN can 
        improve safety.
      </li>

      <li>
        <b>Bootstrapping:</b> To estimate uncertainty and standard error in key 
        quantities of interest by creating simulated datasets from the original data.
      </li>
      
    </ul>

  </div>
")),
          
          tags$hr(),
          tabsetPanel(
            tabPanel(
              "SIPOC Diagram",
              wellPanel(imageOutput("sipoc_img", height = "500px"))
            ),
            tabPanel(
              "Process Map",
              wellPanel(imageOutput("process_img", height = "500px"))
            ),
            tabPanel(
              "Voice of Customer Tree",
              wellPanel(imageOutput("voc_img", height = "500px"))
            )
          )
        ),
        
        # ========= FATALITIES & COSTS ==========
        tabPanel(
          "Raw Data Summary",
          h3("Fatalities and Settlement Costs by Year"),
          
          fluidRow(
            column(4, uiOutput("kpi_total_fatalities")),
            column(4, uiOutput("kpi_total_cost")),
            column(4, uiOutput("kpi_avg_monthly_fatalities"))
          ),
          
          tags$hr(),
          fluidRow(
            column(12, wellPanel(plotOutput("fatalities_by_year", height = 350)))
          ),
          fluidRow(
            column(12, wellPanel(plotOutput("costs_by_year", height = 350)))
          ),
          
          tags$hr(),
          h3("Quantities of Interest"),
          fluidRow(
            column(6, h4("Yearly Summary"), tableOutput("summary_table")),
            column(6, h4("Monthly Statistics"), tableOutput("monthly_stats_table"))
          ),
          
          tags$hr(),
          fluidRow(
            column(6, h4("Top Manufacturers Totals"), tableOutput("manufacturer_totals_table")),
            column(6, h4("Manufacturer Monthly Averages"), tableOutput("manufacturer_monthly_avg_table"))
          ),
          
          tags$hr(),
          h4("Interpretation of Quantities of Interest"),
          wellPanel(HTML("
<p style='font-size:16px; line-height:1.6;'>

<b>Manufacturer Monthly Averages Table</b> shows us that the aircraft manufacturer <b>Boeing</b> has the 
highest monthly average λ of <b>7.96</b> while <b>Airbus</b> has the lowest average λ of 
<b>0.25</b>. This shows that from 2021 to 2024, Boeing aircraft are more likely to experience 
higher fatalities when flying compared to Airbus planes. Since fatalities and settlement 
cost are directly correlated, Boeing also pays the most in monthly settlement cost with 
<b>$11,500,482</b> while Airbus pays <b>$361,271</b>. 
</p>

<p style='font-size:16px; line-height:1.6;'>
<b>Monthly Statistics Table</b> calculates the total statistics of these manufacturers' averages, giving us 
a picture of how companies performed overall from 2021 to 2024 in terms of safety and how 
much they varied. Overall, across all manufacturers, the average monthly grouped accident 
resulted in <b>2.52 fatalities</b>, meaning that manufacturers in general had about 
<b>3 fatalities every month</b>. This brings the yearly total to about <b>36 fatalities</b>. 
</p>

<p style='font-size:16px; line-height:1.6;'>
The standard deviation of the average monthly fatality is <b>2.76 fatalities</b>, meaning the 
monthly averages of fatalities per manufacturer vary significantly from the overall mean. There 
is a similar trend in the average settlement costs across manufacturers. 
</p>

<p style='font-size:16px; line-height:1.6;'>
From a broader perspective, the Top Manufacturers Totals Table shows that, over 4 years, Boeing aircraft have resulted in the most fatalities with 382 which correlates with a total settlement cost of $552,023,123. Airbus has the least amount of fatalities with 12 bringing the settlement cost to $17,341,041. The table also showed that smaller aircraft like Cessna have accumulated the second most fatalities with 114. This highlights that both small and large aircraft have experienced high fatalities over the 4 years.   
</p>

"))
        ),
        
        # ========= GRAPH OVER TIME ==========
        tabPanel(
          "Time Varying Trends",
          
          wellPanel(plotOutput("spc_plot", height = "450px")),
          
          tags$hr(),
          h4("Interpretation of Control Chart"),
          wellPanel(uiOutput("spc_text")),
          
          h4("Trend Summary Table"),
          wellPanel(
            tableOutput("trend_table")
          ),
          
          h4("Interpretation of Trends Over Time"),
          wellPanel(HTML("
    <p style='font-size:16px; line-height:1.6;'>
    After application of a simple linear regression to both the fatalities and payouts
    over the selected time period, the trends are summarized above, using a predetermined 
    threshold of 0.15 to determine a strong or weak goodness of fit with a linear model.
    Because the team assumed the settlement cost is directly proportional to the fatality count, 
    it is expected that the R^2 values are the same for both models. </p>
    
    <p style='font-size:16px; line-height:1.6;'>
    When analyzing all of the manufaturers over the full time period, Embraer aircraft
    seem to have the 'strongest' increase in fatalities over those years. A pure
    linear anlaysis is definitely skewed because most aviation related accidents 
    have zero fatalities, generating the necessity of a multivariate regeression model
    to glean any valuable information. 
    </p>
  "))
        ),
        
        # ========= FMEA (NEW SEPARATE TAB) ==========
        tabPanel(
          "FMEA",
          h3("FMEA for Common Failure Modes"),
          wellPanel(imageOutput("fmea_img", height = "500px")),
          
          tags$hr(style = "margin-top: 820px; margin-bottom: 30px;"),
          
          h3("FMEA Results"),
          wellPanel(uiOutput("fmea_text"))
        ),
        
        # ========= RISK ANALYSIS (NEW SEPARATE TAB) ==========
        tabPanel(
          "Risk Analysis",
          
          h3("Accident Cause Frequencies"),
          wellPanel(plotOutput("causes_plot", height = 350)),
          
          tags$hr(style = "margin: 30px 0;"),
          
          h3("Risk Contour: Predicted Fatalities"),
          wellPanel(imageOutput("contour_plot", height = "450px")),
          
          tags$hr(style = "margin: 30px 0;"),
          
          h3("Interpretation of Contour Plot"),
          wellPanel(uiOutput("contour_text"))
        ),
        
        # ========= BOOTSTRAPPING ==========
        tabPanel(
          "Bootstrapping",
          h3("Bootstrap Distribution of Mean Settlement Cost"),
          wellPanel(plotOutput("bootstrap_plot", height = 400)),
          tags$hr(),
          uiOutput("bootstrap_text")
        ),
        
        # ========= Multivariate Regression Analysis  ==========
        tabPanel(
          "Multivariate Regression Analysis",
          tags$hr(),
          h3("Regression Coefficient Table"),
          tableOutput("coef_table_clean"),
          tags$hr(),
          h3("Interpretation of Regression Results"),
          wellPanel(HTML("
<p style='font-size:16px; line-height:1.6;'>
The results from the linear regression model, indicate that several variables are statistically significant contributors 
to the number of fatalities in aviation accidents. The variables <b>Everyone Involved</b>, <b>Bird Strike</b>, and <b>Pilot Error</b> 
have p-values less than 0.001, marking them as highly significant predictors. Mechanical Failures and Maintenance are also statistically 
significant, with p-values less than 0.01.
</p>

<p style='font-size:16px; line-height:1.6;'>
Among the aircraft manufacturers, <b>Boeing</b> shows a statistically significant effect on fatalities, with a p-value less than 0.05. 
It also has the largest estimate of all manufacturers included in the model, suggesting a meaningful impact on the number of fatalities.
</p>

<p style='font-size:16px; line-height:1.6;'>
In terms of effect size, the predictors <b>Bird Strike</b>, <b>Maintenance</b>, <b>Mechanical Failure</b>, and <b>Pilot Error</b> have some 
of the largest estimates in the model, indicating that they contribute most strongly to fatality counts when they occur.
</p>

<p style='font-size:16px; line-height:1.6;'>
Upon examining the results in more detail, it is surprising that <b>Bird Strike</b> appears as highly significant, given its relatively 
low frequency in the dataset. However, one catastrophic accident involving a bird strike resulted in <b>179 fatalities</b>, 
which can influences the model and amplifies the statistical effect.
</p>
"))
        ),  
        
        # ========= RECOMMENDATIONS ==========
        tabPanel(
          "Recommendations",
          
          h3("AI-Driven Safety Improvements"),
          wellPanel(HTML("
  <ul style='font-size: 16px; line-height: 1.6;'>
    <li>
      The team developed an alternative scenario where aircraft manufacturers
      use an <b>AI-based flight interface</b> to reduce fatalities.
    </li>
    <li>
      Based on Bautista-Hernández and Martín-Prats, AI could prevent 
      approximately <b>90% of fatalities</b>.
    </li>
    <li>
      From 2021–2024, there were <b>343 fatalities</b> attributed to mechanical 
      failures and maintenance issues.
    </li>
    <li>
      Assuming AI is used and 90% prevention, then approximately <b>308 people would have survived</b>,
      meaning <b>35 people</b> would have died from mechanical failures and maintenance issues.
    </li>
    <li>
      Financial impact:
      <ul>
        <li>Baseline settlement: <b>$495,664,741.53</b></li>
        <li>AI-enabled settlement: <b>$50,578,034.85</b></li>
        <li><b>Savings: $445,086,706.68</b> (≈89.8% reduction)</li>
      </ul>
    </li>
    <li>
      The team notes that preventing 90% of crashes might be optimistic, so more conservative 
      scenarios (9% and 45% prevention) were also evaluated.
    </li>
    <li>
      Across all scenarios, higher prevention percentages lead to a
      <b>decrease in accumulated settlement costs</b>.
    </li>
    <li>
      For further reference, see: Bautista-Hernández, Jorge, and María Ángeles Martín-Prats. 2024. 
      <i>Artificial Intelligence Approach in Aerospace for Error Mitigation</i>. 
      <em>Aerospace</em> 11(4): 300. https://doi.org/10.3390/aerospace11040300
    </li>
  </ul>
")),
          tags$hr(),
          h3("AI Prevention Scenarios"),
          tableOutput("ai_scenario_table")
        )
      )
    )
  )
)


## ======================= SERVER =======================

server <- function(input, output, session) {
  
  ## STATIC IMAGES
  output$sipoc_img  <- renderImage({ list(src = "pics/SIPOC_Diagram.png", contentType = "image/png") }, deleteFile = FALSE)
  output$process_img <- renderImage({ list(src = "pics/Process_map.png", contentType = "image/png") }, deleteFile = FALSE)
  output$voc_img    <- renderImage({ list(src = "pics/VOC.png", contentType = "image/png") }, deleteFile = FALSE)
  output$fmea_img   <- renderImage({ 
    list(
      src = "pics/FMEA.png",
      contentType = "image/png"
    )
  }, deleteFile = FALSE)
  
  
  ## FILTERED DATA (respects sidebar filters)
  filtered_data <- eventReactive(input$update, ignoreNULL = FALSE, {
    filter_data(master, input$manufacturer, input$year_range)
  })
  
  
  ## DOWNLOAD HANDLER
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  ## SUMMARY TABLES
  # 1) All manufacturers (for tables and yearly plots)
  summary_year_all <- reactive({
    filtered_data() %>%
      filter(Year <= 2024) %>%
      group_by(Year) %>%
      summarise(
        total_fatalities = sum(Fatalities, na.rm = TRUE),
        total_cost       = sum(Settlement_Cost, na.rm = TRUE),
        n_incidents      = n(),
        .groups = "drop"
      )
  })
  
  # 2) Top 6 manufacturers only (for KPI boxes)
  summary_year_top6 <- reactive({
    filtered_data() %>%
      filter(Manufacturer %in% top6_manufacturers) %>%
      filter(Year <= 2024) %>%
      group_by(Year) %>%
      summarise(
        total_fatalities = sum(Fatalities, na.rm = TRUE),
        total_cost       = sum(Settlement_Cost, na.rm = TRUE),
        n_incidents      = n(),
        .groups = "drop"
      )
  })
  
  summary_year_with_pred <- reactive({
    df <- summary_year_all()
    if (nrow(df) < 2) return(df %>% mutate(type = "Actual"))
    
    mod_fat  <- lm(total_fatalities ~ Year, data = df)
    mod_cost <- lm(total_cost ~ Year, data = df)
    
    pred2025 <- tibble(
      Year = 2025,
      total_fatalities = pmax(0, as.numeric(predict(mod_fat, tibble(Year = 2025)))),
      total_cost       = pmax(0, as.numeric(predict(mod_cost, tibble(Year = 2025)))),
      n_incidents      = NA
    )
    
    
    bind_rows(
      df %>% mutate(type = "Actual"),
      pred2025 %>% mutate(type = "Predicted")
    )
  })
  
  ## KPI CARDS (TOP 6 MANUFACTURERS ONLY)
  output$kpi_total_fatalities <- renderUI({
    df <- summary_year_top6()
    if (nrow(df) == 0) return(NULL)
    total_fat <- sum(df$total_fatalities, na.rm = TRUE)
    div(class = "kpi-box kpi-blue",
        div(class = "kpi-title", "Total Fatalities (Top 6, 2021–2024)"),
        div(class = "kpi-value", format(round(total_fat), big.mark = ",")),
        div(class = "kpi-sub", "Across top 6 manufacturers only if “All manufacturers” selected.")
    )
  })
  
  output$kpi_total_cost <- renderUI({
    df <- summary_year_top6()
    if (nrow(df) == 0) return(NULL)
    total_cost <- sum(df$total_cost, na.rm = TRUE)
    div(class = "kpi-box kpi-red",
        div(class = "kpi-title", "Total Settlement Cost (Top 6)"),
        div(class = "kpi-value", scales::dollar(total_cost)),
        div(class = "kpi-sub", "Aggregate cost for 2021–2024 (top 6)")
    )
  })
  
  output$kpi_avg_monthly_fatalities <- renderUI({
    df <- summary_year_top6()
    if (nrow(df) == 0) return(NULL)
    yrs <- length(unique(df$Year))
    months <- yrs * 12
    avg_fat <- sum(df$total_fatalities, na.rm = TRUE) / months
    div(class = "kpi-box kpi-green",
        div(class = "kpi-title", "Average Monthly Fatalities (Top 6)"),
        div(class = "kpi-value", round(avg_fat, 2)),
        div(class = "kpi-sub", "Based on selected years for top 6 manufacturers")
    )
  })
  
  ## YEARLY PLOTS (ALL MANUFACTURERS IN FILTERED DATA)
  output$fatalities_by_year <- renderPlot({
    df <- summary_year_with_pred()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = factor(Year), y = total_fatalities, fill = type)) +
      geom_col() +
      scale_fill_manual(values = c("Actual" = "steelblue", "Predicted" = "orange")) +
      theme_minimal() +
      labs(
        title = "Total Fatalities per Year (2021–2024 Actual, 2025 Predicted)",
        x = "Year", y = "Total Fatalities"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
  })
  
  output$costs_by_year <- renderPlot({
    df <- summary_year_with_pred()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = factor(Year), y = total_cost / 1e6, fill = type)) +
      geom_col() +
      scale_fill_manual(values = c("Actual" = "darkred", "Predicted" = "orange")) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Total Settlement Cost per Year (2021–2024 Actual, 2025 Predicted)",
        x = "Year", y = "Total Cost (Millions USD)"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
  })
  
  ## SUMMARY TABLE RENDER (ALL MANUFACTURERS)
  output$summary_table <- renderTable({
    summary_year_all() %>%
      mutate(
        Year = as.integer(Year),
        `Total Fatalities`       = total_fatalities,
        `Total Cost`             = total_cost,
        `Number of Incidents`    = n_incidents,
        `Total Cost (Millions)`  = round(total_cost / 1e6, 2)
      ) %>%
      dplyr::select(
        Year,
        `Total Fatalities`,
        `Total Cost`,
        `Number of Incidents`,
        `Total Cost (Millions)`
      )
  })
  
  output$monthly_stats_table <- renderTable({
    tibble(
      Metric = c(
        "Average Monthly Fatalities",
        "Standard Deviation of Monthly Fatalities",
        "Average Monthly Settlement Costs (USD)",
        "Standard Deviation of Monthly Settlement Costs (USD)"
      ),
      Value = c("2.524", "2.760", "$3,647,840.41", "$3,989,031.97")
    )
  })
  
  output$manufacturer_totals_table <- renderTable({
    tibble(
      Manufacturer = c("Airbus", "Antonov", "Beechcraft", "Boeing", "Cessna", "Embraer"),
      `Total Fatalities` = c(12, 96, 58, 382, 114, 65),
      `Total Settlement Cost (USD)` = c(
        17341041,
        138728324,
        83815029,
        552023123,
        164739885,
        93930636
      )
    )
  })
  
  output$manufacturer_monthly_avg_table <- renderTable({
    tibble(
      Manufacturer = c("Airbus","Antonov","Beechcraft","Boeing","Cessna","Embraer"),
      `Average Monthly Fatalities` = c(0.25, 2.00, 1.21, 7.96, 2.38, 1.35),
      'Lower 95% CL' = c(0.056, 0.680, 0.485, 2.178, 1.557, -0.549),
      'Upper 95% CL' = c(0.444, 3.32, 1.932, 13.739, 3.193, 3.257),
      `Average Monthly Cost (USD)` = c("$361,271","$2,890,173","$1,746,146","$11,500,482","$3,432,081","$1,956,888"),
      `Lower 95% CL (USD)` = c("$80,231","$982,733","$700,497","$3,147,155","$2,249,296","-$792,939"),
      `Upper 95% CL (USD)` = c("$642,313","$4,797,614","$2,791,796","$19,853,809","4,614,866","$4,706,716")
    )
  })
  
  ## TIME SERIES PLOTS (TOP 6 MANUFACTURERS)
  ts_top6 <- reactive({
    filtered_data() %>%
      filter(
        Manufacturer %in% top6_manufacturers,
        !is.na(MonthIndex),
        is.finite(Fatalities),
        is.finite(Settlement_Cost)
      )
  })
  
  ## Trend Table
  output$trend_table <- renderTable({
    df <- ts_top6()
    req(nrow(df) > 0)
    
    # Compute R² and slopes for fatalities
    fat <- df |>
      group_by(Manufacturer) |>
      summarise(
        r2_fatal   = summary(lm(Fatalities ~ MonthIndex))$r.squared,
        slope_fatal = coef(lm(Fatalities ~ MonthIndex))[2],
        .groups = "drop"
      )
    
    # Compute R² and slopes for payout
    pay <- df |>
      group_by(Manufacturer) |>
      summarise(
        r2_pay   = summary(lm(Settlement_Cost ~ MonthIndex))$r.squared,
        slope_pay = coef(lm(Settlement_Cost ~ MonthIndex))[2],
        .groups = "drop"
      )
    
    out <- left_join(fat, pay, by = "Manufacturer") |>
      mutate(
        Trend_Fatalities = case_when(
          slope_fatal > 0 ~ "Increasing",
          slope_fatal < 0 ~ "Decreasing",
          TRUE ~ "Flat"
        ),
        Trend_Payout = case_when(
          slope_pay > 0 ~ "Increasing",
          slope_pay < 0 ~ "Decreasing",
          TRUE ~ "Flat"
        ),
        Strength_Fatalities = ifelse(r2_fatal < 0.15, "Weak", "Strong"),
        Strength_Payout     = ifelse(r2_pay   < 0.15, "Weak", "Strong")
      ) |>
      mutate(
        r2_fatal = round(r2_fatal, 3),
        r2_pay   = round(r2_pay,   3)
      ) |>
      dplyr::select(
        Manufacturer,
        `Fatalities R²`    = r2_fatal,
        `Fatality Trend`   = Trend_Fatalities,
        `Fatality Strength`= Strength_Fatalities,
        `Payout R²`        = r2_pay,
        `Payout Trend`     = Trend_Payout,
        `Payout Strength`  = Strength_Payout
      )
    
    out
  })
  
  # FATALITIES PLOT---------
  output$fatalities_time_plot <- renderPlot({
    df <- ts_top6()
    if (nrow(df) == 0) return(NULL)
    
    # Compute R² per manufacturer
    r2_df <- df |>
      dplyr::group_by(Manufacturer) |>
      dplyr::summarise(
        r2 = summary(lm(Fatalities ~ MonthIndex))$r.squared,
        .groups = "drop"
      )
    
    # Create a named vector of colors
    colors <- viridis::viridis(n = nrow(r2_df), option = "plasma")
    names(colors) <- r2_df$Manufacturer
    
    # Update legend labels to include R²
    legend_labels <- paste0(r2_df$Manufacturer, " (R²=", round(r2_df$r2, 3), ")")
    names(legend_labels) <- r2_df$Manufacturer
    
    ggplot(df, aes(x = MonthIndex, y = Fatalities, color = Manufacturer)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = colors, labels = legend_labels) +
      theme_classic() +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
      ) +
      labs(
        x = "Time (Months)",
        y = "Fatalities",
        color = "Manufacturer"
      )
  })
  
  #PAYOUT PLOT-------
  output$payout_time_plot <- renderPlot({
    df <- ts_top6()
    if (nrow(df) == 0) return(NULL)
    
    # Compute R² per manufacturer
    r2_df <- df |>
      dplyr::group_by(Manufacturer) |>
      dplyr::summarise(
        r2 = summary(lm(Settlement_Cost ~ MonthIndex))$r.squared,
        .groups = "drop"
      )
    
    # Create a named vector of colors
    colors <- viridis::viridis(n = nrow(r2_df), option = "plasma")
    names(colors) <- r2_df$Manufacturer
    
    # Update legend labels to include R²
    legend_labels <- paste0(r2_df$Manufacturer, " (R²=", round(r2_df$r2, 3), ")")
    names(legend_labels) <- r2_df$Manufacturer
    
    ggplot(df, aes(x = MonthIndex, y = Settlement_Cost, color = Manufacturer)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
      scale_color_manual(values = colors, labels = legend_labels) +
      scale_y_continuous(labels = scales::comma) +
      theme_classic() +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA),
        legend.title = element_text(size = 16),
        legend.text  = element_text(size = 14)
      ) +
      labs(
        x = "Time (Months)",
        y = "Payout per Customer (USD)",
        color = "Manufacturer"
      )
  })
  
  ## CAUSES PLOT
  output$causes_plot <- renderPlot({
    df <- filtered_data()
    if (!("Causes" %in% names(df))) return(NULL)
    
    freq <- df %>%
      filter(!is.na(Causes), Causes != "") %>%
      count(Causes, sort = TRUE)
    
    if (nrow(freq) == 0) return(NULL)
    
    ggplot(freq, aes(x = reorder(Causes, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Frequency of Accident Causes (Filtered Years)",
        x = "Cause", y = "Incidents"
      ) +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
  })
  
  ## RISK CONTOUR (m6 model, synced to sliders)
  output$contour_plot <- renderPlot({
    
    df <- filtered_data() %>%
      filter(
        is.finite(Fatalities),
        is.finite(Everyone_Involved),
        is.finite(MonthIndex),
        Everyone_Involved > 0
      )
    
    if (nrow(df) < 10) return(NULL)
    
    # Fit model using MonthIndex instead of Month
    square <- lm(
      formula = sqrt(Fatalities) ~ MonthIndex * Everyone_Involved +
        I(MonthIndex^2) + I(Everyone_Involved^2),
      data = df
    )
    
    # Prediction grid MUST match the variable names in the model!!!
    myx <- expand_grid(
      Everyone_Involved = seq(min(df$Everyone_Involved),
                              max(df$Everyone_Involved),
                              length.out = 100),
      MonthIndex = seq(min(df$MonthIndex),
                       max(df$MonthIndex),
                       length.out = 100)
    )
    
    # Predictions
    mypred <- myx %>%
      mutate(yhat = predict(square, newdata = myx))
    
    mypredt <- mypred %>%
      mutate(Fatalities_pred = yhat^2)
    
    g2 <- ggplot() +
      geom_contour_fill(
        data = mypredt,
        aes(x = Everyone_Involved, y = MonthIndex, z = Fatalities_pred),
        binwidth = 1,
        color = "white"
      ) +
      geom_text_contour(
        data = mypredt,
        aes(x = Everyone_Involved, y = MonthIndex, z = Fatalities_pred),
        skip = 0,
        stroke.color = "white",
        stroke = 0.2,
        label.placer = label_placer_n(1)
      ) +
      scale_fill_viridis(option = "plasma", name = "Predicted Fatalities") +
      labs(
        x = "Number of People Involved",
        y = "Month (Index)"
      ) +
      theme_classic() +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
    
    g2
  })
  
  output$contour_text <- renderUI({
    df <- filtered_data()
    req(nrow(df) >= 10)
    
    # Fit model
    square <- lm(
      sqrt(Fatalities) ~ MonthIndex * Everyone_Involved +
        I(MonthIndex^2) + I(Everyone_Involved^2),
      data = df
    )
    
    # Prediction grid
    myx <- expand_grid(
      Everyone_Involved = seq(min(df$Everyone_Involved), max(df$Everyone_Involved), length.out = 100),
      MonthIndex = seq(min(df$MonthIndex), max(df$MonthIndex), length.out = 100)
    )
    
    pred <- myx %>% mutate(Fatalities_pred = predict(square, newdata = myx)^2)
    
    # Summary numbers
    min_f <- round(min(pred$Fatalities_pred), 2)
    max_f <- round(max(pred$Fatalities_pred), 2)
    
    # max-risk location
    worst <- pred %>% slice_max(Fatalities_pred, n = 1)
    worst_m <- round(worst$MonthIndex, 1)
    worst_e <- round(worst$Everyone_Involved, 1)
    
    # effect comparison
    c_month <- abs(coef(square)["MonthIndex"])
    c_ev    <- abs(coef(square)["Everyone_Involved"])
    driver  <- ifelse(c_ev > c_month, "the number of people involved", "the month index")
    
    HTML(sprintf("
    <p style='font-size:16px; line-height:1.6;'>
      The contour plot displays predicted fatalities across Month and
      number of people involved based on the model 
      <b>lm(sqrt(Fatalities) ~ MonthIndex * Everyone_Involved + I(MonthIndex^2) + I(Everyone_Involved^2))</b>.
      A square model was selected because it provided a higher R² than linear or logarithmic alternatives.
    </p>

    <p style='font-size:16px; line-height:1.6;'>
      Within the selected timeframe and manufacturer, predicted fatalities range from 
      <b>%s</b> to <b>%s</b>. The highest predicted value occurs around
      <b>%s people involved</b> and <b>Month Index %s</b>. In this model, 
      predicted fatalities vary more strongly with <b>%s</b>, which is reflected
      in the steepness of the contour gradients in that direction.
    </p>
  ", min_f, max_f, worst_e, worst_m, driver))
  })
  
  
  ############################# Control chart ##################################
  ##############################################################################
  output$spc_plot <- renderPlot({
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df <- df %>%
      mutate(Settlement_Cost = as.numeric(Settlement_Cost))
    
    # Short-term statistics per MonthIndex
    stat_s <- df %>%
      group_by(MonthIndex) %>%
      filter(n() > 1) %>%
      summarise(
        xbar = mean(Settlement_Cost, na.rm = TRUE),
        r    = max(Settlement_Cost, na.rm = TRUE) - min(Settlement_Cost, na.rm = TRUE),
        sd   = sd(Settlement_Cost, na.rm = TRUE),
        nw   = n(),
        df   = nw - 1
      )
    
    # Compute sigma_short (pooled SD) and standard error
    avg_nw <- mean(stat_s$nw, na.rm = TRUE)
    sigma_s <- sqrt(mean(stat_s$sd^2, na.rm = TRUE))
    se <- sigma_s / sqrt(avg_nw)
    
    # Add control limits
    stat_s <- stat_s %>%
      mutate(
        sigma_s = sigma_s,
        se = se,
        upper = mean(xbar, na.rm = TRUE) + 3 * se,
        lower = mean(xbar, na.rm = TRUE) - 3 * se
      )
    
    # Prepare labels for annotation
    labels <- tibble(
      time  = max(stat_s$MonthIndex) + 1.5,
      type  = c("mean", "+3σ", "-3σ"),
      value = c(mean(stat_s$xbar, na.rm = TRUE),
                unique(stat_s$upper),
                unique(stat_s$lower)),
      text  = paste0(type, " = ", round(value, 2))
    )
    
    ggplot(stat_s, aes(x = MonthIndex, y = xbar)) +
      geom_hline(aes(yintercept = mean(xbar)), color = "red", size = 1.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.2) +
      geom_line(size = 1, color = "black") +
      geom_point(size = 3, color = "black") +
      geom_label(data = labels, aes(x = time, y = value, label = text),
                 hjust = 1, vjust = 0.5, size = 4) +
      labs(
        title = "Control Chart for Aircraft Settlements",
        x = "Time (Months)",
        y = "Average Settlement Cost per Month"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
  })
  
  
  output$spc_text <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df <- df |>
      mutate(Settlement_Cost = as.numeric(Settlement_Cost))
    
    stat_s <- df |>
      group_by(MonthIndex) |>
      filter(n() > 1) |>
      summarise(
        xbar = mean(Settlement_Cost, na.rm = TRUE),
        sd   = sd(Settlement_Cost, na.rm = TRUE),
        nw   = n(),
        .groups = "drop"
      )
    
    req(nrow(stat_s) >= 3)
    
    avg_nw  <- mean(stat_s$nw, na.rm = TRUE)
    sigma_s <- sqrt(mean(stat_s$sd^2, na.rm = TRUE))
    se      <- sigma_s / sqrt(avg_nw)
    
    CL  <- mean(stat_s$xbar)
    UCL <- CL + 3 * se
    LCL <- CL - 3 * se
    
    x <- stat_s$xbar
    
    above <- sum(x > UCL)
    below <- sum(x < LCL)
    total <- above + below
    
    paragraph <- paste0(
      "<p style='font-size:16px; line-height:1.6;'>",
      
      "A statistical process control (SPC) chart is used here to monitor the ",
      "<b>average monthly settlement cost</b> over the selected time period. ",
      "The centerline represents the overall mean settlement cost, while the upper and lower control limits ",
      "(set at ±3 standard errors) define the expected range of natural variation in the process. ",
      "Values outside these limits indicate unusual or special-cause variation that may warrant investigation.<br><br>",
      
      "For the selected time range, the SPC chart shows <b>", total, "</b> point(s) outside the 3-sigma control limits. Therefore, ",
      if (above > 0) paste0("there are <b>", above, "</b> unusually high months that exceed the upper limit. ") else "",
      if (below > 0) paste0("there are <b>", below, "</b> unusually low months that fall below the lower limit. ") else "",
      if (total == 0) "all monthly averages fall within the expected range of natural variation. " else "",
      
      "If points continue to trend in one direction or fall outside the expected range, this may indicate shifts in the underlying settlement dynamics or changes in event severity.",
      "<br><br>",
      
      "When analyzing the SPC for the average monthly settlement cost over the entire time period, only 2 data points seem to fall outside of the control ranges. ",
      "These two points, along with others that are slightly higher than the mean, seem to occur during winter months, indicating that ",
      "average settlement costs (and proportionally fatalities) are higher than normal during those times and might require special attention.",
      
      "</p>"
    )
    
    HTML(paragraph)
  })
  
  
  ################################# FMEA #######################################
  ##############################################################################
  output$fmea_text <- renderUI({
    df <- filtered_data()
    if (!("Causes" %in% names(df))) {
      return(HTML("<p style='font-size:16px; line-height:1.6;'>No cause data available.</p>"))
    }
    
    freq <- df %>%
      filter(!is.na(Causes), Causes != "", 
             Causes != "Miscellaneous", 
             Causes != "Undetermined") %>% 
      count(Causes, sort = TRUE)
    
    if (nrow(freq) == 0) {
      return(HTML("<p style='font-size:16px; line-height:1.6;'>No qualifying failure modes.</p>"))
    }
    
    top_cause <- freq$Causes[1]
    top_n     <- freq$n[1]
    
    HTML(paste0(
      "<p style='font-size:16px; line-height:1.6;'>",
      "The failure mode with the highest occurrence (other than miscellaneous or undetermined failures) ",
      "is <b>", top_cause, "</b> with <b>", top_n, "</b> occurrences for the current selection of parameters.<br><br>",
      
      "For any failures that are not characterized as miscellaneous or undetermined, 
      a FMEA is constructed to identify the potential effects of the aforementioned 
      failure, including severity, occurrence, and detection ratings. These results,
      which are summarized in an overall risk priority number, are utilized to 
      identify potential failure modes that pose the highest risk to the system 
      and tailor potential advised actions. The team identified mechanical failures, 
      pilot error, and maintenance to be the failure modes with highest impact on occurrence.",
      "</p>"
    ))
  })
  
  ## BOOTSTRAPPING
  boot_results <- reactive({
    x <- filtered_data()$Settlement_Cost
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NULL)
    
    B <- max(100, min(20000, as.integer(input$boot_iters)))
    tibble(stat = replicate(B, mean(sample(x, replace = TRUE))))
  })
  
  output$bootstrap_plot <- renderPlot({
    res <- boot_results()
    if (is.null(res)) return(NULL)
    
    ci <- quantile(res$stat, c(.025, .975))
    m  <- mean(res$stat)
    
    ggplot(res, aes(x = stat)) +
      geom_histogram(bins = 40, fill = "steelblue", color = "white") +
      geom_vline(xintercept = m,  color = "darkblue") +
      geom_vline(xintercept = ci, color = "darkblue", linetype = "dashed") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma) +
      labs(
        x = "Mean Settlement Cost (USD)", y = "Frequency"
      ) +
      theme(
        plot.background  = element_rect(fill = "#f9f9f9", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.background = element_rect(fill = "#f9f9f9", color = NA),
        legend.key        = element_rect(fill = "#f9f9f9", color = NA)
      )
  })
  
  output$bootstrap_text <- renderUI({
    res <- boot_results()
    if (is.null(res)) {
      return(HTML("<p style='font-size:16px; line-height:1.6;'>No settlement cost data available.</p>"))
    }
    
    ci <- quantile(res$stat, c(.025, .975))
    m  <- mean(res$stat)
    B  <- input$boot_iters
    
    fmt <- function(x) scales::dollar(round(x, 2))
    
    HTML(paste0(
      "<p style='font-size:16px; line-height:1.6;'>",
      "<b>Bootstrap Mean Settlement Cost</b><br>",
      "--------------------------------------<br>",
      "Mean: ", fmt(m), "<br>",
      "95% CI: ", fmt(ci[1]), " to ", fmt(ci[2]), "<br><br>",
      
      "<b>Interpretation:</b><br>",
      "An analysis was performed using bootstrapping across all incidents that occurred during the years 2021–2024 to estimate a confidence<br>",
      "interval for the mean settlement cost, while allowing the team to not assume normal distribuation.<br>",
      "Using ", B, " resamples with replacement a 95% confidence interval was calculated to range from ",
      fmt(ci[1]), " to ", fmt(ci[2]), ".<br><br>",
      
      "This means that the monthly average settlement cost can potentially fall<br>",
      "between ", fmt(ci[1]), " and ", fmt(ci[2]), ".",
      "</p>"
    ))
  })
  
  ## AI SCENARIO TABLE
  output$ai_scenario_table <- renderTable({
    tibble(
      Scenario = c("Baseline", "9% Preventive", "45% Preventive", "90% Preventive"),
      `Total Fatalities`       = c(343, 312, 189, 35),
      `Number of People Saved` = c(0, 31, 154, 308),
      `Settlement Cost (USD)`  = c(
        495664741.53,
        450867053.52,
        273121388.19,
        50578034.85
      ),
      `Money Saved (USD)`      = c(
        0,
        44797688.01,
        222543353.34,
        445086706.68
      )
    ) %>%
      mutate(
        `Settlement Cost (USD)` = scales::dollar(`Settlement Cost (USD)`),
        `Money Saved (USD)`     = scales::dollar(`Money Saved (USD)`)
      )
  })
  
  ## REGRESSION COEFFICIENT TABLE
  ## REGRESSION COEFFICIENT TABLE
  output$coef_table_clean <- renderTable({
    tibble::tibble(
      Predictor = c(
        "Intercept", "Everyone Involved",
        "Antonov", "Beechcraft", "Boeing",
        "Cessna", "Embraer",
        "Month",
        "Bird Strike", "Fire", "Maintenance",
        "Mechanical Failure", "Miscellaneous",
        "Pilot Error", "Runway", "Turbulence",
        "Undetermined"
      ),
      Estimate = c(
        -12.44333, 0.22162, 2.66416, 1.21159, 3.40243,
        1.37867, 2.43713,
        -0.03190,
        22.05192, 10.91658, 14.03505,
        11.15217, 11.19539,
        13.13497, 12.29160, 10.43547,
        12.88970
      ),
      Std_Error = c(
        3.98042, 0.01903, 1.90726, 1.73522, 1.43756,
        1.57247, 1.98089,
        0.03419,
        4.67782, 5.16595, 4.47356,
        4.00172, 3.94450,
        3.94738, 4.73186, 4.03979,
        4.11539
      ),
      t_value = c(
        -3.126, 11.647, 1.397, 0.698, 2.367,
        0.877, 1.230,
        -0.933,
        4.714, 2.113, 3.137,
        2.787, 2.838,
        3.328, 2.598, 2.583,
        3.132
      ),
      p_value = c(
        "0.001890", "<2e-16", "0.163169", "0.485405", "0.018379",
        "0.381103", "0.219243",
        "0.351242",
        "3.27e-06", "0.035154", "0.001821",
        "0.005555", "0.004749",
        "0.000951", "0.009705", "0.010115",
        "0.001853"
      ),
      Significance = c(
        "**", "***", "", "", "*",
        "", "",
        "",
        "***", "*", "**",
        "**", "**",
        "***", "**", "*",
        "**"
      )
    )
  })
}

shinyApp(ui = ui, server = server)


# Header
header <- dashboardHeader(
  title = "Online Retail"
)

# Side_Bar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Home",
      tabName = "tab-1",
      icon = icon("home")
    ),
    menuItem(
      text = "Exploratory Data",
      tabName = "tab-2",
      icon = icon("database")
    ),
    menuItem(
      text = "Market Basket Analysis",
      tabName = "tab-3",
      icon = icon("chart-bar")
    ),
    menuItem(
      text = "Product Recommendation",
      tabName = "tab-4",
      icon = icon("star")
    ),
    menuItem(
      text = "Contact",
      tabName = "tab-5",
      icon = icon("user")
    )
  )
)

# Body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "tab-1",
        fluidRow(
          box(
            width = 12, img(src = 'dashboard.jpg'),
            height = "560px",
            align="center",
          ),
          box(
            width = 5,
            height = "630px",
            span("Background", style = "font-size: 30px"),
            br(),
            plotlyOutput("plotbackground"),
            br(),
            span("Source: ",
                 style = "font-size: 16px"),
            tags$a(href="https://www.bps.go.id/publication/2020/12/24/2548417ddc6dab8247553124/statistik-e-commerce-2020.html",
                   "Statistics E-Commerce, BPS Statistics Indonesia", style = "font-size: 16px"),
            br(), 
            span("Percentage growth of Online Retail increase year by year.
                 It can be assumed online retail has potential to always grow 
                 and be profitable.", 
                 style = "font-size: 17px"),
            br(),
            span("For this reason, online retail companies need 
                  to know how to take advantage of this potential.", 
                  style = "font-size: 17px"),  
          ),
          box(
            width = 7,
            height = "630px",
            span("Market Basket Analysis", style = "font-size: 30px"),
            br(),
            span("Definition", style = "font-size: 22px"),
            br(),
            br(),
            img(src = "visualization_mba.jpg", width="40%"),
            br(),
            span(
              "Analytics technique employed by retailers to understand
               customer purchase behaviour. It is used to determine what 
               items are frequently bought together or placed in the same 
              basket by customers.",
              style = "font-size: 18px"
            ),
            br(), 
            br(),
            span("Benefits", style = "font-size: 22px"),
            br(),
            span("1. Targeting products to specific segments", style = "font-size: 18px"),
            br(),
            span("2. Knowing customers purchasing behaviour", style = "font-size: 18px"),
            br(),
            span("3. Optimizing product placement position", style = "font-size: 18px"),
            br(),
            span("4. Create appropriate for product bundles", style = "font-size: 18px"),
            br(),
            span("5. Provide special offers on some products", style = "font-size: 18px")
          )
        )
    ),
    tabItem(
      tabName = "tab-2",
      fluidRow(
        box(
          width = 12,
            span("Dataset", style = "font-size: 28px"),
            br(),
            span("This project using online retail transaction datataset that occurred 
                 between 01/12/2010 and 29/11/2011 at online retail company based in 
                 the United Kingdom.", style = "font-size: 21px")
        ),
        box(
          span("Visualization Data", style = "font-size: 28px"),
          width = 12,
          br(),
          br(),
          valueBoxOutput(outputId = "totalsales", width = 3),
          valueBoxOutput(outputId = "totalcustomer", width = 3),
          valueBoxOutput(outputId = "totaltransaction", width = 3),
          valueBoxOutput(outputId = "totalcountry", width = 3),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          plotlyOutput("plotdata0"),
          br(),
          br(),
          splitLayout(cellWidths = c("50%", "50%"),
                      plotlyOutput("plotdata1"),
                      plotlyOutput("plotdata2")),
          br(),
          splitLayout(
            cellWidths = c("33%", "33%", "33%"),
            plotlyOutput("plotdata3"),
            plotlyOutput("plotdata4"),
            plotlyOutput("plotdata5")
          )
        ),
        box(
          width = 12,
          span("Raw Data", style = "font-size: 28px"),
          br(),
          span("Original data source from",
               style = "font-size: 20px"),
          tags$a(href="https://archive.ics.uci.edu/ml/datasets/online+retail",
                 "Online Retail Dataset", style = "font-size: 20px"),
          span(".",
               style = "font-size: 20px"),
          dataTableOutput(outputId = "tableonlineretail")
        )
      )
    ),
    tabItem(
      tabName = "tab-3",
      fluidRow(
        box(
          width = 12,
          span("Rules", style = "font-size: 28px"),
          dataTableOutput(outputId = "tabelmba")
        ),
        box(
          width = 6, height = "555px",
          span("Rules Point", style = "font-size: 28px"),
          br(),
          span("Hover your cursor to see detail", style = "font-size: 16px"),
          br(),
          br(),
          plotlyOutput("plotmba1")
        ),
        box(
          width = 6,
          span("Rules Connection", style = "font-size: 28px"),
          br(),
          span("Select product or rule", style = "font-size: 18px"),
          br(),
          span("Zoom in or zoom out to see detail", style = "font-size: 15px"),
          br(),
          br(),
          visNetworkOutput("plotmba2")
        )
      )
    ), 
    tabItem(
      tabName = "tab-4",
      fluidRow(
        box(
          width = 6,
          span("Select Product", style = "font-size: 28px"),
          br(),
          br(),
          selectInput(
            inputId = "selectinputproduct1",
            label = "Product 1",
            choices = c("NOTHING",unique(rules_df$Product_Selection_1) %>% 
              sort()),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct2",
            label = "Product 2",
            choices = c(unique(rules_df$Product_Selection_2) %>%
                               sort()),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct3",
            label = "Product 3",
            choices = unique(rules_df$Product_Selection_3) %>% sort(),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct4",
            label = "Product 4",
            choices = unique(rules_df$Product_Selection_4) %>% sort(),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct5",
            label = "Product 5",
            choices = unique(rules_df$Product_Selection_5) %>% sort(),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct6",
            label = "Product 6",
            choices = unique(rules_df$Product_Selection_6) %>% sort(),
            selectize = F,
            selected = "NOTHING"
          ),
          selectInput(
            inputId = "selectinputproduct7",
            label = "Product 7",
            choices = unique(rules_df$Product_Selection_7) %>% sort(),
            selectize = F,
            selected = "NOTHING"
          ),
          column(
            12,
            offset = 0,
            submitButton(
              "Submit",
              width = "100%")
          )
        ),
        box(
          width = 6,
          height = "635px",
          span("Recommendation Product", style = "font-size: 28px"),
          br(),
          br(),
          tags$head(tags$style(HTML(".small-box {height: 115px}"))),
          valueBoxOutput(outputId = "vbox", width = 12),
          br(),
          valueBoxOutput(outputId = "vbox2", width = 12),
          br(),
          valueBoxOutput(outputId = "vbox3", width = 12),
          br(),
          span("Note :", style = "font-size: 24px"),
          br(),
          span("- If product name is duplicate, it's mean same product", 
               style = "font-size: 20px"),
          br(),
          span("- If product name isn't displayed, selected product doesn't 
               have a product recommendation", style = "font-size: 20px")
        )
      )   
    ), 
    tabItem(
      tabName = "tab-5",
      fluidRow(
        box(
          width = 12,
          align = "center",
          img(
            src = 'fotoprofil2.jpg',
            height = '354px',
            width = '265px'
          ),
          br(),
          br(),
          span("Contact", style = "font-size: 28px"),
          br(),
          span("Email : gashasarwono@gmail.com", style = "font-size: 20px"),
          br(),
          br(),
          span("Portofolio", style = "font-size: 28px"),
          br(),
          column(
            12,
            splitLayout(
              cellWidths = c("8%", "8%", "8%"),
              actionButton(
                inputId = 'linkedin',
                label = "LinkedIn",
                icon = icon("linkedin"),
                onclick = "window.open('https://www.linkedin.com/in/gasha-sarwono-putra-ba556a147/', '_blank')",
                style="color: #222D32; background-color: #F39C12; border-color: #F39C12"
              ),
              actionButton(
                inputId = 'github',
                label = "GitHub",
                icon = icon("github"),
                onclick = "window.open('https://github.com/gashasp', '_blank')",
                style="color: #222D32; background-color: #F39C12; border-color: #F39C12"
              ),
              actionButton(
                inputId = 'rpubs',
                label = "RPubs",
                icon = icon("r-project"),
                onclick = "window.open('https://rpubs.com/gashasp', '_blank')",
                style="color: #222D32; background-color: #F39C12; border-color: #F39C12"
              )
            )
          ),
          br(),
          br(),
          br()
        )
      )
    )
  )
)

# Full_Page
dashboardPage(
    skin = "yellow",
    header = header,
    body = body,
    sidebar = sidebar
)


  
  
  
  
  


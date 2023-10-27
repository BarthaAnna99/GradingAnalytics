
library("shiny")
library("shinydashboard")
library("shinyFiles")
library("readxl")
library("dplyr")
library("DT")

### HEADER ###
header <- dashboardHeader(
  title = "Grading Analytics",
  dropdownMenuOutput("messageMenu")
)

### SIDEBAR ###
sidebar <- dashboardSidebar(
  p(img(src = "Logo.svg", height = 70, width = 80)),
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction"),
    menuItem("Introduction", tabName = "introduction_alt"),
    menuItem("Import Data", tabName = "importdata"),
    menuItem("Prepare Dataset", tabName = "preparedataset")
  )
)

### BODY ###
body <- dashboardBody(
  includeCSS("www/styles.css"),

  tabItems(
    tabItem(tabName = "introduction",
            h1("Introduction to Grading Analytics"),
            p(class = "italic", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
            fluidRow(
              column(4, class = "describe",
                     h2(icon("upload"), "Import Data"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginiar")
              ),
              column(4, class = "describe",
                     h2(icon("table"), "Prepare Dataset"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              ),
              column(4, class = "describe",
                     h2(icon("sitemap"), "Program Structure"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              )
            ),
            fluidRow(
              column(6, class = "describe2",
                     h2(icon("percent"), "Metrics"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur")
              ),
              column(6, class = "describe3",
                     h2(icon("spinner"), "Modeling"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur")
              )
            )
    ),

    tabItem(tabName = "introduction_alt",
            h1("Introduction to Grading Analytics"),
            p(class = "italic", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
            fluidRow(
              column(12, class = "describe",
                     br(),
                     br(),
                     p(img(src = "process.png", width="950px", height="270"))
                    )
            )

    ),

    tabItem(tabName = "importdata",
            h1("Import Data"),
            fluidRow( class = 'separate',
              column(2),
              column(2, class = 'title',
                     p("Upload Zip file: ")
              ),
              column(4, class = 'upload',
                     fileInput('zipFile', label= NULL, multiple = FALSE, accept = c(".zip"), width = "400px")
              ),
              column(2, class = 'button',
                     actionButton("upload", "Upload")
              ),
              column(2)
            ),
            fluidRow(
              column(12,
                     DTOutput("fileList")
              )
            )
    ),

    tabItem(tabName = "preparedataset",
            h1("Prepare Dataset"),
            #fluidRow( class = 'separate2'),
            fluidRow(
              column(3, class = 'left_panel',
                     h3("Select the classes"),
                     uiOutput("class_checkboxes", class = 'options'),
                     actionButton("class_select_all", "Select All"),
                     actionButton("class_deselect_all", "Deselect All"),
                     h3("Select the subjects"),
                     uiOutput("subject_checkboxes", class = 'options'),
                     actionButton("subject_select_all", "Select All"),
                     actionButton("subject_deselect_all", "Deselect All")
              ),
              column(9)
            )
    )
  )
)

dashboardPage(
  header, sidebar, body
    )

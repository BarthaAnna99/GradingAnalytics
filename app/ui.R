
library("shiny")
library("shinydashboard")
library("shinyFiles")
library("readxl")
library("dplyr")
library("DT")
library("stringr")
library("fs")
library("ggplot2")

### HEADER ###
header <- dashboardHeader(
  title = "Grading Analytics",
  dropdownMenuOutput("messageMenu")
)

### SIDEBAR ###
sidebar <- dashboardSidebar(
  p(img(src = "logo.png", height = 65, width = 90)),
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction"),
    #menuItem("Introduction", tabName = "introduction_alt"),
    menuItem("Import Data", tabName = "importdata"),
    menuItem("Data Preparation", tabName = "preparedataset"),
    menuItem("Program Structure", tabName = "programsturcture"),
    menuItem("Visualization", tabName = "visualization"),
    menuItem("Metrics", tabName = "metrics")
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
                     h2(icon("upload", class = "my-icon"), "Import Data"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginiar")
              ),
              column(4, class = "describe",
                     h2(icon("table", class = "my-icon"), "Prepare Dataset"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              ),
              column(4, class = "describe",
                     h2(icon("sitemap", class = "my-icon"), "Program Structure"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              )
            ),
            fluidRow(
              column(6, class = "describe2",
                     h2(icon("percent", class = "my-icon"), "Metrics"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              ),
              column(6, class = "describe3",
                     h2(icon("spinner", class = "my-icon"), "Modeling"),
                     p("Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia")
              )
            )
    ),

    # tabItem(tabName = "introduction_alt",
    #         h1("Introduction to Grading Analytics"),
    #         p(class = "italic", "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
    #         fluidRow(
    #           column(12, class = "describe",
    #                  br(),
    #                  br(),
    #                  p(img(src = "process.png", width="950px", height="270"))
    #                 )
    #         )
    #
    # ),

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
            h1("Data Preparation"),
            fluidRow( class = 'separate2',
              column(1),
              column(3,
                     h3("Select Classes"),
                     uiOutput("class_checkboxes"),
                     actionButton("class_select_all", "Select All"),
                     actionButton("class_deselect_all", "Deselect All")
              ),
              column(7,
                     h3("Select Subjects"),
                     uiOutput("subject_checkboxes"),
                     actionButton("subject_select_all", "Select All"),
                     actionButton("subject_deselect_all", "Deselect All")
              ),
              column(1)
            ),
            fluidRow(
              column(12, class ='center',
                     uiOutput("action_button_ui")
              )
            ),
            fluidRow(
              column(12,
                     DTOutput("all_data")
              )
            )
    ),

    tabItem(tabName = "programsturcture",
            h1("Program Structure"),
            fluidRow(
              class = 'separate3',
              column(4,
                     h3("Specify the Semesters"),
              ),
              column(1),
              column(7,
                     h3("The Structure"),
              )
            ),
            fluidRow(
              column(3, class= 'semester_label',
                     uiOutput("semester_label_input")
              ),
              column(1, class = 'semester_input',
                     uiOutput("semester_input")
              ),
              column(1, class ='container',
                     div(class = 'centered-element', h1(uiOutput("action_right_button_ui")))
              ),
              column(7, class = 'structure',
                     DTOutput("program_structure_table")
              )
            )
    ),

    tabItem(tabName = "visualization",
            h1("Visualization"),
            fluidRow(
              class = 'separate3'
            ),
            fluidRow(
              column(4, class = "center",
                     h3("Number of the students in each classes"),
                     plotOutput("bar_chart", height = "210px")
              ),
              column(4, class = "center",
                     h3("Distribution of genders"),
                     plotOutput("pie_chart", height = "210px")
              ),
              column(4, class = "center",
                     h3("Churn of the students over the semesters"),
                     plotOutput("line_chart", height = "210px"),
              )
            ),
            fluidRow(
              class = 'separate4'
            ),
            fluidRow(
              column(1),
              column(2, class = "right",
                     h3('Select classes: ')
              ),
              column(3, class = "left",
                     uiOutput("class_checkboxes_filter")
              ),
              column(2, class = "right2",
                     h3('Select subject: ')
              ),
              column(3, class = "left",
                     uiOutput("subject_checkboxes_filter")
              ),
              column(1)
          ),
          fluidRow(
            column(4, class = "centerLeft",
                   plotOutput("bar_chart_grades", height = "230px")
            ),
            column(4, class = "centerMiddle",
                   plotOutput("bar_chart_exams", height = "230px")
            ),
            column(4, class = "centerRight",
                   plotOutput("bar_chart_subjects", height = "230px")
            )
          ),
    ),

    tabItem(tabName = "metrics",
            h1("Metrics"),
            fluidRow(
              class = 'separate3'
            )
    )

  )
)

dashboardPage(
  header, sidebar, body
    )

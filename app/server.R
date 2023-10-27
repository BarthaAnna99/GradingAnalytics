
server <- function(input, output, session) {

  uploaded_files <- reactiveVal(NULL)
  temp_dir_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  all_classes <- reactiveVal(NULL)
  all_subjects <- reactiveVal(NULL)
  #selected_classes_df <- data.frame(SelectedClasses = character(0))

  ############ Uploding zip file #############
  observeEvent(input$upload, {

    req(input$zipFile)

    # Get the path to the uploaded file
    zip_file <- input$zipFile$datapath

    # Create a temporary directory to extract the contents
    temp_dir_path <- tempdir()

    # Extract the zip file
    unzip(zip_file, exdir = temp_dir_path)

    # Get a list of all XLSX files in the extracted subfolders
    file_list <- list.files(temp_dir_path, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)

    file_list_df <- data.frame(
                        Path = file_list,
                        Folder_Name = sub(".*/", "", dirname(file_list)),
                        File_Name = basename(file_list)
                    )

    temp_dir_data(file_list_df)

    # Store the list of uploaded files
    uploaded_files(file_list_df[ ,2:3])

  })

  output$fileList <- renderDT({
    datatable(uploaded_files(), selection = 'single')
  })


  ############ Showing data in dialog #############
  observeEvent(input$fileList_rows_selected, {
    if (is.null(uploaded_files())) return()

    selected_row <- input$fileList_rows_selected
    if (length(selected_row) == 0) return()

    selected_folder <- uploaded_files()$Folder_Name[selected_row]
    selected_file <- uploaded_files()$File_Name[selected_row]
    selected_item_df <- temp_dir_data()
    selected_item <- selected_item_df[temp_dir_data()$Folder_Name == selected_folder & temp_dir_data()$File_Name == selected_file,]
    if (file.exists(selected_item$Path) && tolower(tools::file_ext(selected_item$Path)) %in% c("xls", "xlsx")) {
      data <- read_xlsx(selected_item$Path)
      selected_data(data)
      showModal(modalDialog(
        dataTableOutput("selectedData"),
        title = paste("Selected File Data - ", selected_file),
        size = "l",
        class = "my-modal-dialog",
        footer = modalButton("Close")
      ))
    }
  })

  output$selectedData <- renderDataTable({
    data <- selected_data()
    if (!is.null(data)) {
      datatable(data, options = list(pageLength = 8))
    }
  })


  ############ Data preparation #############
  # Create a reactive expression for checkbox options
  class_options <- reactive({
    if (!is.null(temp_dir_data())) {
      selected_item_df <- temp_dir_data()
      classes_data <- subset(selected_item_df, grepl("evfolyam", Folder_Name, ignore.case = TRUE))
      classes <- unique(substring(classes_data$File_Name,1,5))
      all_classes(classes)
      if (!is.null(classes)) {
        checkboxGroupInput("selected_classes", label = NULL, choices = classes)
      }
    }
  })

  # Render the dynamic checkboxes
  output$class_checkboxes <- renderUI({
    class_options()
  })

  # Select All button logic
  observeEvent(input$class_select_all, {
    classes <- all_classes()
    updateCheckboxGroupInput(session, "selected_classes", choices = classes, selected = classes)
  })

  # Deselect All button logic
  observeEvent(input$class_deselect_all, {
    classes <- all_classes()
    updateCheckboxGroupInput(session, "selected_classes", choices = classes, selected = NULL)
  })

  # Create a reactive expression for checkbox options
  subject_options <- reactive({
    if (!is.null(temp_dir_data())) {
      selected_item_df <- temp_dir_data()
      subject_data <- subset(selected_item_df, !grepl("evfolyam", Folder_Name, ignore.case = TRUE))
      subjects <- unique(gsub("\\.", "", sub("^(.*?)(_[0-9].*|\\.[^.]+)$", "\\1", subject_data$File_Name)))
      all_subjects(subjects)
      if (!is.null(subjects)) {
        checkboxGroupInput("selected_subjects", label = NULL, choices = subjects)
      }
    }
  })

  # Render the dynamic checkboxes
  output$subject_checkboxes <- renderUI({
    subject_options()
  })

  # Select All button logic
  observeEvent(input$subject_select_all, {
    subjects <- all_subjects()
    updateCheckboxGroupInput(session, "selected_subjects", choices = subjects, selected = subjects)
  })

  # Deselect All button logic
  observeEvent(input$subject_deselect_all, {
    subjects <- all_subjects()
    updateCheckboxGroupInput(session, "selected_subjects", choices = subjects, selected = NULL)
  })

  # Save selected options in a dataframe
  # observeEvent(input$save_selections, {
  #   selected <- isolate(input$selected)
  #   if (length(selected) > 0) {
  #     new_selections <- data.frame(Option = selected)
  #     selected_classes_df <<- rbind(selected_classes_df, new_selections)
  #   }
  # })



}

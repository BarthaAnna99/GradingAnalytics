
server <- function(input, output, session) {

  uploaded_files <- reactiveVal(NULL)
  temp_dir_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  all_classes <- reactiveVal(NULL)
  all_subjects <- reactiveVal(NULL)
  merged_data <- reactiveVal(NULL)
  selected_classes_df <- data.frame(Classes = character(0))
  selected_subjects_df <- data.frame(Subjects = character(0))

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

    file_list <- fs::path(file_list)

    file_list_df <- data.frame(
                        Path = file_list,
                        Folder_Name = fs::path(file_list) %>% dirname() %>% basename(),#sub(".*/", "", dirname(file_list)),
                        File_Name = fs::path(file_list) %>% basename()#basename(file_list)
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
        checkboxGroupInput("selected_classes", label = NULL, choices = classes, selected = classes, inline= TRUE)
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
    updateCheckboxGroupInput(session, "selected_classes", choices = classes, selected = classes, inline= TRUE)
  })

  # Deselect All button logic
  observeEvent(input$class_deselect_all, {
    classes <- all_classes()
    updateCheckboxGroupInput("selected_classes", choices = classes, selected = NULL, inline= TRUE)
  })

  # Create a reactive expression for checkbox options
  subject_options <- reactive({
    if (!is.null(temp_dir_data())) {
      selected_item_df <- temp_dir_data()
      subject_data <- subset(selected_item_df, !grepl("evfolyam", Folder_Name, ignore.case = TRUE))
      subjects <- sort(unique(gsub("\\.", "", sub("^(.*?)(_[0-9].*|\\.[^.]+)$", "\\1", subject_data$File_Name))))
      all_subjects(subjects)
      if (!is.null(subjects)) {
        checkboxGroupInput("selected_subjects", label = NULL, choices = subjects, selected = subjects, inline= TRUE)
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
    updateCheckboxGroupInput(session, "selected_subjects", choices = subjects, selected = subjects, inline= TRUE)
  })

  # Deselect All button logic
  observeEvent(input$subject_deselect_all, {
    subjects <- all_subjects()
    updateCheckboxGroupInput(session, "selected_subjects", choices = subjects, selected = NULL, inline= TRUE)
  })

  # Create a dynamic UI for selectOption elements
  label_options <- reactive({
    if (!is.null(all_subjects())) {
      subjects <- all_subjects()
      subjects <- data.frame(Subjects = subjects)
      options <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
      subject_count <- nrow(subjects)
      inputs <- lapply(1:subject_count, function(i) {
        p(subjects$Subjects[i])
      })
      do.call(tagList, inputs)
    }
  })

  # Render the dynamic textipnuts
  output$semester_label_input <- renderUI({
    label_options()
  })

  # Create a dynamic UI for selectOption elements
  semester_options <- reactive({
    if (!is.null(all_subjects())) {
      subjects <- all_subjects()
      subjects <- data.frame(Subjects = subjects)
      options <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
      subject_count <- nrow(subjects)
      inputs <- lapply(1:subject_count, function(i) {
        selectInput(inputId = paste0("subject_", i), label = subjects$Subjects[i], choices = options, width = "60px")
      })
      do.call(tagList, inputs)
    }
  })

  # Render the dynamic textipnuts
  output$semester_input <- renderUI({
    semester_options()
  })

  # Render arrow button
  output$action_button_ui <- renderUI({
    if (!is.null(uploaded_files())) {
      actionButton("arrowbutton", icon("angle-down"))
    }

  })

  observeEvent(input$arrowbutton, {
    # Save selected classes
    selected_classes <- input$selected_classes
     if (length(selected_classes) > 0) {
       new_selections <- data.frame(Classes = selected_classes)
       selected_classes_df <- rbind(selected_classes_df, new_selections)
     }

    # Save selected subjects
    selected_subjects <- input$selected_subjects
    if (length(selected_subjects) > 0) {
      new_selections <- data.frame(Subjects = selected_subjects)
      selected_subjects_df <- rbind(selected_subjects_df, new_selections)
    }

    # Load all the data into a df
    if (!is.null(temp_dir_data())) {

      file_list <- temp_dir_data()$Path

      data_frames <- lapply(file_list, function(file_path) {

        if (!grepl("evfolyam", file_path, ignore.case = TRUE)) {
          # Read the Excel file
          df <- read_xlsx(file_path)
          names(df)[1] <- "NeptunID"
          names(df)[2] <- "Nem"
          names(df)[3] <- "Jegy"
          names(df)[4] <- "Tipus"
          names(df)[5] <- "Eredmeny"

          # Add a column for the file name
          df <- df %>% mutate(Tantargy = gsub("\\.", "", sub("^(.*?)(_[0-9].*|\\.[^.]+)$", "\\1", tools::file_path_sans_ext(basename(file_path)))))

        }
      })

      merged_data <- bind_rows(data_frames)

      data_frames <- lapply(file_list, function(file_path) {

        if (grepl("evfolyam", file_path, ignore.case = TRUE)) {
          # Read the Excel file
          df <- read_xlsx(file_path)
          names(df)[1] <- "NeptunID"

          df <- df %>% select(NeptunID)

          # Add a column for the file name
          df <- df %>% mutate(Evfolyam = substring(tools::file_path_sans_ext(basename(file_path)),1,5))

        }
      })

      classes_data <- bind_rows(data_frames)

      # Filter data to selected values
      classes_data <- classes_data[classes_data$Evfolyam %in% selected_classes_df$Classes, ]
      merged_data <- merged_data[merged_data$Tantargy %in% selected_subjects_df$Subjects, ]
      result <- inner_join(classes_data, merged_data, by = "NeptunID")

      # Remove those who reapplied
      classes_data <- classes_data %>%
        group_by(NeptunID, Evfolyam) %>%
        arrange(desc(Evfolyam)) %>%
        summarize(RowNumber = row_number())

      classes_data <- classes_data %>%
        filter(RowNumber == 1) %>%
        select(NeptunID, Evfolyam)

      subjects <- result %>%
        filter(Tipus == c("Gyakorlati jegy", "Vizsgajegy")) %>%
        group_by(Tantargy, Tipus) %>%
        distinct(Tantargy, Tipus)

      result <- result %>% select(NeptunID, Nem, Jegy, Eredmeny, Tantargy, Evfolyam)

      result <- inner_join(result, subjects, by = "Tantargy")


      # Decode grade
      result <- result %>%
        mutate(
          Jegy = case_when(
            Jegy == "Jeles" ~ 5,
            Jegy == "Jó" ~ 4,
            Jegy == "Közepes" ~ 3,
            Jegy == "Elégséges" ~ 2,
            Jegy == "Elégtelen" ~ 1,
            TRUE ~ 0
          )
        )

      # Count exams
      result <- result %>%
        mutate(
          Count_Elegtelen = str_count(result$Eredmeny, "Elégtelen"),
          Count_Elégseges = str_count(result$Eredmeny, "Elégséges"),
          Count_Kozepes = str_count(result$Eredmeny, "Közepes"),
          Count_Jo = str_count(result$Eredmeny, "Jó"),
          Count_Jeles = str_count(result$Eredmeny, "Jeles"),
          Vizsgafelv_prep = Count_Elegtelen + Count_Elégseges + Count_Kozepes + Count_Jo + Count_Jeles
          )


      result_prep <- result %>%
        group_by(NeptunID, Tantargy) %>%
        arrange(desc(Evfolyam)) %>%
        summarize(Vizsgafelv = sum(Vizsgafelv_prep), Targyfelv = n(), RowNumber = row_number())

      QA_data <- inner_join(result_prep, result, by = c("NeptunID", "Tantargy"))

      QA_data <- QA_data %>%
        filter(RowNumber == 1) %>%
        select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv)

      #QA_Data <- data.frame(Name = c("Evfolyam", "NeptunID", "Nem", "Tantargy", "Tipus", "Jegy", "Targyfelv", "Vizsgafelv" ))


      output$all_data <- renderDT({
        datatable(QA_data, options = list(pageLength = 10), selection = 'none')
      })
    }

  })






}

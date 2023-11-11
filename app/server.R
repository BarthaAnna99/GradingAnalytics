
server <- function(input, output, session) {

  uploaded_files <- reactiveVal(NULL)
  temp_dir_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  all_classes <- reactiveVal(NULL)
  all_subjects <- reactiveVal(NULL)
  prep_merged_data <- reactiveVal(NULL)
  prep_classes_data <- reactiveVal(NULL)
  merged_data <- reactiveVal(NULL)
  selected_classes_df <- data.frame(Classes = character(0))
  selected_subjects_df <- data.frame(Subjects = character(0))
  GA_data <- reactiveVal(NULL)
  GA_final_data <- reactiveVal(NULL)
  selected_classes_filter_df <- reactiveVal(NULL)
  selected_subjects_filter_df <- reactiveVal(NULL)

  ########### Tabs #################
  # Import Data #################
  ## Uploading zip file ####
  observeEvent(input$upload, {

    req(input$zipFile)

    # Get the path to the uploaded file
    zip_file <- input$zipFile$datapath

    # Create a temporary directory to extract the contents
    temp_dir_path <- tempdir()
    setwd(temp_dir_path)

    # Extract the zip file
    unzip(zip_file, exdir = temp_dir_path)

    # Get a list of all XLSX files in the extracted subfolders
    file_list <- list.files(temp_dir_path, pattern = "\\.xlsx$", recursive = TRUE, full.names = TRUE)

    file_list <- fs::path(file_list)

    file_list_df <- data.frame(
                        Path = file_list,
                        Folder_Name = fs::path(file_list) %>% dirname() %>% basename(),
                        File_Name = fs::path(file_list) %>% basename()
                    )

    temp_dir_data(file_list_df)

    # Store the list of uploaded files
    uploaded_files(file_list_df[ ,2:3])



    df_end <- data.frame()
    df_class_end <- data.frame()
    # Process the extracted files
    for (file_path in file_list) {
      if (!grepl("evfolyam", file_path, ignore.case = TRUE)) {
        if (tolower(substr(file_path, nchar(file_path) - 4, nchar(file_path))) == ".xlsx") {
          df <- read_xlsx(file_path, sheet = 1)
          # Add a column for the file name
          df <- df %>% mutate(Tantargy = gsub("\\.", "", sub("^(.*?)(_[0-9].*|\\.[^.]+)$", "\\1", tools::file_path_sans_ext(basename(file_path)))))
          df <- df %>% mutate(Tantargy_long = gsub("\\.", "", tools::file_path_sans_ext(basename(file_path))))
          df_end <- rbind(df_end, df)
        }
      }
      else if (grepl("evfolyam", file_path, ignore.case = TRUE)) {
        if (tolower(substr(file_path, nchar(file_path) - 4, nchar(file_path))) == ".xlsx") {
          df_class <- read_xlsx(file_path, sheet = 1)
          df_class <- df_class[,1]
          # Add a column for the file name
          df_class <- df_class %>% mutate(Evfolyam = substring(tools::file_path_sans_ext(basename(file_path)),1,5))
          df_class <- df_class %>% mutate(Evfolyam_long = gsub("\\.", "", tools::file_path_sans_ext(basename(file_path))))
          names(df_class)[1] <- "NeptunID"
          df_class_end <- rbind(df_class_end, df_class)
        }
      }

    }

    if (nrow(df_end) > 0) {
      names(df_end)[1:5] <- c("NeptunID","Nem","Jegy","Tipus","Eredmeny")
      prep_merged_data(df_end)
      }
    if (nrow(df_class_end) > 0) {
      df_class_end <- df_class_end %>% select(NeptunID, Evfolyam, Evfolyam_long)
      prep_classes_data(df_class_end)
      }

  })

  output$fileList <- renderDT({
    datatable(uploaded_files(), selection = 'single')
  })


  ## Showing data in dialog #############
  observeEvent(input$fileList_rows_selected, {
    if (is.null(prep_classes_data()) && is.null(prep_merged_data())) return()

    selected_row <- input$fileList_rows_selected
    if (length(selected_row) == 0) return()

    selected_folder <- uploaded_files()$Folder_Name[selected_row]
    selected_file <- uploaded_files()$File_Name[selected_row]
    selected_file<- str_sub(selected_file, end = -6)

    merged_data <- prep_merged_data()
    classes_data <- prep_classes_data()


    if (grepl("Evfolyam", selected_folder, ignore.case = TRUE)) {
      data <- subset(classes_data, grepl(selected_file, classes_data$Evfolyam_long))
      data <- data[,1]
    }
    else {
      data <- subset(merged_data, grepl(selected_file, merged_data$Tantargy_long))
      data <- data[,1:5]
      }

    if (nrow(data) > 0)  {
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


  # Data preparation ################
  ## Filter options #####
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
    updateCheckboxGroupInput(session, "selected_classes", choices = classes, selected = NULL, inline= TRUE)
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


  # Render arrow button
  output$action_button_ui <- renderUI({
    if (!is.null(uploaded_files())) {
      actionButton("arrowbutton", icon("angle-down"))
    }

  })

  ## Save data #########
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
    if (!is.null(prep_merged_data()) && !is.null(prep_classes_data())) {

      classes_data <- prep_classes_data()
      merged_data <- prep_merged_data()

      # Filter data to selected values
      classes_data <- classes_data[classes_data$Evfolyam %in% selected_classes_df$Classes, ]

      # Remove those who reapplied
      NeptunID_df <- classes_data %>%
        group_by(NeptunID) %>%
        summarize(cnt = n()) %>%
        filter(cnt == 1)

      classes_data <- classes_data[classes_data$NeptunID %in% NeptunID_df$NeptunID, ]

      merged_data <- merged_data[merged_data$Tantargy %in% selected_subjects_df$Subjects, ]

      result <- inner_join(classes_data, merged_data, by = "NeptunID")

      subjects <- result %>%
        filter(Tipus == c("Gyakorlati jegy", "Vizsgajegy")) %>%
        group_by(Tantargy, Tipus) %>%
        distinct(Tantargy, Tipus)

      result <- result %>% select(NeptunID, Nem, Jegy, Eredmeny, Tantargy, Evfolyam, Tantargy_long)

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

      GA_data <- inner_join(result_prep, result, by = c("NeptunID", "Tantargy"))

      GA_data <- GA_data %>%
        filter(RowNumber == 1) %>%
        select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv, Tantargy_long)

      # Remove duplicates from GA_data
      GA_data_prep <- GA_data %>%
        group_by(NeptunID, Tantargy) %>%
        summarize(Tantargy_long = max(Tantargy_long)) %>%
        select(NeptunID, Tantargy, Tantargy_long)

      GA_data <- inner_join(GA_data, GA_data_prep, by = c("NeptunID", "Tantargy", "Tantargy_long"))

      GA_data <- GA_data %>%
        select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv)

      GA_data$Nem <- gsub("Nõ", "Nő", GA_data$Nem)

      GA_data(GA_data)

      output$all_data <- renderDT({
        datatable(GA_data, options = list(pageLength = 7), selection = 'none')
      })
    }

  })

  # Program Sructure #############
  ## Set the semesters ##########
  # Create a dynamic UI for selectOption elements
  label_options <- reactive({
    if (!is.null(GA_data())) {
      subjects <- GA_data()
      subjects <- data.frame(Subjects = unique(subjects$Tantargy))
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
    if (!is.null(GA_data())) {
      subjects <- GA_data()
      subjects <- data.frame(Subjects = unique(subjects$Tantargy))
      options <- c(1,2,3,4,5,6,7,8,9)
      subject_count <- nrow(subjects)
      inputs <- lapply(1:subject_count, function(i) {
        selectInput(inputId = subjects$Subjects[i], label = NULL, choices = options, width = "60px")
      })
      do.call(tagList, inputs)
    }
  })

  # Render the dynamic textipnuts
  output$semester_input <- renderUI({
    semester_options()
  })

  # Render arrow button
  output$action_right_button_ui <- renderUI({
    if (!is.null(GA_data())) {
      actionButton("arrowrightbutton", icon("chevron-right"))
    }
  })

  ## Show the structure ###########
  observeEvent(input$arrowrightbutton, {
    subjects <- GA_data()
    subjects <- data.frame(Subjects = unique(subjects$Tantargy))
    subject_count <- nrow(subjects)
    program_structure_df <- data.frame(Subject = character(0), Semester = numeric(0))
    for (i in 1:subject_count) {
      current_input <- subjects[i,]
      selected_value <- input[[current_input]]
      new_row <- data.frame(Subject = current_input, Semester = selected_value)
      program_structure_df <- rbind(program_structure_df, new_row)
    }

    GA_data <- GA_data()

    GA_data <- inner_join(GA_data, program_structure_df, by = c("Tantargy" = "Subject"))

    GA_final_data(GA_data)

    listagg_program_structure_df <- program_structure_df %>%
      group_by(Semester) %>%
      summarise(Subject = paste(Subject, collapse = ", ")) %>%
      ungroup() %>%
      mutate(Semester = as.numeric(Semester)) %>%
      arrange(Semester)%>%
      mutate(Semester = paste(Semester, '. semester'))

    # Define the background coloring
    app_colors <- c('#FCF1C8', '#F7D865', '#DFC8A9', '#CEAB7C', '#AEB6BE', '#99A3AD', '#D9E1F2', '#8EA9DB','#FCF1C8', '#F7D865', '#DFC8A9', '#CEAB7C', '#AEB6BE', '#99A3AD', '#D9E1F2', '#8EA9DB')

    # Create used_colors as a subset of app_colors
    used_colors <- app_colors[1:(length(listagg_program_structure_df$Semester) + 1)]

    # Render the table using DT
    output$program_structure_table <- renderDT({
      datatable(
        listagg_program_structure_df,
        options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE),
        selection = 'none',
        rownames = FALSE,
        class = "cell-border",
        colnames = rep("", ncol(listagg_program_structure_df))
      )  %>%
        formatStyle(
          'Semester',
          target = 'row',
          backgroundColor = styleInterval(
            listagg_program_structure_df$Semester, used_colors)
          ) %>%
        formatStyle(
          names(listagg_program_structure_df),
          target = 'cell',
          valueColumns = names(listagg_program_structure_df),
          lineHeight = "30px",
          textAlign = "center"
        )%>%
        formatStyle(
          'Semester',
          target = 'cell',
          width = '100px'
        )
    })
  })

  # Vizualization #########
  output$bar_chart <- renderPlot({

    if (!is.null(GA_final_data())) {
      GA_data <- GA_final_data()

      data <- GA_data %>%
        group_by(Evfolyam) %>%
        summarise(
          Number_of_Students = n_distinct(NeptunID)
        )

      # Create the bar chart
      bar_chart <- ggplot(data, aes(x = factor(Evfolyam), y = Number_of_Students)) +
        geom_bar(stat = "identity", fill = "#1A213D") +
        geom_text(aes(label = Number_of_Students, fontface = "bold"), position = position_stack(vjust = 0.9), size = 4, color = 'white') +
        labs(x = "Classes", y = "Number of Students") +
        theme_minimal() +
        theme(
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)
        )

      print(bar_chart)
    }

  })

  output$line_chart <- renderPlot({

    if (!is.null(GA_final_data())) {
      GA_data <- GA_final_data()

      NeptunID <- data.frame(NeptunID = unique(GA_data$NeptunID))
      # Calculate Total_students
      Total_students <- nrow(NeptunID)

      Reprep_data <- GA_data %>%
        group_by(Semester, Tantargy) %>%
        summarise(
          Max_students = n_distinct(NeptunID)
        )

      # Prepare the data
      Prep_data <- Reprep_data %>%
        group_by(Semester) %>%
        summarise(
          Real_student_num = n_distinct(Tantargy) * max(Max_students),
          Expected_student_num = n_distinct(Tantargy) * Total_students
        )

      # Calculate Percentage
      data <- Prep_data %>%
        mutate(Percentage = 1 - Real_student_num / Expected_student_num) %>%
        select(Semester, Percentage)

      line_chart <- ggplot(data, aes(x = Semester, y = Percentage, group = 1)) +
        geom_line(color = "#1A213D") +
        geom_point(size = 3, color = "#BF9053", fill = "white") +
        geom_text(aes(label = round(Percentage, 2)), vjust = 2) +
        theme_minimal() +
        theme(
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)
        )

      print(line_chart)
    }

  })

  output$pie_chart <- renderPlot({

    if (!is.null(GA_final_data())) {
      GA_data <- GA_final_data()

      data <- GA_data %>%
        group_by(Nem) %>%
        summarise(
          Number_of_Students = n_distinct(NeptunID)
        )

      # Create the bar chart
      pie_chart <- ggplot(data, aes(x = "", y = Number_of_Students, fill = Nem)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = Number_of_Students), position = position_stack(vjust = 0.5), size = 5, color = 'white', family = "bold") +
        theme_void() +
        scale_fill_manual(values = c("#1A213D", "#BF9053", '#FCF1C8', '#F7D865', '#DFC8A9', '#CEAB7C', '#AEB6BE', '#99A3AD')) +
        theme(axis.text.y = element_text(size = 12),
              legend.title = element_text(size=12),
              legend.text = element_text(size=12)
              )

      print(pie_chart)
    }

  })

  # Create a reactive expression for checkbox options
  class_options_filter <- reactive({
    if (!is.null(all_classes())) {
      options <- all_classes()
      if (!is.null(options)) {
        selected_classes_filter_df(options)
        checkboxGroupInput("selected_classes_filter", label = NULL, choices = options, selected = options, inline= TRUE)
      }
    }
  })

  # Render the dynamic checkboxes
  output$class_checkboxes_filter <- renderUI({
    if(!is.null(class_options_filter())) {
      class_options_filter()
    }
  })

  # Create a reactive expression for checkbox options
  subject_options_filter <- reactive({
    if (!is.null(all_subjects())) {
      options <- all_subjects()
      selected_subjects_filter_df(options[1])
      selectInput(inputId = "selected_subjects_filter", label = NULL, choices = options, selected = options[1], width = "260px")
    }
  })

  # Render the dynamic checkboxes
  output$subject_checkboxes_filter <- renderUI({
    subject_options_filter()
  })

  #Change class in filter
  observeEvent(input$selected_classes_filter, {
    selected_classes_filter_df(input$selected_classes_filter)
  })

  #Change subject in filter
  observeEvent(input$selected_subjects_filter, {
    selected_subjects_filter_df(input$selected_subjects_filter)
  })

  output$bar_chart_grades <- renderPlot({
    if (!is.null(GA_final_data()) && !is.null(selected_classes_filter_df()) && !is.null(selected_subjects_filter_df()) ) {
      GA_data <- GA_final_data()
      class <- selected_classes_filter_df()
      subject <- selected_subjects_filter_df()
      GA_data <- GA_data[GA_data$Evfolyam %in% class,]
      GA_data <- GA_data[GA_data$Tantargy %in% subject, ]

      data <- GA_data %>%
        group_by(Jegy) %>%
        summarise(
          Number_of_grades = n()
        )

      # Create the bar chart
      bar_chart_grades <- ggplot(data, aes(x = factor(Jegy), y = Number_of_grades)) +
        geom_bar(stat = "identity", fill = "#1A213D") +
        geom_text(aes(label = Number_of_grades), nudge_y = 1.5, vjust = 0, size = 4, color = '#1A213D') +
        labs(x = "Number of grades", y = "Number of students") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)
        )

      print(bar_chart_grades)
    }
  })

  output$bar_chart_exams <- renderPlot({
    if (!is.null(GA_final_data()) && !is.null(selected_classes_filter_df()) && !is.null(selected_subjects_filter_df()) ) {
      GA_data <- GA_final_data()
      class <- selected_classes_filter_df()
      subject <- selected_subjects_filter_df()
      GA_data <- GA_data[GA_data$Evfolyam %in% class,]
      GA_data <- GA_data[GA_data$Tantargy %in% subject, ]

      data <- GA_data %>%
        group_by(Vizsgafelv) %>%
        summarise(
          Number_of_exams = n()
        )

      # Create the bar chart
      bar_chart_exams <- ggplot(data, aes(x = factor(Vizsgafelv), y = Number_of_exams)) +
        geom_bar(stat = "identity", fill = "#1A213D") +
        geom_text(aes(label = Number_of_exams), nudge_y = 1.5, vjust = 0, size = 4, color = '#1A213D') +
        labs(x = "Number of exams", y = NULL) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)
        )

      print(bar_chart_exams)
    }
  })

  output$bar_chart_subjects <- renderPlot({
    if (!is.null(GA_final_data()) && !is.null(selected_classes_filter_df()) && !is.null(selected_subjects_filter_df()) ) {
      GA_data <- GA_final_data()
      class <- selected_classes_filter_df()
      subject <- selected_subjects_filter_df()
      GA_data <- GA_data[GA_data$Evfolyam %in% class,]
      GA_data <- GA_data[GA_data$Tantargy %in% subject, ]

      data <- GA_data %>%
        group_by(Targyfelv) %>%
        summarise(
          Number_of_course_register = n()
        )

      # Create the bar chart
      bar_chart_subjects <- ggplot(data, aes(x = factor(Targyfelv), y = Number_of_course_register)) +
        geom_bar(stat = "identity", fill = "#1A213D") +
        geom_text(aes(label = Number_of_course_register), nudge_y = 1.5, vjust = 0, size = 4, color = '#1A213D') +
        labs(x = "Number of course registry", y = NULL) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)
        )

      print(bar_chart_subjects)
    }
  })

}

# server.R

# Set the maximum request size in megabytes
options(shiny.maxRequestSize = 30*1024^2)  # 30 MB

# Shiny server logic
server <- function(input, output, session) {

  uploaded_files <- reactiveVal(NULL)
  extension <- reactiveVal(NULL)
  temp_dir_data <- reactiveVal(NULL)
  selected_data <- reactiveVal(NULL)
  all_programs <- reactiveVal(NULL)
  all_classes <- reactiveVal(NULL)
  all_subjects <- reactiveVal(NULL)
  prep_merged_data <- reactiveVal(NULL)
  prep_classes_data <- reactiveVal(NULL)
  merged_data <- reactiveVal(NULL)
  selected_programs_df <- data.frame(Programs = character(0))
  selected_classes_df <- data.frame(Classes = character(0))
  selected_subjects_df <- data.frame(Subjects = character(0))
  GA_data <- reactiveVal(NULL)
  GA_final_data <- reactiveVal(NULL)
  GA_metrics_data <- reactiveVal(NULL)
  selected_classes_filter_df <- reactiveVal(NULL)
  selected_subjects_filter_df <- reactiveVal(NULL)

  # Import Data #################
  ## Uploading zip file ####
  observeEvent(input$upload, {

    req(input$zipFile)

    # Different extensions have to be handled differently
    file <- input$zipFile

    if (!is.null(file)) {
      # Get the file extension
      file_ext <- tools::file_ext(file$name)
      extension(file_ext)

      # Check if it's an Excel file
      if (file_ext %in% c("xls", "xlsx")) {
        # Handle Excel file
        df <- read_xlsx(file$datapath, sheet = 1)
        if (nrow(df) > 0) {
          names(df)[1:11] <- c("Programkod", "Program", "NeptunID", "Nem", "Evfolyam", "Tantargy", "Targykod","Jegy","Tipus","Datum", "Felev")
          df <- df %>%
            mutate(Program = paste(Program, Programkod, sep = ' - ')) %>%
            dplyr::select(Program, NeptunID, Nem, Evfolyam, Tantargy, Jegy, Tipus, Datum, Felev)
          uploaded_files(df)
        }
      }

      # Check if it's a zip file
      else if (file_ext %in% c("zip")) {
        # Handle zip file
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
          #file_path <- paste("~", file_path)

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
              df_class <- df_class %>% mutate(Evfolyam = gsub("-", "_", substring(tools::file_path_sans_ext(basename(file_path)),1,5)))
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
          prep_classes_data(df_class_end)
        }
      }
    }


  })

  output$fileList <- renderDT({
    if(!is.null(extension()) && extension() %in% c("zip")) {
      datatable(uploaded_files(), selection = 'single')
    }
    else {
      datatable(uploaded_files(), options = list(pageLength = 7), selection = 'none')
    }

  })


  ## Showing data in dialog #############
  # Only when a zip is uploaded (selection is disabled otherwise)
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
        title = paste("Selected File - ", selected_file),
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
  ### Program #####
  # Create a reactive expression for checkbox options
  programs_options <- reactive({
    if(!is.null(extension()) && extension() %in% c("zip")) {
      if (!is.null(temp_dir_data())) {
        programs <- c("Default")
        all_programs(programs)
        if (!is.null(programs)) {
          selectInput("selected_programs", label = NULL, choices = programs, selected = programs[1])
        }
      }
    }
    else {
      if (!is.null(uploaded_files())) {
        df <- uploaded_files()
        programs <- unique(df$Program)
        programs <- programs[order(programs)]
        all_programs(programs)
        if (!is.null(programs)) {
          selectInput("selected_programs", label = NULL, choices = programs, selected = programs[1])
        }
      }
    }

  })

  # Render the dynamic checkboxes
  output$program_checkboxes <- renderUI({
    programs_options()
  })

  # Render program_arrow button
  output$program_action_button_ui <- renderUI({
    if (!is.null(uploaded_files())) {
      actionButton("program_arrowbutton", icon("angle-down"))
    }
  })

  observeEvent(input$program_arrowbutton, {

    output$select_classes <- renderUI({
      h3("Select Classes")
    })
    output$class_select_all_ui <- renderUI({
      actionButton("class_select_all", "Select All")
    })
    output$class_deselect_all_ui <- renderUI({
      actionButton("class_deselect_all", "Deselect All")
    })

    selected_programs <- input$selected_programs
    if (length(selected_programs) > 0) {
      new_selections <- data.frame(Programs = selected_programs)
      selected_programs_df <- rbind(selected_programs_df, new_selections)
    }

    ### Class #####
    # Create a reactive expression for checkbox options
    class_options <- reactive({
      if(!is.null(extension()) && extension() %in% c("zip")) {
        if (!is.null(temp_dir_data())) {
          selected_item_df <- temp_dir_data()
          classes_data <- subset(selected_item_df, grepl("evfolyam", Folder_Name, ignore.case = TRUE))
          classes <- unique(gsub("-", "_", substring(classes_data$File_Name,1,5)))
        }
      }
      else {
        df <- uploaded_files()
        df <- df[df$Program %in% selected_programs,]
        classes <- unique(df$Evfolyam)
        classes <- classes[order(classes)]
      }
      all_classes(classes)
      if (!is.null(classes)) {
        checkboxGroupInput("selected_classes", label = NULL, choices = classes, selected = classes, inline= TRUE)
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

    # Render subject_action_button_ui button
    output$subject_action_button_ui <- renderUI({
      if (!is.null(uploaded_files())) {
        actionButton("subject_arrowbutton", icon("angle-down"))
      }
    })

    output$select_subjects <- renderUI({
      h3("Select Subjects")
    })
    output$subject_select_all_ui <- renderUI({
      actionButton("subject_select_all", "Select All")
    })
    output$subject_deselect_all_ui <- renderUI({
      actionButton("subject_deselect_all", "Deselect All")
    })

    selected_programs <- input$selected_programs
    if (length(selected_programs) > 0) {
      new_selections <- data.frame(Programs = selected_programs)
      selected_programs_df <- rbind(selected_programs_df, new_selections)
    }

    ### Subject #####
    # Create a reactive expression for checkbox options
    subject_options <- reactive({
      if(!is.null(extension()) && extension() %in% c("zip")) {
        if (!is.null(temp_dir_data())) {
          selected_item_df <- temp_dir_data()
          subject_data <- subset(selected_item_df, !grepl("evfolyam", Folder_Name, ignore.case = TRUE))
          subjects <- sort(unique(gsub("\\.", "", sub("^(.*?)(_[0-9].*|\\.[^.]+)$", "\\1", subject_data$File_Name))))
        }
      }
      else {
        df <- uploaded_files()
        df <- df[df$Program %in% selected_programs,]
        selected_classes <- input$selected_classes
        if (length(selected_classes) > 0) {
          df <- df[df$Evfolyam %in% selected_classes,]
        }

        df <- df %>%
          distinct(Tantargy, NeptunID) %>%
          group_by(Tantargy) %>%
          summarize(Nr = n()) %>%
          mutate(TantargyNr = paste(Tantargy ,  ' (', Nr, ')'))

        subjects <- unique(df$TantargyNr)
        subjects <- subjects[order(subjects)]
      }
      all_subjects(subjects)
      if (!is.null(subjects)) {
        checkboxGroupInput("selected_subjects", label = NULL, choices = subjects, selected = subjects)
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

    # Render button
    output$preparation_button_ui <- renderUI({
      actionButton("preparationbutton", "Process data")
    })

  })

  ## Save data #########
  observeEvent(input$preparationbutton, {
    # Save selected classes
    selected_classes <- input$selected_classes
     if (length(selected_classes) > 0) {
       new_selections <- data.frame(Classes = selected_classes)
       selected_classes_df <- rbind(selected_classes_df, new_selections)
     }

    # Save selected subjects
    selected_subjects <- input$selected_subjects
    selected_subjects <- str_replace(selected_subjects, '\\s*\\([^)]+\\)', '')

    if (length(selected_subjects) > 0) {
      new_selections <- data.frame(Subjects = selected_subjects)
      selected_subjects_df <- rbind(selected_subjects_df, new_selections)
    }

    ### For zip #######
    if(!is.null(extension()) && extension() %in% c("zip")) {
      if (!is.null(prep_merged_data()) && !is.null(prep_classes_data())) {

        classes_data <- prep_classes_data()
        merged_data <- prep_merged_data()
      }
    }
    ## For excel #######
    else {
      selected_programs <- input$selected_programs

      merged_data <- uploaded_files() %>%
        mutate(Datum = as.POSIXct(Datum, format="%Y-%m-%d %H:%M:%S"))

      merged_data <- merged_data[merged_data$Program %in% selected_programs, ]

      classes_data <- merged_data %>%
        distinct(Evfolyam, NeptunID)

    }


    # Filter data to selected values
    classes_data <- classes_data[classes_data$Evfolyam %in% selected_classes_df$Classes, ]

    # Remove those who reapplied
    NeptunID_df <- classes_data %>%
      group_by(NeptunID) %>%
      summarize(cnt = n()) %>%
      filter(cnt == 1)

    classes_data <- classes_data[classes_data$NeptunID %in% NeptunID_df$NeptunID, ]

    merged_data <- merged_data[merged_data$Tantargy %in% selected_subjects_df$Subjects, ]

    if(!is.null(extension()) && !(extension() %in% c("zip"))) {

      temp_result <- merged_data %>%
        group_by(NeptunID, Nem, Tantargy) %>%
        summarise(
          MaxDatum = max(Datum),
        ) %>%
        dplyr::select(NeptunID, Nem, Tantargy, MaxDatum)

      final_grade <- merged_data %>%
        inner_join(temp_result, by = c("NeptunID", "Nem", "Tantargy", "Datum" = "MaxDatum")) %>%
        dplyr::select(NeptunID, Nem, Tantargy, Jegy)

      merged_data <- merged_data %>%
        group_by(NeptunID, Nem, Tantargy, Felev) %>%
        summarise(
          Tipus = max(Tipus),
          Eredmeny = paste(Jegy, collapse = ';')
        ) %>%
        dplyr::select(NeptunID, Nem, Tantargy, Tipus, Eredmeny)

      merged_data <- merged_data %>%
        left_join(final_grade, by = c("NeptunID", "Nem", "Tantargy")) %>%
        dplyr::select(NeptunID, Nem, Tantargy, Tipus, Jegy, Eredmeny)

    }

    result <- inner_join(classes_data, merged_data, by = "NeptunID")

    subjects <- result %>%
      filter(Tipus == c("Gyakorlati jegy", "Vizsgajegy")) %>%
      group_by(Tantargy) %>%
      summarize(Tipus = max(Tipus)) %>%
      dplyr::select(Tantargy, Tipus)


    # For zip
    if(!is.null(extension()) && extension() %in% c("zip")) {
      result <- result %>% dplyr::select(NeptunID, Nem, Jegy, Eredmeny, Tantargy, Evfolyam, Tantargy_long)
    }
    # For excel
    else {
      result <- result %>% dplyr::select(NeptunID, Nem, Jegy, Eredmeny, Tantargy, Evfolyam)
    }


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

    # For zip
    if(!is.null(extension()) && extension() %in% c("zip")) {
      GA_data <- GA_data %>%
        filter(RowNumber == 1) %>%
        dplyr::select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv, Tantargy_long)

      # Remove duplicates from GA_data
      GA_data_prep <- GA_data %>%
        group_by(NeptunID, Tantargy) %>%
        summarize(Tantargy_long = max(Tantargy_long)) %>%
        dplyr::select(NeptunID, Tantargy, Tantargy_long)

      GA_data <- inner_join(GA_data, GA_data_prep, by = c("NeptunID", "Tantargy", "Tantargy_long"))
    }
    # For excel
    else {
      GA_data <- GA_data %>%
        filter(RowNumber == 1) %>%
        distinct(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv)  %>%
        dplyr::select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv)
    }

    GA_data <- GA_data %>%
      dplyr::select(Evfolyam, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv)

    GA_data$Nem <- gsub("Nõ", "Nő", GA_data$Nem)

    GA_data(GA_data)

    output$all_data <- renderDT({
      datatable(GA_data, options = list(pageLength = 15),
                selection = 'none',
                rownames = FALSE,
                class = "cell-border")
    })

  })

  # Program Sructure #############
  ### Set the semesters ##########
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

  ### Show the structure ###########
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
  ### Barchart ##############
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

  ### Line chart ###############
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
        dplyr::select(Semester, Percentage)

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

  ### Pie chart #############
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

  ### Filter options ###########
  # Create a reactive expression for checkbox options
  class_options_filter <- reactive({
    if (!is.null(GA_final_data())) {
      options <- unique(GA_final_data()$Evfolyam)
      options <- options[order(options)]
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
    if (!is.null(GA_final_data())) {
      options <- unique(GA_final_data()$Tantargy)
      options <- options[order(options)]
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

  ### Subject bar charts ###############
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

  # Metrics ###############
  ### Porcess button ###########

  # Render button
  output$metrics_button_ui <- renderUI({
    if (!is.null(GA_final_data())) {
      actionButton("metricsbutton", "Calculate the metrics")
    }
  })

  ### Calculate metrics ###########
  observeEvent(input$metricsbutton, {
    GA_final_data <- GA_final_data()

    GA_final_data <- GA_final_data %>%
      mutate(
        Kitevo_Targyfelv = case_when(
          Jegy == 5 ~ 0.7,
          Jegy == 4 ~ 0.6,
          Jegy == 3 ~ 0.5,
          Jegy == 2 ~ 0.5,
          Jegy == 1 ~ 0.6,
          Jegy == 0 ~ 0.7,
          TRUE ~ 0
        ),
        Prep_Kitevo_Vizsgafelv = case_when(
          Jegy == 5 ~ 0.5,
          Jegy == 4 ~ 0.4,
          Jegy == 3 ~ 0.3,
          Jegy == 2 ~ 0.3,
          Jegy == 1 ~ 0.4,
          Jegy == 0 ~ 0.5,
          TRUE ~ 0
        ),
        Kitevo_Vizsgafelv = case_when(
          Tipus == 'Gyakorlati jegy' ~ Prep_Kitevo_Vizsgafelv * 1.3,
          TRUE ~ Prep_Kitevo_Vizsgafelv
        ),
        Metrics = round(Jegy + 1 - (1.5 * Targyfelv^Kitevo_Targyfelv + 0.5 * Vizsgafelv^Kitevo_Vizsgafelv) / 2, 4)
      ) %>%
      ungroup() %>%
      dplyr::select(Evfolyam, Semester, NeptunID, Nem, Tantargy, Tipus, Jegy, Vizsgafelv, Targyfelv, Metrics)

    GA_metrics_data(GA_final_data)

    Metrics_description_data <- GA_final_data %>%
      group_by(Tantargy) %>%
      summarize(Minimum = round(min(Metrics),2),
                Maximum = round(max(Metrics), 2),
                Median = round(median(Metrics),2),
                Mode = round(Mode(Metrics), 2)
                #Mean = round(mean(Metrics), 2),
                #Deviation = round(sd(Metrics), 2)
                ) %>%
      dplyr::select(Tantargy, Minimum, Maximum, Median, Mode)
      #dplyr::select(Tantargy, Minimum, Maximum, Median, Mean, Deviation)

    output$metrics_analysis <- renderDT({
      if (nrow(Metrics_description_data) > 0) {
        datatable(
          Metrics_description_data,
          options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE,
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'text-align': 'center', 'color': '#BF9053'});",
                           "}")),
          selection = 'none',
          rownames = FALSE,
          class = "cell-border"
        )  %>%
          formatStyle(
            names(Metrics_description_data),
            textAlign = "center"
          ) %>%
          formatStyle(
            "Minimum", "Maximum", "Median", "Mode",
            #"Minimum", "Maximum", "Median", "Mean", "Deviation",
            target = "cell",
            lineHeight = "20px",
            textAlign = "center"
          ) %>%
          formatStyle(
            "Tantargy",
            target = "cell",
            lineHeight = "20px",
            textAlign = "left"
          )
      }
    })

  })

  # Dataset ##########

  output$GA_dataset <- renderDT({
    if(!is.null(GA_metrics_data())) {
      datatable(GA_metrics_data(),
                options = list(pageLength = 10),
                selection = 'none',
                extensions = 'Buttons'
      )
    }

  })

  # Modeling ##############

  # Render button
  output$modelingbutton_ui <- renderUI({
    if (!is.null(GA_metrics_data())) {
      actionButton("modelingbutton", "Calculate")
    }
  })


  ### Calculate betas ###########
  observeEvent(input$modelingbutton, {
    tryCatch({
      GA_metrics_data <- GA_metrics_data()

      # Define a function to perform the replacements
      replace_characters <- function(text) {
        text <- gsub("ö", "o", text)
        text <- gsub("ü", "u", text)
        text <- gsub("ú", "u", text)
        text <- gsub("ó", "o", text)
        text <- gsub("ő", "o", text)
        text <- gsub("ú", "u", text)
        text <- gsub("é", "e", text)
        text <- gsub("á", "a", text)
        text <- gsub("ű", "u", text)
        text <- gsub("í", "i", text)
        text <- gsub("Ö", "O", text)
        text <- gsub("Ü", "U", text)
        text <- gsub("Ú", "U", text)
        text <- gsub("Ü", "U", text)
        text <- gsub("Ó", "O", text)
        text <- gsub("Ő", "O", text)
        text <- gsub("Ú", "U", text)
        text <- gsub("É", "E", text)
        text <- gsub("Á", "A", text)
        text <- gsub("Ű", "U", text)
        text <- gsub("Í", "I", text)
        text <- gsub(",", "", text)
        text <- gsub("\\)", "", text)
        text <- gsub("\\(", "", text)
        text <- gsub("-", "_", text)
        text <- gsub(" ", "_", text)

        return(text)
      }

      # Apply the function to the Tantargy column
      GA_metrics_data$Tantargy <- sapply(GA_metrics_data$Tantargy, replace_characters)

      AllEvfolyam <- c(unique(GA_metrics_data$Evfolyam))
      AllEvfolyam[1] <- ""

      GA_metrics_data <- GA_metrics_data %>%
        mutate(Nem = case_when(Nem == "Nő" ~ 0,
                               TRUE ~ 1)) %>%
        mutate(Evfolyam = factor(Evfolyam, labels = AllEvfolyam)) %>%
        mutate(Semester = as.numeric(Semester))

      GA_metrics_data <- GA_metrics_data %>%
        pivot_wider(names_from = Evfolyam,
                    values_from = Evfolyam,
                    values_fill = 0,
                    names_prefix = "Evfolyam_",
                    values_fn = function(x) as.integer(!is.na(x)))%>%
        dplyr::select(-Evfolyam_)

      Subject_semester <- GA_metrics_data %>%
        dplyr::select(Semester, Tantargy) %>%
        distinct() %>%
        arrange(Semester, Tantargy)

      AllTantargy <- c('X.Intercept.', 'Nem', paste("Evfolyam_", AllEvfolyam[-1]), Subject_semester$Tantargy)

      # Create an empty data frame with column names
      LAD_result <- data.frame(matrix(nrow = 0, ncol = length(AllTantargy) + length(AllEvfolyam[-1])))
      colnames(LAD_result) <- AllTantargy
      column_names_row <- as.data.frame(t(colnames(LAD_result)))
      column_names_row[1,1] <- 'Beta'
      colnames(column_names_row) <- AllTantargy

      LAD_result <- rbind(LAD_result, column_names_row)
      rownames(LAD_result)[1] <- " "

      Subject_semester <- Subject_semester %>%
        filter(Semester > 1)

      independent_variables <- ""

        for (i in 1:nrow(Subject_semester)) {
          outcome <- as.character(Subject_semester[i, 2])
          semester_filter <- as.numeric(Subject_semester[i, 1])

          GA_metrics_data_n <- GA_metrics_data %>%
            filter(Semester < semester_filter | Tantargy %in% outcome) %>%
            dplyr::select(NeptunID, Nem, starts_with("Evfolyam"), Tantargy, Metrics)

          # Unpivot the DataFrame
          unpivoted_GA_metrics_data <- GA_metrics_data_n %>%
            pivot_wider(
              id_cols = c(NeptunID, Nem, starts_with("Evfolyam")),
              names_from = Tantargy,
              values_from = Metrics
            ) %>%
            dplyr::select(-NeptunID)

          unpivoted_GA_metrics_data <- unpivoted_GA_metrics_data[complete.cases(unpivoted_GA_metrics_data), ]

          feltetel <- TRUE
          szamlalo <- 1

          while(feltetel) {
            lad_regression <- lad(paste(outcome ,"~.", sep=""), data = unpivoted_GA_metrics_data)
            lad_sum <- summary(lad_regression)
            aicA <- AIC(lad_regression)
            cat(paste("\n\n", "Step:", szamlalo, "\n", sep=" "))
            print(lad_sum)
            cat(paste("AIC: ", aicA,"\n", sep=" "))

            lad_coeff <- data.frame(lad_sum[["coefficients"]])
            lad_coeff_m <- lad_coeff[-1,]

            szelektalt <- gsub("`", "", row.names(lad_coeff_m[lad_coeff_m$p.value == max(lad_coeff_m$p.value), ]))
            cat(paste("Szelektált változó: ", szelektalt, "\n", sep=" "))

            adatok <- unpivoted_GA_metrics_data[, !names(unpivoted_GA_metrics_data) %in% szelektalt, drop = FALSE]
            lad_regression2 <- lad(paste(outcome ,"~.", sep=""), data = adatok)
            lad_sum2 <- summary(lad_regression2)
            aicB <- AIC(lad_regression2)

            if (ncol(adatok) <= 1) {
              feltetel <- FALSE
              cat("No more variables")
              GA_metrics_data <- GA_metrics_data %>%
                filter(!(Tantargy %in% szelektalt))
              independent_variables <- paste(independent_variables, outcome, "; ")
            }
            else if (any(is.nan(lad_coeff_m$p.value)) || any(is.infinite(lad_coeff_m$p.value))) {
              stop("Error: Numerical instability in LAD regression.")
            }
            else if (aicA >= aicB || max(lad_coeff_m$p.value)  > 0.15) {
              unpivoted_GA_metrics_data <- adatok
              szamlalo <- szamlalo + 1
            }
            else {
              feltetel <- FALSE
              cat(paste("AIC szelektálás után: ", aicB,"\n", sep=" "))
              lad_coeff <- lad_coeff %>%
                mutate(P_starts = case_when(
                  p.value <= 0.0001 ~ "****",
                  p.value <= 0.001 ~ "***",
                  p.value <= 0.01 ~ "**",
                  p.value <= 0.05 ~ "*",
                  TRUE ~ ""
                )) %>%
                mutate(Estimate = paste(round(Estimate, 3), P_starts, sep = "", collapse=NULL))

              lad_coeff <- data.frame(lad_coeff$Estimate, row.names = row.names(lad_coeff))
              lad_coeff <- data.frame(t(lad_coeff))
              rownames(lad_coeff) <- outcome

              # Get the column names from LAD_result
              result_columns <- colnames(LAD_result)

              # Create a new data frame with columns from LAD_result
              new_lad_coeff <- data.frame(matrix(NA, nrow = nrow(lad_coeff), ncol = length(result_columns)))
              colnames(new_lad_coeff) <- result_columns

              # Assign values from lad_coeff to the matching columns
              new_lad_coeff[, intersect(colnames(new_lad_coeff), colnames(lad_coeff))] <- lad_coeff[, intersect(colnames(new_lad_coeff), colnames(lad_coeff))]

              # Bind the modified lad_coeff to LAD_result
              LAD_result <- rbind(LAD_result, new_lad_coeff)

              rownum <- nrow(LAD_result)
              rownames(LAD_result)[rownum] <- outcome

            }
          }

        }

        ### Show the results ############

        # Check if the value in the first row and last column is empty
        if (is.na(LAD_result[1, ncol(LAD_result)])) {
          # Remove the last column
          LAD_result <- LAD_result[, -ncol(LAD_result)]
        }

        LAD_result <- LAD_result %>%
          rownames_to_column(var = "RowNames")
        LAD_result <- LAD_result %>%
          mutate_all(~substr(., 1, 20)) %>%
          mutate_all(~str_replace_all(., "_", "_ "))

        output$LAD_dataset <- renderDT({
          if(nrow(LAD_result) > 0) {
            datatable(
              LAD_result,
              extensions = "FixedColumns",
              options = list(
                rowCallback = JS(
                  "function(row, data, index) {",
                  "var cells = $('td', row);",
                  "cells.each(function() {",
                  "  if ($(this).text() === '') {",
                  "    $(this).css('background-color', '#F5F5F5');",
                  "  }",
                  "});",
                  "}"
                ),
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE,
                info = FALSE,
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 1)
              ),
              selection = 'none',
              rownames = FALSE,
              class = "cell-border",
              colnames = rep("", ncol(LAD_result))
            ) %>%
              formatStyle(
                columns = 1,
                valueColumns = 1,
                target = "cell",
                color = "white",
                backgroundColor = "#BF9053",
                fontWeight = "bold"
              ) %>%
              formatStyle(
                columns = 1,
                target = "row",
                color = styleEqual(c(" "), c('white')),
                backgroundColor = styleEqual(c(" "), c('#BF9053')),
                fontWeight = styleEqual(c(" "), c('bold'))
              )
          }

        })

        output$independent_output <- renderText({
          paste("The following subjects are independent variables, so they were eliminated from the model: ", independent_variables)
        })

      }
      , error = function(e) {
        # Handle errors or exceptions here
        output$result_output <- renderText({
          paste("LAD regression cannot be calculated. Error:", e$message, " Output variable: ", outcome)
        })
        output$alias_output <- renderPrint({
          tryCatch({
            output$result_output_alias <- renderText({
              paste("Aliased values", e$message)
            })
            lmmd <- lm(paste(outcome, "~ ."), data = unpivoted_GA_metrics_data)
            alias_result <- alias(lmmd)
            alias_result

          }, error = function(e) {
            # Handle errors or exceptions here
            output$result_output_alias <- renderText({
              paste("Aliased or linearly dependent coefficients cannot be calculated. Error:", e$message)
            })
          })
        })
        output$vif_output <- renderPrint({
          tryCatch({
            output$result_output_vif <- renderText({
              paste("VIF values", e$message)
            })
            vif_values <- vif(lm(paste(outcome, "~ ."), data = unpivoted_GA_metrics_data))
            vif_values
          }, error = function(e) {
            # Handle errors or exceptions here
            output$result_output_vif <- renderText({
              paste("VIF for each predictor cannot be calculated. Error:", e$message)
            })
          })
        })
      })
    })


}

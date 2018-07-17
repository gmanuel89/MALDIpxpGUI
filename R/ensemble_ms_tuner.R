ensemble_ms_tuner <- function() {
  #################### ENSEMBLE MS TUNER ####################
  # All the functions in the ensemble_ms_tuner() function are run within the ensemble_ms_tuner() function's environment, but they are called from the global environment since they were previously assigned with the <<- in the functions_mass_spectrometry() function.
  # Each value that must go to the global environment is assigned with the <<- so that it can be called from any function of the ensemble_ms_tuner() function.
  # In the debugging phase, run the whole code block within the {}, like as if the script was directly sourced from the file.

  ### Program version (Specified by the program writer!!!!)
  R_script_version <- "2017.12.21.1"
  ### Force update (in case something goes wrong after an update, when checking for updates and reading the variable force_update, the script can automatically download the latest working version, even if the rest of the script is corrupted, because it is the first thing that reads)
  force_update <- FALSE
  ### GitHub URL where the R file is
  github_R_url <-
    "https://raw.githubusercontent.com/gmanuel89/Ensemble-MS-Tuner/master/ENSEMBLE%20MS%20TUNER.R"
  ### GitHub URL of the program's WIKI
  github_wiki_url <-
    "https://github.com/gmanuel89/Ensemble-MS-Tuner/wiki"
  ### Name of the file when downloaded
  script_file_name <- "ENSEMBLE MS TUNER"
  # Change log
  change_log <-
    "1. Bugfix\n2. Choose manually which features to retain"






  ############## INSTALL AND LOAD THE REQUIRED PACKAGES
  install_and_load_required_packages(
    c(
      "tcltk",
      "parallel",
      "caret",
      "stats",
      "pROC",
      "nnet",
      "e1071",
      "kernlab",
      "MASS",
      "klaR",
      "pls",
      "randomForest",
      "lda",
      "SparseM",
      "stringi",
      "doParallel"
    ),
    repository = NULL,
    update_packages = FALSE,
    print_messages = TRUE
  )






  ###################################### Initialize the variables (default values)
  check_for_updates_value <- ""
  filepath_import <- NULL
  peaklist <- NULL
  output_folder <- getwd()
  file_type_export_matrix <- "csv"
  allow_parallelization <- FALSE
  model_tuning <- "after"
  selection_metric <- "Accuracy"
  automatically_select_features <- FALSE
  generate_plots <- TRUE
  preprocessing = c("center", "scale")
  feature_reranking <- FALSE
  try_combination_of_parameters <- FALSE
  class_list <- NULL
  outcome_list <- NULL




  ################## Values of the variables (for displaying and dumping purposes)
  filepath_import_value <- NULL
  output_folder_value <- output_folder
  allow_parallelization_value <- "NO"
  model_tuning_value <-
    paste("YES\n( ", model_tuning, " )", sep = "")
  automatically_select_features_value <- "NO"
  generate_plots_value <- "YES"
  preprocessing_value <- "center + scale"
  feature_reranking_value <- "NO"
  try_combination_of_parameters_value <- "NO"
  outcome_list_value <- "OUTCOME LIST\nNOT SET"






  ##################################################### DEFINE WHAT THE BUTTONS DO

  ##### Check for updates (from my GitHub page) (it just updates the label telling the user if there are updates) (it updates the check for updates value that is called by the label). The function will read also if an update should be forced.
  check_for_updates_function <- function() {
    ### Initialize the version number
    online_version_number <- NULL
    ### Initialize the force update
    online_force_update <- FALSE
    ### Initialize the variable that says if there are updates
    update_available <- FALSE
    ### Initialize the change log
    online_change_log <- "Bug fixes"
    # Check if there is internet connection by pinging a website
    there_is_internet <-
      check_internet_connection(method = "getURL", website_to_ping = "www.google.it")
    # Check for updates only in case of working internet connection
    if (there_is_internet == TRUE) {
      try({
        ### Read the file from the web (first 10 lines)
        online_file <- readLines(con = github_R_url)
        ### Retrieve the version number
        for (l in online_file) {
          if (length(grep("R_script_version <-", l, fixed = TRUE)) > 0) {
            # Isolate the "variable" value
            online_version_number <-
              unlist(strsplit(l, "R_script_version <- ", fixed = TRUE))[2]
            # Remove the quotes
            online_version_number <-
              unlist(strsplit(online_version_number, "\""))[2]
            break
          }
        }
        ### Retrieve the force update
        for (l in online_file) {
          if (length(grep("force_update <-", l, fixed = TRUE)) > 0) {
            # Isolate the "variable" value
            online_force_update <-
              as.logical(unlist(strsplit(l, "force_update <- ", fixed = TRUE))[2])
            break
          }
          if (is.null(online_force_update)) {
            online_force_update <- FALSE
          }
        }
        ### Retrieve the change log
        for (l in online_file) {
          if (length(grep("change_log <-", l, fixed = TRUE)) > 0) {
            # Isolate the "variable" value
            online_change_log <-
              unlist(strsplit(l, "change_log <- ", fixed = TRUE))[2]
            # Remove the quotes
            online_change_log_split <-
              unlist(strsplit(online_change_log, "\""))[2]
            # Split at the \n
            online_change_log_split <-
              unlist(strsplit(online_change_log_split, "\\\\n"))
            # Put it back to the character
            online_change_log <- ""
            for (o in online_change_log_split) {
              online_change_log <- paste(online_change_log, o, sep = "\n")
            }
            break
          }
        }
        ### Split the version number in YYYY.MM.DD
        online_version_YYYYMMDDVV <-
          unlist(strsplit(online_version_number, ".", fixed = TRUE))
        ### Compare with the local version
        local_version_YYYYMMDDVV = unlist(strsplit(R_script_version, ".", fixed = TRUE))
        ### Check the versions (from the Year to the Day)
        # Check the year
        if (as.numeric(local_version_YYYYMMDDVV[1]) < as.numeric(online_version_YYYYMMDDVV[1])) {
          update_available <- TRUE
        }
        # If the year is the same (update is FALSE), check the month
        if (update_available == FALSE) {
          if ((
            as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[2]) < as.numeric(online_version_YYYYMMDDVV[2])
          )) {
            update_available <- TRUE
          }
        }
        # If the month and the year are the same (update is FALSE), check the day
        if (update_available == FALSE) {
          if ((
            as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[2]) == as.numeric(online_version_YYYYMMDDVV[2])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[3]) < as.numeric(online_version_YYYYMMDDVV[3])
          )) {
            update_available <- TRUE
          }
        }
        # If the day and the month and the year are the same (update is FALSE), check the daily version
        if (update_available == FALSE) {
          if ((
            as.numeric(local_version_YYYYMMDDVV[1]) == as.numeric(online_version_YYYYMMDDVV[1])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[2]) == as.numeric(online_version_YYYYMMDDVV[2])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[3]) == as.numeric(online_version_YYYYMMDDVV[3])
          ) &&
          (
            as.numeric(local_version_YYYYMMDDVV[4]) < as.numeric(online_version_YYYYMMDDVV[4])
          )) {
            update_available <- TRUE
          }
        }
        ### Return messages
        if (is.null(online_version_number)) {
          # The version number could not be ckecked due to internet problems
          # Update the label
          check_for_updates_value <-
            paste(
              "Version: ",
              R_script_version,
              "\nUpdates not checked:\nconnection problems",
              sep = ""
            )
        } else {
          if (update_available == TRUE) {
            # Update the label
            check_for_updates_value <-
              paste(
                "Version: ",
                R_script_version,
                "\nUpdate available:\n",
                online_version_number,
                sep = ""
              )
          } else {
            # Update the label
            check_for_updates_value <-
              paste("Version: ",
                    R_script_version,
                    "\nNo updates available",
                    sep = "")
          }
        }
      }, silent = TRUE)
    }
    ### Something went wrong: library not installed, retrieving failed, errors in parsing the version number
    if (is.null(online_version_number)) {
      # Update the label
      check_for_updates_value <-
        paste(
          "Version: ",
          R_script_version,
          "\nUpdates not checked:\nconnection problems",
          sep = ""
        )
    }
    # Escape the function
    update_available <<- update_available
    online_change_log <<- online_change_log
    check_for_updates_value <<- check_for_updates_value
    online_version_number <<- online_version_number
    online_force_update <<- online_force_update
  }

  ##### Download the updated file (from my GitHub page)
  download_updates_function <- function() {
    # Download updates only if there are updates available
    if (update_available == TRUE || online_force_update == TRUE) {
      # Changelog
      tkmessageBox(
        title = "Changelog",
        message = paste0(
          "The updated script contains the following changes:\n",
          online_change_log
        ),
        icon = "info"
      )
      # Initialize the variable which says if the file has been downloaded successfully
      file_downloaded <- FALSE
      # Choose where to save the updated script
      tkmessageBox(title = "Download folder",
                   message = "Select where to save the updated script file",
                   icon = "info")
      download_folder <- tclvalue(tkchooseDirectory())
      # Download the file only if a download folder is specified, otherwise don't
      if (download_folder != "") {
        # Go to the working directory
        setwd(download_folder)
        tkmessageBox(
          message = paste0(
            "The updated script file will be downloaded in:\n\n",
            download_folder
          )
        )
        # Download the file
        try({
          download.file(
            url = github_R_url,
            destfile = paste0(script_file_name, ".R"),
            method = "auto"
          )
          file_downloaded <- TRUE
        }, silent = TRUE)
        if (file_downloaded == TRUE) {
          tkmessageBox(
            title = "Updated file downloaded!",
            message = paste0(
              "The updated script, named:\n\n",
              paste0(script_file_name, ".R"),
              "\n\nhas been downloaded to:\n\n",
              download_folder,
              "\n\nThe current window will now close and the new updated script will be loaded!"
            ),
            icon = "info"
          )
          # Destroy the window
          try(tkdestroy(window), silent = TRUE)
          # Relaunch the script
          try(source(paste0(script_file_name, ".R")), silent = TRUE)
        } else {
          tkmessageBox(
            title = "Connection problem",
            message = paste(
              "The updated script file could not be downloaded due to internet connection problems!\n\nManually download the updated script file at:\n\n",
              github_R_url,
              sep = ""
            ),
            icon = "warning"
          )
        }
      } else {
        # No download folder specified!
        tkmessageBox(message = "The updated script file will not be downloaded!")
      }
    } else {
      tkmessageBox(title = "No update available",
                   message = "NO UPDATES AVAILABLE!\n\nThe latest version is running!",
                   icon = "info")
    }
    # Raise the focus on the main window (if there is)
    try(tkraise(window), silent = TRUE)
  }

  ### Downloading forced updates
  check_for_updates_function()
  if (online_force_update == TRUE) {
    download_updates_function()
  }

  ### Force check for updates
  force_check_for_updates_function <- function() {
    # Check for updates
    check_for_updates_function()
    # Display a message
    if (update_available == TRUE) {
      # Message
      tkmessageBox(
        title = "Update available",
        message = paste0(
          "Update available!\n",
          online_version_number,
          "\n\nPress the 'DOWNLOAD UPDATE...' button to retrieve the updated script!"
        ),
        icon = "info"
      )
    } else {
      # Message
      tkmessageBox(title = "No update available",
                   message = "No update available!",
                   icon = "info")
    }
  }

  # Outcome list function
  outcome_list_function <- function() {
    # Run only if a class list has been read from the peaklist file...
    if (!is.null(class_list) && !is.null(peaklist)) {
      # Fix the values of the class list for the displaying label (one under the other)
      class_list_values <- paste(class_list, collapse = "\n")
      ##### Function for the button in the GUI
      submit_outcome_list_function <- function() {
        # Take the input and split it at the comma
        outcome_list_input <-
          as.character(tclvalue(outcome_list_input))
        outcome_list <-
          unlist(strsplit(outcome_list_input, ",", fixed = TRUE))
        # Remove the blank spaces around the text
        for (ou in 1:length(outcome_list)) {
          if (base::startsWith(outcome_list[ou], " ")) {
            outcome_list[ou] <- unlist(strsplit(outcome_list[ou], ""))[2]
          } else if (base::endsWith(outcome_list[ou], " ")) {
            outcome_list[ou] <- unlist(strsplit(outcome_list[ou], ""))[1]
          }
        }
        # Fix the outcome names to a universal name (benign, malignant, other)
        for (ou in 1:length(outcome_list)) {
          if (length(grep("ben", outcome_list[ou])) > 0 ||
              outcome_list[ou] == "b" || outcome_list[ou] == "B") {
            outcome_list[ou] <- "benign"
          } else if (length(grep("mal", outcome_list[ou])) > 0 ||
                     outcome_list[ou] == "m" || outcome_list[ou] == "M") {
            outcome_list[ou] <- "malignant"
          } else {
            outcome_list[ou] <- "other"
          }
        }
        # Set the displaying value
        outcome_list_value <- "OUTCOME LIST\nSET"
        outcome_list_value_label <-
          tklabel(
            window,
            text = outcome_list_value,
            font = label_font,
            bg = "white",
            width = 30
          )
        tkgrid(
          outcome_list_value_label,
          row = 7,
          column = 4,
          padx = c(10, 10),
          pady = c(10, 10)
        )
        # Escape the function
        outcome_list <<- outcome_list
        outcome_list_value <<- outcome_list_value
        # Destroy the window upon committing
        tkdestroy(outcome_list_window)
        # Raise the focus on the main window
        tkraise(window)
      }
      ##### List of variables to get from the GUI
      outcome_list_input <- tclVar("")
      ##### Window
      outcome_list_window <- tktoplevel(bg = "white")
      tkwm.resizable(outcome_list_window, FALSE, FALSE)
      tktitle(outcome_list_window) <- "Set outcome list"
      #tkpack.propagate(outcome_list_window, FALSE)
      # GUI elements
      class_list_label <-
        tklabel(
          outcome_list_window,
          text = "Class list",
          font = label_font,
          bg = "white",
          width = 20
        )
      class_list_values_label <-
        tklabel(
          outcome_list_window,
          text = class_list_values,
          font = label_font,
          bg = "white",
          width = 30
        )
      outcome_list_label <-
        tklabel(
          outcome_list_window,
          text = "Outcome list\n(separated by comma)\n(b = benign ; m = malignant ; o = other)",
          font = label_font,
          bg = "white",
          width = 40,
          justify = "center"
        )
      outcome_list_entry <-
        tkentry(
          outcome_list_window,
          width = 20,
          textvariable = outcome_list_input,
          font = entry_font,
          bg = "white",
          width = 30,
          justify = "center"
        )
      submit_outcome_list_button <-
        tkbutton(
          outcome_list_window,
          text = "Submit outcome list",
          command = submit_outcome_list_function,
          font = button_font,
          bg = "white",
          width = 20
        )
      tkgrid(
        class_list_label,
        row = 1,
        column = 1,
        padx = c(10, 10),
        pady = c(10, 10)
      )
      tkgrid(
        class_list_values_label,
        row = 2,
        column = 1,
        padx = c(10, 10),
        pady = c(10, 10)
      )
      tkgrid(
        outcome_list_label,
        row = 1,
        column = 2,
        padx = c(10, 10),
        pady = c(10, 10)
      )
      tkgrid(
        outcome_list_entry,
        row = 2,
        column = 2,
        padx = c(10, 10),
        pady = c(10, 10)
      )
      tkgrid(
        submit_outcome_list_button,
        row = 3,
        column = 1,
        columnspan = 2,
        padx = c(10, 10),
        pady = c(10, 10)
      )
    } else {
      tkmessageBox(title = "Class list not found",
                   message = "The peaklist file has to be imported first, in order to determine the class list to be matched to a list of outcomes!",
                   icon = "warning")
    }
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### File type export MATRIX
  file_type_export_matrix_choice <- function() {
    # Catch the value from the menu
    file_type_export_matrix <-
      select.list(
        c("csv", "xlsx", "xls"),
        title = "Choose",
        multiple = FALSE,
        preselect = "csv"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (file_type_export_matrix == "") {
      file_type_export_matrix <- "csv"
    }
    if (file_type_export_matrix == "xls" ||
        file_type_export_matrix == "xlsx") {
      # Try to install the XLConnect (it will fail if Java is not installed)
      Java_is_installed <- FALSE
      try({
        install_and_load_required_packages("XLConnect")
        Java_is_installed <- TRUE
      }, silent = TRUE)
      # If it didn't install successfully, set to CSV
      if (Java_is_installed == FALSE) {
        tkmessageBox(title = "Java not installed",
                     message = "Java is not installed, therefore the package XLConnect cannot be installed and loaded.\nThe output format is switched back to CSV",
                     icon = "warning")
        file_type_export_matrix <- "csv"
      }
    }
    # Escape the function
    file_type_export_matrix <<- file_type_export_matrix
    # Set the value of the displaying label
    file_type_export_matrix_value_label <-
      tklabel(
        window,
        text = file_type_export_matrix,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(file_type_export_matrix_value_label,
           row = 5,
           column = 5)
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Preprocessing parameters
  preprocessing_choice <- function() {
    # Prompt the selection only if the combination of parameters is not selected, otherwise there is no point in selecting it...
    if (isTRUE(try_combination_of_parameters)) {
      preprocessing <- c("center", "scale")
      preprocessing_value <- "center + scale"
    } else {
      # Catch the value from the menu
      preprocessing <-
        select.list(
          c("center", "scale"),
          title = "Preprocessing",
          multiple = TRUE,
          preselect = NULL
        )
      # Raise the focus on the main window
      tkraise(window)
      # Default
      if (length(preprocessing) == 1 && preprocessing == "") {
        preprocessing <- NULL
        preprocessing_value <- "NO"
      } else {
        if ("center" %in% preprocessing && "scale" %in% preprocessing) {
          preprocessing_value <- "center + scale"
        } else {
          preprocessing_value <- preprocessing
        }
      }
      # Set the value of the displaying label
      preprocessing_value_label <-
        tklabel(
          window,
          text = preprocessing_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 2
        )
      tkgrid(preprocessing_value_label,
             row = 4,
             column = 4)
    }
    # Escape the function
    preprocessing <<- preprocessing
    preprocessing_value <<- preprocessing_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Peaklist file
  file_import_function <- function() {
    filepath_import_select <-
      tkmessageBox(title = "Input file",
                   message = "Select the peaklist file containing all the mass spectrometric data",
                   icon = "info")
    filepath_import <-
      tclvalue(tkgetOpenFile(filetypes = "{{Comma Separated Value files} {.csv}}"))
    if (!nchar(filepath_import)) {
      tkmessageBox(message = "No file selected")
    } else {
      tkmessageBox(message = paste(
        "The following file will be read:\n\n",
        filepath_import,
        sep = ""
      ))
    }
    if (filepath_import != "") {
      #####IMPORT THE DATA FROM THE FILE
      ### Put all the import block under the try() statement, so that if there are blocking errors (such as no files), the spectra variable remains NULL.
      try({
        peaklist <- read.csv(filepath_import, header = TRUE, sep = ",")
        ## Rownames (can lead the RFE to crash!)
        try({
          rownames(peaklist) <-
            make.names(peaklist$Sample, unique = TRUE)
        }, silent = TRUE)
        ## Data type
        try({
          peaklist$Sample <- as.character(peaklist$Sample)
        }, silent = TRUE)
        try({
          peaklist$Class <-
            make.names(as.character(peaklist$Class), unique = FALSE)
          peaklist$Class <- as.factor(peaklist$Class)
        }, silent = TRUE)
        ##### Separate the mass spectrometric data from the demographic data
        # All features
        feature_vector <- colnames(peaklist)
        # Non signals (columns)
        tkmessageBox(title = "Demographical data",
                     message = "Select the demographical data (to be separated from the mass spectrometric data)",
                     icon = "info")
        non_features <-
          select.list(
            feature_vector,
            title = "Demographical features",
            multiple = TRUE,
            preselect = c("Sample", "Class")
          )
        ##### Determine the features of interest
        # Discriminant column
        tkmessageBox(title = "Discriminant feature",
                     message = "Select the discriminant feature",
                     icon = "info")
        discriminant_attribute <-
          select.list(c(non_features, "NONE"),
                      title = "Discriminant attribute",
                      preselect = "Class")
        if (discriminant_attribute == "NONE") {
          discriminant_attribute <- NULL
        }
        ## Class list
        if (discriminant_attribute == "NONE" ||
            discriminant_attribute == "") {
          class_list <- discriminant_attribute
        } else {
          # Fix the class names when there are messy characters
          peaklist[, discriminant_attribute] <-
            make.names(as.character(peaklist[, discriminant_attribute]))
          class_list <-
            levels(as.factor(peaklist[, discriminant_attribute]))
        }
        ##### Force peaks
        # Peaks to retain
        tkmessageBox(title = "Features to retain",
                     message = "Select the features to retain",
                     icon = "info")
        features_to_retain <-
          select.list(
            c(feature_vector[!(feature_vector %in% non_features)], "ALL"),
            title = "Features to retain",
            multiple = TRUE,
            preselect = "ALL"
          )
        if ("ALL" %in% features_to_retain) {
          features_to_retain <-
            feature_vector[!(feature_vector %in% non_features)]
        }
        # Shrink the peaklist
        peaklist <- peaklist[, c(features_to_retain, non_features)]
        #### Retrieve the input file name
        input_filename <- NULL
        try({
          if (Sys.info()[1] == "Linux" || Sys.info()[1] == "Darwin") {
            input_filename <- unlist(strsplit(filepath_import, "/"))
            input_filename <- input_filename[length(input_filename)]
            input_filename <-
              unlist(strsplit(input_filename, ".", fixed = TRUE))[1]
          } else if (Sys.info()[1] == "Windows") {
            input_filename <- unlist(strsplit(filepath_import, "\\\\"))
            input_filename <- input_filename[length(input_filename)]
            input_filename <-
              unlist(strsplit(input_filename, ".", fixed = TRUE))[1]
          }
        }, silent = TRUE)
        # Escape the function
        filepath_import <<- filepath_import
        peaklist <<- peaklist
        input_filename <<- input_filename
        feature_vector <<- feature_vector
        non_features <<- non_features
        discriminant_attribute <<- discriminant_attribute
        class_list <<- class_list
        tkmessageBox(title = "File imported",
                     message = "The data has been successfully imported from the file!",
                     icon = "info")
        tkmessageBox(title = "Set outcome list",
                     message = "Remember to set the outcome list to associate the outcomes with green/red pixels!",
                     icon = "info")
        # Initialize the outcome_list variable (in case it is not the first import of the session)
        outcome_list <<- NULL
        outcome_list_value <<- "OUTCOME LIST\nNOT SET"
        outcome_list_value_label <-
          tklabel(
            window,
            text = outcome_list_value,
            font = label_font,
            bg = "white",
            width = 30
          )
        tkgrid(
          outcome_list_value_label,
          row = 7,
          column = 4,
          padx = c(10, 10),
          pady = c(10, 10)
        )
      }, silent = TRUE)
    } else {
      peaklist <- NULL
      # Escape the function
      filepath_import <<- filepath_import
      peaklist <<- peaklist
      tkmessageBox(title = "No input file selected",
                   message = "No input file has been selected!!!\nPlease, select a file to be imported",
                   icon = "warning")
    }
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Test set definition
  test_set_definition_function <- function() {
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Output
  browse_output_function <- function() {
    output_folder <- tclvalue(tkchooseDirectory())
    if (!nchar(output_folder)) {
      # Get the output folder from the default working directory
      output_folder <- getwd()
    }
    tkmessageBox(message = paste("Every file will be saved in:\n\n", output_folder))
    setwd(output_folder)
    # Exit the function and put the variable into the R workspace
    output_folder <<- output_folder
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Exit
  end_session_function <- function () {
    q(save = "no")
  }

  ##### Model tuning
  model_tuning_choice <- function() {
    # Catch the value from the menu
    model_tuning <-
      select.list(
        c("None", "after", "embedded"),
        title = "Model tuning",
        multiple = FALSE,
        preselect = "after"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (model_tuning == "") {
      model_tuning <- "None"
    }
    # Fix the values
    if (model_tuning == "None") {
      model_tuning <- "none"
      model_tuning_value <- "NO"
    } else {
      model_tuning_value <- paste("YES\n( ", model_tuning, " )", sep = "")
    }
    model_tuning_value_label <-
      tklabel(
        window,
        text = model_tuning_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 2
      )
    tkgrid(model_tuning_value_label,
           row = 2,
           column = 4)
    # Escape the function
    model_tuning <<- model_tuning
    model_tuning_value <<- model_tuning_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Automatically select features
  automatically_select_features_choice <- function() {
    # Catch the value from the menu
    automatically_select_features <-
      select.list(
        c("YES", "NO"),
        title = "Automatically select features",
        multiple = FALSE,
        preselect = "NO"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (automatically_select_features == "YES") {
      automatically_select_features <- TRUE
    }
    if (automatically_select_features == "NO" ||
        automatically_select_features == "") {
      automatically_select_features <- FALSE
    }
    # Set the value of the displaying label
    if (automatically_select_features == TRUE) {
      automatically_select_features_value <- "YES"
    } else {
      automatically_select_features_value <- "NO"
    }
    automatically_select_features_value_label <-
      tklabel(
        window,
        text = automatically_select_features_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(automatically_select_features_value_label,
           row = 3,
           column = 2)
    # Escape the function
    automatically_select_features <<- automatically_select_features
    automatically_select_features_value <<-
      automatically_select_features_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Generate plots
  generate_plots_choice <- function() {
    # Catch the value from the menu
    generate_plots <-
      select.list(
        c("YES", "NO"),
        title = "Generate plots",
        multiple = FALSE,
        preselect = "NO"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (generate_plots == "YES") {
      generate_plots <- TRUE
    }
    if (generate_plots == "NO" || generate_plots == "") {
      generate_plots <- FALSE
    }
    # Set the value of the displaying label
    if (generate_plots == TRUE) {
      generate_plots_value <- "YES"
    } else {
      generate_plots_value <- "NO"
    }
    generate_plots_value_label <-
      tklabel(
        window,
        text = generate_plots_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(generate_plots_value_label,
           row = 2,
           column = 4)
    # Escape the function
    generate_plots <<- generate_plots
    generate_plots_value <<- generate_plots_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Try combination of parameters
  try_combination_of_parameters_choice <- function() {
    # Catch the value from the menu
    try_combination_of_parameters <-
      select.list(
        c("YES", "NO"),
        title = "Try combination of parameters",
        multiple = FALSE,
        preselect = "YES"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (try_combination_of_parameters == "YES") {
      try_combination_of_parameters <- TRUE
      preprocessing <- c("center", "scale")
      feature_reranking <- FALSE
    }
    if (try_combination_of_parameters == "NO" ||
        try_combination_of_parameters == "") {
      try_combination_of_parameters <- FALSE
      preprocessing <- c("center", "scale")
      feature_reranking <- FALSE
    }
    # Set the value of the displaying label
    if (try_combination_of_parameters == TRUE) {
      try_combination_of_parameters_value <- "YES"
      preprocessing_value <- "center + scale\n(combinations)"
      feature_reranking_value <- "YES/NO\n(combinations)"
    } else {
      try_combination_of_parameters_value <- "NO"
      preprocessing_value <- "center + scale"
      feature_reranking_value <- "NO"
    }
    # Update the values of all the displaying labels
    try_combination_of_parameters_value_label <-
      tklabel(
        window,
        text = try_combination_of_parameters_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(try_combination_of_parameters_value_label,
           row = 4,
           column = 2)
    preprocessing_value_label <-
      tklabel(
        window,
        text = preprocessing_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 2
      )
    tkgrid(preprocessing_value_label,
           row = 4,
           column = 4)
    feature_reranking_value_label <-
      tklabel(
        window,
        text = feature_reranking_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 2
      )
    tkgrid(feature_reranking_value_label,
           row = 4,
           column = 6)
    # Escape the function
    try_combination_of_parameters <<- try_combination_of_parameters
    try_combination_of_parameters_value <<-
      try_combination_of_parameters_value
    preprocessing <<- preprocessing
    preprocessing_value <<- preprocessing_value
    feature_reranking <<- feature_reranking
    feature_reranking_value <<- feature_reranking_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Feature reranking
  feature_reranking_choice <- function() {
    # Prompt the selection only if the combination of parameters is not selected, otherwise there is no point in selecting it...
    if (isTRUE(try_combination_of_parameters)) {
      feature_reranking <- FALSE
      feature_reranking_value <- "NO"
    } else {
      # Catch the value from the menu
      feature_reranking <-
        select.list(
          c("YES", "NO"),
          title = "Feature reranking",
          multiple = FALSE,
          preselect = "YES"
        )
      # Raise the focus on the main window
      tkraise(window)
      # Default
      if (feature_reranking == "YES") {
        feature_reranking <- TRUE
      }
      if (feature_reranking == "NO" || feature_reranking == "") {
        feature_reranking <- FALSE
      }
      # Set the value of the displaying label
      if (feature_reranking == TRUE) {
        feature_reranking_value <- "YES"
      } else {
        feature_reranking_value <- "NO"
      }
      feature_reranking_value_label <-
        tklabel(
          window,
          text = feature_reranking_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 2
        )
      tkgrid(feature_reranking_value_label,
             row = 4,
             column = 6)
    }
    # Escape the function
    feature_reranking <<- feature_reranking
    feature_reranking_value <<- feature_reranking_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Selection metric
  selection_metric_choice <- function() {
    # Catch the value from the menu
    selection_metric <-
      select.list(
        c("Accuracy", "Kappa"),
        title = "Selection metric",
        multiple = FALSE,
        preselect = "Accuracy"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (selection_metric == "") {
      selection_metric <- "Accuracy"
    }
    selection_metric_value_label <-
      tklabel(
        window,
        text = selection_metric,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(selection_metric_value_label,
           row = 2,
           column = 6)
    # Escape the function
    selection_metric <<- selection_metric
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Multicore processing
  allow_parallelization_choice <- function() {
    ##### Messagebox
    tkmessageBox(title = "Parallel processing is resource hungry",
                 message = "Parallel processing is resource hungry.\nBy activating it, the computation becomes faster, but the program will eat a lot of RAM, possibly causing your computer to freeze. If you want to play safe, do not enable it",
                 icon = "warning")
    # Catch the value from the menu
    allow_parallelization <-
      select.list(
        c("YES", "NO"),
        title = "Parallelization",
        multiple = FALSE,
        preselect = "NO"
      )
    # Default
    if (allow_parallelization == "YES") {
      if (Sys.info()[1] == "Windows") {
        allow_parallelization <- "foreach"
      } else {
        allow_parallelization <- "lapply"
      }
    }
    if (allow_parallelization == "NO" ||
        allow_parallelization == "") {
      allow_parallelization <- FALSE
    }
    # Set the value of the displaying label
    if (allow_parallelization == "foreach" ||
        allow_parallelization == "lapply") {
      allow_parallelization_value <- "YES"
    } else {
      allow_parallelization_value <- "  NO  "
    }
    allow_parallelization_value_label <-
      tklabel(
        window,
        text = allow_parallelization_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(allow_parallelization_value_label,
           row = 5,
           column = 3)
    # Escape the function
    allow_parallelization <<- allow_parallelization
    allow_parallelization_value <<- allow_parallelization_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Run the Ensemble MS Tuner function
  run_ensemble_ms_tuner_function <- function() {
    ######## Run only if all the elements needed are there
    if (!is.null(filepath_import) && !is.null(outcome_list)) {
      ##### Get the values from the entries
      filename_export <- as.character(tclvalue(filename_export))
      features_to_select <- as.integer(tclvalue(features_to_select))
      cv_repeats_control <- as.integer(tclvalue(cv_repeats_control))
      k_fold_cv_control <- as.integer(tclvalue(k_fold_cv_control))
      ##### Run the feature selection function
      model_ensemble_tuner <-
        model_ensemble_embedded_fs(
          training_set = peaklist,
          features_to_select = features_to_select,
          model_tuning = model_tuning,
          selection_metric = selection_metric,
          discriminant_attribute = discriminant_attribute,
          non_features = non_features,
          seed = 12345,
          automatically_select_features = automatically_select_features,
          generate_plots = generate_plots,
          cv_repeats_control = cv_repeats_control,
          k_fold_cv_control = k_fold_cv_control,
          preprocessing = preprocessing,
          allow_parallelization = allow_parallelization,
          feature_reranking = feature_reranking,
          try_combination_of_parameters = try_combination_of_parameters,
          outcome_list = outcome_list,
          progress_bar = "tcltk",
          test_set = NULL
        )
      ### Extract the variables
      model_list <- model_ensemble_tuner$model_list
      feature_list <- model_ensemble_tuner$feature_list
      model_performance_matrix <-
        model_ensemble_tuner$model_performance_matrix
      common_features_matrix <-
        model_ensemble_tuner$common_features_matrix
      model_performance_parameter_list <-
        model_ensemble_tuner$model_performance_parameter_list
      ##### DUMP THE FILES
      # Create a folder with the filename export name
      dumping_subfolder <- file.path(output_folder, filename_export)
      dir.create(dumping_subfolder)
      setwd(dumping_subfolder)
      # Dump the RData file
      save(
        "model_list",
        "feature_list",
        "model_performance_matrix",
        "common_features_matrix",
        "model_performance_parameter_list",
        file = paste0(filename_export, ".RData")
      )
      # Dump the model performance matrix file
      if (file_type_export_matrix == "csv") {
        write.csv(
          model_performance_matrix,
          file = paste0(
            "Model performance matrix",
            ".",
            file_type_export_matrix
          ),
          row.names = FALSE
        )
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        writeWorksheetToFile(
          file = paste0(
            "Model performance matrix",
            ".",
            file_type_export_matrix
          ),
          data = model_performance_matrix,
          clearSheets = TRUE,
          sheet = "Model performances"
        )
      }
      # Dump the model common features matrix file
      if (file_type_export_matrix == "csv") {
        write.csv(
          common_features_matrix,
          file = paste0("Common features matrix", ".", file_type_export_matrix),
          row.names = FALSE
        )
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        writeWorksheetToFile(
          file = paste0("Common features matrix", ".", file_type_export_matrix),
          data = common_features_matrix,
          clearSheets = TRUE,
          sheet = "Common features"
        )
      }
      # Dump the model features matrix file
      if (file_type_export_matrix == "csv") {
        write.csv(
          feature_list$model_features_matrix,
          file = paste0("Model features matrix", ".", file_type_export_matrix),
          row.names = FALSE
        )
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        writeWorksheetToFile(
          file = paste0("Model features matrix", ".", file_type_export_matrix),
          data = feature_list$model_features_matrix,
          clearSheets = TRUE,
          sheet = "Model features"
        )
      }
      # Dump the list of major features matrix file
      if (file_type_export_matrix == "csv") {
        write.csv(
          as.matrix(cbind(feature_list$model_features)),
          file = paste0("Model features", ".", file_type_export_matrix),
          row.names = FALSE
        )
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        writeWorksheetToFile(
          file = paste0("Model features", ".", file_type_export_matrix),
          data = feature_list$model_features,
          clearSheets = TRUE,
          sheet = "Model features"
        )
      }
      # Dump the list of models matrix file
      model_list_matrix <- as.matrix(cbind(names(model_list)))
      colnames(model_list_matrix) <- "Models"
      if (file_type_export_matrix == "csv") {
        write.csv(
          model_list_matrix,
          file = paste0("Model list", ".", file_type_export_matrix),
          row.names = FALSE
        )
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        writeWorksheetToFile(
          file = paste0("Model list", ".", file_type_export_matrix),
          data = model_list_matrix,
          clearSheets = TRUE,
          sheet = "Model list"
        )
      }
      ### Messagebox
      tkmessageBox(
        title = "Done!",
        message = paste0(
          "The feature selection has been performed and the model ensemble has been generated and tuned!\n\nThe RData file, named '",
          paste0(filename_export, ".RData"),
          "' has been dumped!"
        ),
        icon = "info"
      )
    } else {
      model_ensemble_tuner <- NULL
      # Escape the function
      model_ensemble_tuner <<- model_ensemble_tuner
      ### Messagebox
      tkmessageBox(title = "Something is wrong",
                   message = "No peaklist file has provided or no outcomes have been specified!",
                   icon = "warning")
    }
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Show info function
  show_info_function <- function() {
    if (Sys.info()[1] == "Linux") {
      system(command = paste("xdg-open", github_wiki_url),
             intern = FALSE)
    } else if (Sys.info()[1] == "Darwin") {
      system(command = paste("open", github_wiki_url),
             intern = FALSE)
    } else if (Sys.info()[1] == "Windows") {
      system(command = paste("cmd /c start", github_wiki_url),
             intern = FALSE)
    }
  }












  ##################################################################### WINDOW GUI



  ########## List of variables, whose values are taken from the entries in the GUI
  features_to_select <- tclVar("")
  cv_repeats_control <- tclVar("")
  k_fold_cv_control <- tclVar("")
  filename_export <- tclVar("")




  ######################## GUI

  ### Get system info (Platform - Release - Version (- Linux Distro))
  system_os = Sys.info()[1]
  os_release = Sys.info()[2]
  os_version = Sys.info()[3]

  ### Get the screen resolution
  try({
    # Windows
    if (system_os == "Windows") {
      # Get system info
      screen_info <-
        system("wmic path Win32_VideoController get VideoModeDescription",
               intern = TRUE)[2]
      # Get the resolution
      screen_resolution <- unlist(strsplit(screen_info, "x"))
      # Retrieve the values
      screen_height <- as.numeric(screen_resolution[2])
      screen_width <- as.numeric(screen_resolution[1])
    } else if (system_os == "Linux") {
      # Get system info
      screen_info <- system("xdpyinfo -display :0", intern = TRUE)
      # Get the resolution
      screen_resolution <-
        screen_info[which(screen_info == "screen #0:") + 1]
      screen_resolution <-
        unlist(strsplit(screen_resolution, "dimensions: ")[1])
      screen_resolution <-
        unlist(strsplit(screen_resolution, "pixels"))[2]
      # Retrieve the wto dimensions...
      screen_width <-
        as.numeric(unlist(strsplit(screen_resolution, "x"))[1])
      screen_height <-
        as.numeric(unlist(strsplit(screen_resolution, "x"))[2])
    }
  }, silent = TRUE)



  ### FONTS
  # Default sizes (determined on a 1680x1050 screen) (in order to make them adjust to the size screen, the screen resolution should be retrieved)
  title_font_size_default <- 18
  other_font_size_default <- 9
  title_font_size <- title_font_size_default
  other_font_size <- other_font_size_default

  # Adjust fonts size according to the pixel number
  try({
    # Windows
    if (system_os == "Windows") {
      # Determine the font size according to the resolution
      total_number_of_pixels <- screen_width * screen_height
      # Determine the scaling factor (according to a complex formula)
      scaling_factor_title_font <-
        as.numeric((0.03611 * total_number_of_pixels) + 9803.1254)
      scaling_factor_other_font <-
        as.numeric((0.07757 * total_number_of_pixels) + 23529.8386)
      title_font_size <-
        as.integer(round(total_number_of_pixels / scaling_factor_title_font))
      other_font_size <-
        as.integer(round(total_number_of_pixels / scaling_factor_other_font))
    } else if (system_os == "Linux") {
      # Linux
      # Determine the font size according to the resolution
      total_number_of_pixels <- screen_width * screen_height
      # Determine the scaling factor (according to a complex formula)
      scaling_factor_title_font <-
        as.numeric((0.03611 * total_number_of_pixels) + 9803.1254)
      scaling_factor_other_font <-
        as.numeric((0.07757 * total_number_of_pixels) + 23529.8386)
      title_font_size <-
        as.integer(round(total_number_of_pixels / scaling_factor_title_font))
      other_font_size <-
        as.integer(round(total_number_of_pixels / scaling_factor_other_font))
    } else if (system_os == "Darwin") {
      # macOS
      print("Using default font sizes...")
    }
    # Go back to defaults if there are NAs
    if (is.na(title_font_size)) {
      title_font_size <- title_font_size_default
    }
    if (is.na(other_font_size)) {
      other_font_size <- other_font_size_default
    }
  }, silent = TRUE)

  # Define the fonts
  # Windows
  if (system_os == "Windows") {
    garamond_title_bold = tkfont.create(family = "Garamond",
                                        size = title_font_size,
                                        weight = "bold")
    garamond_other_normal = tkfont.create(family = "Garamond",
                                          size = other_font_size,
                                          weight = "normal")
    arial_title_bold = tkfont.create(family = "Arial",
                                     size = title_font_size,
                                     weight = "bold")
    arial_other_normal = tkfont.create(family = "Arial",
                                       size = other_font_size,
                                       weight = "normal")
    trebuchet_title_bold = tkfont.create(family = "Trebuchet MS",
                                         size = title_font_size,
                                         weight = "bold")
    trebuchet_other_normal = tkfont.create(family = "Trebuchet MS",
                                           size = other_font_size,
                                           weight = "normal")
    trebuchet_other_bold = tkfont.create(family = "Trebuchet MS",
                                         size = other_font_size,
                                         weight = "bold")
    calibri_title_bold = tkfont.create(family = "Calibri",
                                       size = title_font_size,
                                       weight = "bold")
    calibri_other_normal = tkfont.create(family = "Calibri",
                                         size = other_font_size,
                                         weight = "normal")
    calibri_other_bold = tkfont.create(family = "Calibri",
                                       size = other_font_size,
                                       weight = "bold")
    # Use them in the GUI
    title_font = calibri_title_bold
    label_font = calibri_other_normal
    entry_font = calibri_other_normal
    button_font = calibri_other_bold
  } else if (system_os == "Linux") {
    #Linux
    # Ubuntu
    if (length(grep("Ubuntu", os_version, ignore.case = TRUE)) > 0) {
      # Define the fonts
      ubuntu_title_bold = tkfont.create(
        family = "Ubuntu",
        size = (title_font_size + 2),
        weight = "bold"
      )
      ubuntu_other_normal = tkfont.create(
        family = "Ubuntu",
        size = (other_font_size),
        weight = "normal"
      )
      ubuntu_other_bold = tkfont.create(
        family = "Ubuntu",
        size = (other_font_size),
        weight = "bold"
      )
      liberation_title_bold = tkfont.create(family = "Liberation Sans",
                                            size = title_font_size,
                                            weight = "bold")
      liberation_other_normal = tkfont.create(family = "Liberation Sans",
                                              size = other_font_size,
                                              weight = "normal")
      liberation_other_bold = tkfont.create(family = "Liberation Sans",
                                            size = other_font_size,
                                            weight = "bold")
      bitstream_charter_title_bold = tkfont.create(family = "Bitstream Charter",
                                                   size = title_font_size,
                                                   weight = "bold")
      bitstream_charter_other_normal = tkfont.create(family = "Bitstream Charter",
                                                     size = other_font_size,
                                                     weight = "normal")
      bitstream_charter_other_bold = tkfont.create(family = "Bitstream Charter",
                                                   size = other_font_size,
                                                   weight = "bold")
      # Use them in the GUI
      title_font = ubuntu_title_bold
      label_font = ubuntu_other_normal
      entry_font = ubuntu_other_normal
      button_font = ubuntu_other_bold
    } else if (length(grep("Fedora", os_version, ignore.case = TRUE)) > 0) {
      # Fedora
      cantarell_title_bold = tkfont.create(family = "Cantarell",
                                           size = title_font_size,
                                           weight = "bold")
      cantarell_other_normal = tkfont.create(family = "Cantarell",
                                             size = other_font_size,
                                             weight = "normal")
      cantarell_other_bold = tkfont.create(family = "Cantarell",
                                           size = other_font_size,
                                           weight = "bold")
      liberation_title_bold = tkfont.create(family = "Liberation Sans",
                                            size = title_font_size,
                                            weight = "bold")
      liberation_other_normal = tkfont.create(family = "Liberation Sans",
                                              size = other_font_size,
                                              weight = "normal")
      liberation_other_bold = tkfont.create(family = "Liberation Sans",
                                            size = other_font_size,
                                            weight = "bold")
      # Use them in the GUI
      title_font = cantarell_title_bold
      label_font = cantarell_other_normal
      entry_font = cantarell_other_normal
      button_font = cantarell_other_bold
    } else {
      # Other linux distros
      liberation_title_bold = tkfont.create(family = "Liberation Sans",
                                            size = title_font_size,
                                            weight = "bold")
      liberation_other_normal = tkfont.create(family = "Liberation Sans",
                                              size = other_font_size,
                                              weight = "normal")
      liberation_other_bold = tkfont.create(family = "Liberation Sans",
                                            size = other_font_size,
                                            weight = "bold")
      # Use them in the GUI
      title_font = liberation_title_bold
      label_font = liberation_other_normal
      entry_font = liberation_other_normal
      button_font = liberation_other_bold
    }
  } else if (system_os == "Darwin") {
    # macOS
    helvetica_title_bold = tkfont.create(family = "Helvetica",
                                         size = title_font_size,
                                         weight = "bold")
    helvetica_other_normal = tkfont.create(family = "Helvetica",
                                           size = other_font_size,
                                           weight = "normal")
    helvetica_other_bold = tkfont.create(family = "Helvetica",
                                         size = other_font_size,
                                         weight = "bold")
    # Use them in the GUI
    title_font = helvetica_title_bold
    label_font = helvetica_other_normal
    entry_font = helvetica_other_normal
    button_font = helvetica_other_bold
  }



  # The "area" where we will put our input lines
  window <- tktoplevel(bg = "white")
  tkwm.resizable(window, FALSE, FALSE)
  #tkpack.propagate(window, FALSE)
  tktitle(window) <- "ENSEMBLE MS TUNER"
  # Raise the focus on the main window
  tkraise(window)
  # Title label
  title_label <-
    tkbutton(
      window,
      text = "ENSEMBLE MS TUNER",
      command = show_info_function,
      font = title_font,
      bg = "white",
      relief = "flat"
    )
  #### Browse
  # Library
  file_import_button <-
    tkbutton(
      window,
      text = "BROWSE\nPEAKLIST...",
      command = file_import_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Output
  browse_output_button <-
    tkbutton(
      window,
      text = "BROWSE\nOUTPUT FOLDER...",
      command = browse_output_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  #### Entries
  # Preprocessing
  preprocessing_entry <-
    tkbutton(
      window,
      text = "PREPROCESSING",
      command = preprocessing_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Model tuning mode
  model_tuning_entry <-
    tkbutton(
      window,
      text = "MODEL\nTUNING",
      command = model_tuning_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Automatially select features
  automatically_select_features_entry <-
    tkbutton(
      window,
      text = "AUTOMATICALLY\nSELECT\nFEATURES",
      command = automatically_select_features_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Generate plots
  generate_plots_entry <-
    tkbutton(
      window,
      text = "GENERATE\nPLOTS",
      command = generate_plots_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Combination of parameters
  try_combination_of_parameters_entry <-
    tkbutton(
      window,
      text = "COMBINATION\nOF\nPARAMETERS",
      command = try_combination_of_parameters_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Feature reranking
  feature_reranking_entry <-
    tkbutton(
      window,
      text = "FEATURE\nRERANKING",
      command = feature_reranking_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # File type export matrix
  file_type_export_matrix_entry <-
    tkbutton(
      window,
      text = "FILE TYPE\nEXPORT\nMATRIX",
      command = file_type_export_matrix_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Selection metric
  selection_metric_entry <-
    tkbutton(
      window,
      text = "SELECTION\nMETRIC",
      command = selection_metric_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Multicore
  allow_parallelization_button <-
    tkbutton(
      window,
      text = "ALLOW\nPARALLEL\nCOMPUTING",
      command = allow_parallelization_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # End session
  end_session_button <-
    tkbutton(
      window,
      text = "QUIT",
      command = end_session_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Run the Ensemble MS Tuner
  run_ensemble_ms_tuner_function_button <-
    tkbutton(
      window,
      text = "RUN\nENSEMBLE MS\nTUNER...",
      command = run_ensemble_ms_tuner_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Outcome list
  outcome_list_entry <-
    tkbutton(
      window,
      text = "SET OUTCOME\nLIST...",
      command = outcome_list_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Updates
  download_updates_button <-
    tkbutton(
      window,
      text = "DOWNLOAD\nUPDATE...",
      command = download_updates_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Features to select
  features_to_select_entry <-
    tkentry(
      window,
      textvariable = features_to_select,
      font = entry_font,
      bg = "white",
      width = 5,
      justify = "center"
    )
  tkinsert(features_to_select_entry, "end", "20")
  # CV repeats control
  cv_repeats_control_entry <-
    tkentry(
      window,
      textvariable = cv_repeats_control,
      font = entry_font,
      bg = "white",
      width = 5,
      justify = "center"
    )
  tkinsert(cv_repeats_control_entry, "end", "5")
  # k-fold CV control
  k_fold_cv_control_entry <-
    tkentry(
      window,
      textvariable = k_fold_cv_control,
      font = entry_font,
      bg = "white",
      width = 5,
      justify = "center"
    )
  tkinsert(k_fold_cv_control_entry, "end", "10")
  # Filename export
  filename_export_entry <-
    tkentry(
      window,
      textvariable = filename_export,
      font = entry_font,
      bg = "white",
      width = 50,
      justify = "center"
    )
  tkinsert(filename_export_entry, "end", "Model list MS")

  #### Displaying labels
  file_type_export_matrix_value_label <-
    tklabel(
      window,
      text = file_type_export_matrix,
      font = label_font,
      bg = "white",
      width = 20
    )
  preprocessing_value_label <-
    tklabel(
      window,
      text = preprocessing_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  automatically_select_features_value_label <-
    tklabel(
      window,
      text = automatically_select_features_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  model_tuning_value_label <-
    tklabel(
      window,
      text = model_tuning_value,
      font = label_font,
      bg = "white",
      width = 20,
      height = 2
    )
  generate_plots_value_label <-
    tklabel(
      window,
      text = generate_plots_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  allow_parallelization_value_label <-
    tklabel(
      window,
      text = allow_parallelization_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  outcome_list_value_label <-
    tklabel(
      window,
      text = outcome_list_value,
      font = label_font,
      bg = "white",
      width = 30
    )
  try_combination_of_parameters_value_label <-
    tklabel(
      window,
      text = try_combination_of_parameters_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  feature_reranking_value_label <-
    tklabel(
      window,
      text = feature_reranking_value,
      font = label_font,
      bg = "white",
      width = 20,
      height = 2
    )
  selection_metric_value_label <-
    tklabel(
      window,
      text = selection_metric,
      font = label_font,
      bg = "white",
      width = 20
    )
  filename_export_label <-
    tklabel(
      window,
      text = "<--- Set the file name",
      font = button_font,
      bg = "white",
      width = 20
    )
  check_for_updates_value_label <-
    tkbutton(
      window,
      text = check_for_updates_value,
      command = force_check_for_updates_function,
      font = label_font,
      bg = "white",
      width = 20,
      relief = "flat"
    )
  k_fold_cv_control_label <-
    tklabel(
      window,
      text = "K value of k-fold\ncross-validation",
      font = button_font,
      bg = "white",
      width = 20
    )
  cv_repeats_control_label <-
    tklabel(
      window,
      text = "Cross-validation\niterations",
      font = button_font,
      bg = "white",
      width = 20
    )
  features_to_select_label <-
    tklabel(
      window,
      text = "Number of features\nto be selected",
      font = button_font,
      bg = "white",
      width = 20
    )

  #### Geometry manager
  # Scrollbar
  #window_scrollbar <- tkscrollbar(window, command = function(...)tkyview(window,...))
  # tkgrid
  tkgrid(
    title_label,
    row = 1,
    column = 1,
    columnspan = 4,
    padx = c(20, 20),
    pady = c(20, 20)
  )
  tkgrid(
    features_to_select_label,
    row = 2,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    features_to_select_entry,
    row = 2,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    model_tuning_entry,
    row = 2,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    model_tuning_value_label,
    row = 2,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    selection_metric_entry,
    row = 2,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    selection_metric_value_label,
    row = 2,
    column = 6,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    automatically_select_features_entry,
    row = 3,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    automatically_select_features_value_label,
    row = 3,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    cv_repeats_control_label,
    row = 3,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    cv_repeats_control_entry,
    row = 3,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    k_fold_cv_control_label,
    row = 3,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    k_fold_cv_control_entry,
    row = 3,
    column = 6,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    preprocessing_entry,
    row = 4,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    preprocessing_value_label,
    row = 4,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    feature_reranking_entry,
    row = 4,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    feature_reranking_value_label,
    row = 4,
    column = 6,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    try_combination_of_parameters_entry,
    row = 4,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    try_combination_of_parameters_value_label,
    row = 4,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    allow_parallelization_button,
    row = 5,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    allow_parallelization_value_label,
    row = 5,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    outcome_list_entry,
    row = 7,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    outcome_list_value_label,
    row = 7,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    filename_export_entry,
    row = 6,
    column = 2,
    columnspan = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    filename_export_label,
    row = 6,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_type_export_matrix_entry,
    row = 5,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_type_export_matrix_value_label,
    row = 5,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    browse_output_button,
    row = 6,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_import_button,
    row = 7,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    run_ensemble_ms_tuner_function_button,
    row = 7,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    end_session_button,
    row = 7,
    column = 6,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    download_updates_button,
    row = 1,
    column = 5,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    check_for_updates_value_label,
    row = 1,
    column = 6,
    padx = c(10, 10),
    pady = c(10, 10)
  )

  ################################################################################
}

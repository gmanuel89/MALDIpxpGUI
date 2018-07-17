ms_pixel_typer <- function() {
  #################### MS PIXEL TYPER ####################
  # All the functions in the ms_pixel_typer() function are run within the ms_pixel_typer() function's environment, but they are called from the global environment since they were previously assigned with the <<- in the functions_mass_spectrometry() function.
  # Each value that must go to the global environment is assigned with the <<- so that it can be called from any function of the ms_pixel_typer() function.
  # In the debugging phase, run the whole code block within the {}, like as if the script was directly sourced from the file.

  ### Program version (Specified by the program writer!!!!)
  R_script_version <- "2017.12.21.1"
  ### Force update (in case something goes wrong after an update, when checking for updates and reading the variable force_update, the script can automatically download the latest working version, even if the rest of the script is corrupted, because it is the first thing that reads)
  force_update <- FALSE
  ### GitHub URL where the R file is
  github_R_url <-
    "https://raw.githubusercontent.com/gmanuel89/MS-Pixel-Typer/master/MS%20PIXEL%20TYPER.R"
  ### GitHub URL of the program's WIKI
  github_wiki_url <-
    "https://github.com/gmanuel89/MS-Pixel-Typer/wiki"
  ### Name of the file when downloaded
  script_file_name <- "MS PIXEL TYPER"
  # Change log
  change_log <-
    "1. New vote weights!!\n2. Fixed GUI\n3. Parallel now in foreach!\n4. Allow to set tolerance in ppm\n5. Multiple choice in ensemble vote\n6.Dump an RData file for R experts\n7. Bayesian probabilities now faster!!"






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
      "XML",
      "MALDIquant",
      "MALDIquantForeign",
      "doParallel"
    ),
    repository = NULL,
    update_packages = FALSE,
    print_messages = TRUE
  )






  ###################################### Initialize the variables (default values)
  filepath_import <- NULL
  tof_mode <- "linear"
  tolerance_ppm <- 1000
  output_folder <- getwd()
  spectra_format <- "imzml"
  peaks_filtering <- TRUE
  low_intensity_peak_removal_threshold_method <- "element-wise"
  peak_picking_algorithm <- "SuperSmoother"
  file_type_export_matrix <- "csv"
  file_type_export_images <- "png"
  spectra <- NULL
  peaks <- NULL
  allow_parallelization <- FALSE
  transform_data_algorithm <- NULL
  smoothing_algorithm <- "SavitzkyGolay"
  smoothing_strength <- "medium"
  baseline_subtraction_algorithm <- "SNIP"
  baseline_subtraction_algorithm_parameter <- 200
  normalization_algorithm <- "TIC"
  normalization_mass_range <- NULL
  preprocess_spectra_in_packages_of <- 0
  mass_range <- c(3000, 15000)
  spectral_alignment_algorithm <- NULL
  spectral_alignment_reference <- NULL
  peak_deisotoping <- FALSE
  peak_enveloping <- FALSE
  RData_file_integrity <- FALSE
  classification_mode <- "pixel"
  pixel_grouping <- "single"
  decision_method_ensemble <-
    c("unweighted majority", "bayesian probabilities")
  number_of_hca_nodes <- 4
  moving_window_size <- 5
  files_dumped <- FALSE
  preprocessing_parameters <-
    list(
      mass_range = mass_range,
      transformation_algorithm = transform_data_algorithm,
      smoothing_algorithm = smoothing_algorithm,
      smoothing_strength = smoothing_strength,
      baseline_subtraction_algorithm = baseline_subtraction_algorithm,
      baseline_subtraction_algorithm_parameter = baseline_subtraction_algorithm_parameter,
      normalization_algorithm = normalization_algorithm,
      normalization_mass_range = normalization_mass_range,
      preprocess_spectra_in_packages_of = preprocess_spectra_in_packages_of,
      spectral_alignment_algorithm = spectral_alignment_algorithm,
      spectral_alignment_reference = spectral_alignment_reference
    )




  ################## Values of the variables (for displaying and dumping purposes)
  tof_mode_value <- "Linear"
  tolerance_ppm_value <- as.character()
  filepath_import_value <- ""
  output_folder_value <- output_folder
  spectra_format_value <- "imzML"
  peaks_filtering_value <- "YES"
  peak_picking_algorithm_value <- "Super Smoother"
  low_intensity_peak_removal_threshold_method_value <-
    "element-wise"
  allow_parallelization_value <- "NO"
  transform_data_value <- "NO"
  smoothing_value <-
    paste0("YES", "\n( ", "Savitzky-Golay", ",\n" , "medium", " )")
  baseline_subtraction_value <-
    paste0("YES", "\n( ", "SNIP", ",\niterations: ", "200", " )")
  normalization_value <- paste0("YES", "\n( ", "TIC", " )")
  spectral_alignment_value <- "NO"
  spectral_alignment_algorithm_value <- ""
  spectral_alignment_reference_value <- ""
  peak_deisotoping_enveloping_value <- "None"
  RData_file_integrity_value <- "NO RDATA FILE\nSELECTED"
  classification_mode_value <- "pixel"
  decision_method_ensemble_value <- "Majority\nBayesian probs"






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

  ##### Preprocessing window
  preprocessing_window_function <- function() {
    ##### Functions
    # Transform the data
    transform_data_choice <- function() {
      # Ask for the algorithm
      transform_data_algorithm_input <-
        select.list(
          c(
            "Square root",
            "Natural logarithm",
            "Decimal logarithm",
            "Binary Logarithm",
            "None"
          ),
          title = "Data transformation",
          multiple = FALSE,
          preselect = "None"
        )
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      # Default and fix
      if (transform_data_algorithm_input == "Square root") {
        transform_data_algorithm <- "sqrt"
      } else if (transform_data_algorithm_input == "Natural logarithm") {
        transform_data_algorithm <- "log"
      } else if (transform_data_algorithm_input == "Binary Logarithm") {
        transform_data_algorithm <- "log2"
      } else if (transform_data_algorithm_input == "Decimal logarithm") {
        transform_data_algorithm <- "log10"
      } else if (transform_data_algorithm_input == "" ||
                 transform_data_algorithm_input == "None") {
        transform_data_algorithm <- NULL
      }
      # Set the value of the displaying label
      if (!is.null(transform_data_algorithm)) {
        transform_data_value <-
          paste0("YES", "\n( ", transform_data_algorithm_input, " )")
      } else {
        transform_data_value <- "None"
      }
      transform_data_value_label <-
        tklabel(
          preproc_window,
          text = transform_data_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 2
        )
      tkgrid(
        transform_data_value_label,
        row = 3,
        column = 2,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      transform_data_algorithm <<- transform_data_algorithm
      transform_data_value <<- transform_data_value
    }
    # Smoothing
    smoothing_choice <- function() {
      # Ask for the algorithm
      smoothing_algorithm_input <-
        select.list(
          c("Savitzky-Golay", "Moving Average", "None"),
          title = "Smoothing algorithm",
          multiple = FALSE,
          preselect = "SavitzkyGolay"
        )
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      # Default and fix
      if (smoothing_algorithm_input == "" ||
          smoothing_algorithm_input == "Savitzky-Golay") {
        smoothing_algorithm <- "SavitzkyGolay"
      } else if (smoothing_algorithm_input == "Moving Average") {
        smoothing_algorithm <- "MovingAverage"
      } else if (smoothing_algorithm_input == "None") {
        smoothing_algorithm <- NULL
      }
      # Strength
      if (!is.null(smoothing_algorithm)) {
        smoothing_strength <-
          select.list(
            c("small", "medium", "strong", "stronger"),
            title = "Smoothing strength",
            multiple = FALSE,
            preselect = "medium"
          )
        # Raise the focus on the preproc window
        tkraise(window)
        tkraise(preproc_window)
        if (smoothing_strength == "") {
          smoothing_strength <- "medium"
        }
      }
      # Set the value of the displaying label
      if (!is.null(smoothing_algorithm)) {
        smoothing_value <-
          paste0("YES",
                 "\n( ",
                 smoothing_algorithm_input,
                 ",\n" ,
                 smoothing_strength,
                 " )")
      } else {
        smoothing_value <- "None"
      }
      smoothing_value_label <-
        tklabel(
          preproc_window,
          text = smoothing_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 3
        )
      tkgrid(
        smoothing_value_label,
        row = 4,
        column = 2,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      smoothing_strength <<- smoothing_strength
      smoothing_algorithm <<- smoothing_algorithm
      smoothing_value <<- smoothing_value
    }
    # Baseline subtraction
    baseline_subtraction_choice <- function() {
      # Ask for the algorithm
      baseline_subtraction_algorithm <-
        select.list(
          c("SNIP", "TopHat", "ConvexHull", "median", "None"),
          title = "Baseline subtraction algorithm",
          multiple = FALSE,
          preselect = "SNIP"
        )
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      # Default
      if (baseline_subtraction_algorithm == "") {
        baseline_subtraction_algorithm <- "SNIP"
        baseline_subtraction_algorithm_parameter <- 200
      }
      if (baseline_subtraction_algorithm == "None") {
        baseline_subtraction_algorithm <- NULL
      }
      # SNIP
      if (!is.null(baseline_subtraction_algorithm) &&
          baseline_subtraction_algorithm == "SNIP") {
        baseline_subtraction_algorithm_parameter <-
          tclvalue(baseline_subtraction_algorithm_parameter2)
        baseline_subtraction_algorithm_parameter_value <-
          as.character(baseline_subtraction_algorithm_parameter)
        baseline_subtraction_algorithm_parameter <-
          as.integer(baseline_subtraction_algorithm_parameter)
      } else if (!is.null(baseline_subtraction_algorithm) &&
                 baseline_subtraction_algorithm == "TopHat") {
        baseline_subtraction_algorithm_parameter <-
          tclvalue(baseline_subtraction_algorithm_parameter2)
        baseline_subtraction_algorithm_parameter_value <-
          as.character(baseline_subtraction_algorithm_parameter)
        baseline_subtraction_algorithm_parameter <-
          as.integer(baseline_subtraction_algorithm_parameter)
      } else if (!is.null(baseline_subtraction_algorithm) &&
                 baseline_subtraction_algorithm == "median") {
        baseline_subtraction_algorithm_parameter <-
          tclvalue(baseline_subtraction_algorithm_parameter2)
        baseline_subtraction_algorithm_parameter_value <-
          as.character(baseline_subtraction_algorithm_parameter)
        baseline_subtraction_algorithm_parameter <-
          as.integer(baseline_subtraction_algorithm_parameter)
      }
      # Set the value of the displaying label
      if (!is.null(baseline_subtraction_algorithm) &&
          baseline_subtraction_algorithm == "SNIP") {
        baseline_subtraction_value <-
          paste0(
            "YES",
            "\n( ",
            baseline_subtraction_algorithm,
            ",\nIterations: ",
            baseline_subtraction_algorithm_parameter,
            " )"
          )
      } else if (!is.null(baseline_subtraction_algorithm) &&
                 baseline_subtraction_algorithm == "TopHat") {
        baseline_subtraction_value <-
          paste0(
            "YES",
            "\n( ",
            baseline_subtraction_algorithm,
            ",\nHalf window size: ",
            baseline_subtraction_algorithm_parameter,
            " )"
          )
      } else if (!is.null(baseline_subtraction_algorithm) &&
                 baseline_subtraction_algorithm == "median") {
        baseline_subtraction_value <-
          paste0(
            "YES",
            "\n( ",
            baseline_subtraction_algorithm,
            ",\nHalf window size: ",
            baseline_subtraction_algorithm_parameter,
            " )"
          )
      } else if (!is.null(baseline_subtraction_algorithm) &&
                 baseline_subtraction_algorithm == "ConvexHull") {
        baseline_subtraction_value <-
          paste0("YES", "\n( ", baseline_subtraction_algorithm, ")")
      } else {
        baseline_subtraction_value <- "None"
      }
      baseline_subtraction_value_label <-
        tklabel(
          preproc_window,
          text = baseline_subtraction_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 3
        )
      tkgrid(
        baseline_subtraction_value_label,
        row = 5,
        column = 3,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      baseline_subtraction_algorithm_parameter <<-
        baseline_subtraction_algorithm_parameter
      baseline_subtraction_algorithm <<-
        baseline_subtraction_algorithm
      baseline_subtraction_value <<- baseline_subtraction_value
    }
    # Normalization
    normalization_choice <- function() {
      # Ask for the algorithm
      normalization_algorithm <-
        select.list(
          c("TIC", "RMS", "PQN", "median", "None"),
          title = "Normalization algorithm",
          multiple = FALSE,
          preselect = "TIC"
        )
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      if (normalization_algorithm == "") {
        normalization_algorithm <- "TIC"
      }
      if (normalization_algorithm == "None") {
        normalization_algorithm <- NULL
      }
      # TIC
      if (!is.null(normalization_algorithm) &&
          normalization_algorithm == "TIC") {
        normalization_mass_range <- tclvalue(normalization_mass_range2)
        normalization_mass_range_value <-
          as.character(normalization_mass_range)
        if (normalization_mass_range != 0 &&
            normalization_mass_range != "") {
          normalization_mass_range <-
            unlist(strsplit(normalization_mass_range, ","))
          normalization_mass_range <-
            as.numeric(normalization_mass_range)
        } else if (normalization_mass_range == 0 ||
                   normalization_mass_range == "") {
          normalization_mass_range <- NULL
        }
      }
      # Set the value of the displaying label
      if (!is.null(normalization_algorithm) &&
          normalization_algorithm != "TIC") {
        normalization_value <-
          paste0("YES", "\n( ", normalization_algorithm, " )\n")
      } else if (!is.null(normalization_algorithm) &&
                 normalization_algorithm == "TIC") {
        if (!is.null(normalization_mass_range)) {
          normalization_value <-
            paste0(
              "YES",
              "\n( ",
              normalization_algorithm,
              ",\nrange:\n",
              normalization_mass_range_value,
              " )"
            )
        } else {
          normalization_value <-
            paste0("YES", "\n( ", normalization_algorithm, " )")
        }
      } else {
        normalization_value <- "None"
      }
      normalization_value_label <-
        tklabel(
          preproc_window,
          text = normalization_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 4
        )
      tkgrid(
        normalization_value_label,
        row = 7,
        column = 3,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      normalization_mass_range <<- normalization_mass_range
      normalization_algorithm <<- normalization_algorithm
      normalization_value <<- normalization_value
    }
    # Spectral alignment
    spectral_alignment_choice <- function() {
      # Ask for the algorithm
      spectral_alignment_algorithm <-
        select.list(
          c("cubic", "quadratic", "linear", "lowess", "None"),
          title = "Spectral alignment algorithm",
          multiple = FALSE,
          preselect = "cubic"
        )
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      # Default
      if (spectral_alignment_algorithm == "") {
        spectral_alignment_algorithm <- "None"
      }
      if (spectral_alignment_algorithm == "None") {
        spectral_alignment_algorithm <- NULL
      }
      ## Ask for the reference peaklist
      if (!is.null(spectral_alignment_algorithm)) {
        spectral_alignment_reference <-
          select.list(
            c("auto", "average spectrum", "skyline spectrum"),
            title = "Spectral alignment reference",
            multiple = FALSE,
            preselect = "average spectrum"
          )
        # Raise the focus on the preproc window
        tkraise(window)
        tkraise(preproc_window)
        if (spectral_alignment_reference == "") {
          spectral_alignment_reference <- "average spectrum"
        }
      } else {
        spectral_alignment_reference <- NULL
      }
      # Set the value of the displaying label
      if (!is.null(spectral_alignment_algorithm)) {
        spectral_alignment_value <-
          paste0(
            "YES",
            "\n( ",
            spectral_alignment_algorithm,
            ",\n",
            spectral_alignment_reference,
            " )"
          )
      } else {
        spectral_alignment_value <- "None"
      }
      spectral_alignment_value_label <-
        tklabel(
          preproc_window,
          text = spectral_alignment_value,
          font = label_font,
          bg = "white",
          width = 20,
          height = 3
        )
      tkgrid(
        spectral_alignment_value_label,
        row = 8,
        column = 2,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      spectral_alignment_algorithm <<- spectral_alignment_algorithm
      spectral_alignment_reference <<- spectral_alignment_reference
      spectral_alignment_value <<- spectral_alignment_value
    }
    # TOF mode
    tof_mode_choice <- function() {
      # Catch the value from the menu
      tof_mode <-
        select.list(c("Linear", "Reflectron"), title = "TOF mode")
      # Raise the focus on the preproc window
      tkraise(window)
      tkraise(preproc_window)
      # Default
      if (tof_mode == "" || tof_mode == "Linear") {
        tof_mode <- "linear"
      }
      if (tof_mode == "Reflectron") {
        tof_mode <- "reflectron"
      }
      # Set the value of the displaying label
      if (tof_mode == "linear") {
        tof_mode_value <- "Linear"
      } else if (tof_mode == "reflectron") {
        tof_mode_value <- "Reflectron"
      }
      tof_mode_value_label <-
        tklabel(
          preproc_window,
          text = tof_mode_value,
          font = label_font,
          bg = "white",
          width = 20
        )
      tkgrid(
        tof_mode_value_label,
        row = 2,
        column = 3,
        padx = c(5, 5),
        pady = c(5, 5)
      )
      # Escape the function
      tof_mode <<- tof_mode
      tof_mode_value <<- tof_mode_value
    }
    # Commit preprocessing
    commit_preprocessing_function <- function() {
      # Get the values (they are filled with the default anyway)
      # Mass range
      mass_range <- tclvalue(mass_range2)
      mass_range <- as.numeric(unlist(strsplit(mass_range, ",")))
      mass_range_value <-
        as.character(paste(mass_range[1], ",", mass_range[2]))
      # Preprocessing
      preprocess_spectra_in_packages_of <-
        tclvalue(preprocess_spectra_in_packages_of2)
      preprocess_spectra_in_packages_of <-
        as.integer(preprocess_spectra_in_packages_of)
      preprocess_spectra_in_packages_of_value <-
        as.character(preprocess_spectra_in_packages_of)
      # Preprocessing
      tolerance_ppm <- tclvalue(tolerance_ppm2)
      if (tolerance_ppm == "") {
        tolerance_ppm <- NULL
        tolerance_ppm_value <- ""
      } else {
        tolerance_ppm <- as.numeric(tolerance_ppm)
        tolerance_ppm_value <- as.character(tolerance_ppm)
      }
      # Escape the function
      mass_range <<- mass_range
      mass_range_value <<- mass_range_value
      preprocess_spectra_in_packages_of <<-
        preprocess_spectra_in_packages_of
      preprocess_spectra_in_packages_of_value <<-
        preprocess_spectra_in_packages_of_value
      tolerance_ppm <<- tolerance_ppm
      tolerance_ppm_value <<- tolerance_ppm_value
      preprocessing_parameters <<-
        list(
          mass_range = mass_range,
          transformation_algorithm = transform_data_algorithm,
          smoothing_algorithm = smoothing_algorithm,
          smoothing_strength = smoothing_strength,
          baseline_subtraction_algorithm = baseline_subtraction_algorithm,
          baseline_subtraction_algorithm_parameter = baseline_subtraction_algorithm_parameter,
          normalization_algorithm = normalization_algorithm,
          normalization_mass_range = normalization_mass_range,
          preprocess_spectra_in_packages_of = preprocess_spectra_in_packages_of,
          spectral_alignment_algorithm = spectral_alignment_algorithm,
          spectral_alignment_reference = spectral_alignment_reference
        )
      # Destroy the window upon committing
      tkdestroy(preproc_window)
      # Raise the focus on the preproc window
      tkraise(window)
    }
    ##### List of variables, whose values are taken from the entries in the GUI (create new variables for the sub window, that will replace the ones in the global environment, only if the default are changed)
    mass_range2 <- tclVar("")
    preprocess_spectra_in_packages_of2 <- tclVar("")
    tolerance_ppm2 <- tclVar("")
    baseline_subtraction_algorithm_parameter2 <- tclVar("")
    normalization_mass_range2 <- tclVar("")
    ##### Window
    preproc_window <- tktoplevel(bg = "white")
    tkwm.resizable(preproc_window, FALSE, FALSE)
    tktitle(preproc_window) <- "Spectral preprocessing parameters"
    #tkpack.propagate(preproc_window, FALSE)
    # Mass range
    mass_range_label <-
      tklabel(
        preproc_window,
        text = "Mass range",
        font = label_font,
        bg = "white",
        width = 20
      )
    mass_range_entry <-
      tkentry(
        preproc_window,
        textvariable = mass_range2,
        font = entry_font,
        bg = "white",
        width = 20,
        justify = "center"
      )
    tkinsert(mass_range_entry, "end", as.character(paste(mass_range[1], ",", mass_range[2])))
    # Preprocessing (in packages of)
    preprocess_spectra_in_packages_of_label <-
      tklabel(
        preproc_window,
        text = "Preprocess spectra\nin packages of",
        font = label_font,
        bg = "white",
        width = 20
      )
    preprocess_spectra_in_packages_of_entry <-
      tkentry(
        preproc_window,
        textvariable = preprocess_spectra_in_packages_of2,
        font = entry_font,
        bg = "white",
        width = 10,
        justify = "center"
      )
    tkinsert(
      preprocess_spectra_in_packages_of_entry,
      "end",
      as.character(preprocess_spectra_in_packages_of)
    )
    # Tof mode
    tof_mode_label <-
      tklabel(
        preproc_window,
        text = "Select the TOF mode",
        font = label_font,
        bg = "white",
        width = 20
      )
    tof_mode_entry <-
      tkbutton(
        preproc_window,
        text = "Choose the TOF mode",
        command = tof_mode_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    # Tolerance in ppm
    tolerance_ppm_label <-
      tklabel(
        preproc_window,
        text = "Tolerance (in ppm)",
        font = label_font,
        bg = "white",
        width = 20
      )
    tolerance_ppm_entry <-
      tkentry(
        preproc_window,
        textvariable = tolerance_ppm2,
        font = entry_font,
        bg = "white",
        width = 10,
        justify = "center"
      )
    tkinsert(tolerance_ppm_entry, "end", as.character(tolerance_ppm))
    # Transform the data
    transform_data_button <-
      tkbutton(
        preproc_window,
        text = "Data transformation",
        command = transform_data_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    # Smoothing
    smoothing_button <-
      tkbutton(
        preproc_window,
        text = "Smoothing",
        command = smoothing_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    # Baseline subtraction
    baseline_subtraction_button <-
      tkbutton(
        preproc_window,
        text = "Baseline subtraction",
        command = baseline_subtraction_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    baseline_subtraction_algorithm_parameter_entry <-
      tkentry(
        preproc_window,
        textvariable = baseline_subtraction_algorithm_parameter2,
        font = entry_font,
        bg = "white",
        width = 10,
        justify = "center"
      )
    tkinsert(
      baseline_subtraction_algorithm_parameter_entry,
      "end",
      as.character(baseline_subtraction_algorithm_parameter)
    )
    # Normalization
    normalization_button <-
      tkbutton(
        preproc_window,
        text = "Normalization",
        command = normalization_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    normalization_mass_range_entry <-
      tkentry(
        preproc_window,
        textvariable = normalization_mass_range2,
        font = entry_font,
        bg = "white",
        width = 20,
        justify = "center"
      )
    tkinsert(
      normalization_mass_range_entry,
      "end",
      as.character(normalization_mass_range)
    )
    # Spectral alignment
    spectral_alignment_button <-
      tkbutton(
        preproc_window,
        text = "Align spectra",
        command = spectral_alignment_choice,
        font = button_font,
        bg = "white",
        width = 20
      )
    # Commit preprocessing
    commit_preprocessing_button <-
      tkbutton(
        preproc_window,
        text = "Commit preprocessing",
        command = commit_preprocessing_function,
        font = button_font,
        bg = "white",
        width = 20
      )
    ##### Displaying labels
    tof_mode_value_label <-
      tklabel(
        preproc_window,
        text = tof_mode_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    transform_data_value_label <-
      tklabel(
        preproc_window,
        text = transform_data_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 2
      )
    smoothing_value_label <-
      tklabel(
        preproc_window,
        text = smoothing_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 3
      )
    baseline_subtraction_value_label <-
      tklabel(
        preproc_window,
        text = baseline_subtraction_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 3
      )
    normalization_value_label <-
      tklabel(
        preproc_window,
        text = normalization_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 4
      )
    spectral_alignment_value_label <-
      tklabel(
        preproc_window,
        text = spectral_alignment_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 3
      )
    #### Geometry manager
    tkgrid(
      mass_range_label,
      row = 1,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      mass_range_entry,
      row = 1,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      tof_mode_label,
      row = 2,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      tof_mode_entry,
      row = 2,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      tof_mode_value_label,
      row = 2,
      column = 3,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      transform_data_button,
      row = 3,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      transform_data_value_label,
      row = 3,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      smoothing_button,
      row = 4,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      smoothing_value_label,
      row = 4,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      baseline_subtraction_button,
      row = 5,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      baseline_subtraction_algorithm_parameter_entry,
      row = 5,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      baseline_subtraction_value_label,
      row = 5,
      column = 3,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      normalization_button,
      row = 7,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      normalization_mass_range_entry,
      row = 7,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      normalization_value_label,
      row = 7,
      column = 3,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      spectral_alignment_button,
      row = 8,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      spectral_alignment_value_label,
      row = 8,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      preprocess_spectra_in_packages_of_label,
      row = 9,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      preprocess_spectra_in_packages_of_entry,
      row = 9,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      tolerance_ppm_label,
      row = 10,
      column = 1,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      tolerance_ppm_entry,
      row = 10,
      column = 2,
      padx = c(5, 5),
      pady = c(5, 5)
    )
    tkgrid(
      commit_preprocessing_button,
      row = 11,
      column = 1,
      columnspan = 3,
      padx = c(5, 5),
      pady = c(5, 5)
    )
  }

  ##### File type export MATRIX
  file_type_export_matrix_choice <- function() {
    # Catch the value from the menu
    file_type_export_matrix <-
      select.list(
        c("csv", "xlsx", "xls"),
        title = "File type export",
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
           row = 2,
           column = 2)
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### File type export IMAGES
  file_type_export_images_choice <- function() {
    # Catch the value from the menu
    file_type_export_images <-
      select.list(
        c("png", "jpg", "tiff"),
        title = "Image type export",
        multiple = FALSE,
        preselect = "png"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (file_type_export_images == "") {
      file_type_export_images <- "png"
    }
    # Escape the function
    file_type_export_images <<- file_type_export_images
    # Set the value of the displaying label
    file_type_export_images_value_label <-
      tklabel(
        window,
        text = file_type_export_images,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(file_type_export_images_value_label,
           row = 2,
           column = 4)
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Classification mode choice
  classification_mode_choice <- function() {
    # Catch the value from the menu
    classification_mode <-
      select.list(
        c("pixel", "profile"),
        title = "Classification mode",
        multiple = TRUE,
        preselect = "pixel"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (length(classification_mode) == 1 &&
        classification_mode == "") {
      classification_mode <- "pixel"
      classification_mode_value <- "pixel"
    }
    # Choose the pixel grouping
    if ("pixel" %in% classification_mode) {
      ## Choose the pixel grouping
      pixel_grouping <-
        select.list(
          c("single", "hca", "moving window average"),
          title = "Pixel grouping",
          preselect = "single"
        )
      if (pixel_grouping == "") {
        pixel_grouping <- "single"
      }
      # Escape the function
      pixel_grouping <<- pixel_grouping
      if (pixel_grouping == "moving window average") {
        moving_window_size <-
          as.integer(select.list(
            c(2, 3, 4, 5, 10, 15, 20, 25, 50, 100, 200, 300, 500, 1000),
            title = "Moving window size",
            preselect = 5
          ))
        if (moving_window_size == "") {
          moving_window_size <- 5
        }
        # Escape the function
        moving_window_size <<- moving_window_size
      } else if (pixel_grouping == "hca") {
        number_of_hca_nodes <-
          as.integer(select.list(
            c(2, 3, 4, 5, 10, 15, 20, 25, 50, 100, 200, 300, 500, 1000),
            title = "Number of HC nodes",
            preselect = 4
          ))
        if (number_of_hca_nodes == "") {
          number_of_hca_nodes <- 4
        }
        # Escape the function
        number_of_hca_nodes <<- number_of_hca_nodes
      }
    }
    # Fix the displaying value
    if ("profile" %in% classification_mode &&
        "pixel" %in% classification_mode) {
      classification_mode_value <- "pixel + profile"
    } else if (classification_mode == "profile") {
      classification_mode_value <- "   profile   "
    } else if (classification_mode == "pixel") {
      classification_mode_value <- "  pixel  "
    }
    # Escape the function
    classification_mode <<- classification_mode
    classification_mode_value <<- classification_mode_value
    # Set the value of the displaying label
    classification_mode_value_label <-
      tklabel(
        window,
        text = classification_mode_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(classification_mode_value_label,
           row = 3,
           column = 2)
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Samples
  select_samples_function <- function() {
    # Set the working directory
    setwd(getwd())
    ########## Prompt if a folder has to be selected or a single file
    # Catch the value from the popping out menu
    spectra_input_type <-
      select.list(
        c("file", "folder"),
        title = "Folder or file?",
        multiple = FALSE,
        preselect = "file"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (spectra_input_type == "") {
      spectra_input_type <- "file"
    }
    # Folder
    if (spectra_input_type == "folder") {
      filepath_import_select <-
        tkmessageBox(title = "Samples",
                     message = "Select the folder for the spectra (stored as imzML files) to be imported: each imzML file should correspond to a patient's spectral dataset, and the imzML files should be put in folders corresponding to the different classes of samples",
                     icon = "info")
      filepath_import <- tclvalue(tkchooseDirectory())
      if (!nchar(filepath_import)) {
        tkmessageBox(message = "No folder selected")
        filepath_import <- NULL
      } else {
        tkmessageBox(message = paste(
          "The sample spectra will be read from:\n\n",
          filepath_import
        ))
      }
    } else if (spectra_input_type == "file") {
      # File
      filepath_import_select <-
        tkmessageBox(title = "Sample file",
                     message = "Select the imzML file for the spectra to be imported: the imzML file should correspond to a patient's spectral dataset",
                     icon = "info")
      filepath_import <-
        tclvalue(tkgetOpenFile(filetypes = "{{imzML files} {.imzML .imzml}}"))
      if (!nchar(filepath_import)) {
        tkmessageBox(message = "No file selected")
        filepath_import <- NULL
      } else {
        tkmessageBox(message = paste(
          "The sample spectra will be read from:\n\n",
          filepath_import
        ))
      }
    }
    # Set the value for displaying purposes
    filepath_import_value <- filepath_import
    # Exit the function and put the variable into the R workspace
    filepath_import <<- filepath_import
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### RData file containing the models
  select_RData_file_function <- function() {
    # Set the working directory
    setwd(getwd())
    filepath_R_import_select <-
      tkmessageBox(title = "RData file",
                   message = "Select the RData file containing the model list",
                   icon = "info")
    filepath_R <-
      tclvalue(tkgetOpenFile(filetypes = "{{RData files} {.RData}}"))
    if (!nchar(filepath_R)) {
      tkmessageBox(message = "No RData file selected")
      filepath_R <- NULL
      RData_file_integrity <- FALSE
      RData_file_integrity_value <- "NO RDATA FILE\nSELECTED"
    } else {
      tkmessageBox(message = paste("The model list will be read from:\n\n", filepath_R))
      # Define the progressbar
      RData_progress_bar <-
        tkProgressBar(
          title = "RData integrity verification",
          label = "",
          min = 0,
          max = 1,
          initial = 0,
          width = 300
        )
      # Retrieve the content of the RData workspace
      setTkProgressBar(
        RData_progress_bar,
        value = 0,
        title = NULL,
        label = "0 %"
      )
      RData_file_integrity <- FALSE
      RData_file_integrity_value <- "INTEGRITY TEST\nFAILED"
      try(RData_variables <-
            R_workspace_data_retriever(filepath_R)$variable_list)
      setTkProgressBar(
        RData_progress_bar,
        value = 0.50,
        title = NULL,
        label = "50 %"
      )
      # Check for integrity of the RData file
      try({
        if (all(c("model_list", "model_performance_parameter_list") %in% RData_variables)) {
          RData_file_integrity <- TRUE
          RData_file_integrity_value <- "INTEGRITY TEST\nSUCCEDED"
        }
      })
      setTkProgressBar(
        RData_progress_bar,
        value = 1,
        title = NULL,
        label = "100 %"
      )
      close(RData_progress_bar)
    }
    RData_file_integrity_value_label <-
      tklabel(
        window,
        text = RData_file_integrity_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(RData_file_integrity_value_label,
           row = 4,
           column = 2)
    # Exit the function and put the variable into the R workspace
    filepath_R <<- filepath_R
    RData_file_integrity <<- RData_file_integrity
    RData_file_integrity_value <<- RData_file_integrity_value
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

  ##### Peak picking algorithm
  peak_picking_algorithm_choice <- function() {
    # Catch the value from the menu
    peak_picking_algorithm <-
      select.list(
        c("MAD", "SuperSmoother"),
        title = "Peak picking algorithm",
        multiple = FALSE,
        preselect = "MAD"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (peak_picking_algorithm == "") {
      peak_picking_algorithm <- "MAD"
    }
    # Set the value of the displaying label
    peak_picking_algorithm_value <- peak_picking_algorithm
    if (peak_picking_algorithm_value == "MAD") {
      peak_picking_algorithm_value <- "Median\nAbsolute Deviation"
    } else if (peak_picking_algorithm_value == "SuperSmoother") {
      peak_picking_algorithm_value <- "Super Smoother"
    }
    peak_picking_algorithm_value_label <-
      tklabel(
        window,
        text = peak_picking_algorithm_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 2
      )
    tkgrid(peak_picking_algorithm_value_label,
           row = 2,
           column = 2)
    # Escape the function
    peak_picking_algorithm <<- peak_picking_algorithm
    peak_picking_algorithm_value <<- peak_picking_algorithm_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Peaks deisotoping or enveloping
  peak_deisotoping_enveloping_choice <- function() {
    # Catch the value from the menu
    peak_deisotoping_enveloping <-
      select.list(
        c("Peak Deisotoping", "Peak Enveloping", "None"),
        title = "Peak Deisotoping/Enveloping",
        multiple = FALSE,
        preselect = "Peak Deisotoping"
      )
    # Raise the focus on the main window
    tkraise(window)
    # Default
    if (peak_deisotoping_enveloping == "") {
      peak_deisotoping_enveloping <- "Peak Deisotoping"
    }
    if (peak_deisotoping_enveloping == "Peak Deisotoping") {
      peak_deisotoping <- TRUE
      peak_enveloping <- FALSE
    } else if (peak_deisotoping_enveloping == "Peak Enveloping") {
      peak_deisotoping <- FALSE
      peak_enveloping <- TRUE
    } else if (peak_deisotoping_enveloping == "None") {
      peak_deisotoping <- FALSE
      peak_enveloping <- FALSE
    }
    # Set the value of the displaying label
    peak_deisotoping_enveloping_value <- peak_deisotoping_enveloping
    peak_deisotoping_enveloping_value_label <-
      tklabel(
        window,
        text = peak_deisotoping_enveloping_value,
        font = label_font,
        bg = "white",
        width = 20
      )
    tkgrid(
      peak_deisotoping_enveloping_value_label,
      row = 2,
      column = 4,
      padx = c(10, 10),
      pady = c(10, 10)
    )
    # Escape the function
    peak_deisotoping <<- peak_deisotoping
    peak_enveloping <<- peak_enveloping
    peak_deisotoping_enveloping_value <<-
      peak_deisotoping_enveloping_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Decision method ensemble
  decision_method_ensemble_choice <- function() {
    # Catch the value from the menu
    decision_method_ensemble_input <-
      select.list(
        c("Unweighted majority", "Bayesian probabilities"),
        title = "Weighted Decision Method",
        multiple = TRUE,
        preselect = c("Unweighted majority", "Bayesian probabilities")
      )
    # Raise the focus on the main window
    tkraise(window)
    # Initialize
    decision_method_ensemble <- character()
    # Default
    if (length(decision_method_ensemble_input) == 1 &&
        decision_method_ensemble_input == "") {
      decision_method_ensemble <- "bayesian probabilities"
      decision_method_ensemble_value <- "Bayesian probs"
    } else {
      if ("Bayesian probabilities" %in% decision_method_ensemble_input) {
        decision_method_ensemble <-
          append(decision_method_ensemble, "bayesian probabilities")
        decision_method_ensemble_value_bayesian_probs <-
          "Bayesian probs"
      }
      if ("Unweighted majority" %in% decision_method_ensemble_input) {
        decision_method_ensemble <-
          append(decision_method_ensemble, "unweighted majority")
        decision_method_ensemble_value_unweighted_maj <- "Majority"
      }
    }

    # Displaying value
    decision_method_ensemble_value <- NULL
    for (s in 1:length(decision_method_ensemble)) {
      if (is.null(decision_method_ensemble_value)) {
        if (decision_method_ensemble[s] == "unweighted majority") {
          decision_method_ensemble_value <-
            decision_method_ensemble_value_unweighted_maj
        } else if (decision_method_ensemble[s] == "bayesian probabilities") {
          decision_method_ensemble_value <-
            decision_method_ensemble_value_bayesian_probs
        } else {
          decision_method_ensemble_value <-
            as.character(decision_method_ensemble[s])
        }
      } else {
        if (decision_method_ensemble[s] == "unweighted majority") {
          decision_method_ensemble_value <-
            as.character(
              paste0(
                decision_method_ensemble_value,
                "\n",
                decision_method_ensemble_value_unweighted_maj
              )
            )
        } else if (decision_method_ensemble[s] == "bayesian probabilities") {
          decision_method_ensemble_value <-
            as.character(
              paste0(
                decision_method_ensemble_value,
                "\n",
                decision_method_ensemble_value_bayesian_probs
              )
            )
        } else {
          decision_method_ensemble_value <-
            as.character(
              paste0(
                decision_method_ensemble_value,
                "\n",
                decision_method_ensemble[s]
              )
            )
        }
      }
    }
    # Value label
    decision_method_ensemble_value_label <-
      tklabel(
        window,
        text = decision_method_ensemble_value,
        font = label_font,
        bg = "white",
        width = 20,
        height = 4
      )
    tkgrid(decision_method_ensemble_value_label,
           row = 4,
           column = 4)
    # Escape the function
    decision_method_ensemble <<- decision_method_ensemble
    decision_method_ensemble_value <<-
      decision_method_ensemble_value
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
           row = 3,
           column = 4)
    # Escape the function
    allow_parallelization <<- allow_parallelization
    allow_parallelization_value <<- allow_parallelization_value
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Run the Peaklist Export function
  run_ms_pixel_typer_function <- function() {
    ### Go to the working directory
    setwd(output_folder)
    # Initialize the file_dumped variable
    files_dumped <- FALSE
    # Escape the function
    files_dumped <<- files_dumped
    ######## Run only if all the elements needed are there
    if (!is.null(filepath_import) && RData_file_integrity == TRUE) {
      # Choose the legends to plot
      plot_legends <-
        select.list(
          c("sample name", "legend", "plot name", "None"),
          multiple = TRUE,
          preselect = "None",
          title = "Legend to plot"
        )
      ### Put all the import block under the try() statement, so that if there are blocking errors (such as no files), the spectra variable remains NULL.
      try({
        # Progress bar
        program_progress_bar <-
          tkProgressBar(
            title = NULL,
            label = "",
            min = 0,
            max = 1,
            initial = 0,
            width = 300
          )
        setTkProgressBar(
          program_progress_bar,
          value = 0,
          title = NULL,
          label = "0 %"
        )
        setTkProgressBar(
          program_progress_bar,
          value = 0.25,
          title = "Performing classification...",
          label = "25 %"
        )
        ########## Run the classification function
        classification_of_patients <-
          spectral_classification(
            spectra_path = filepath_import,
            filepath_R = filepath_R,
            model_list = NULL,
            model_performance_parameter_list = NULL,
            classification_mode = classification_mode,
            peak_picking_algorithm = peak_picking_algorithm,
            deisotope_peaklist = peak_deisotoping,
            preprocessing_parameters = preprocessing_parameters,
            tof_mode = tof_mode,
            allow_parallelization = allow_parallelization,
            decision_method_ensemble = decision_method_ensemble,
            pixel_grouping = pixel_grouping,
            moving_window_size = moving_window_size,
            number_of_hca_nodes = number_of_hca_nodes,
            correlation_method_for_adjacency_matrix = "pearson",
            correlation_threshold_for_adjacency_matrix = 0.95,
            pvalue_threshold_for_adjacency_matrix = 0.05,
            max_GA_generations = 50,
            iterations_with_no_change_GA = 5,
            seed = 12345,
            plot_figures = TRUE,
            plot_graphs = TRUE,
            plot_legends = plot_legends,
            tolerance_ppm = tolerance_ppm,
            alpha = 0.05
          )
        # Escape the function
        classification_of_patients <<- classification_of_patients
        setTkProgressBar(
          program_progress_bar,
          value = 0.75,
          title = "Saving files...",
          label = "75 %"
        )
        if (files_dumped == FALSE) {
          # Dump the files
          ms_pixel_typer_data_dumper_function()
          files_dumped <- TRUE
        }
        # Escape the function
        files_dumped <<- files_dumped
        # Progress bar
        setTkProgressBar(
          program_progress_bar,
          value = 1.00,
          title = NULL,
          label = "100 %"
        )
        close(program_progress_bar)
        ### Messagebox
        tkmessageBox(title = "Done!",
                     message = "The classification has been performed and the files have been dumped!",
                     icon = "info")
      }, silent = TRUE)
    } else if (is.null(filepath_import) ||
               RData_file_integrity == FALSE) {
      classification_of_patients <- NULL
      # Escape the function
      classification_of_patients <<- classification_of_patients
      ### Messagebox
      tkmessageBox(title = "Something is wrong",
                   message = "Either no proper RData files or spectra files are provided!",
                   icon = "warning")
    }
    # Raise the focus on the main window
    tkraise(window)
  }

  ##### Dump the output files
  ms_pixel_typer_data_dumper_function <- function() {
    if (!is.null(classification_of_patients)) {
      if (files_dumped == FALSE) {
        cat("\nThe classification files will be now dumped!\n")
      } else if (files_dumped == TRUE) {
        tkmessageBox(title = "Duplicate files",
                     message = "All the files have been already dumped and will be dumped again!",
                     icon = "warning")
      }
      ########## Dump the files
      ##### Create a subfolder (CLASSIFICATION X)
      ### Go to the working directory
      setwd(output_folder)
      ##### Automatically create a subfolder with all the results
      # Add the date and time to the filename
      current_date <-
        unlist(strsplit(as.character(Sys.time()), " "))[1]
      current_date_split <- unlist(strsplit(current_date, "-"))
      current_time <-
        unlist(strsplit(as.character(Sys.time()), " "))[2]
      current_time_split <- unlist(strsplit(current_time, ":"))
      final_date <- ""
      for (x in 1:length(current_date_split)) {
        final_date <- paste0(final_date, current_date_split[x])
      }
      final_time <- ""
      for (x in 1:length(current_time_split)) {
        final_time <- paste0(final_time, current_time_split[x])
      }
      final_date_time <- paste(final_date, final_time, sep = "_")
      CLASSIFICATION_subfolder <-
        paste0("CLASSIFICATION", " (", final_date_time, ")")
      # Generate the new subfolder
      subfolder <-
        file.path(output_folder, CLASSIFICATION_subfolder)
      # Create the subfolder
      dir.create(subfolder)
      # Go to the new working directory
      setwd(subfolder)
      ##### Dump the files
      # Dump the RData file
      save(classification_of_patients, file = "Patient classification DATABASE.RData")
      # Determine the number of patients, to create subfolders (try with the msi list, if it is still zero because only the profile has been classified, use the profile list)
      number_of_patients <-
        length(classification_of_patients$final_result_matrix_msi_list)
      if (number_of_patients == 0) {
        number_of_patients <-
          length(classification_of_patients$final_result_matrix_profile_list)
      }
      ##### For each patient...
      for (p in 1:number_of_patients) {
        # Retrieve the patient name
        if (length(classification_of_patients$final_result_matrix_msi_list) > 0) {
          patient_name <-
            names(classification_of_patients$final_result_matrix_msi_list)[[p]]
        } else {
          patient_name <- NULL
        }
        if (is.null(patient_name)) {
          if (length(classification_of_patients$final_result_matrix_profile_list) > 0) {
            patient_name <-
              names(classification_of_patients$final_result_matrix_profile_list)[[p]]
          } else {
            patient_name <- NULL
          }
        }
        if (is.null(patient_name)) {
          patient_name <- p
        }
        # Create the subfolder and go to it
        subfolder_patient <- file.path(subfolder, patient_name)
        dir.create(subfolder_patient)
        setwd(subfolder_patient)
        ### Dump the files
        ## MSI classification matrix
        if (file_type_export_matrix == "csv") {
          try(write.csv(
            classification_of_patients$final_result_matrix_msi_list[[p]],
            file = paste0(
              "MSI classification matrix",
              ".",
              file_type_export_matrix
            )
          ),
          silent = TRUE)
        } else if (file_type_export_matrix == "xls" ||
                   file_type_export_matrix == "xlsx") {
          try({
            writeWorksheetToFile(
              file = paste0(
                "MSI classification matrix",
                ".",
                file_type_export_matrix
              ),
              data = classification_of_patients$final_result_matrix_msi_list[[p]],
              sheet = "MSI classification",
              clearSheets = TRUE
            )
          }, silent = TRUE)
        }
        ## MS profile matrix
        if (file_type_export_matrix == "csv") {
          try(write.csv(
            classification_of_patients$final_result_matrix_profile_list[[p]],
            file = paste0(
              "MS profile classification matrix",
              ".",
              file_type_export_matrix
            )
          ),
          silent = TRUE)
        } else if (file_type_export_matrix == "xls" ||
                   file_type_export_matrix == "xlsx") {
          try({
            writeWorksheetToFile(
              file = paste0(
                "MS profile classification matrix",
                ".",
                file_type_export_matrix
              ),
              data = classification_of_patients$final_result_matrix_profile_list[[p]],
              sheet = "MS profile classification",
              clearSheets = TRUE
            )
          }, silent = TRUE)
        }
        ## MSI pixel classification
        # PNG
        if (file_type_export_images == "png") {
          try({
            for (i in 1:length(classification_of_patients$classification_ms_images_list[[p]])) {
              # Retrieve the model name
              model_name <-
                names(classification_of_patients$classification_ms_images_list[[p]])[i]
              # Save the plot
              png(
                filename = paste(
                  "Pixel-by-pixel classification ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(classification_of_patients$classification_ms_images_list[[p]][[i]])
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "tiff") {
          # TIFF
          try({
            for (i in 1:length(classification_of_patients$classification_ms_images_list[[p]])) {
              # Retrieve the model name
              model_name <-
                names(classification_of_patients$classification_ms_images_list[[p]])[i]
              # Save the plot
              tiff(
                filename = paste(
                  "Pixel-by-pixel classification ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(classification_of_patients$classification_ms_images_list[[p]][[i]])
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "jpg" ||
                   file_type_export_images == "jpeg") {
          # JPEG
          try({
            for (i in 1:length(classification_of_patients$classification_ms_images_list[[p]])) {
              # Retrieve the model name
              model_name <-
                names(classification_of_patients$classification_ms_images_list[[p]])[i]
              # Save the plot
              jpeg(
                filename = paste(
                  "Pixel-by-pixel classification ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150,
                quality = 100
              )
              replayPlot(classification_of_patients$classification_ms_images_list[[p]][[i]])
              dev.off()
            }
          }, silent = TRUE)
        }
        ## Ensemble classification MSI matrix
        if (file_type_export_matrix == "csv") {
          try(write.csv(
            classification_of_patients$classification_ensemble_matrix_msi_all[[p]],
            file = paste0(
              "Ensemble MSI classification matrix",
              ".",
              file_type_export_matrix
            )
          ),
          silent = TRUE)
        } else if (file_type_export_matrix == "xls" ||
                   file_type_export_matrix == "xlsx") {
          try({
            writeWorksheetToFile(
              file = paste0(
                "Ensemble MSI classification matrix",
                ".",
                file_type_export_matrix
              ),
              data = classification_of_patients$classification_ensemble_matrix_msi_all[[p]],
              sheet = "Ensemble MSI classification",
              clearSheets = TRUE
            )
          }, silent = TRUE)
        }
        ## Ensemble classification profile matrix
        if (file_type_export_matrix == "csv") {
          try(write.csv(
            classification_of_patients$classification_ensemble_matrix_profile_all[[p]],
            file = paste0(
              "Ensemble MS profile classification matrix",
              ".",
              file_type_export_matrix
            )
          ),
          silent = TRUE)
        } else if (file_type_export_matrix == "xls" ||
                   file_type_export_matrix == "xlsx") {
          try({
            writeWorksheetToFile(
              file = paste0(
                "Ensemble MS profile classification matrix",
                ".",
                file_type_export_matrix
              ),
              data = classification_of_patients$classification_ensemble_matrix_profile_all[[p]],
              sheet = "Ensemble MS classification",
              clearSheets = TRUE
            )
          }, silent = TRUE)
        }
        ## Ensemble MSI pixel classification
        # PNG
        if (file_type_export_images == "png") {
          try({
            for (l in 1:length(classification_of_patients$classification_ensemble_ms_image_list[[p]])) {
              png(
                filename = paste0(
                  "Ensemble Pixel-by-pixel classification (",
                  names(
                    classification_of_patients$classification_ensemble_ms_image_list[[p]]
                  )[l],
                  ").",
                  file_type_export_images
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(
                classification_of_patients$classification_ensemble_ms_image_list[[p]][[l]]
              )
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "tiff") {
          # TIFF
          try({
            for (l in 1:length(classification_of_patients$classification_ensemble_ms_image_list[[p]])) {
              tiff(
                filename = paste0(
                  "Ensemble Pixel-by-pixel classification (",
                  names(
                    classification_of_patients$classification_ensemble_ms_image_list[[p]]
                  )[l],
                  ").",
                  file_type_export_images
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(
                classification_of_patients$classification_ensemble_ms_image_list[[p]][[l]]
              )
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "jpg" ||
                   file_type_export_images == "jpeg") {
          # JPEG
          try({
            for (l in 1:length(classification_of_patients$classification_ensemble_ms_image_list[[p]])) {
              jpeg(
                filename = paste0(
                  "Ensemble Pixel-by-pixel classification (",
                  names(
                    classification_of_patients$classification_ensemble_ms_image_list[[p]]
                  )[l],
                  ").",
                  file_type_export_images
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150,
                quality = 100
              )
              replayPlot(
                classification_of_patients$classification_ensemble_ms_image_list[[p]][[l]]
              )
              dev.off()
            }
          }, silent = TRUE)
        }
        ## Average spectrum with bars
        # PNG
        if (file_type_export_images == "png") {
          try({
            for (i in 1:length(
              classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
            )) {
              # Retrieve the model name
              model_name <-
                names(
                  classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
                )[i]
              # Save the plot
              png(
                filename = paste(
                  "Average spectrum with bars ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(
                classification_of_patients$average_spectrum_with_bars_profile_list[[p]][[i]]
              )
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "tiff") {
          # TIFF
          try({
            for (i in 1:length(
              classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
            )) {
              # Retrieve the model name
              model_name <-
                names(
                  classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
                )[i]
              # Save the plot
              tiff(
                filename = paste(
                  "Average spectrum with bars ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(
                classification_of_patients$average_spectrum_with_bars_profile_list[[p]][[i]]
              )
              dev.off()
            }
          }, silent = TRUE)
        } else if (file_type_export_images == "jpg" ||
                   file_type_export_images == "jpeg") {
          # JPEG
          try({
            for (i in 1:length(
              classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
            )) {
              # Retrieve the model name
              model_name <-
                names(
                  classification_of_patients$average_spectrum_with_bars_profile_list[[p]]
                )[i]
              # Save the plot
              jpeg(
                filename = paste(
                  "Average spectrum with bars ",
                  model_name,
                  ".",
                  file_type_export_images,
                  sep = ""
                ),
                width = 1920,
                height = 1080,
                pointsize = 20,
                units = "px",
                res = 150
              )
              replayPlot(
                classification_of_patients$average_spectrum_with_bars_profile_list[[p]][[i]]
              )
              dev.off()
            }
          }, silent = TRUE)
        }
      }
      ## Ensemble classification profile matrix ALL
      setwd(subfolder)
      if (file_type_export_matrix == "csv") {
        try(write.csv(
          classification_of_patients$classification_ensemble_matrix_profile_all,
          file = paste0(
            "Ensemble MS profile classification matrix ALL",
            ".",
            file_type_export_matrix
          )
        ),
        silent = TRUE)
      } else if (file_type_export_matrix == "xls" ||
                 file_type_export_matrix == "xlsx") {
        try({
          writeWorksheetToFile(
            file = paste0(
              "Ensemble MS profile classification matrix ALL",
              ".",
              file_type_export_matrix
            ),
            data = classification_of_patients$classification_ensemble_matrix_profile_all,
            sheet = "Ensemble MS classification",
            header = TRUE,
            rownames = rownames(
              classification_of_patients$classification_ensemble_matrix_profile_all
            )
          )
        }, silent = TRUE)
      }
      if (files_dumped == TRUE) {
        ### Messagebox
        tkmessageBox(title = "Files dumped!",
                     message = "The classification files have been dumped!",
                     icon = "info")
      }
      ### Go to the working directory
      setwd(output_folder)
    } else {
      ### Messagebox
      tkmessageBox(title = "No classification found",
                   message = "No classification files have been found!\nRun the classification before dumping the files!",
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
  # Raise the focus on the main window
  tkraise(window)
  tktitle(window) <- "MS PIXEL TYPER"
  # Title label
  title_label <-
    tkbutton(
      window,
      text = "MS PIXEL TYPER",
      command = show_info_function,
      font = title_font,
      bg = "white",
      relief = "flat"
    )
  #### Browse
  # Library
  select_samples_button <-
    tkbutton(
      window,
      text = "BROWSE\nSPECTRA...",
      command = select_samples_function,
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
  # Peak picking method
  peak_picking_algorithm_entry <-
    tkbutton(
      window,
      text = "PEAK PICKING\nALGORITHM",
      command = peak_picking_algorithm_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Peaks deisotoping
  peak_deisotoping_entry <-
    tkbutton(
      window,
      text = "PEAK\nDEISOTOPING",
      command = peak_deisotoping_enveloping_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Decision method ensemble
  decision_method_ensemble_entry <-
    tkbutton(
      window,
      text = "DECISION METHOD\nENSEMBLE",
      command = decision_method_ensemble_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Classification mode
  classification_mode_entry <-
    tkbutton(
      window,
      text = "CLASSIFICATION\nMODE",
      command = classification_mode_choice,
      font = button_font,
      bg = "white",
      width = 20
    )
  # RData input
  select_RData_file_entry <-
    tkbutton(
      window,
      text = "SELECT MODEL\nDATABASE...",
      command = select_RData_file_function,
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
  # File type export images
  file_type_export_images_entry <-
    tkbutton(
      window,
      text = "FILE TYPE\nEXPORT\nIMAGES",
      command = file_type_export_images_choice,
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
  # Spectra preprocessing button
  spectra_preprocessing_button <-
    tkbutton(
      window,
      text = "SPECTRA\nPREPROCESSING\nPARAMETERS...",
      command = preprocessing_window_function,
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
  # Run the MS Pixel Typer
  run_ms_pixel_typer_function_button <-
    tkbutton(
      window,
      text = "RUN\nMS PIXEL TYPER...",
      command = run_ms_pixel_typer_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Dump the files
  ms_pixel_typer_data_dumper_button <-
    tkbutton(
      window,
      text = "DUMP\nFILES",
      command = ms_pixel_typer_data_dumper_function,
      font = button_font,
      bg = "white",
      width = 20
    )
  # Updates
  download_updates_button <-
    tkbutton(
      window,
      text = "DOWNLOAD\nUPDATE",
      command = download_updates_function,
      font = button_font,
      bg = "white",
      width = 20
    )

  #### Displaying labels
  file_type_export_matrix_value_label <-
    tklabel(
      window,
      text = file_type_export_matrix,
      font = label_font,
      bg = "white",
      width = 20
    )
  file_type_export_images_value_label <-
    tklabel(
      window,
      text = file_type_export_images,
      font = label_font,
      bg = "white",
      width = 20
    )
  peak_picking_algorithm_value_label <-
    tklabel(
      window,
      text = peak_picking_algorithm_value,
      font = label_font,
      bg = "white",
      width = 20,
      height = 2
    )
  peak_deisotoping_enveloping_value_label <-
    tklabel(
      window,
      text = peak_deisotoping_enveloping_value,
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
  classification_mode_value_label <-
    tklabel(
      window,
      text = classification_mode_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  RData_file_integrity_value_label <-
    tklabel(
      window,
      text = RData_file_integrity_value,
      font = label_font,
      bg = "white",
      width = 20
    )
  decision_method_ensemble_value_label <-
    tklabel(
      window,
      text = decision_method_ensemble_value,
      font = label_font,
      bg = "white",
      width = 20,
      height = 4
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

  #### Geometry manager
  # Scrollbar
  #window_scrollbar <- tkscrollbar(window, command = function(...)tkyview(window,...))
  # tkgrid
  tkgrid(
    title_label,
    row = 1,
    column = 1,
    columnspan = 2,
    padx = c(20, 20),
    pady = c(20, 20)
  )
  tkgrid(
    select_samples_button,
    row = 7,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    browse_output_button,
    row = 7,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  #tkgrid(peak_deisotoping_entry, row = 2, column = 3, padx = c(10, 10), pady = c(10, 10))
  #tkgrid(peak_deisotoping_enveloping_value_label, row = 2, column = 4, padx = c(10, 10), pady = c(10, 10))
  #tkgrid(peak_picking_algorithm_entry, row = 2, column = 1, padx = c(10, 10), pady = c(10, 10))
  #tkgrid(peak_picking_algorithm_value_label, row = 2, column = 2, padx = c(10, 10), pady = c(10, 10))
  tkgrid(
    file_type_export_matrix_entry,
    row = 2,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_type_export_matrix_value_label,
    row = 2,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_type_export_images_entry,
    row = 2,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    file_type_export_images_value_label,
    row = 2,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    decision_method_ensemble_entry,
    row = 4,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    decision_method_ensemble_value_label,
    row = 4,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    classification_mode_entry,
    row = 3,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    classification_mode_value_label,
    row = 3,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    select_RData_file_entry,
    row = 4,
    column = 1,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    RData_file_integrity_value_label,
    row = 4,
    column = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    allow_parallelization_button,
    row = 3,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    allow_parallelization_value_label,
    row = 3,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    spectra_preprocessing_button,
    row = 7,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    run_ms_pixel_typer_function_button,
    row = 7,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  #tkgrid(ms_pixel_typer_data_dumper_button, row = 7, column = 4, padx = c(10, 10), pady = c(10, 10))
  tkgrid(
    end_session_button,
    row = 8,
    column = 2,
    columnspan = 2,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    download_updates_button,
    row = 1,
    column = 3,
    padx = c(10, 10),
    pady = c(10, 10)
  )
  tkgrid(
    check_for_updates_value_label,
    row = 1,
    column = 4,
    padx = c(10, 10),
    pady = c(10, 10)
  )



  ################################################################################
}

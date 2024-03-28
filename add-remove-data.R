# ===========
# Constants
# ===========

SYS_CLEAR <- "clear"    # The OS clear command
SYS_INPUT <- "stdin"    # The OS input source

# The main menu that will be printed out
MENU_PROMPT <- paste("\n1. Add a datafile to a dataset\n",
    "2. Remove a datafile from a dataset\n",
    "3. Exit\n\n", sep="")
# The prompt for the user to select a dataset (abbreviation)
DATASET_PROMPT <- paste("\nEnter the dataset (abbreviation) that the file belongs to\n",
    "{\n",
    "   CFS (Calls for Service),\n",
    "   COL (Collisions),\n",
    "   CIUOF (Complaints, Inquiries, Use of Force),\n",
    "   CON (Contacts),\n",
    "   OFF (Offenses)\n",
    "}: ")

# ===================================================================
# Create a directory for the given dataset and year.
# Get a list of files from the user and add them to this directory.
# ===================================================================
addFiles <- function(dataset, year) {
    cat("List \"/full/path/to/file(s)\" to be added to the dataset (separated by spaces): ")
    # Get a list of filenames from the user
    filenames <- readLines(con = SYS_INPUT, 1)
    filenames <- as.list(strsplit(filenames, " "))
    # Path to destination folder
    destination <- paste("./data/", dataset,"/", year, "/unjoined/", sep="")
    # Create a directory if it doesn't exist
    ifelse(dir.exists(file.path(destination)), dir.create(file.path(destination))) 
}

# =========================================================
# Get a dataset and year from the user.
# List the files for that dataset and year.
# Delete specified user files from that dataset and year.
# =========================================================
removeFiles <- function(dataset, year) {

}

# ======================
# Main driver function
# ======================
main <- function() {
    cat(MENU_PROMPT)    # Print menu

    # Get the user's first choice of option
    cat("Select option (1, 2, or 3): ")
    menu_choice <- readLines(con = SYS_INPUT, 1)
    # Clear the console to improve readability
    system(SYS_CLEAR)

    while (menu_choice != "3") {
        # Ensure that the user input is a valid option
        while (menu_choice != "1" && menu_choice != "2" 
                                  && menu_choice != "3") {
            # Print the menu again 
            # and get a (hopefully) valid choice
            cat("INVALID INPUT\n")
            cat (MENU_PROMPT)
            menu_choice <- readLines(con = SYS_INPUT, 1)
            # Clear the console to improve readability
            system(SYS_CLEAR)
        }

        # Get user input for the dataset to be added onto
        cat(DATASET_PROMPT)
        dataset <- readLines(con = SYS_INPUT, 1)
        # Ensure that the dataset choice was valid
        while (!(dataset %in% c("CFS", "COL", "CIUOF", "CON", "OFF"))) {
            cat("INVALID INPUT\n")
            cat(DATASET_PROMPT)
            dataset <- readLines(con = SYS_INPUT, 1)
        }

        # Get user input for the year of the chosen dataset
        cat("Enter the year of the data file (YYYY): ")
        year <- readLines(con = SYS_INPUT, 1)

        # Ensure that the year is in the correct format (4 digits)
        while (nchar(year) != 4) {
            cat("INVALID INPUT\n")
            cat("Enter the year of the data file (YYYY): ")
            year <- readLines(con = SYS_INPUT, 1)
        }

        # Go to the corresponding function based on user input
        if (menu_choice == "1") {
            # Add a list of files
            addFiles(dataset, year)
        } 
        else {
            # Remove a list of files
            removeFiles(dataset, year)
        }

        # Give the user the option to perform another action
        cat(MENU_PROMPT)
        cat("Select option (1, 2, or 3): ")
        menu_choice <- readLines(con = SYS_INPUT, 1)
        # Clear the console to improve readability
        system(SYS_CLEAR)
    }
}

main()
import os
import shutil

"""
===================================================================
Create a directory for the given dataset and year.
Get a list of files from the user and add them to this directory.
===================================================================
"""
def addFile(dataset, year):
    # Get all the files to be added
    user_input = input('List \"/full/path/to/file(s)\" to be added to the dataset (separated by spaces): ').rstrip()
    filenames = user_input.split(' ')   # Split the filenames and add them to a list
    destination = f'./data/{dataset}/{year}/unjoined'    # Relative path to the destination directory
    os.makedirs(destination, exist_ok = True)   # Make the destination directory if it doesn't already exist

    # Copy the specified files to the destination directory
    for filename in filenames:
        try:
            shutil.copy(filename, f'{destination}')    # Copy files to the destination directory (in case files need to be joined)
        # Handle error if file doesn't exist
        except FileNotFoundError as noFileError:
            print(f'ERROR: {noFileError.filename} -- {noFileError.strerror}')
        # Handle error if specified file is a directory
        except IsADirectoryError as isDirError:
            print(f'ERROR: {isDirError.filename} -- {isDirError.strerror}')
# End addFile

"""
=========================================================
Get a dataset and year from the user.
List the files for that dataset and year.
Delete specified user files from that dataset and year.
=========================================================
"""
def removeFile(dataset, year):
    os.system(f'ls ./data/{dataset}/{year}/unjoined')   # List files for specified dataset and year
    # Get files that need to be deleted from user
    user_input = input('List \"files\" that will be deleted (separated by spaces): ').rstrip()
    # Separate files into list
    filenames = user_input.split(' ')
    # Location directory of files
    # Files are deleted from unjoined directory because that is where raw data is stored
    # Joined files can be overwritten
    destination = f'./data/{dataset}/{year}/unjoined'

    # Delete files
    for filename in filenames:
        try:
            os.remove(f'{destination}/{filename}')
        # Handle error if file doesn't exist
        except FileNotFoundError as noFileError:
            print(f'ERROR: {noFileError.filename} -- {noFileError.strerror}')
        # Handle error if specified file is a directory
        except IsADirectoryError as isDirError:
            print(f'ERROR: {isDirError.filename} -- {isDirError.strerror}')
# End removeFile
            
"""
======================
Main driver function
======================
"""
def main():
    MENU_PROMPT = ('\n1. Add a datafile to a dataset\n'
                  '2. Remove a datafile from a dataset\n'
                  '3. Exit\n')
    print(MENU_PROMPT)  # Print prompt
    menu_choice = input('Select option (1, 2, or 3): ').rstrip() # Get the user's choice of what to do
    os.system('cls' if os.name == 'nt' else 'clear')    # Clear the terminal to make it more readable

    # Loop until user chooses to exit
    while menu_choice != '3':
        # Check that the input was a valid option
        while menu_choice != '1' and menu_choice != '2' and menu_choice != '3':
            print('INVALID INPUT')
            print(MENU_PROMPT)  # Print menu again
            menu_choice = input('Please enter a valid option (1, 2, or 3): ').rstrip()   # Attempt another 'valid' user input
            os.system('cls' if os.name == 'nt' else 'clear')    # Clear the terminal to make it more readable

        # Get user input about the dataset to be added to
        dataset = input(('Enter the dataset (abbreviation) that the file belongs to\n'
                            '{\n'
                            '    CFS (Calls for Service),\n'
                            '    COL (Collisions),\n'
                            '    CIUOF (Complaints, Inquiries, Use of Force),\n'
                            '    CON (Contacts),\n'
                            '    OFF (Offenses)\n'
                            '}: ')).rstrip()

        # If the dataset is not an option, retry
        while dataset not in ['CFS', 'COL', 'CIUOF', 'CON', 'OFF']:
            print('INVALID INPUT')
            dataset = input(('Enter the dataset (abbreviation) that the file belongs to\n'
                            '{\n'
                            '    CFS (Calls for Service),\n'
                            '    COL (Collisions),\n'
                            '    CIUOF (Complaints, Inquiries, Use of Force),\n'
                            '    CON (Contacts),\n'
                            '    OFF (Offenses)\n'
                            '}: ')).rstrip()

        # Get the year of the dataset to be added to
        year = input('Enter the year of the data file (YYYY): ').rstrip()

        # If the year isn't four digits, retry
        while len(year) != 4:
            print('INVALID INPUT')
            year = input('Enter the year of the data file (YYYY): ').rstrip()
        
        # Menu choise 1 is to add a file
        if menu_choice == '1':
            addFile(dataset, year)
        # Menu choice 2 is to remove a file
        elif menu_choice == '2':
            removeFile(dataset, year)
        
        # Give the user the option to perform another action
        print(MENU_PROMPT)  # Print the menu again
        menu_choice = input('Select option (1, 2, or 3): ').rstrip() # Get the user's choice
        os.system('cls' if os.name == 'nt' else 'clear')    # Clear terminal
# End main

main()  # Call main
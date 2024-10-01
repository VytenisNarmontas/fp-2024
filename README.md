# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`



Vytenis Narmontas "Car garage" project:

BNF of a simple car garage.
Functions:
    *add car - adds a car to the garage with some basic information
    *remove car - removes car from garage
    *list cars - lists all cars in the garage
    *service car - adds a service to a vehicle with some basic service information (recursive so service can consist of multiple services)
    *list services - lists all the services done to a car based on the license plate
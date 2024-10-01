# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`



### Vytenis Narmntas "Car Garage" project:

### Introuction:

This is a BNF of a simple car garage for personal use.
I have picked this Domain, because I am interested and have good knowledge of cars. 
This project allows the user to add and remove cars from their garage aswell as save services and their date of each car.
This project can be useful if you want to sell your cars and you want to show the buyer a full history of the vehicle they are buying.

### Domain:

The main operations are:

-add_car - adds a car to the garage with some basic information
usage:
```add car ABC123 BMW e36 touring 1997```

-remove_car - removes a car from the garage
usage:
```remove car ABC123```

-list_cars - lists all the cars from the garage
usage:
```list cars```

-service_car - adds a service to a car with the date of the service (can be recursive to allow multiple services to be made at the same time)
usage:
```service car ABC123 oil change 20-01-2004```
```service car ABC123 oil change and tire change 21-01-2005```

-list_services - lists all the services done to a car
usage:
```list_services ABC123```

The operations are:

-make - the make of the vehicle

-model - the model of the vehicle (e36, e36 touring ect.)

-license_plate - license plate of the vehicle (used to add services to the specific vehicle and not the model)

-service_type - the service that you are doing to the vehicle

-service_date - the date when the service was completed





# Validation of groundwater loggere data

Analysis on validation methods for groundwater logger data 

## Rationale (dutch)

Het OIS grondwater heeft als doel om meer en betere grondwatergegevens sneller ter beschikking stellen via het DOV-portaal. Met de verschillende partners willen we hiervoor de validatie procedures van (hoogfrequente) grondwater loggers beter overeenstemmen, zodat de data die op DOV ter beschikking zal gesteld worden zo consistent mogelijk is voor de externe gebruiker.

Bij de datatstroom tussen ruwe data en gekalibreerd peil zijn er validatiestappen op het level van (1) de ruwe data zelf, (2) de waterpeilen en (3) vergelijking met referentiewaarden, ook wel kalibratie genoemd:

![](/docs/static/workflow_data_validation.png)

De methodes/algoritmes die in dit project getest en ontwikkeld worden hebben als doen om validatieprocedures te voorzien die operationeel kunnen worden ingezet door instanties die grondwaterpeilen aan DOV aanleveren. Meer achtergrond over het voortraject iste vinden op [confluence](https://www.milieuinfo.be/confluence/display/GWM/Compilatie+workshop+validatie+diver+metingen+2018-09-14) (login required).

De doelstellingen zijn:

- Een set van robuste en geteste methodes voor data validatie van ruwe loggerdata en peilen.
- Documentatie over de algoritmes, de noodzakelijke inputs, de toepassingsmogelijkheden  (randvoorwaarden waarvoor het algoritme werkt).
- Documentatie van een _controlled vocabulary_ van error/warning flags.
- Een referentie-implementatie van de verschillende algoritmes.

## Workflow

...

## Repo structure

The repository structure is based on [Cookiecutter Data Science](http://drivendata.github.io/cookiecutter-data-science/). Files and directories indicated with GENERATED should not be edited manually.:


```
├── README.md              : Description of this repository
├── LICENSE                : Repository license
├── .gitignore             : Files and directories to be ignored by git
│
├── data
│   ├── raw                : Source logger data as received from the partners (INBO,...)
│   │    ├── inbo           : A subfolder for each partner 
│   │    └── ...
│   ├── interim            : Folder for interim outputs GENERATED
│   └── processed          : Resulting outputs used for reporting,...  GENERATED
│
├── docs                   : Documentation of the algorithms, flagging,...
│
└── src                    : source code for the analysis 
```

## Contributors

[List of contributors](https://github.com/DOV-Vlaanderen/groundwater-logger-validation/graphs/contributors)

## License

[BSD 3-Clause License](https://github.com/DOV-Vlaanderen/groundwater-logger-validation/blob/master/LICENSE) for the code and documentation in this repository. The included data is released under another license, which depends on the data provider. For groundwater logger data access, check the [DOV portal](https://www.dov.vlaanderen.be/portaal/?module=verkenner#ModulePage) or machine access using [pydov](https://pydov.readthedocs.io/en/stable/).


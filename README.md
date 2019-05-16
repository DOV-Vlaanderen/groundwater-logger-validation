# groundwater-logger-validation

Analysis on validation methods for groundwater logger data 

## Rationale

...

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


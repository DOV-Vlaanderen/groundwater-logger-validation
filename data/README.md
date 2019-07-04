# Example data from the partners

The example data provided to test the algorithms are contained in the `raw` data folder, with a subfoler for each of the partners providing data.

## INBO

### Procedure
csv-test files are generated from the WATINA production database by a powershell-script

### Filenames
#### General structure  
"SamplePoint_LoggerSerialnumber.csv" e.g. DYLP223X_G4340.csv

#### Structure of SamplePoint  
logically structured 8 character code:   
  * **DYL** = regioncode (e.g. DYL = Dijlevallei). Air pressure measurements are always linked to the dummy region BAO  
  * **P** = type of sample point  (P=Piezometer/Peilbuis, S=Peilschaal, L=Lucht (only used for BAO samplepoints)
  * **223** = running number of samplepoint (reseeds to 1 for every region)  
  * **X** = version of samplepoint (X = initial version and then A, B, C etc.)

### Fields

Field | Description | Remarks
------------- | ------------- | -------------
PPNT_ID | Watina unique identifier of samplepoint | 
PPNT_CDE | Samplepoint code | 
DRSO_ID | Watina unique identifier of logger | 
DRSO_SER_NBR | Serial number of logger | not unique due to different suppliers
DRME_ID | Watina unique identifier of measurement | unique identifier for the outcome of the algorithms
DRME_OCR_UTC_DTE | Date of measurement (datetimeoffset UTC) | 
DRME_DRU | Measurement value (pressure) | NULL allowed
DRME_TPU | Measurement value (temperature) | NULL allowed
DRME_DMTP_CDE | Type of measurement | LDME = air pressure, DRME = pressure
DRME_DMST_CDE | Validation status of measurement | ENT (entered) is the basic status meaning that these measurements are used in the workflow; DEL (deleted) and INV (invalid) are measurements that were visually detected by users as being suspicious, hence those that have to be detected by the algorithm. --> **no constistent use in the database, of little value to evaluate the algorithms**


## Geotechniek

De unieke naam van de diver is terug te vinden bij de Location (hier ff_b29d). Deze naam zit meestal ook verwerkt in de filenaam.

Bovenin wordt eerst een deel info gegeven over de logger, de opgemeten parameters (+ eenheden) en de periode waarin werd gemeten.
Onze loggers meten standaard de druk in cm (Channel 1) en de temperatuur in °C (Channel 2) binnen een bepaald bereik.

De eigenlijke meetdata wordt in het blokje [Data] weergegeven:
Eerst wordt het aantal metingen weergegeven (hier = 12763)
Vervolgens wordt de het tijdstip van de meting (kolom 1), de meetwaarde voor de druk (kolom 2 => Channel 1) en de meetwaarde voor de temperatuur (kolom 3 = Channel 2) gegeven.

De aangeleverde data omvat:

3 divers van dossier GEO-08/051:

  * ff_b29d
  * ff_b51d
  * ff_b59d
  
4 divers van dossier GEO-10/108:
  * pp01-1
  * pp08-2
  * pp09-1
  * pp09-2
  
5 divers van dossier GEO-15/079:
  * born_pb71
  * born_pb94d
  * born_pb94o
  * born_pb112d
  * born_pb112o

Voor elke diver zijn meerdere MON-files beschikbaar. Elke file bevat de metingen voor een andere periode (over het algemeen volgend deze periodes elkaar op)

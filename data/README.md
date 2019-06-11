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

...

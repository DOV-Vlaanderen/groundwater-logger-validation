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

## VMM - Grondwater

### Procedure
3 kinds of files:  
- txt-files are generated from **Keller** Levelogger 4 software - raw measured pressures in mbar, no compensation for air pressure or conversion to water column/depth beneath reference level
- csv-files from **Solinst** software - converted but not compensated water column in meters above sensor and temperature data in °C. The files are stored in *groundwater-logger-validation/data/raw/vmm/Slugtest/
These data can be compared to small pumping tests, water head changes are nearly instantaneous at the start of every test, about 60 cm, followed by a slower recovery, rising or falling.
- **Ellitrack** files: The pressure sensors are mechanically and instantaneously compensated for air pressure. The sensor is fitted with a vented cable that so the atmospheric pressure "pushes" the water pressure membrane down. We haven't currently got access to the raw measured pressures (but these could be recalculated from the data, making use of their conversion formula's). The files contain depth of water below reference level. From their user manual we find the conversion formula that is used:
   
   pressure of 1 m watercolumn = density of non saline water (rhosT, kg/m<sup>3</sup>) * gravity (g, m/s<sup>2</sup>)  
   rhosT = 1000*(1 - (T+288.9414)/(508929.2*(T+68.12963))\*(T-3.9863)<sup>2</sup>)  
   g = standard value in Ellitrack, can be changed, is 9.812 m/s<sup>2</sup) for all files  

   Example for *Meetpunt '2193-Abeek/Lossing' - 'G2.4' (18032111)*:
   water level on 2019-08-01 at 07:00:00	= -210.13	cm  
   temperature = 13.8 °C  
   cable length = 382.65 cm  (fixed during whole period)
   water column = 382.65 - 210.13 = 172.52 cm = 1.7252 m
   rhosT = 1000*(1 - (13.8+288.9414)/(508929.2*(13.8+68.12963))\*(13.8-3.9863)<sup>2</sup>) =  999.301 kg/m<sup>3</sup>  
   pressure 1 m water = 999.3007395 * 9.812 = 9805.139 Pa/m = 0.09805139 bar/m  
   original pressure mesured = 1.7252 * 0.09805139 = **0.16916 bar**
   

### Filenames
#### General structure 
- Keller : "SamplePointNumber_LocationName_Date_time_Start.txt" e.g. 3-0057_ROLLEGEM_06082009_13210800.txt
- Solinst : "loggerID_LocationName(with underscores)\_date.csv" e.g. 22006280_2_0105_F2_2013_06_06.csv
- Ellitrack : "export-LoggerID-FromeDate_ToDate_LoggerName.txt" e.g. export-18032111-2018-06-26_0715-2019-08-01_0700-G2.4.txt  


#### Structure of SamplePoint
**Keller**  
a file with header and data:  

* Date/Time	: e.g. 6/08/2009 13:21:08
* P1-P2: compensated water presuure, certain loggers measure air and water presure with two sensors
* P1: raw watercolumn + air presuure	e.g. 3,192
* P2: air presure, if mesured
* T: some sensors measure temperature as T
* TOB1: water temperature, removed in proposed test files for most files
* TOB2: air temperature, if measured

**Solinst**  
composed of a first section with metadata e.g.: 

Serial_number:  
2006280  
Project ID:  
Slugtest  
Location:  
2-0105 F2  
LEVEL  
UNIT: m  
Offset: 0.000000 m  
TEMPERATURE  
UNIT: °C  

and the data under a header:
* Date: YYYY/MM/DD e.g. 2013/06/06
* Time: hh:mm:ss e.g. 12:57:35
* ms: milliseconds possible values are 0, 125, 250, 500, 750, 875
* LEVEL: water level in meter above sensor, not compensated for atmospheric pressure e.g. 10.3675
* TEMPERATURE: in °C e.g. 26.222  

**Ellitrack**  
One empty row
header:  
* Datum: date&time e.g. 2018-06-26 07:15:00
* Waterstand: water level beneath reference level, negatif values are down, higher values are lower water levels	in cm e.g. -194.76
* Temperatuur water: water temperature in °C e.g. 13.26 	
* Temperatuur intern: logger temperature in °C, used as instrument control e.g. 10.49

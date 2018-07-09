# Bellevue_for_R dataset
This data is derived from the admissions ledgers for New York City's Bellevue Almshouse. The archived ledgers are housed at the New York Municipal Archives (to read more about the archival materials on which this dataset is based, see the <a href="http://www.archives.nyc/almshouse/">Almshouse Ledgers website</a>).  The ledgers were transcribed between 2003 and 2007. The years 1845-1848 were transcribed. The transcriptions included all inmates whose nationality was classified as "Irish."  These inmates made up the majority of inmates in the period under review.

The intention of this dataset was to document Irish-born people in New York City's nascent public health system.

Bellevue_for_R.csv is a cleaned version of this dataset.  It does not currently include the names of inmates, the ships they arrived on, or the agents who covered their bond in New York City.

To read more about this data, see the <a href="http://www.nyuirish.net/almshouse/">Digital Almshouse Project website</a>

# Data Dictionary
Bellevue_for_R

|Field Name|Data Type|Description|
|----------|---------|-----------|
|Index|Integer|Unique ID for each admission record|
|date_in|Date (yyyy-mm-dd)|Date of admission to the Bellevue Almshouse|
|age_standard|Integer|Age at admission, converted from variable written text and rounded (i.e. "9 mo." in the ledgers is rendered as 1 here)|
|gender|Variant Character Field|Gender of inmate (m or f)|
|Occupation|Variant Character Field|Listed occupation of inmate|
|reason_cleaned|Variant Character Field; Linked Data Field|The "disease" for which the inmate was admitted|
|admittor_1_cleaned|Variant Character Field|One of two possible individuals or organizations listed in the "By Whom Sent" column of the Almshouse ledgers|
|admittor_2_cleaned|Variant Character Field|One of two possible individuals or organizations listed in the "By Whom Sent" column of the Almshouse ledgers|
|site|Variant Character Field|The NY public health site to which the inmate was sent after diagnosis|
|institution|Variant Character Field|The institution within the NY public health site to which the inmate was sent after diagnosis (not all inamtes were sent to a specific institution|
|sent_to|Variant Character Field|Combined field of site and institution|

medical_terms.csv

|Field Name|Data Type|Description|
|----------|---------|-----------|
|bellevue_term_number|Integer|Unique ID for each medical term used in the Bellevue ledgers|
|bellevue_term|Variable Character Field|Medical term entered into the "disease" column of the Bellevue ledgers (standardized for spelling)|
|historical_diagnosis|Variable Character Field|If the medical term from the Bellevue ledger appears in the HHARP medical terms index, the medical term|
|HHARP_description|Variable Character Field|A description of the historical medical term and the year in which it came into use (from the <a href="http://www.hharp.org/medical-terms">Historic Hospital Admissions Records medical terms list</a>.|

# Support
The creation of this dataset was supported by NYU's Glucksman Ireland House.

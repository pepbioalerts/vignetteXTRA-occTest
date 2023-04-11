# What does occTest do?

*occTest* implements a workflow to integrate, assess and control data quality of species occurrences, and is especially designed species distribution and environmental analysis and modeling. The quality control workflow follows a modular procedure (Figure 1; see Serra-Diaz et al. 2018 and Figure 1) and performs multiple tests to identify potential errors in occurrence data using geographical and environmental information. As a second step, using the *occFilter* function allows to subset the initial dataset according to some desired accepted level of error on different test types.

The packages is based on a combination of new created functions by J.M. Serra-Diaz, C.Merow and B.Maitner, J. Borderieux and A.Callebaut, and a wrapper of functions from packages biogeo (Patterson et al. 2016) and CoordinateCleaner(Zizka et al. 2019) among others. Tests are organized in a modular fashion in types of test (testType, e.g. all tests that identify issues in coordinates), and test blocks ('testBlocks', e.g., all tests that should be used for filtering vs. other test that characterize the accuracy).These helps including future tests and at the same time summarize results across different types of tests, and different blocks.

## Workflow overview and data needed

The main function, that implements the workflow is called *occTest*. The workflow is divided in several steps detailed below, but it consist of two main procedures: the filtering procedure and the analysis procedure (Fig.1).

The filtering step flags occurrences that are not usable for SDMs and a strong correction of the coordinates is needed (see *biogeo* package for potential solutions), for instance locations in the sea for terrestrial species or the absence of coordinates (NA) in the record. The analysis step applies in order to assess potential errors, and how these may affect the model. These multiple tests are grouped in test blocks. These identify the nature of the error: geography, land use, environment, time and allow for a quick assessment of the dataset.

The workflow NEEDS a MINIMUM of THREE parameters:

1.  Species name (sp.name; character)

2.  Species occurrence dataset (sp.table; data.frame) The species occurrence data.frame contains the presence records in latitude-longitude WGS84 geographic coordinate system, with column name 'decimalLongitude' for longitude and column name 'decimalLatitude' for latitude (NOTE: these column names can be flexible and user defined)

3.  Environmental raster (r.env; rasterStack), with quantitative continuous variables such as a climate. The environment raster is a rasterStack object that contains the climate variables intended for use in an SDM or another analysis relating species occurrence and climate.

``` r
#load data
library(spocc)
df <- occ(query = 'Martes martes')
occ.data <- occ2df(df)

#we change the names here but we could just change it in the tableSettings (we will introduce settings see that later)
names(occ.data)[2] <- 'decimalLongitude'
names(occ.data)[3] <- 'decimalLatitude'

#load needed environmental data
library(raster)
```

    ## Loading required package: sp

``` r
environmentRaster = raster::getData(name='worldclim',var='bio', res=10,path = tempdir())

#test occurrences (with minimum parameters)
library(occTest)
```

    ## To get started, see the vignettes outside CRAN in GitHub at 
    ##  'pepbioalerts/vignetteXTRA-occTest'

``` r
outMartesMartes = occTest::occTest(sp.name='Martes martes',
                                           sp.table = occ.data,
                                           r.env = environmentRaster)
```

    ## Initial checks and formatting started...

    ## Output directory not specified. Set to a temporary directory

    ## Initial checks and formatting: 2 sec elapsed

    ## Filter major coordinate Issues started...

    ## Filter major coordinate Issues: 0.07 sec elapsed

    ## Filter duplicates started

    ## Filter duplicates: 1 sec elapsed

    ## Resolving coastal reassignment started...

    ## Resolving coastal reassignment: 4.43 sec elapsed
    ## [1] "INFO: Species with no associated country. We assume all locations are native range"
    ## [1] "INFO: No invasive country provided. Analysis of invasive ranges not performed"
    ## [1] "INFO: No info on table of country of registration. Analysis of country recorded vs. coordinates not performed"
    ## [1] "No country reported in occurrence database"
    ## [1] "No info of native country. Assuming all records are native"
    ## [1] "No info of invasive countries"
    ## Resolving countryStatusRange Analysis: 0.08 sec elapsed
    ## Centroid detection: 3.66 sec elapsed
    ## Land Use Land Cover analysis: 7.56 sec elapsed
    ## Institution locality: 2.2 sec elapsed
    ## Records in land use: 0 sec elapsed
    ## geographic outliers detection: 1.27 sec elapsed
    ## Environmental outliers: 1.5 sec elapsed
    ## [1] "No dataset field provided. Assuming all records are from the same dataset"
    ## [1] "no coordinate accuracy/uncertainty field provided"
    ## geoEnvironmental accuracy: 4.01 sec elapsed
    ## time accuracy: 0.02 sec elapsed
    ## Analysis phase:: 20.22 sec elapsed
    ## Preparing and Writing outputs: 0 sec elapsed
    ## 27.8 sec elapsed

## *occTest* output

The outputs of occTest is a a data.frame with coordinates, and a number of additional columns that inform of tests, corrections and summary measures of tests grouped by kinds of tests.

``` r
#check the output
class(outMartesMartes)
```

    ## [1] "occTest"    "data.frame"

``` r
#number of records
nrow (outMartesMartes)
```

    ## [1] 500

``` r
#number of variables
ncol(outMartesMartes)
```

    ## [1] 115

``` r
#take a look at the names of the new variables
head(names(outMartesMartes),20)
```

    ##  [1] "name"                           "decimalLongitude"               "decimalLatitude"               
    ##  [4] "prov"                           "date"                           "key"                           
    ##  [7] "Species"                        "elev"                           "locality"                      
    ## [10] "countryRecorded"                "time"                           "taxonobservationID"            
    ## [13] "x_original"                     "y_original"                     "Correction"                    
    ## [16] "Modified"                       "Exclude"                        "Reason"                        
    ## [19] "comments"                       "coordIssues_coordMissing_value"

### Inspect the results

The outputs of the different tests are available to users in the output table. Users will thus be able to classify or scrub the occurrences the way they want to. The names of the columns show this rationale:

$$testType$$\_$$testName$$\_value: numeric or logical. Shows the quantitative result of the test (sometimes the same as in the result of the \_test)

$$testType$$\_$$testName$$\_test: logical Shows whether the occurrence passes or not the test, being T a flag for a wrong record and NA indicating that the test was not performed on that record.

$$testType$$\_$$testName$$\_comment: character. Shows some comments related to the specific test.

$$testType$$\_score: numeric. Averages the results of different tests performed. It informs, for a given set of tests what is the percentatge of test that flagged the record.

Examples: *HumanDetection_HumanInfluence_value* gives you the score of current human influence in the record *HumanDetection_HumanInfluence_test* gives you whether we consider the former value an error/bias (T) or not (F) *HumanDetection_HumanInfluence_comment* gives you a commen that give further detail on the analysis. In this case that the threshold of 45 was used for the test. *HumanDetection_score* summarizes all the other HumanDetection tests and outputs a value from 0 to 1. A value of 0.5 would indicate that half of the tests used indicate that is an a Human signal in the record.

| centroidDetection_CoordinateCleaner_test | centroidDetection_CoordinateCleaner_comment | centroidDetection_score | HumanDetection_HumanInfluence_value | HumanDetection_HumanInfluence_test | HumanDetection_HumanInfluence_comments | HumanDetection_UrbanAreas_value |
|:-----------------------------------------|:--------------------------------------------|------------------------:|------------------------------------:|:-----------------------------------|:---------------------------------------|--------------------------------:|
| FALSE                                    |                                             |                       0 |                                  50 | TRUE                               | Threshold influence= 45                |                               1 |
| FALSE                                    |                                             |                       0 |                                  32 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  40 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  19 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  25 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  50 | TRUE                               | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  39 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  NA | NA                                 | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  22 | FALSE                              | Threshold influence= 45                |                               0 |
| FALSE                                    |                                             |                       0 |                                  59 | TRUE                               | Threshold influence= 45                |                               1 |

Here is the metadata to characterize the different tests and the hierarchy grouping blocks : testTypes and TestBlocks

|     | phase  | testBlock | testType            | method                  |
|:----|:-------|:----------|:--------------------|:------------------------|
| 1   | filter | filter    | coordIssues         | missing                 |
| 2   | filter | filter    | coordIssues         | invalCoord              |
| 3   | filter | filter    | coordIssues         | zeroCoord               |
| 4   | filter | filter    | coordIssues         | coordConv               |
| 5   | filter | filter    | duplicates          | dupGrid                 |
| 6   | filter | filter    | duplicates          | dupExact                |
| 7   | test   | geo       | countryStatusRange  | wrongNTV                |
| 8   | test   | geo       | countryStatusRange  | wrongCTRY               |
| 9   | test   | geo       | countryStatusRange  | wrongINV                |
| 10  | test   | geo       | centroidDetection   | BIEN                    |
| 11  | test   | geo       | centroidDetection   | CoordinateCleaner       |
| 12  | test   | lu        | humanDetection      | HumanInfluence          |
| 13  | test   | lu        | humanDetection      | UrbanAreas              |
| 14  | test   | lu        | landUseType         | wrongLU                 |
| 15  | test   | lu        | institutionLocality | fromBotanicLocalityName |
| 16  | test   | lu        | institutionLocality | fromCoordinates         |
| 17  | test   | geo       | geoOutliers         | alphaHull               |
| 18  | test   | geo       | geoOutliers         | distance                |
| 19  | test   | geo       | geoOutliers         | median                  |
| 20  | test   | geo       | geoOutliers         | quantileSamplingCorr    |
| 21  | test   | geo       | geoOutliers         | Grubbs                  |
| 22  | test   | env       | envOutliers         | missingEnv              |
| 23  | test   | env       | envOutliers         | bxp                     |
| 24  | test   | env       | envOutliers         | Grubbs                  |
| 25  | test   | geo       | geoenvLowAccuracy   | lattice                 |
| 26  | test   | env       | geoenvLowAccuracy   | percDiffCell            |
| 27  | test   | geo       | geoenvLowAccuracy   | envDiff                 |
| 28  | test   | geo       | geoenvLowAccuracy   | elevDiff                |
| 29  | test   | time      | timeAccuracy        | noDate                  |
| 30  | test   | time      | timeAccuracy        | noDateFormatKnown       |
| 301 | test   | time      | timeAccuracy        | outDateRange            |

## Customizing your analysis

Many specifications may be set to the different tests, although not all will be useful in all cases. Bear in mind that a lot of tests are available, but performing more tests means more time, and also implies you need to provide more information or parameters. The version currently implemented is that of minimalSettings().

We guide you here to set specific parameters to your analysis. All the settings are accessible and can be modified. The settings for *occTest* are organized in a list of four tables.

1.  tableSettings : providing the specifics of your input data table of occurrence records
2.  analysisSetting : providing the specifics of each test
3.  writeoutSettings: providing the specifics of which and where the outputs should be stored

### tableSettings: when you have more than xy in your table

Some occurrence data has more than x and y coordinates. If that is the case you can provide important information. The parameter *tableSettings* consists of a list with the following elements that should be identified. Note that even if you want to change only one value of the default you need to provide a full list, with each named elements.

| Named element in list |                                      Description                                       |     Value |
|-----------------------|:--------------------------------------------------------------------------------------:|----------:|
| taxonobservation.id   |             Name of the colum in the species table for the observation ID              | character |
| x.field               |                Name of the colum in the species table for the longitude                | character |
| y.field               |                Name of the colum in the species table for the latitude                 | character |
| t.field               |            Name of the colum in the species table for the observation date             | character |
| l.field               |              Name of the colum in the species table for the locality name              | character |
| c.field               |            Name of the colum in the species table for the REPORTED COUNTRY             | character |
| e.field               |     Name of the colum in the species table for the REPORTED ELEVATION (in meters)      | character |
| a.field               | Name of the colum in the species table for the accuracy (in meters) of the coordinates | character |
| ds.field              |           Name of the colum in the species table for the source of the data            | character |

### analysisSettings: when you want to select or change the tests to be performed

Do you want to set up different implementations of the analysis? Set up your own parameters. The parameter *analysisSettings* consists of a list with the following elements that should be identified. Note that even if you want to change only one value of the default you need to provide the full list, with each named elements.

| Named element in list                                                                                                                              |                                                            Description                                                            |                                                                                   Value |
|----------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------:|----------------------------------------------------------------------------------------:|
| $geoSettings$coordinate.decimal.precision                                                                                                          |                                                decimal precision of the coordintes                                                |                                                                                 numeric |
| $geoSettings$points.proj4string                                                                                                                    |                                                         Projection to use                                                         | Proj4 character. CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") |
| $rangeAnalysis$countryfield.shapefile                                                                                                              |               spatialPolygonsDataFrame with the countries and their associated names or ISO3 codes in the dataframe               |                     spatialPolygonsDataFrame. Defaults to rnaturalearth::ne_countries() |
| $rangeAnalysis$doRangeAnalysis                                                                                                                     |   whether the user wants to do perform the analysis of comparing coordinates countries to reported native or invasive countries   |                                                                  logical. Dafaults to T |
| $rangeAnalysis$excludeUnknownRanges                                                                                                                |                           keep the analysis for occurrences out of the known native or alien countries?                           |                                                                  logical. Defaults to F |
| $rangeAnalysis$doCountryRecordAnalysis                                                                                                             |    whether the user wants to do perform the analysis of comparing reported countries to reported native or invasive countries     |                                                                  logical. Dafaults to T |
| $rangeAnalysis$excludeNotmatchCountry keep the analysis for occurrences when the reported country is different than the native or alien countries? |                                                      logical. Defaults to F                                                       |                                                                                         |
| $centroidAnalysis$doCentroidDetection                                                                                                              |                              whether the user wants to do perform the analysis of centroid detection                              |                                                                  logical. Dafaults to T |
| $centroidAnalysis$methodCentroidDetection                                                                                                          |             methods to use to detect centroids. Implemented are: 'BIEN','speciesGeoCodeR','CoordinateCleaner', 'all'              |                                                            character. Dafaults to 'all' |
| $humanAnalysis$doHyperHumanDetection                                                                                                               |                               whether the user wants to do perform the analysis of human influence                                |                                                                  logical. Dafaults to T |
| $humanAnalysis$methodHyperHumanDetection                                                                                                           |                     what methods to use to detect high human influence Implemented are: 'hii','urban', 'all'                      |                                                            character. Dafaults to 'all' |
| $humanAnalysis$th.human.influence                                                                                                                  |                          when methods are 'all' or 'hii' what is the threshold value for human influence                          |                                                                 numeric. Dafaults to 40 |
| $humanAnalysis$ras.hii                                                                                                                             |                                                raster of the human influence index                                                |                                          raster. Defaults to SEDAC human infuence index |
| $institutionAnalysis$doInstitutionLocality                                                                                                         |               whether the user wants to do perform the analysis of records potentialy in biodiversity institutions                |                                                                  logical. Dafaults to T |
| $institutionAnalysis$methodInstitutionLocality                                                                                                     | methods to use to detect records in biodiversity institutions Implemented are: 'fromBotanicLocalityName','fromCoordinates', 'all' |                                                            character. Defaults to 'all' |
| $geooutliersAnalysis$doGeoOutliers                                                                                                                 |                            whether the user wants to do perform the analysis of geographical outliers                             |                                                                  logical. Dafaults to T |
| $geooutliersAnalysis$methodGeoOutliers                                                                                                             |  methods to use to geographic outliers. Implemented are: 'alphaHull','distance','median','quantSamplingCorrected','grubbs, 'all'  |                                                            character. Defaults to 'all' |
| $geooutliersAnalysis$alpha.parameter                                                                                                               |                                          alpha parameter desired for method 'alphaHull'                                           |                                                                  numeric. Defaults to 2 |
| $envoutliersAnalysis$doEnvOutliers                                                                                                                 |                             whether the user wants to perform the analysis of environmental outliers                              |                                                                  logical. Dafaults to T |
| $envoutliersAnalysis$methodEnvOutliers                                                                                                             |                         methods to use to environmental outliers. Implemented are: 'bxp','grubbs', 'all'                          |                                                            character. Defaults to 'all' |
| $envoutliersAnalysis$th.perc.outenv                                                                                                                |                for method 'bxp' what is the percentage of variables found as an outlier to define it as an outlier                |                                                                Numeric. Defaults to 0.2 |
| $accuracyAnalysis$do.geoEnvAccuracy                                                                                                                |                                      whether the user wants to perform the accuracy analysis                                      |                                                                  logical. Dafaults to T |
| $accuracyAnalysis$methodGeoEnvAccuracy                                                                                                             | methods to use for the accuracy analysis. Implemented are: 'envDeviation','percDiffCell', 'elevDiff','lattice','rasterCell','all' |                                                            character. Defaults to 'all' |
| $accuracyAnalysis$elev.quality.threshold                                                                                                           |                     for elevDiff method, threshold of the difference between reported and inferred elevation                      |                                                                numeric. Defaults to 100 |

### writeoutSettings: do you want to write the outputs?

The user can specify if outputs are to be written.

| Named element in list  |                   Description                    |                          Value |
|------------------------|:------------------------------------------------:|-------------------------------:|
| \$output.dir           |           Name of the output directory           |     character.Defaults to NULL |
| \$writeAllOutput       |          should all outptus be written           |         logical. Defaults to F |
| \$write.simple.output  |      should simple output table be written       |         logical. Defaults to F |
| \$write.simple.output  |       should full output table be written        |         logical. Defaults to F |
| \$output.base.filename | what is the suffix of the output table filename? | character. Deafaults to 'QAQC' |

### An example of how to retrieve and modify settings

In this example we are going to turn off tests related to geoenvironmental accuracy analysis because our data has not enough information on coordinate uncertainty. $$NOTE: even under such
cirumstances where you do not have that data,the result of the test
would be NA and would not affect the result$$

``` r
#load minimal settings
mySettings <- minimalSettings()
#change the attribute of interest (in this case setting an analysis off)
mySettings$analysisSettings$accuracyAnalysis$do.geoEnvAccuracy = F
#add your settings to the function
out = occTest::occTest(sp.name='Martes martes', sp.table = occ.data,r.env = environmentRaster, analysisSettings = mySettings$analysisSettings,verbose = F)
```

    ## Initial checks and formatting started...

    ## Output directory not specified. Set to a temporary directory

    ## Initial checks and formatting: 0.22 sec elapsed

    ## Filter major coordinate Issues started...

    ## Filter major coordinate Issues: 0.03 sec elapsed

    ## Filter duplicates started

    ## Filter duplicates: 0.02 sec elapsed

    ## Resolving coastal reassignment started...

    ## Resolving coastal reassignment: 5.23 sec elapsed
    ## [1] "INFO: Species with no associated country. We assume all locations are native range"
    ## [1] "INFO: No invasive country provided. Analysis of invasive ranges not performed"
    ## [1] "INFO: No info on table of country of registration. Analysis of country recorded vs. coordinates not performed"
    ## [1] "No country reported in occurrence database"
    ## [1] "No info of native country. Assuming all records are native"
    ## [1] "No info of invasive countries"
    ## Resolving countryStatusRange Analysis: 0.14 sec elapsed
    ## Centroid detection: 4.86 sec elapsed
    ## Land Use Land Cover analysis: 6.03 sec elapsed
    ## Institution locality: 2.61 sec elapsed
    ## Records in land use: 0 sec elapsed
    ## geographic outliers detection: 0.83 sec elapsed
    ## Environmental outliers: 1.74 sec elapsed
    ## [1] "No dataset field provided. Assuming all records are from the same dataset"
    ## [1] "no coordinate accuracy/uncertainty field provided"
    ## geoEnvironmental accuracy: 6.46 sec elapsed
    ## time accuracy: 0 sec elapsed
    ## Analysis phase:: 22.53 sec elapsed
    ## Preparing and Writing outputs: 0 sec elapsed
    ## 28.17 sec elapsed

# Example 1: The hands-off analysis (default). Classifying and filtering

We can perform a quick test with all defaults. This has a minimal number of inputs and therefore a lot of the tests cannot be performed (note the large amount of NAs in the outputs. These minimal inputs are the data.frame of species occurrence and environmental variables. Parmeter settings are minimal, and can be displayed using the minimalSettings() function

``` r
#load packages
library(occTest)
library(spocc)

df <- occ(query = 'Pseudotsuga menziesii')
occ.data <- occ2df(df)

#we change the names here but we could just change it in the tableSettings
names(occ.data)[2] <- 'decimalLongitude'
names(occ.data)[3] <- 'decimalLatitude'

#load needed environmental data
library(raster)
renv = raster::getData(name='worldclim',var='bio', res=10,path = tempdir())

#test occurrence
library(occTest)
outDougFir = occTest(sp.name='Pseudotsuga menziesii',
                                sp.table = occ.data,
                                r.env = renv,verbose = F)
```

    ## Initial checks and formatting started...

    ## Output directory not specified. Set to a temporary directory

    ## Initial checks and formatting: 2.61 sec elapsed

    ## Filter major coordinate Issues started...

    ## Filter major coordinate Issues: 0.11 sec elapsed

    ## Filter duplicates started

    ## Filter duplicates: 0.72 sec elapsed

    ## Resolving coastal reassignment started...

    ## Resolving coastal reassignment: 3.46 sec elapsed
    ## [1] "INFO: Species with no associated country. We assume all locations are native range"
    ## [1] "INFO: No invasive country provided. Analysis of invasive ranges not performed"
    ## [1] "INFO: No info on table of country of registration. Analysis of country recorded vs. coordinates not performed"
    ## [1] "No country reported in occurrence database"
    ## [1] "No info of native country. Assuming all records are native"
    ## [1] "No info of invasive countries"
    ## Resolving countryStatusRange Analysis: 0.15 sec elapsed
    ## Centroid detection: 7.49 sec elapsed
    ## Land Use Land Cover analysis: 9.5 sec elapsed
    ## Institution locality: 13.06 sec elapsed
    ## Records in land use: 0.01 sec elapsed
    ## geographic outliers detection: 0.97 sec elapsed
    ## Environmental outliers: 0.74 sec elapsed
    ## [1] "No dataset field provided. Assuming all records are from the same dataset"
    ## [1] "no coordinate accuracy/uncertainty field provided"
    ## geoEnvironmental accuracy: 16.28 sec elapsed
    ## time accuracy: 0 sec elapsed
    ## Analysis phase:: 48.06 sec elapsed
    ## Preparing and Writing outputs: 0 sec elapsed
    ## 55.11 sec elapsed

``` r
#check outputs
#knitr::kable(head(as.data.frame(outDougFir))')
knitr::kable(outDougFir[1:8,1:20],"markdown")
```

|     | name                                 | decimalLongitude | decimalLatitude | prov | date       | key        | Species               | elev | locality | countryRecorded | time | taxonobservationID | x_original | y_original | Correction | Modified | Exclude | Reason               | comments | coordIssues_coordMissing_value |
|:----|:-------------------------------------|-----------------:|----------------:|:-----|:-----------|:-----------|:----------------------|:-----|:---------|:----------------|:-----|-------------------:|:-----------|:-----------|:-----------|:---------|--------:|:---------------------|:---------|:-------------------------------|
| 1   | Pseudotsuga menziesii (Mirb.) Franco |        -122.6551 |         45.4617 | gbif | 2023-01-01 | 4011713183 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  1 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |
| 2   | Pseudotsuga menziesii (Mirb.) Franco |        -123.6375 |         48.3257 | gbif | 2023-01-01 | 4011545165 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  2 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |
| 3   | Pseudotsuga menziesii (Mirb.) Franco |        -123.3579 |         48.4116 | gbif | 2023-01-01 | 4022185606 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  3 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |
| 4   | Pseudotsuga menziesii var. menziesii |        -121.9734 |         47.1737 | gbif | 2023-01-01 | 4011853219 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  4 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |
| 209 | Pseudotsuga menziesii var. menziesii |        -121.9755 |         47.1706 | gbif | 2023-01-01 | 4011705219 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  5 | NA         | NA         | NA         | NA       |       1 | Duplicated--GridCell |          | FALSE                          |
| 210 | Pseudotsuga menziesii (Mirb.) Franco |        -123.3860 |         48.4377 | gbif | 2023-01-01 | 4011680169 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  6 | NA         | NA         | NA         | NA       |       1 | Duplicated--GridCell |          | FALSE                          |
| 5   | Pseudotsuga menziesii (Mirb.) Franco |        -104.8310 |         31.9152 | gbif | 2023-01-01 | 4011606187 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  7 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |
| 6   | Pseudotsuga menziesii (Mirb.) Franco |        -124.0903 |         49.2258 | gbif | 2023-01-01 | 4011548227 | Pseudotsuga_menziesii | NA   | NA       | NA              | NA   |                  8 | NA         | NA         | NA         | NA       |       0 | NA                   |          | FALSE                          |

Then we can use *occFilter* to filter the data according to different acceptance levels of issues in each record. You can have three broad thresholds for filtering data: relaxed (accept up 70% of tests showing issues), strict (20%) or majority (50%). These percentages are applied to groups of tests depending on their nature (testBlocks or testTypes). For instance, a relaxed approach to filtering will filter a record if for a given test groups (e.g. environmental data tests, geographic tests, human detection tests) more than 70% of the tests fail.

``` r
#render_markdown( occTest::show_tests())
```

The result is a list where the first element fitleredDataset corresponds to the filtered occurrence records and the second element summaryStats provides the summary statistics of the number of tests performed.

``` r
#filter data
outDougFirSelec = occFilter(df = outDougFir,errorAcceptance = 'relaxed')

#show filtered dataset
nrow(outDougFir)
```

    ## [1] 500

``` r
nrow(outDougFirSelec[["filteredDataset"]])
```

    ## [1] 207

Show summary values of tests, grouped by

``` r
outDougFirSelec = occFilter(df = outDougFir,errorAcceptance = 'relaxed')

#show filtered dataset
knitr::kable(head(outDougFirSelec[["summaryStats"]]),format="markdown")
```

| geo_score | geo_rateTestPerformed | geo_NtestsPerformed |  lu_score | lu_rateTestPerformed | lu_NtestsPerformed | env_score | env_rateTestPerformed | env_NtestsPerformed | time_score | time_rateTestPerformed | time_NtestsPerformed |
|----------:|----------------------:|--------------------:|----------:|---------------------:|-------------------:|----------:|----------------------:|--------------------:|-----------:|-----------------------:|---------------------:|
| 0.1428571 |                   0.5 |                   7 | 0.6666667 |                  0.6 |                  3 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |
| 0.1428571 |                   0.5 |                   7 | 0.0000000 |                  0.4 |                  2 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |
| 0.1428571 |                   0.5 |                   7 | 0.5000000 |                  0.4 |                  2 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |
| 0.1428571 |                   0.5 |                   7 | 0.3333333 |                  0.6 |                  3 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |
| 0.1428571 |                   0.5 |                   7 | 0.0000000 |                  0.6 |                  3 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |
| 0.1428571 |                   0.5 |                   7 | 0.0000000 |                  0.6 |                  3 |         0 |                   0.8 |                   4 |        NaN |                      0 |                    0 |

The function also allows to set your own acceptance threshold level, and change the grouping of thests (see ?occFilter for further info).

``` r
outDougFirSelec = occFilter(df = outDougFir,errorAcceptance = 'relaxed')
names(outDougFirSelec)
```

    ## [1] "filteredDataset" "summaryStats"    "rule"

``` r
outDougFirSelec[['filteredDataset']]
```

    ## # A tibble: 207 × 115
    ##    name    decimalLongitude decimalLatitude prov  date       key   Species elev 
    ##    <chr>              <dbl>           <dbl> <chr> <date>     <chr> <chr>   <lgl>
    ##  1 Pseudo…          -123.              45.5 gbif  2023-01-01 4011… Pseudo… NA   
    ##  2 Pseudo…          -124.              48.3 gbif  2023-01-01 4011… Pseudo… NA   
    ##  3 Pseudo…          -123.              48.4 gbif  2023-01-01 4022… Pseudo… NA   
    ##  4 Pseudo…          -122.              47.2 gbif  2023-01-01 4011… Pseudo… NA   
    ##  5 Pseudo…          -105.              31.9 gbif  2023-01-01 4011… Pseudo… NA   
    ##  6 Pseudo…          -124.              49.2 gbif  2023-01-01 4011… Pseudo… NA   
    ##  7 Pseudo…          -122.              37.5 gbif  2023-01-01 4011… Pseudo… NA   
    ##  8 Pseudo…             9.22            49.6 gbif  2023-01-02 4011… Pseudo… NA   
    ##  9 Pseudo…          -115.              51.1 gbif  2023-01-01 4011… Pseudo… NA   
    ## 10 Pseudo…          -123.              37.9 gbif  2023-01-01 4011… Pseudo… NA   
    ## # ℹ 197 more rows
    ## # ℹ 107 more variables: locality <lgl>, countryRecorded <lgl>, time <lgl>,
    ## #   taxonobservationID <int>, x_original <lgl>, y_original <lgl>,
    ## #   Correction <chr>, Modified <lgl>, Exclude <dbl>, Reason <chr>,
    ## #   comments <chr>, coordIssues_coordMissing_value <lgl>,
    ## #   coordIssues_coordMissing_test <lgl>, coordIssues_invalidCoord_value <lgl>,
    ## #   coordIssues_invalidCoord_test <lgl>, coordIssues_zeroCoord_value <lgl>, …

The output statisics allow you to inspect, for different groupingTestLevels the $$groupLeve$$\_score (the average across specific tests), $$groupLevel$$\_rateTestPerformed (e.g. 0.14 means you performed 14% of the available tests for a given groupLevel) and $$groupLevel$$\_\_rateTesttPerformed_NtestPeformed (e.g. the total number of tests perfomed).

``` r
head(outDougFirSelec[['summaryStats']])
```

    ##   geo_score geo_rateTestPerformed geo_NtestsPerformed  lu_score
    ## 1 0.1428571                   0.5                   7 0.6666667
    ## 2 0.1428571                   0.5                   7 0.0000000
    ## 3 0.1428571                   0.5                   7 0.5000000
    ## 4 0.1428571                   0.5                   7 0.3333333
    ## 5 0.1428571                   0.5                   7 0.0000000
    ## 6 0.1428571                   0.5                   7 0.0000000
    ##   lu_rateTestPerformed lu_NtestsPerformed env_score env_rateTestPerformed
    ## 1                  0.6                  3         0                   0.8
    ## 2                  0.4                  2         0                   0.8
    ## 3                  0.4                  2         0                   0.8
    ## 4                  0.6                  3         0                   0.8
    ## 5                  0.6                  3         0                   0.8
    ## 6                  0.6                  3         0                   0.8
    ##   env_NtestsPerformed time_score time_rateTestPerformed time_NtestsPerformed
    ## 1                   4        NaN                      0                    0
    ## 2                   4        NaN                      0                    0
    ## 3                   4        NaN                      0                    0
    ## 4                   4        NaN                      0                    0
    ## 5                   4        NaN                      0                    0
    ## 6                   4        NaN                      0                    0

check how the groupings changed with different levels and different threshold acceptance levels

``` r
outDougFirSelecstrict = occFilter(df = outDougFir, errorAcceptance = 'strict')
outDougFirSelecMajority = occFilter(df = outDougFir, errorAcceptance = 'majority')
nrow(outDougFirSelecstrict [['filteredDataset']] )
```

    ## [1] 125

``` r
nrow(outDougFirSelecMajority [['filteredDataset']] )
```

    ## [1] 180

``` r
nrow(outDougFirSelec[['filteredDataset']])
```

    ## [1] 207

We can also setup a custom filtering process, by appending a column named `errorThreshold` to the data.frame describing the various tests. The threshold you pick must be the same for every tests of the same type (testBlocks or testTypes), determined by the `by` argument of occFilter. in the example below, we load the data.frame of test types and set a more strict threshold to the geographic test (geo testBlock), and relaxed threshold to the land uses and human influence (lu testBlock) tests.

``` r
custom_rules<-readRDS(system.file('ext/fieldMetadata.rds',package='occTest'))
custom_rules$errorThreshold<-rep(0.5,nrow(custom_rules))
custom_rules[custom_rules$testBlock=="lu","errorThreshold"]<-0.7
custom_rules[custom_rules$testBlock=="geo","errorThreshold"]<-0.2

outDougFirSelectcustom = occFilter(df = outDougFir, custom = custom_rules)
```

## Let's visualize the strict selection with the plotting function

the plot method for the occTest object returned by *occTest* display the initial coordinates filtering.

If the second argument of the plot function is provided, the output of the corresponding *occFilter* call, then then occurrences removed by each testType our testBlock are displayed.

By default, the functon returns a list of ggplot objects, so you can store them. You can also display the plots one by one when storing the result and setting show_plot to TRUE

``` r
list_of_plots <- plot(outDougFir, 
                      occFilter_list = outDougFirSelecstrict, 
                      show_plot = F)
```

    ## Spherical geometry (s2) switched off

    ## Spherical geometry (s2) switched on

``` r
list_of_plots
```

    ## [[1]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-14-1.png)

    ## 
    ## [[2]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-14-2.png)

    ## 
    ## [[3]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-14-3.png)

    ## 
    ## [[4]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-14-4.png)

    ## 
    ## [[5]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-14-5.png)

# Example 2: Making the function turn with more detailed data (and more options)

We will apply *occTest* to a dataset that has more information like the locality names, or the accuracy of the coordinates. For that we need to specify that before running the function.

The example is a Mediterranean species present in France, Catalonia, Spain and Andorra. But the information of the species is however variable. Some records have the date collected, other records have data on the elevation, others have information on the locality, etc...

We will now fully use the information on that table to perform as many tests as possible.

``` r
#load data
spMed = system.file('ext/exampleOccData.csv',package='occTest')
occ.species.nogeoscrub <- read.table(file = spMed,header = T,sep = ',',as.is = T)

#load the raster of the environmental variables
#ras.env.f = system.file('ext/env.rds',package='occTest')
ras.env.f = system.file('ext/AllEnv.tif', package='occTest')
#ras.env = readRDS(ras.env.f)
ras.env = raster::stack(ras.env.f)

######## CHECK THE INPUT DATA TABLE
str (occ.species.nogeoscrub)
```

    ## 'data.frame':    58 obs. of  9 variables:
    ##  $ ID_GRAFIC      : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ MAPX           : num  1.54 2.47 2.72 2.72 2.72 ...
    ##  $ MAPY           : num  42.5 42.5 42.6 42 42 ...
    ##  $ DATE           : chr  "" "" "" "2/23/2018" ...
    ##  $ ELEVATION      : int  NA 121 123 NA NA NA NA NA NA NA ...
    ##  $ COUNTRYRECORD  : chr  "" "" "" "ESP" ...
    ##  $ LOCALITYNAME   : chr  "" "" "" "" ...
    ##  $ UNCERTAINTY_X_M: int  NA 10000 NA NA 34 NA 100 NA 300 NA ...
    ##  $ UNCERTAINTY_Y_M: int  NA 10000 NA NA 34 NA 100 NA 300 NA ...

The table data shows: - Longitude and latitude are named as MAPX and MAPY (MAPX, MAPY) - There is a recorded name of a country in the table (COUNTRYRECORD) - Locality name field (LOCALITYNAME) - Uncertainty field value (UNCERTEINTY_X\_M, UNCERTAINTY_Y\_M) - Date field (DATE)

We will incorporate this information into the workflow. To do so we need to modify the minimalSettings relatedo to table

``` r
mySettings <- occTest::minimalSettings()
mySettings$tableSettings$x.field <- 'MAPX'
mySettings$tableSettings$y.field <- 'MAPY'
mySettings$tableSettings$t.field <- 'DATE'
mySettings$tableSettings$l.field <- 'LOCALITYNAME'
mySettings$tableSettings$c.field <- 'COUNTRYRECORD'
mySettings$tableSettings$e.field <- 'ELEVATION'
mySettings$tableSettings$a.field <- c('UNCERTAINTY_X_M','UNCERTAINTY_Y_M')
```

We have a more detailed raster of elevation that will allow us to know whether the elevation recorded in our dataset is different from that inferred from the terrain model used to build the climate variables

``` r
#load a high resolution raster of elevations
ras.dem.f = system.file('ext/DEM.tif',package='occTest')
ras.dem = raster::raster(ras.dem.f)
```

We have a more detailed raster of human influence than that of the report. In our of map high human influence is considered when a record is in a cell above 50. Let's modify our settings

``` r
#load your own human influence map
someDefaultSettings = readRDS(system.file('ext/exSettings.rds', package='occTest'))
ras.humaninfluence = someDefaultSettings$analysisSettings$humanDetection$ras.hii
#specify in the settings the raster to use for human influence
mySettings$analysisSettings$humanAnalysis$methodHyperHumanDetection <- ras.humaninfluence
#specify in the settings the theshold for to identify high influence records
mySettings$analysisSettings$humanAnalysis$th.human.influence <-50
```

We also have a more detailed data on country borders

``` r
#load detailed country borders and shorelines
countries.pol = someDefaultSettings$analysisSettings$countryStatusRange$countries.shapefile
#specify in the settings the spatial object for countries
mySettings$analysisSettings$rangeAnalysis$countries.shapefile <- countries.pol
#specify in the settings the column name indicating the ISO 3 digit code in the spatial object
mySettings$analysisSettings$rangeAnalysis$countryfield.shapefile <- 'ISO'
```

We also know that the species is native to Spain ('ESP') and considered invasive in France ('FRA') so we will also specify it, and flag anything that is not in these countries

``` r
out <- occTest::occTest(sp.name = 'My species',
                          sp.table = occ.species.nogeoscrub,
                          r.env = ras.env,
                          tableSettings = mySettings$tableSettings,
                          analysisSettings = mySettings$analysisSettings,
                          r.dem =   ras.dem,
                          ntv.ctry = 'ESP',
                          inv.ctry = 'FRA',
                          verbose = T)
```

    ## Initial checks and formatting started...

    ## Output directory not specified. Set to a temporary directory

    ## Initial checks and formatting: 0.26 sec elapsed

    ## **** FILTERING PHASE STARTED  ****

    ## Filter major coordinate Issues started...

    ## Filter major coordinate Issues: 0.08 sec elapsed

    ## Filter duplicates started

    ## **** RESOLVING: duplicates ****

    ## Filter duplicates: 0.03 sec elapsed

    ## Resolving coastal reassignment started...

    ## **** RESOLVING : sea/terrestrial reassignment ****

    ## Assuming a terrestrial species. Modify parameter Habitat otherwise

    ## Resolving coastal reassignment: 8.9 sec elapsed

    ## Resolving countryStatusRange Analysis started...

    ## Resolving countryStatusRange Analysis: 0.16 sec elapsed

    ## **** ANALYSIS PHASE STARTED  ****

    ## Centroid detection started ...

    ## Centroid detection: 13.73 sec elapsed

    ## Land Use Land Cover analysis started ...

    ## Land Use Land Cover analysis: 7.38 sec elapsed

    ## Institution locality started ...

    ## Institution locality: 3.41 sec elapsed

    ## Records in land use started...

    ## Records in land use: 0.01 sec elapsed

    ## geographic outliers detection started

    ## geographic outliers detection: 30.78 sec elapsed

    ## Environmental outliers analysis started...

    ## Environmental outliers: 0.27 sec elapsed

    ## geoEnvironmental accuracy analysis started...

    ## [1] "No dataset field provided. Assuming all records are from the same dataset"
    ## geoEnvironmental accuracy: 5.51 sec elapsed

    ## time accuracy analysis started...

    ## time accuracy: 0.24 sec elapsed
    ## Analysis phase:: 61.33 sec elapsed

    ## Preparing and Writing outputs started ...

    ## Preparing and Writing outputs: 0 sec elapsed
    ## 70.78 sec elapsed

Now, filter the results with a rule based on majority and grouped based on

``` r
outFiltered_byBlock  = occFilter(df = out, errorAcceptance = 'relaxed', by = 'testBlock')
outFiltered_byTestType  = occFilter(df = out, errorAcceptance = 'relaxed', by = 'testType')
```

## Let's plot the results

``` r
list_of_plots<-plot(out, 
                    occFilter_list = outFiltered_byTestType, 
                    show_plot = F)
```

    ## Spherical geometry (s2) switched off

    ## Spherical geometry (s2) switched on

``` r
list_of_plots
```

    ## [[1]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-1.png)

    ## 
    ## [[2]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-2.png)

    ## 
    ## [[3]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-3.png)

    ## 
    ## [[4]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-4.png)

    ## 
    ## [[5]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-5.png)

    ## 
    ## [[6]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-6.png)

    ## 
    ## [[7]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-7.png)

    ## 
    ## [[8]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-8.png)

    ## 
    ## [[9]]

![](Intro_occTest_v2_files/figure-markdown_github/unnamed-chunk-23-9.png)

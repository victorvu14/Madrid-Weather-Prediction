# Madrid-Weather-Prediction

The work consists in analyzing a big dataset with hourly pollution data from Madrid, in the period 2011 to 2016.

There are 4 major parts of the project. 

1. Reading every piece of raw data and creating the whole initial raw_data set.
2. Processing raw_data to create a daily dataset, by averaging each hourly measure, and containing also the weather variables and the
names for each pollutant parameter.
3. Generating a descriptive analysis with correlation matrices, scatterplots, time series charts …
####  4. Creating a linear regression model that explains NO2


## Dataset 

In dataset zip file there are 72 csv files, each one containing a piece of the information. Specifically, each csv contains the raw data for a concrete month of a concrete year (6 years, 12 months per year).
 -  Raw data is at an hourly level.
 - There are many stations that measure air pollutants in Madrid (around 24).
 - There are several pollutants measured (around 10).
 - Hourly data is available for every pollutant, for every station, for every hour, for every day of each month-year.

There are also two additional datasets:
 - Weather.xlsx, containing daily time series for min, average, and max temperature, precipitations, humidity and wind in Madrid.
 - parameters.png (image), containing the key for every pollutant code (eg. “08” - NO2)



code to accompany "Multiple Environmental Conditions Precede Ebola Spillovers in Central Africa" by Baranowski and Bharti, 2025, submitted to Biology letters

Data for this manuscript come from open-source data: 
MODIS MYD13Q1 16-day EVI from Google Earth Engine: https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13Q1
summarized as field "spmean_EVI" in csv files in "environmental_time_series_DTW.zip"

ERA5-Land post-processed daily statistics from 1950 to present 2m-temperature from the Climate Data Store: https://cds.climate.copernicus.eu/datasets/derived-era5-land-daily-statistics?tab=overview
summarized as field "spmean_temp" in csv files in "environmental_time_series_DTW.zip"

CHIRPS Daily Rainfall from R API code: https://data.chc.ucsb.edu/products/CHIRPS-2.0/
summarized as field "spmean_rain" in csv files in "environmental_time_series_DTW.zip"

Landscan Annual Human Population Counts from Google Earth Engine: https://developers.google.com/earth-engine/datasets/catalog/projects_sat-io_open-datasets_ORNL_LANDSCAN_GLOBAL
summarized in each buffer size in "after200spills_allbuffs_landscanfinaldf_20250810.csv"

WorldPop Annual Human Population Counts from Worldpop: https://hub.worldpop.org/geodata/listing?id=74
calibrated ratio of population in each buffer size in "after200spills_allbuffs_landscan_wpopr_finaldf_20250820.csv"

Copernicus Climate Change Initiative Annual Land Cover from the Climate Data Store: https://cds.climate.copernicus.eu/datasets/satellite-land-cover?tab=overview
area of each lands summarized in each buffer in "allspills_allbuffs_CCIland_20250828.csv"

Resolve Ecoregions and Biomes from ArcGIS Living Atlas: https://hub.arcgis.com/datasets/esri::resolve-ecoregions-and-biomes/about
used for visualization and grouping in Figure 2C and Figure S5.

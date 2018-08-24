# TRAFx Data Processing
R scripts to process data from [TRAFx](https://www.trafx.net/) vehicle counters.

## Description
There are two R scripts in this repository that process data from TRAFx vehicle 
counters. 
* [dataProcessing_singleFile.R](dataProcessing_singleFile.R) will 
processes one file at at time.
* [dataProcessing_batch.R](dataProcessing_batch.R) 
will batch process all the files in a directory or folder.

These scripts will separate the metadata and data from the TRAFx data file, 
run some simple data quality control routines, and save them to seperate tables in a 
database.

## Authors
* **Matthew Van Scoyoc** - *Initial work* - [scoyoc](https://github.com/scoyoc)

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

# Tripod-frame_Performance-test

This repository contains the code to compare the performance of two different deployment set-ups for acoustic receivers: a commonly used stone mooring and a tripod frame. Full details on the design of the frame and on the analysis are outlined in:

**Goossens J., ‘Tjampens M., Deneudt K, Reubens, J. Mooring scientific instruments on the seabed – Design, deployment protocol and performance of a recoverable frame for acoustic receivers. Methods Ecol Evol. 2020; 00: 1– 6. https://doi.org/10.1111/2041-210X.13404**

Data are available through DOI: https://doi.org/10.14284/404

The following protocol executes the analysis outlined in the publication. Scripts can be found in the folder *src*.
## 1) Get data
Script in folder **src/data**.

*Get_data.R*: Create necessary folders and save the data in the correct folder.

## 2) Read and prepare data
Scripts in folder **src/features**.

*Preparation_detection_data.R*: Detection data are prepared for analysis. For every transmitted signal of built-in transmitters, it is calculated how many receivers detected this signal. This information is then linked to station metadata, as well as to tilt and noise measurements. The computation of the number of detections of each transmitted signal involves staged applications over lists within lists and takes some time. 

*Preparation_detection_hour.R*: Calculate hourly average detection percentages, noise and tilt values.

*Preparation_calculation_tilt.R*: Investigate tilt values, as measured by each receiver. Compute autocorrelation and moving standard deviation and calculate summary statistics.

## 3) Model
Script in folder **src/models**.

*Model_detection_efficiency.R*: Compute generalized linear model with the Bernoulli distribution on detection data (0/1), thus estimating the detection probability as a three-way interaction between deployment set-up, distance and noise. After model comparison, data are predicted for the optimal model.

## 4) Plotting
Scripts in folder **src/visualization**.

*Fig2_Map.R*: Create a map of the study area. Shape files originate from MarineRegions.org.

*Fig3_Receiver_positions.R*: Make a figure representing the positions of receivers around a turbine. 

*Fig4_Tilt.R*: Plot the tilt and standard deviation of tilt over time, as well as tilt autocorrelation.

*Fig5_Detection_percentage.R*: Plot hourly detection percentages over time for every turbine at different distances.

*Fig6_Model.R*: Predict the output of the model.


Funding note: The development of the frame was financed by VLIZ as part of the Flemish contribution to the LifeWatch ESFRI funded by the Research Foundation – Flanders (FWO). Jolien Goossens holds a doctoral grant from FWO. 
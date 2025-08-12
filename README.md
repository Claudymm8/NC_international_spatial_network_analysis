# NC_international_spatial_network_analysis
## 1. Some Introductury Information 
The files in this repository are part of the work created during my 6 weeks as an intern at the UKCEH with their NC-International programme (https://www.ceh.ac.uk/our-science/projects/international-science-net-zero-plus). 
During this research, using a Graph Theory modelling approach, we analysed the fragmented Dorset Heathland landscape and its connectivity by assessing pollinator and shrub’s dispersal capacity. The project aimed not only to understand the current state of the landscape but also to assess how the network would change by gradually restoring patches depending on their importance. This importance was inferred by the usage of centrality metrics which can gives information on which patches are most important for the network’s stability and resilience.

The aim of this work was also to create a workflow which would serve other people in analysing their targeted landscape. In the files you will find complete and commented scripts which I used to create my analysis (preliminary folder) and a master script which will be useful to conduct your own research (master_script folder). 

The work was divided into:
1. Literature review
- Collect studies where real landscape have been represented as networks
- Extract important information on which type of analysis was carried out, what time of
networks were created, and which metrics were used
2. Case study
- Build own network for the Dorset landscape and analyse its structure
- Simulate different restoration scenarios by adding habitat patches and analyse their
structure
3. Creation of reproducible script

## 2. Data needed 
### Landscape
To work with the scripts provided you will need a dataframe which contains information regarding your landscape (see image below for example):
- patch = this is an identification number
- area_m2 = size of the patch in square meters
- longitude 
- latitude
- cluster/group = in the original scripts this column is present as "cluster"; in the final version this column is "group". This column is optional. If conducting restoration work, it can be useful to include it for original patches/restored patches. 

See data/heaths_dorset.csv & data/heaths_dorset_restore.csv

<img width="360" height="137" alt="image" src="https://github.com/user-attachments/assets/ac685f54-a39a-488c-bfb1-3230dbbd0ac2" />

NOTE: Latitude and Longitude are expressed as a meter value. This can be achieved through specific settings on QGIS. 

<img width="397" height="481" alt="gis_metric_coord" src="https://github.com/user-attachments/assets/9da3f017-e6c6-4c47-a897-4fa8ad6471a0" />


### Species 
To create your network you will need information regarding the dispersal capacity of your target species. This can be added directly as a value of your chosing or can be calculated within the script.
The functions created follow a negative exponential curve. In our case we created a mean dispesal capacity of various species.
The dataset needed for this contains:
- group = species group if you are analysing more than one species (in our case bumblebee, butterfly etc.)
- species = name of species
- theta_per_m = this is the probability of distribution per meter

See datasets/plant_pollinator_dispersal.xlsx 

<img width="254" height="90" alt="image" src="https://github.com/user-attachments/assets/228372df-618f-4872-a38a-c0a865139d88" />


## 3. Files 

### /data
Folder containing datasets for the analysis. 
- heaths_dorset.csv = original dorset landscape patches 
- heaths_dorset_restore.csv = 1930s patches for restoration plans
- df_species_example.xlsm = information about the species and their dispersal
- tot_jbdf_metrics.csv = extracted data for plant distribution from Soons and Bullock (2008 - https://doi.org/10.1111/j.1365-2745.2008.01370.x). This dataset already contains information regarding various dispersal capacities.

### /information
The xlsx file here contains information regarding all the scripts in the "preliminary" folder. 

### /preliminary
Here you will find all the cleaned up and commented scripts that I used for the full analysis. You can refer to these scripts to see my work during the internship and specifically how I analysed the Dorset landscare but also to see a more in depth analysis of how you might use the master script


### /master_script
- spatial_network_analysis.R
This is the files which contains only major functions and an easy to follow step by step workflow.
The file contains in depth commenting to guide you through the analysis and to point you to the right direction to be able to edit the script to create a correct analysis of your landscape.




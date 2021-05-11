# Final-Project
This is the repository that houses all of the files associated with my final project for Data Science for Economics (ECON 5253) Spring 2021. 

## Data
The data set for this project is an extract from the 2018 ACS
The file size is 315 MB which aparently exceeds github's 100MB file size cap. I was originally going to upload it in a nice little folder. Then I messed around with the Large File System but I wasn't able to get it to work. 

Therefore, I think the best bet unfortunately is to download the same extract from IPUMS. From the USA tab, select the following variables:
SEX, AGE, RACE, LANGUAGE, SPEAKENG, EMPSTAT, IND, INCWAGE, and PWSTATE2

Then for the sample select the 2018 ACS only

This is the best way I can think of to replicate the results. Be sure not to change the file names when you download them because they need to reference eachother and it will mess up the R script that IPUMS gives you to read in the data. I believe this will need to be your unique code which will replace lines 8 and 9 of the code I have provided. Also, your working directory must be set to the location where the data is stored.

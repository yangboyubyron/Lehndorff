# Consumers Energy BEEP Sample Design Task -- 2018 data
# The purpose of this task is to simulate the creation of a sample design for Consumers Energy BEEP PY2018.

##Step 1 of this task is to assign measures to measure groups and then projects to measure groups based on the measures in the project.

# This will require:
## Customer contact data located at "/volumes/Projects/407004 - Consumers Energy Business Solutions Impact Evaluation/PY2018/Raw Data/Consumers Energy/CE_BS_Customer_2018_onsites.csv"
## measure-level data at"/volumes/Projects/407004 - Consumers Energy Business Solutions Impact Evaluation/PY2018/Raw Data/Consumers Energy/CE_BS_Measure_2018_onsites_05.21.18.csv"

# in addition the measure group defintions for 2018 should conform to the measure groups from the most recent electric evaluation (PY 2016) found here based on measure code (Measure.Code):
## "/volumes/Projects/407004 - Consumers Energy Business Solutions Impact Evaluation/PY2016/Gross Impact Analysis (onsite adj)/Workbooks/CE Install Adj Tables HL 042717.xlsx", sheet "BusSolMeasCatReportGroups"

# Any measures in the 2018 data which do not have matches in the 2016 data should be assigned to exsisting 2016 groups based on measure description.
# This can be done at your discretion, with any input you feel you need from me.

# Once measure groups have been assigned, the project can be assigned a measure group based on these measures using a client defined heirarchy.
# if a project has any Smart Buildings measures (with positive kWh) the project is assigned to Smart Buildings. Else Custom, then Compressed Air, Motors & Drives, Other Prescriptive, Undefined Lighting, Other Lighting, T8/T5 Lighting, and LED Lighting

# The final product for Step 1 is a data frame containing the project ID (ProjectID), the group the project has been asigned to, and the TOTAL savings of the project.  

##Step 2 of this task is to create an optimize sample design using previously created code
# JFAST-Core-Ages-Mapper

## INPUT FILE FORMAT

Input file needs to be a csv with the following columns (named exactly the same):
1. **Sample ID** - identifier used at LDEO for each sample
2. **Depth** - sample location along core in meters
3. **Age** - age in Ma based on Ar mass spec disk run and an estimated K concentration of 2%
4. **Sigma** - error in age between replicates

## SAMPLE INPUT FILE

Sample ID | Depth | Age | Sigma 
--- | --- | --- | --- 
JLE1 | 183 | 5 | 1
JLE2 | 695 | 64 | 6 
JLE3 | 740 | 20 | 3 
JLE4 | 814 | 14 | 5 
JLE5 | 831 | 7 | 2 

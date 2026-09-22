# Cluster Definition

## Cluster relationships

This directory gives the mean amino acid distance between sequences clusters presented in the manuscript. 

Distances are presented for each of the internal gene segments. For segments with more than one protein product, these are provided for the principal protein (e.g. PA for segment 3 and M1 for segment 7). 

For segment 8, NS1 proteins were trimmed to 202 amino acids (the shortest length present) to facilitate comparison, though considerable variation in NS1 length was observed, therefore distances are an underestimate of protein diversity.

## Threshold varied

In the manuscript, we used thresholds of 0.075 for the five longer segments (PB2, PB1, PA, HA and NP) and 0.01 for the three shorter segments (NA, M and NS).

In this directory 'threshold_varied', there are cluster assignments made with these thresholds either decreased or increased by either 10 or 25%.

For each varied threshold, a csv file gives cluster designations for every isolate and a an associated plot gives an idea of how varying thresholds affects the schematic shown in Fig 2 of the manuscript.
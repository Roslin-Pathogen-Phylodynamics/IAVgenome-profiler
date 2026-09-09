# Summary of phyologenetic analyses


| Analysis description                                                                               | Down-sampling strategy                                                          | Program                   | Evolutionary models                                                                                                             |
| -----------------------------------------------------------------------------------------------    | ------------------------------------------------------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| 1. H5Nx in period of transition from H5N8 to H5N1 (Fig 1)                                          | One randomly selected HA sequence per subtype per country<sup>†</sup> per month | BEAST v1.10.4<sup>3</sup> | SRD06 model with strict molecular clock<sup>4</sup> and Bayesian skygrid coalescent model<sup>5</sup>                           |
| 2. Global spread of H5N1 following transition from H5N8 (Fig 2)                                    | One randomly selected HA sequence per subtype per country<sup>†</sup> per week  | Delphy v1.0.3<sup>6</sup> | GTR + GC.                                                                                                                       |
| 3. Spread of H5N1 in Europe (Fig 3)                                                                | One randomly selected HA sequence per country per week                          | BEAST v1.10.4<sup>3</sup> | SRD06 model with relaxed (lognormal distribution) molecular clock<sup>4</sup> and Bayesian skygrid coalescent model<sup>5</sup> |
| 4. Tree summarising NS diversity (Fig 5)                                                           | One randomly selected sequence per genetic cluster<sup>‡</sup>                  | BEAST v1.10.4<sup>3</sup> | HKY + G4model with strict molecular clock and Bayesian skygrid coalescent model                                                 |
| 5. Early H5N1/2022/R10 PA, NP and NS sequences and non-H5 sequences identified using BLAST (Fig 6) | n/a                                                                             | BEAST v1.10.4<sup>3</sup> | HKY + G4model with relaxed (lognormal distribution) molecular clock and Bayesian skygrid coalescent model                       |

‡ Administrative area level 1 (state, province etc.) used for Canada, China, Russia and USA.
† NS genetic clusters identified using farthest neighbour clustering applied using the R function seq_cluster from the bioseq package7 with a threshold of 0.015.
SRD06 (Shapiro-Rambaut-Drummong-2006 model8): HKY + G4 + CP112
HKY: Hasgawa-Kishino-Yano substitution model9.
GTR: General time reversible nucleotide substitution model.
G4 and GC: Site heterogeneity with rates drawn from discrete gamma distribution with four categories or a continuous gamma distribution, respectively10.
CP112: Codon partitioning allowing substitution rate parameters and rate heterogeneity to vary at the third codon position relative to the first two.


References
1.	Altschul, S. F., Gish, W., Miller, W., Myers, E. W. & Lipman, D. J. Basic local alignment search tool. Journal of Molecular Biology 215, 403–410 (1990).
2.	Shu, Y. & McCauley, J. GISAID: Global initiative on sharing all influenza data – from vision to reality. Eurosurveillance 22, (2017).
3.	Suchard, M. A. et al. Bayesian phylogenetic and phylodynamic data integration using BEAST 1.10. Virus Evolution 4, (2018).
4.	Drummond, A. J., Ho, S. Y. W., Phillips, M. J. & Rambaut, A. Relaxed Phylogenetics and Dating with Confidence. PLoS Biol 4, e88 (2006).
5.	Gill, M. S. et al. Improving Bayesian Population Dynamics Inference: A Coalescent-Based Model for Multiple Loci. Molecular Biology and Evolution 30, 713–724 (2013).
6.	Varilly, P. et al. Delphy: scalable, near-real-time Bayesian phylogenetics for outbreaks. Preprint at https://doi.org/10.1101/2025.03.25.645253 (2025).
7.	Keck, F. Handling biological sequences in R with the bioseq package. Methods Ecol Evol 11, 1728–1732 (2020).
8.	Shapiro, B., Rambaut, A. & Drummond, A. J. Choosing Appropriate Substitution Models for the Phylogenetic Analysis of Protein-Coding Sequences. Molecular Biology and Evolution 23, 7–9 (2006).
9.	Hasegawa, M., Kishino, H. & Yano, T. Dating of the human-ape splitting by a molecular clock of mitochondrial DNA. J Mol Evol 22, 160–174 (1985).
10.	Lanave, C., Preparata, G., Sacone, C. & Serio, G. A new method for calculating evolutionary substitution rates. J Mol Evol 20, 86–93 (1984).


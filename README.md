# ParticleSizeAnalysis

## Panga ya Saidi burial sediment analysis replication
The raw data in the folder are the one used for the analysis of Panga ya Saidi burial (Martinón-Torreset al. 2021. Earliest known human burial in Africa. Nature https://doi.org/10.1038/s41586-021-03457-8). These files are coming directly from our laser granulometer with the parameters explained in the paper andor in Sitzia et al. 2017 (10.1016/j.quascirev.2017.06.029,especially in Supp. Mat. 1). The ternary plot produced with this version is rotated since I modified the script after the publication to make it more like the standard ternary diagrams in sedimentology.

## R scripts for transforming the raw data from our machine into a nice table, graphs and ternary plots. 
This need to be done first for replication from raw data, but the end of the script can be used to produce figures and so on if you create an equivalent csv file with your own raw data results. 
The first part of the code creates a nice csv from all the raw data csv (which are a shame from the software of the machine...). It also produces distribution and cumulative figures for each sample, ternary diagrams (with and without labels), and figures gathering all distributons and all cumulative curves.

## R script for multimodal decomposition (co-created with Mathieu Bosq during his PhD, after we decided to stop using the Fityk software used in previous works during Luca Sitzia's PhD)
From the above created csv, I copy/paste some columns in another .csv which I give the example directly here: Distribution_for_decomposition_script.csv (still need to automatize that, not very hard for sure)
This script then uses the Distribution_for_decomposition_script.csv to make a list for all samples in the batch, and then you can play with the multimodal decomposition, which is not easy!

Sometimes it works nicely, and sometimes, for some sediments or even for some samples in a batch of measurements, you spend a lot of time finding the right initial parameters so that it even try to fit... 

Please if you find ways to unmix these distributions better than I did with this mixdist package, do not hesitate to do so and tell me!


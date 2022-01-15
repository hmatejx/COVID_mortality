# Excess mortality - COVID19 or vaccines?

Latest data for 100 countries below. Decide for yourself, what does the data tell you.

![part1](img/part1.jpg)

![part2](img/part2.jpg)

![part3](img/part3.jpg)

![part4](img/part4.jpg)

# How to use

1. Update the data by running `update.R` from the [data_raw](data_raw) subfolder. The country stringency data must be manually downloaded (link and instructions in the source code).
2. Pre-process the data by running `preprocess_data.R` from the [data_proc](data_proc) subfolder.
3. Run the `mortality.R` script to generate the plots.

# Pre-processing

The excess mortality and vaccination data for the various countries is not provided on the same time interval, e.g. some countries report daily, others weekly, and others only monthly or even only quarterly. Temporal diss-aggregation and/or aggregation is therefore used to bring data on the same time scale. While temporal aggregation is straightforward, the [Denton-Cholette](https://journal.r-project.org/archive/2013-2/sax-steiner.pdf) method of temporal diss-aggregation is used.


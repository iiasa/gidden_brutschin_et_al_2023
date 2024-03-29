# Overview

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8026910.svg)](https://doi.org/10.5281/zenodo.8026910)

This is the analysis repository for generating figures for [Gidden et al.
(2023)](https://iopscience.iop.org/article/10.1088/1748-9326/acd8d5).

## Data

To run the analysis notebookes in this repository:

1. download the [data file](https://zenodo.org/record/7986556) for the paper, which can be done programatically using
   [`pooch`](https://www.fatiando.org/pooch/latest/) with:

```python
import pooch

pooch.retrieve("doi:10.5281/zenodo.7986556/gidden_brutschin_et_al_2023.xlsx",
    known_hash="md5:373f96343fac55ce9f940c5b3ed1a359",
    fname="gidden_brutshin_et_al_2023_data.xlsx",
    path='./data')
```

2. Download data and the associated meta file from  [the AR6 explorer
   database](https://data.ene.iiasa.ac.at/ar6/) and place them in the `data` folder

## Generate Figures

- Figures 1-4 can be generated by executing the notebook `figures/figs_1-4.ipynb`
- Figures 5-7 can be generated by executing the script `figures/Figures_5_to_7.R`
- Figure 8 and related equity statements can be generated by executing the notebooks in
  `figures/equity` in numerical order

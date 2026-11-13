# Figure 1: Global breeding distribution of penguins

R code and associated data for generating Figure 1 in:

Quilestino-Olario, R., Berg, S., and Wagner, B.: *Spot the difference! Penguins around the world and how to tell them apart (Finde den Unterschied! Pinguine rund um die Welt und wie man sie auseinanderhält)*, Polarforschung.

## Files

- `figure1_penguin_distribution.R` – R script for generating the map.
- `penguin_sites.csv` – Compiled approximate breeding locations of penguins based on published literature.

## Data sources

Penguin breeding locations are based on Challies and Burleigh (2004), García-Borboroglu and Boersma (2013), de Roy et al. (2022), Hickcox et al. (2019), and MAPPPD (Humphries et al., 2017).

The MAPPPD dataset is not included in this repository and can be downloaded directly from https://www.penguinmap.com/mapppd/.

Humphries, G. R., Naveen, R., Schwaller, M., Che-Castaldo, C., McDowall, P., Schrimpf, M., and Lynch, H. J.: Mapping application for penguin populations and projected dynamics (MAPPPD): data and tools for dynamic management and decision support, Polar Record, 53, 160–166, https://doi.org/10.1017/S0032247417000055, 2017.

The base map uses Natural Earth data.

## Running the code

The script requires R (version 4.1 or later) and the following packages: `ggplot2`, `sf`, `rnaturalearth`, `rnaturalearthdata`, `dplyr`, `readr`, `ggtext`, `stringi`, `dbscan`, and `svglite`.

1. Download the MAPPPD dataset in CSV format from the website above.
2. Save the downloaded file as `MAPPPD.csv` in the same directory as the R script and `penguin_sites.csv`.
3. Set this directory as the working directory in R and run `figure1_penguin_distribution.R`.

The script generates the figure in PDF and SVG formats.

## Note

Breeding locations are approximate. Nearby colonies were grouped and some symbols were repositioned to improve map readability. Emperor penguin breeding locations are represented by coastal line segments rather than individual points.

The map is intended as a general guide to penguin breeding distribution, not as a comprehensive inventory of breeding colonies.

## License

The R code is distributed under the MIT License. MAPPPD data are available separately and remain subject to their original terms of use.

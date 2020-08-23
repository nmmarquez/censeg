# ar.matrix

You can install `censeg` via devtools `devtools::install_github("nmmarquez/censeg")`  

The `censeg` package allows use to quickly calculate segrgeation measures in the United States using the census API. Data available to construct segregation measures come from the ACS 5 years surveys as well as the 2000 and 2010 deccenial Census. You may also quickly calculate segregation measures across the US using convienience functions in the package. 

```
library(censeg)
library(dplyr)
library(ggplot2)

all_census_seg(year = 2010) %>%
    filter(str_ends(cbsatitle, ", CA|, NV|, AZ")) %>%
    {right_join(cbsa_sf, .)} %>%
    ggplot() +
    geom_sf(aes(fill = H)) +
    theme_void() +
    scale_fill_distiller(palette = "Spectral") +
    labs(fill = "Theil's H\nIndex") +
    ggtitle(
        "Segregation Measures for South West Core Based Statistical Areas",
        "Measures Constructed from 2010 Census Data")
```

![seg](https://i.imgur.com/aG5sh81.png "SW Segregation")


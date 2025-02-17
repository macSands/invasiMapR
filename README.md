# invasiMapR

[![License](https://img.shields.io/badge/license-PENDING-orange.svg)](LICENSE) 
[![Coverage Status](https://coveralls.io/repos/github/macSands/invasiMapR/badge.svg?branch=main)](https://coveralls.io/github/macSands/invasiMapR?branch=main)

> **An R toolkit for trait-based invasion modeling and the identification of high-risk invasion hotspots.**

This project is part of the [B3 (B-Cubed) project](https://b-cubed.eu/), a European collaboration dedicated to developing and applying robust biodiversity indicators to inform policies and decision-making. It will also be published through the [B-Cubed GitHub organization](https://github.com/b-cubed-eu).

---

## Overview

**invasiMapR** streamlines the process of quantifying community invasibility using a **trait-based approach**. The package integrates species occurrences (e.g., from [GBIF](https://www.gbif.org/)), functional trait data (e.g., from [TRY](https://www.try-db.org/)), and environmental variables (e.g., from [WorldClim](https://worldclim.org/) or [CHELSA](https://chelsa-climate.org/)).  

### Key Features

- **Automated Data Retrieval**  
  Retrieve species occurrence and environmental data with minimal coding effort.

- **Trait Integration**  
  Combine global trait data (e.g., TRY) with occurrence data through easy fuzzy matching.

- **Trait-Based Interaction Modeling**  
  Use Gaussian-like kernels to estimate competitive interactions among resident and alien species based on trait distances.

- **Parallelized Invasibility Computations**  
  Loop through thousands of hypothetical alien species (by reshuffling trait data) to assess community “openness” in realistic timeframes.

- **Spatial Visualization**  
  Generate invasibility maps as raster or vector layers to identify hotspots for potential establishment by alien species.

---

## Getting Started

### Prerequisites

- **R (version ≥ 4.1.0)**  
- Basic familiarity with R for installing packages, setting working directories, and managing scripts.

### Installation

> **Note**: `invasiMapR` is under active development and will ultimately be hosted in the [B-Cubed GitHub organization](https://github.com/b-cubed-eu). Installation instructions will follow once the repository is public.

For now, you can clone or download the repository from [this GitHub page](https://github.com/macSands/invasiMapR) and load the package locally:

```r
# Example once public:
# install.packages("devtools")
# devtools::install_github("macSands/invasiMapR")

# or if you have cloned the repo:
# setwd("path/to/cloned/invasiMapR")
# devtools::load_all()

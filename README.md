<p align="center">
  <img src="\inst\app\www\LogoPGAcmdr.png" alt="PGA-cmdr" width="150"/>
</p>

<p align="center">
  Interactive R/Shiny application for population genetics analyses<br/>
  <sub>Intertryp · Universite de Montpellier · Cirad · IRD · UCAD</sub>
</p>

PGA-cmdr covers the full workflow for multilocus, individual-based
genotype datasets (microsatellites and other codominant markers): data
import and formatting, allele frequencies, general statistics, panmixia
and subdivision testing, genetic diversities,
linkage disequilibrium, null allele screening, and isolation by
distance — all computed locally, with no data leaving your machine.

## Installation

All dependencies install automatically:

```r
install.packages("remotes")

remotes::install_github("KadNaf/SPG1")

shinypopgen::run_app()
```

### macOS prerequisites (compile from source)

PGA-cmdr contains C++ code compiled with OpenMP. Apple clang does **not**
include OpenMP or gfortran by default. Install these **before** running
`remotes::install_github()`:

**1. gfortran** — download from <https://mac.r-project.org/tools/> and
install `gfortran-14.2-universal.pkg` (or the current version).

**2. libomp** — OpenMP runtime:

```bash
brew install libomp
```

**3. Apple Silicon only** — add to `~/.R/Makevars` (create the file if
absent):

```
LDFLAGS += -L/opt/homebrew/opt/libomp/lib -lomp
CPPFLAGS += -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp
```

## Docker

```bash
git clone https://github.com/KadNaf/SPG1.git
cd SPG1
docker compose up
# open http://localhost:3838
```

## Modules

Every module follows the same pattern: set parameters, click **Run**, and
a `.zip` (or, for single-table modules, a single `.txt`) downloads
automatically with every result file needed — nothing is shown on screen
first. Column numbering, loci detection (single- or paired-column allele
format), and output file names are handled automatically wherever possible.

| Module | What it does |
|---|---|
| **Import Data** | CSV/TXT import (auto-detected comma/semicolon/tab separator, loaded as soon as a file is chosen). Population, Latitude/Longitude column assignment; loci range entered as "number of loci" + "first locus column" (auto-suggested, and automatically adjusted for single- or paired-column allele encoding); a numbered column-reference table is shown so column numbers are never ambiguous. |
| **Allele Freq** | Per-locus, per-population allele frequencies (plus a global column), computed automatically for every population and marker — no selection needed. |
| **General Stats** | Per-locus Ho, Hs, Ht, FIT/FIS/FST (Weir & Cockerham 1984), and optional Fst-max (Meirmans), Fst' (Meirmans), GST (Nei), GST'' (Hedrick/Meirmans); per-population Ho/Hs/FIS summary and per-locus detail for every population; per-allele F-statistics (Weir & Cockerham components). |
| **Local Panmixia** | Within-population FIS (Weir & Cockerham 1984) by locus or by population, with bootstrap CI (over individuals and over sub-samples) and a permutation p-value. Sub-sample or individual counts below 5 are reported as NA rather than an unreliable estimate. |
| **Global Panmixia** | Multilocus FIT across all populations, bootstrap CI (over loci and over sub-samples) and permutation p-value. |
| **Subdivision** | FST (Weir & Cockerham 1984) per locus and overall, bootstrap CI (over loci and over sub-samples), a permutation p-value, and a separate G-test for genotypic differentiation. |
| **Diversities** | HS and HT (Nei 1987, unbiased Nei & Chesser 1983 estimator) per locus and multilocus, with bootstrap CI over individuals, sub-samples, or loci. |
| **LD** | Pairwise linkage disequilibrium between every locus pair, per population and combined ("All"), via a permutation-based G-test. |
| **Null Alleles** | Null allele frequency estimation per locus × sub-sample (FreeNA EM algorithm, Chapuis & Estoup 2007), raw and ENA-corrected FST and DCSE with bootstrap CI (over loci and over sub-samples), and a full pairwise table (FST, FST-ENA, DCSE, DCSE-INA, linearised FST, geographic distance). |
| **IBD** | Isolation by distance: Rousset's (1997) regression of linearised genetic distance against geographic distance, and an independent Mantel permutation test (Pearson, Spearman, and Rousset 1D/2D statistics). |

## Getting started

See the package vignette for a full worked example on the bundled
*Boophilus* tick dataset:

```r
vignette("shinypopgen", package = "shinypopgen")
```

## Statistical methods

F-statistics follow the unbiased moment estimators of **Weir & Cockerham
(1984)**. Gene diversity (Hs, Ht) follows the unbiased estimator of **Nei
& Chesser (1983)**. Confidence intervals are obtained by non-parametric
bootstrap and p-values by Monte Carlo permutation (10,000 replicates by
default; 10,000 for linkage disequilibrium and Mantel tests), parallelised
in C++ via Rcpp and OpenMP. See the in-app **Help** tab for full
references.

## Citation

> PGA-cmdr: an interactive Shiny application for population genetics
> data import, exploration, and descriptive analyses. Intertryp / Universite de Montpellier /
> Cirad / IRD / UCAD.

## Credits

**Programming:** Naffiou Kadiri and Vincent Manzanilla

**Conception:** Thierry de Meeûs

## License

MIT — see [LICENSE.md](LICENSE.md).

## Bugs & support

<https://github.com/KadNaf/SPG1/issues>

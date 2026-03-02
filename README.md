# IRI-2020 3D — Modified IRI for Batch Electron Density Computation

## Overview

This is a modified version of the International Reference Ionosphere (IRI-2020)
model, optimized for computing 3D electron density (Ne) fields over large
latitude/longitude/altitude grids. It is designed to be called from the
`tomoscand` R package as a background model for ionospheric tomography, but can
also be used standalone from the command line.

The standard IRI distribution provides a general-purpose test program
(`iritest.for`) that computes all IRI output parameters. This project adds two
specialized Fortran programs that wrap the IRI_SUB subroutine for efficient
batch processing:

| Binary | Source | Purpose |
|--------|--------|---------|
| `iri_ne` | `iritest_ne.for` | Computes Ne profiles for a grid of lat/lon points |
| `iri_param` | `iritest_param.for` | Computes 2D ionospheric parameters (NmF2, hmF2, etc.) |

A shell script (`run_parallel.sh`) enables multi-core parallel execution.

> **Disclaimer:** This is not the official IRI distribution. For the standard
> IRI-2020 model, visit [irimodel.org](http://irimodel.org). This version is
> optimized for electron density (Ne) only — temperature (Te, Ti, Tn) and ion
> composition outputs are disabled. Most core IRI library subroutines are
> unmodified; `irisub.for` has a minor caching optimization (see below). This
> software is provided "as is", without warranty of any kind, express or
> implied, including but not limited to the warranties of accuracy,
> completeness, or fitness for a particular purpose. Use at your own risk.

## Modifications from Standard IRI-2020

The following core IRI library files are **unmodified** from the official
IRI-2020 distribution: `irifun.for`, `iritec.for`, `iridreg.for`, `iriflip.for`,
`igrf.for`, `cira.for`, `rocdrift.for`.

**`irisub.for`** has one modification:
- Altitude grid caching — `alt.txt` is read once on the first `IRI_SUB` call
  and cached using Fortran `SAVE` variables, instead of being re-read on every
  call (eliminates ~65,000 redundant file reads on a global 1-degree grid).
  Also fixes a bug in the original read loop (`iostat_alt` → `iostat`).
  The unmodified original is kept as `irisub_bu.for` for reference.

The custom driver programs and scripts:

**1. `iritest_ne.for`** (compiles to `iri_ne`)
- Reads lat/lon grid + optional parameters from `param.txt`
- Reads altitude axis from `alt.txt` (cached — read once, not per grid point)
- Disables Te/Ti/Tn computation (`jf(2)=.false.`) — not needed for Ne-only
- Disables ion composition (`jf(3)=.false.`) — not needed for Ne-only
- Per-parameter JF flag control: value `-1` in `param.txt` tells Fortran to use
  IRI internal defaults for that parameter (NmF2, hmF2, NmE, hmE, B0, B1)
- Writes output to `ne.txt`

**2. `iritest_param.for`** (compiles to `iri_param`)
- Reads lat/lon grid from `lat_lon.txt`
- Computes NmF2, hmF2, NmE, hmE, B0, B1 for each grid point
- Writes output to `param.txt`

**3. `run_parallel.sh`**
- Splits `param.txt` into chunks, runs `iri_ne` in parallel across CPU cores
- Concatenates results into a single `ne.txt`

## Directory Structure

```
Fortran source code:
  iritest_ne.for       Custom: batch Ne profiles (-> iri_ne)
  iritest_param.for    Custom: batch 2D parameters (-> iri_param)
  irisub.for           IRI main subroutine library (unmodified)
  irifun.for           IRI functions and MSIS atmosphere (unmodified)
  iritec.for           Total Electron Content (unmodified)
  iridreg.for          D-region models (unmodified)
  iriflip.for          Bottomside ion composition (unmodified)
  igrf.for             Geomagnetic reference field (unmodified)
  cira.for             Neutral atmosphere NRLMSIS-00 (unmodified)
  rocdrift.for         Equatorial vertical ion drift (unmodified)

Compiled binaries:
  iri_ne               Ne profile executable
  iri_param            2D parameter executable

Scripts:
  run_parallel.sh      Multi-core parallel wrapper for iri_ne

Ionospheric coefficients (must be in working directory):
  ccir11.asc ... ccir22.asc     CCIR foF2/M(3000)F2 (12 monthly files)
  ursi11.asc ... ursi22.asc     URSI foF2 (12 monthly files)

Geomagnetic field coefficients:
  dgrf1945.dat ... dgrf2010.dat   Definitive IGRF (5-year intervals)
  igrf2020.dat                    Preliminary IGRF (current epoch)
  igrf2020s.dat                   IGRF secular variation

Solar/magnetic index files (update quarterly from irimodel.org/indices/):
  ig_rz.dat            IG12 and Rz12 indices (1958-present)
  apf107.dat           ap magnetic index and F10.7 solar flux (1960-present)

Supplementary coefficients:
  mcsat11.dat ... mcsat22.dat   Shubin COSMIC-based hmF2 model (12 monthly)

Input/output files (generated at runtime):
  param.txt            Grid points + parameters (input to iri_ne)
  lat_lon.txt          Grid coordinates (input to iri_param)
  alt.txt              Altitude axis in km (input to iri_ne)
  ne.txt               Electron density output (from iri_ne)
```

## Prerequisites

**macOS:**
```bash
# Option A (MacPorts)
sudo port install gcc14
# Compiler: gfortran-mp-14

# Option B (Homebrew)
brew install gcc
# Compiler: gfortran (or gfortran-14)
```

**Ubuntu / Debian:**
```bash
sudo apt install gfortran
```

## Compilation

All Fortran source files must be compiled together. The core IRI library files
are shared between both executables.

```bash
FORTRAN_LIBS="irisub.for irifun.for iritec.for iridreg.for \
              igrf.for cira.for iriflip.for rocdrift.for"
```

**macOS (MacPorts):**
```bash
gfortran-mp-14 -O2 -o iri_ne iritest_ne.for $FORTRAN_LIBS
gfortran-mp-14 -O2 -o iri_param iritest_param.for $FORTRAN_LIBS
```

**macOS (Homebrew) / Ubuntu:**
```bash
gfortran -O2 -o iri_ne iritest_ne.for $FORTRAN_LIBS
gfortran -O2 -o iri_param iritest_param.for $FORTRAN_LIBS
```

Make `run_parallel.sh` executable:
```bash
chmod +x run_parallel.sh
```

> **Note:** Compilation produces warnings about unused variables and legacy Fortran
> constructs — these are expected and harmless from the original IRI code.

## Command-Line Usage

### `iri_ne` — Compute electron density profiles

```
Usage: ./iri_ne <year> <mmdd> <hour> <jchoice>
```

| Argument | Description |
|----------|-------------|
| `year` | 4-digit year (e.g. `2021`) |
| `mmdd` | Month and day (e.g. `1110` for November 10) |
| `hour` | Decimal hour in UT (e.g. `12.5` for 12:30 UT) |
| `jchoice` | `0` = standard IRI (computes all parameters internally) |
| | `1` = use external parameters from `param.txt` |

**Required input files:**

- **`param.txt`** — One row per grid point. Header + data columns:
  `lat long NmF2 hmF2 NmE hmE B0 B1`.
  When `jchoice=0`, only lat/long are used (other columns ignored).
  When `jchoice=1`, parameter values override IRI defaults.
  Value `-1` for any parameter = use IRI internal default.
- **`alt.txt`** — One altitude per line, in kilometres (no header).

**Output:**

- **`ne.txt`** — Header + data columns: `lat long alt Ne`.
  lat, long in degrees; alt in km; Ne in electrons/cm^3.

**Example:**
```bash
./iri_ne 2021 1110 12.0 0
```

### `iri_param` — Compute 2D ionospheric parameters

```
Usage: ./iri_param <year> <mmdd> <hour>
```

**Required input files:**

- **`lat_lon.txt`** — Header (`lat long`) + one row per grid point.

**Output:**

- **`param.txt`** — Header + data columns: `lat long NmF2 hmF2 NmE hmE B0 B1`.
  Densities in m^-3; heights in km.

**Example:**
```bash
./iri_param 2021 1110 12.0
```

### `alt.txt` format

One altitude per line in kilometres, no header:

```
5
15
25
...
595
625
675
...
1250
1750
2250
```

## Parallel Execution

For large grids (>500 lat/lon points), `run_parallel.sh` splits the work across
CPU cores for significant speedup.

```
Usage: ./run_parallel.sh <year> <mmdd> <hour> <jchoice> [-j N]
```

Arguments are the same as `iri_ne`, plus:

| Option | Description |
|--------|-------------|
| `-j N` | Number of parallel processes (default: auto-detect CPU cores) |

**How it works:**
1. Splits `param.txt` into N chunks (preserving header)
2. Creates temporary directories with symlinked data files
3. Runs `iri_ne` in each directory in parallel
4. Concatenates all `ne.txt` results into a single output
5. Cleans up temporary directories

**Example:**
```bash
./run_parallel.sh 2021 1110 12.0 0 -j 8
```

> **Note:** Requires `param.txt` and `alt.txt` in the same directory as `run_parallel.sh`.

## Citation & Terms of Use

The IRI model is freely available from [COSPAR](https://cosparhq.cnes.fr/) and
[URSI](https://www.ursi.org/). There is no formal software license in the
source distribution. The official source is
[irimodel.org](http://irimodel.org); it is also distributed through
[NASA CCMC](https://ccmc.gsfc.nasa.gov/models/IRI~2020/).

**If you use IRI in a publication, please cite:**

> Bilitza, D., Pezzopane, M., Truhlik, V., Altadill, D., Reinisch, B. W.,
> & Pignalberi, A. (2022). The International Reference Ionosphere model:
> A review and description of an ionospheric benchmark.
> *Reviews of Geophysics*, 60, e2022RG000792.
> [doi:10.1029/2022RG000792](https://doi.org/10.1029/2022RG000792)

**Non-default JF switches:** This repository disables Te/Ti/Tn (`jf(2)=.false.`)
and ion composition (`jf(3)=.false.`) for Ne-only computation. Per the IRI
documentation, any use of non-default JF switch values must be disclosed in
publications.

## Updating Index Files

The solar/magnetic index files must be updated periodically for current dates:

- **`ig_rz.dat`** — Download from http://irimodel.org/indices/
- **`apf107.dat`** — Download from http://irimodel.org/indices/

Daily updates are also available from the ECHAIM website (David Themens).
Without current indices, IRI will fail or produce inaccurate results for
recent dates.

## References

- IRI-2020 model: [irimodel.org](http://irimodel.org)
- Original IRI readme: `00readme.txt` in this directory
- [IRI output parameters](http://irimodel.org/IRI-output-arrays.docs)
- [IRI switches/options](http://irimodel.org/IRI-Switches-options.docs)
- [IRI FAQ](http://irimodel.org/docs/IRI_FAQ.pdf)

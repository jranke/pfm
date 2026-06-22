# Create a chemical compound object for FOCUS Step 1 calculations

Create a chemical compound object for FOCUS Step 1 calculations

## Usage

``` r
chent_focus_sw(
  name,
  Koc,
  DT50_ws = NA,
  DT50_soil = NA,
  DT50_water = NA,
  DT50_sediment = NA,
  cwsat = 1000,
  mw = NA,
  max_soil = 1,
  max_ws = 1
)
```

## Arguments

- name:

  Length one character vector containing the name

- Koc:

  Partition coefficient between organic carbon and water in L/kg.

- DT50_ws:

  Half-life in water/sediment systems in days

- DT50_soil:

  Half-life in soil in days

- DT50_water:

  Half-life in water in days (Step 2)

- DT50_sediment:

  Half-life in sediment in days (Step 2)

- cwsat:

  Water solubility in mg/L

- mw:

  Molar weight in g/mol.

- max_soil:

  Maximum observed fraction (dimensionless) in soil

- max_ws:

  Maximum observed fraction (dimensionless) in water/sediment systems

## Value

A list with the substance specific properties

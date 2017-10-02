# les-to-cpog

Given an unfolding, this tool computes the size of the corresponding Labelled Event Structure (LES)
and the corresponding Conditional Partial Order Graph (CPOG).

## Build

Compile the sources using the GHC compiler:
```
ghc src/Main.hs -isrc -O -o l2c
```

## Run

Run the tool by feeding a benchmark into stdin:
```
l2c < benchmarks/filesystem12.txt

Unfolding stats:
  # events     = 157
  # conditions = 564
  # arcs       = 1501

LES stats:
  # events           = 157
  # direct causality = 300
  # direct conflicts = 0

CPOG stats:
  # vertices                 = 121
  # arcs                     = 277
  # literals in predicates   = 157
  # literals in rho function = 1585
```

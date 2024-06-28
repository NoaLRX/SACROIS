README
================
Noa LE ROUX
2024-06-28

# Optimization of IAM forecasting model inputs

<u>**The full analysis (code & results) can be see an**</u> :
<https://noalrx.github.io/SACROIS/>

[Ifremer’s IAM model](https://archimer.ifremer.fr/doc/00784/89579/)
forecasts the evolution of LPUE / VPUE (Landings / Value Per Unit of
Effort) of fish in the Mediterranean sea according to different
scenarios.

The model results can be seen in [STECF
reports.](https://stecf.ec.europa.eu/documents_en?prefLang=fr)

The problem is that only 5 species are “dynamic”, the rest of the
species are static and their LPUE are considered constant in the
scenarios generated by the model.

The idea of this project is to remedy this problem by making the
“static” species as dynamic as possible, in particular by creating the
best possible forecasting models for these species.

The data used are daily data from
[SACROIS](https://archimer.ifremer.fr/doc/00774/88631/). These data are
therefore confidential and cannot be shared on this repo.

Here’s a table summarizing the different species. In bold are the
dynamic variables in [Ifremer’s IAM forecasting
model](https://archimer.ifremer.fr/doc/00784/89579/). The rest of them
are static.

| HKE     | European hake              | MUT     | Red mullet               |
|---------|----------------------------|---------|--------------------------|
| **NEP** | **Norway lobster**         | **ARA** | **Red shrimp**           |
| **DPS** | **Deep-water rose shrimp** | ANE     | European anchovy         |
| BES     | Belone                     | BOG     | Bogue                    |
| BSS     | European seabass           | BZX     | Bonitos                  |
| CTL     | Cuttlefish, bobtail squids | DCP     | Natantian decapods       |
| DEX     | Dentex                     | DPS     | Deep-water rose shrimp   |
| ELX     | River eels                 | FIN     | Finfishes                |
| FLX     | Flatfishes                 | FOX     | Forkbeards               |
| GUX     | Gurnards, searobins        | JAX     | Jack and horse mackerels |
| JLX     | Murex shells               | JOD     | John dory                |
| MAX     | Mackerels                  | MGR     | Meagre                   |
| MNZ     | Monkfishes                 | MUL     | Mullets                  |
| MUX_bis | Red mullets                | OCT     | Octopuses, etc.          |
| PAC     | Common pandora             | PEN     | Penaeus shrimps          |
| PIL     | European pilchard          | POD     | Poor cod                 |
| SAA     | Round sardinella           | SBA     | Axillary seabream        |
| SBG     | Gilthead seabream          | SBX     | Porgies, seabreams       |
| SOX     | Soles                      | SQY     | Squillids                |
| SQZ     | Inshore squids             | SRX     | Rays, stingrays, mantas  |
| SWM     | Swimming crabs, etc.       | TUX     | Tuna-like fishes         |
| VLO     | Spiny lobsters             | WEX     | Weevers                  |
| WHB     | Blue whiting               | XOX     | Sandlances               |

<u>Note</u>: `ZZZ` therefore includes all species not mentioned in the
table above. It is therefore a collection of several species.

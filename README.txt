*************************************
These data files accompany:
"Consequences of pollen defense compounds for pollinators and antagonists in a pollen-rewarding plant".
Sébastien Rivest, Stephen T. Lee, Daniel Cook, Jessica R. K. Forrest, 2023.
*************************************

CONTENTS

There are four datasets:
- "lupinus_data"
- "Alkaloid_composition"
- "Bombus_trials"
- "Bacterial_growth_assays"

*************************************

lupinus_data

Provide information about individuals of Lupinus argenteus sampled at three sites in 2021 and 2022 and their interactions with pollinators and pollen antagonists. This dataset was used in the code "Lupinus_interactions_analysis_SR".

Site: Name of the site sampled, corresponding to a distinct population of L. argenteus
Plot: Name of the plot within a given site.
ID: Identity of the L. argenteus individual sampled
n_flowers: number of open flowers of the individual
Conspecific_abundance: Number of open flowers of L. argenteus, excluding the focal individial, in a 1 meter radius from the focal individual.
CFU_per_mg: Number of colony forming units of bacteria per mg of pollen sample counted on growing media.
Thrips_per_flower: Number of thrips per flower of the focal individual.
Pollen_beetles_per_flower: Number of pollen beetles per flower.
Pollinator_visits: Number of pollinator visits to the focal individual (i.e., the total number of approaches to the plant that resulted in flower visitation).
Tot_flowers_visits: Total number of flowers visited (i.e., the total number of flowers visited across all pollinator approaches).
Flowers_visited_per_visit: Mean number of flowers visited per visit.
Pollen_alkaloids: Total concentration of alkaloids in pollen (μg/g).
Petals_alkaloids: Total concentration of alkaloids in petals (μg/g).

-----------------------------------------------------------------------------

Alkaloid_composition


Provide concentrations in μg/g of all the alkaloids in pollen and petal samples of L. argenteus. This dataset was used in the code "Alkaloid_composition_SR".

Site: Name of the site sampled, corresponding to a distinct population of L. argenteus
Plot: Name of the plot within a given site.
ID: Identity of the L. argenteus individual sampled

Note: We were unable to identify one alkaloid, which is named "Unknown" in the data file.

-----------------------------------------------------------------------------

Bombus_trials

Provide information on Bombus spp. visitation to artificial flowers containing either control pollen or pollen spiked with 2 mg/g of thermopsine—an alkaloid found in the pollen of some L. argenteus populations. The trials were performed in flight cages in controled conditions. This dataset was used in the code "Bombus_trials_SR".

Species: Species of bee used for the trial (B. appositus or B. rufocinctus).
Colony: Identity of the colony from which the bee used for the trial belong.
Bee_ID: Unique identity of the bee within a given colony
Treatment: Either the artificial flower used during the trial contained thermopsine (T) or not (C).
Number_visits: Number of visits made by the bee to the artificial flower during the trial.
Time on flower: Number of seconds spend by the bee collecting pollen on the artifical flower.

-----------------------------------------------------------------------------

Bacterial_growth_assays

Provide information on the inhibition zone of different concentrations of thermopsine on three species of bacteria. This dataset was used in the code "Bacterial_growth_assays_SR".

Species: Species of bacteria (identified to the genus level).
Plate: Identity of the plate on which the bacteria was grown. Five plates were used for each species ("A" to "E") and each bacteria species corresponds to a different number (e.g., "2" in plate "A2").
Concentration: Concentration of the alkaloid thermopsine added to the sterile paper disc (μg/g).
Inhibition: diameter of the inhibition zone around the disc (mm).

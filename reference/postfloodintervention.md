# USAID Flood Response - Post Intervention Survey 2019-2020

This dataset represents the USAID Flood Response Post Intervention
Survey conducted in Mulanje during 2019-2020 using the mWater platform.
It contains comprehensive water quality assessments and operational data
from various water points affected by flooding.

## Format

A tibble with 308 rows and 30 variables:

- submitted_on:

  Date when the survey was submitted

- water_point_name:

  Name of the water point

- latitude:

  Geographic latitude coordinate of the water point

- longitude:

  Geographic longitude coordinate of the water point

- photo_condition_water_point:

  File names or URLs of photos illustrating the current condition of the
  water point

- operational_feel_of_pump:

  Qualitative assessment of how the pump feels during operation

- time_to_pump_20_litres:

  Time taken (in seconds or minutes) to pump 20 liters of water

- number_of_strokes_to_yield_water:

  Number of pump strokes needed to produce water

- sediment_presence:

  Presence or absence of sediment in the water

- electrical_conductivity_magnitude:

  Measured magnitude of electrical conductivity in the water sample

- electrical_conductivity_units:

  Units of electrical conductivity measurement (e.g., μS/cm)

- arsenic_magnitude:

  Concentration of arsenic detected in the water sample

- arsenic_units:

  Units used to measure arsenic concentration (e.g., μg/L)

- ammonia_mg_per_l:

  Ammonia concentration in mg per liter

- fluoride_ppm:

  Fluoride concentration in parts per million

- nitrate_mg_per_l:

  Nitrate concentration in mg per liter

- total_dissolved_solids_ppt:

  Total dissolved solids in parts per thousand

- free_chlorine_mg_per_l:

  Concentration of free chlorine in mg per liter

- ph:

  pH level of the water sample (acidity/alkalinity)

- temperature_magnitude:

  Temperature value of the water sample

- temperature_units:

  Units for temperature measurement (degree celsius)

- turbidity_tube_magnitude:

  Measured turbidity of water using a turbidity tube

- turbidity_tube_units:

  Units for turbidity measurement (e.g., NTU)

- comments:

  Additional notes or observations related to the water point or sample

- type_of_sample:

  Type or source of the water sample (e.g., well, tap, river)

- date_of_sample:

  Date when the water sample was collected

- ecoli_mpn_per_100ml:

  Most probable number (MPN) of E. coli bacteria per 100 milliliters

- ecoli_upper_95ci_per_100ml:

  Upper limit of the 95 percent confidence interval for E. coli MPN per
  100 ml

- ecoli_health_risk_category:

  Health risk classification based on E. coli levels (e.g., low, medium,
  high)

- ecoli_image:

  File name or URL of image showing E. coli test result or sample
  compartment color change

## Source

<https://www.mwater.co>

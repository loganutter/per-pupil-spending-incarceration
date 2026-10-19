# Per Pupil Spending and Incarceration Rates Across U.S. States

Panel analysis of per-pupil spending and state incarceration rates, US states 2011-2021. The rendered version can be found in [analysis.md](analysis.md).

This analysis is a replication of a project originally produced in Stata, co-authored with Moses Yazachew under the supervision of Dr. Luz Saavedra. The objective of this project is to better understand the relationship between per-pupil education spending and incarceration rates. The results show that a 1% increase in per-pupil spending is associated with roughly 1.32 fewer incarcerated individuals per 100,000 residents, significant at the 1% level.

The main limitation is that the model is estimated by pooled OLS without state fixed effects. The estimates therefore describe differences across states rather than changes within them, and should be read as correlational rather than causal. Adding state fixed effects is the natural next step.

# Contents:

- `analysis.qmd` - source document
- `analysis.md` - rendered output
- `data/` - compiled dataset, 550 state-year observations

# Reproducing

Requires R and Quarto with the packages ggplot2, lmtest, car, plm, and sandwich. Open `analysis.qmd` in RStudio and click Render.
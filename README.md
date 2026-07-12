# Update: Infrastructure Conditions and the Spatial Distribution of Public-Space Offenses in New York City, 2021–2025
Three years after the capstone, I rebuilt this project from the ground up. The original study asked whether infrastructure conditions correlate with violence against women in public spaces and addressed this using KDE, KNN, and Random Forest. The rebuilt study asks a harder version of the same question: which of those correlations survive spatial correction, whether the correlates of female-victim incidents actually differ from everyone else's, and whether changes in lighting move incident counts at all.

# What's new

The analysis covers 396,876 public-space incidents with recorded person victims from NYPD complaint data, 2021–2025, across 2,243 census tracts. "Public space" is operationalized through recorded premise types (streets, parks, bridges, tunnels, bus stops, subway facilities, open lots, public parking). Infrastructure is measured by six indicators: streetlight outages, pavement condition, sidewalk defects, subway distance, vacant land, and abandoned buildings, joined with ACS socioeconomic context.

# The update has three parts

- Exploratory spatial analysis. Global Moran's I, LISA, Getis-Ord Gi*, and a seasonal Mann-Kendall trend test. Incidents cluster strongly (Moran's I = 0.510) along a single connected corridor from the South Bronx through Harlem to Midtown, and the entire seasonal profile has shifted upward year over year (τ = 0.650).
- Cross-sectional inference. A negative binomial model with a Markov random field smooth over the tract adjacency graph, which removes the spatial autocorrelation that an aspatial model leaves in its residuals. Subway distance shows the largest association (IRR 0.65 per SD), streetlight outages the largest positive one (IRR 1.20). The same model refit on female-victim incidents returns nearly identical coefficients.
- Panel inference. A fixed-effects Poisson model over 2,228 tracts × 59 months tests whether within-tract changes in darkness change incident counts, plus an event study around the repair of outages lasting 30+ days.


# What changed in the conclusions

The capstone read the streetlight correlation as evidence that infrastructure shapes women's risk. The rebuilt analysis complicates both halves of that claim, and the complications are the findings: The cross-sectional lighting association is robust, but within tracts, month-to-month changes in darkness do nothing detectable. Effects larger than ±1.3% per month are ruled out. The lighting–incident link reflects persistent differences between places, not a short-run response to darkness — consistent with the experimental literature, where only high-dosage lighting interventions have moved crime (Chalfin et al. 2022).
The environmental correlates of female-victim incidents are the same as for all victims. What is distinctively gendered in these data is not geography but timing and offense type: rape is 98% female-victim and 67% of it occurs in darkness, so the burden of after-dark risk — and the stakes of nighttime visibility — fall mainly on women.
Deterioration indicators built from 311 complaints (sidewalk defects, vacant buildings) come out negative after adjustment, a reminder that complaint data measures reporting propensity as much as physical condition.


# 2023 Storymap
Click [here](https://storymaps.arcgis.com/stories/370a10688d2c4874bdc5ddd57467f6b5) to check out the project storymap, better displays on a laptop.

This is a project for CPLN6800 at UPenn. The purpose of my capstone project is to understand the association between infrastructure and women’s public safety in New York City. 

In New York City, unfortunately, there were more than 200 thousand incidents of violence against women during 730 days. And 25.88% of incidents happened in public places. So I focused on 25.88%, which means the location of crime, in open areas and transportation places. In 2022, a UK National Statistics survey showed that 32% of women feel unsafe in public spaces at night. Also, women were 10% more likely than men to feel unsafe in the subway. 

# Background
According to James Scott’s theory in 1988 and Michael Mann’s concept of infrastructure power in 1984, infrastructure is one of the primary instruments the state uses to organize society. There are many different forms of infrastructural violence and gender inequality in the way that our cities are planned. 

Gender power relations are constantly being rearranged, reshaped, embodied, and embedded in even the most common urban infrastructures. The fact that public infrastructures in urban spaces are touched and experienced physically reinforces the idea of infrastructural violence, forcing women to contend with the limitations imposed by time and space. Fear of violence can undermine women’s confidence and limit their activity accessibility in public spaces. 

It is unacceptable that women should be forced to give up their right to access public spaces out of fear. For example, female cyclists are more concerned about overall safety than male cyclists. Additionally, it has an impact on women’s activity duration, which means women are able to remain outside until late in the evening without feeling anxious or depressed because of the darkness. 

Therefore, the requirements for women’s safety must be taken into consideration when developing infrastructure. Characteristics providing prospect, escape, and sufficient lighting should already be considered at the early stage of infrastructure planning. 

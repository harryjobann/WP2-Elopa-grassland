# Modelling Summary for Elopa Grassland

### Data Preparation:

- Log-transformed Buffalo, Cow, Combined livestock, river distance, mountain distance, and distance to cover to address skewness.

### Model Selection & Overdispersion Check:

- Checked for overdispersion using Poisson models.
- Jungle cat was suitable for a Poisson model; all other species were overdispersed and modelled using negative binomial (nbinom2).

### Model Fitting:

- Full model included log-transformed Buffalo, Cow, Combined livestock, Elevation, river distance, mountain distance, and distance to cover, with an offset for trap nights and a random intercept for station.
- Fitted models for Indian hare, Hog deer, Jungle cat, and Sambar.
- Indian jackal was dropped due to insufficient detections.
- Model convergence issues were addressed by adjusting optimizer settings and increasing iterations.

### Results Overview:

- Indian hare: Distance to cover had a significant positive effect (p = 0.0103), suggesting hares favour proximity to cover. No strong effects of livestock presence.
- Hog deer: River distance showed a weak positive trend (p = 0.0503), but no other strong effects were detected.
- Jungle cat: No significant predictors; elevation and river distance showed weak trends (p = 0.159, p = 0.344).
- Sambar: Distance to cover had a significant negative effect (p = 0.0392), suggesting Sambar prefer more open areas.

### Key Takeaways:

- Livestock presence (Buffalo, Cow, or their combination) showed no significant effects on any species.
- Distance to cover is a key factor for Indian hare (positive effect) and Sambar (negative effect).
- River distance may be relevant for Hog deer, though not strongly significant.

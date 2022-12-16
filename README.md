# spinifex_study

Mixed user study comparing the radial tour (animation and steering basis), PCA (discretely jumping to two selected components), and the grand tour with random path (animate, no steering).

We test these visual factors across the experimental factors _location_ of the noise, _shape_ of the distributions and _dimension_-ality of the data.

In summary, using mixed modeling there is strong evidence that use of the radial tour leads to significant (and relatively large) improvement in our accuracy measure, especially with the location of signal is not mixed at 33/66%.


## Structure

This repository contains the code, app, ethics approval, data, analysis, and findings paper. It is structured as follows:


- apps               # Contains the shiny application
- apps_supplementary # Supplementary and setup, simulation, figure production
- ethics             # Ethics documents and approval from Monash University
- paper              # Directory for the article
- paper/data_study   # Data at various stages for the study
- paper/data_survey  # Data at various stages for the post-study survey
- paper/figures      # Figures, and table output (rds files)
- paper/R            # Analysis (mixed_model_regression.rmd) and figure scripts
- tvcg-journal-latex-2019.10.01 # Ignore (example latex template)


Tours are produced and saved with the [spinifex](https://github.com/nspyrison/spinifex) package.

Participant instructions during the study can be viewed at: https://vimeo.com/712674984.

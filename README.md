# Climate_Downscaling

## Components

This repository is divided into five sub-directories. 

* Climate_Data includes raw and processed temperature and precipitation data at a monthly resolution. Currently, I am using climate from the PRISM climate group. These are therefore reconstructions, but they are high-confidence reconstructions at a fine spatial resolution.
* CMIP_Reconstructions contains the historical and past2k temperature and precipitation reconstructions from CMIP6. Currently, I am using MPI-ESM only because this is the only model for which past2k simulations are available.
* Covariates contains elevation data that can be used as a covariate in the downscaling model. Currently, it is not included.
* Fit contains the model fits for the temperature and precipitation models without only latitude and longitude as covariates.
* R contains all the R scripts necessary to run the model.

Any data files that are too large for Github are in an associated Google Drive at this time. Access will be granted for those wishing to run through this workflow.
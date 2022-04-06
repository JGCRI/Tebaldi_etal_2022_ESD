_your zenodo badge here_

# Tebaldi_etal_2022_ESD

**STITCHES: creating new scenarios of climate model output by stitching together pieces of existing simulations**

Claudia Tebaldi<sup>1\*</sup> , Abigail Snyder<sup>2</sup>, and Kalyn Dorheim<sup>2</sup>

<sup>1 </sup> Lawrence Berkeley National Laboratory, Berkeley, CA

<sup>2 </sup> Joint Global Change Research Institute, Pacific Northwest National Laboratory and University of Maryland, College Park, MD


\* corresponding author:  ctebaldi@lbl.gov

## Abstract
In this paper the authors introduce the emulator, STITCHES, which uses existing
archives of Earth System Models’ (ESMs) scenario experiments to construct new scenarios, or enrich existing initial condition ensembles. This repository archives the data and scripts used in the experimental setup and analysis for the manuscript. 

## Journal reference
Submitted to Earth System Dynamics 

## Code reference
UPDATE with the ZENDO infor for this GitHub repo. 


## Contributing modeling software
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| STITCHES | TBD version | https://github.com/jgcri/stitches | TBD link to DOI dataset |


## Workflow

1. Install the software components required to conduct the experiment from [STITCHES](https://github.com/jgcri/stitches#getting-started-using-stitches)
2. Run the following scripts in the `workflow` directory to re-create the manuscript experiments:
    * A scripts that use STITCHES to emulate ESMs 
    * AB scripts that produce STITCHES outputs and processes them for the manuscript
    * B scripts process, analyze, and visualize A outputs for the manuscript
        

| Script Name | Description | 
| --- | --- | 
| `A.inital_cond_exp.py` | Script that emulates ssp245 & ssp370 GSAT at different tolerance and target ensemble sizes | 
| `A.tas_psl_pr.py` | Script that produces gridded outputs for ssp245 & ssp370 joint tas-ps-psl products | 
| `A.intermediate_exp.py` | Script that emulates intermediate scenarios using, GSAT outputs  | 
| `AB.experiment_tolerance_sweep.py` | Script that emulates intermediate scenarios using, GSAT outputs  | 
| `B.TBD script from CT ` | TBD description | 




---
output:
  pdf_document: default
  html_document: default
---

# Electronic Health Record Phenotyping with Internally Assessable Performance (PhIAP) using Anchor-Positive and Unlabeled Patients

## Summary

This repository (https://bitbucket.org/hermanlab/phiap) contains the code for the manuscript submitted to [arxiv](https://arxiv.org/abs/1902.10060). This manuscript describes the development of a novel maximum likelihood approach that efficiently leverages data from anchor-positive and unlabeled patients to develop logistic regression models for electronic health record phenotyping. Additionally, we described novel statistical methods for estimating phenotyping prevalence and assessing model calibration and predictive performance measures. The method was evaluated by extensive simulation and application to identify patients with primary aldosteronism in Penn Medicine.

Primary aldosteronism (PA) causes ~5% of high blood pressure and is highly treatable, but is dramatically underdiagnosed. By applying this positive-only learning approach, we were able to identify PA patients that were not in our labeled training set

## Contents

This repository contains the code for extracting and processing Primary aldosteronism EHR data, excluding protected health information
## Data Sources
* Penn Data Store (http://www.med.upenn.edu/dac/penn-data-store-warehouse.html)
  * Schema: https://www.med.upenn.edu/dac/assets/user-content/images/ODS_Schema_4_1.jpg
  * Data elements
	* Demographics
	* Diagnosis codes
	* Blood pressures
	* Laboratory results
	* Medication Prescriptions
* PennChart (EPIC) Clarity Reporting database
  * Notes (regex)

## Setup environment
### Docker

Build docker image:
```
	cd <repository root dir>
	docker build -t phiap Dockerfile_rstudio
```

Run docker container:
```
	docker run -d --rm -p XXXXX:8787 --storage-opt size=30G --name PHIAP -v `pwd`:/home/<user>/repos/PhIAP -v <DATA>:/data:ro phiap
```

Change password for user:
```
	docker exec -t PHIAP /bin/bash
	passwd <user>
	exit
```

### ini
1. modify `R/PA_analysis/Biostat_process/config_template.ini`:
  - repos_dir
  - seed
2. rename it as `config.ini`

## Create EHR dataset
1.  Create Cohort
In PDS, execute SQL in `sql/RAR_All_Pts.sql`. This will create a table, `rar_full_pts`, containing all patients eligible for the study.
2. Individual Data Tables
   - In PDS, execute `sql/extract_rar_data.sql` to create separate tables for encounters, diagnosis, lab tests, medications, and clinical notes. These tables should be outputed and stored in in location corresponding to `config.ini` file above.
   - Some code for processing notes and medications is in the separate EHR_Transform repos
	 - http://bic.med.upenn.edu/hermanlab/EHR_transform/tree/289c346c5166fa93602ae90cd24cd43dcd303f88
	 - Code to extract, clean and search notes from PennChart is part of the separate EHR_Transform repos
	 - Code to clean and collapse medications to the patient-date level
3. Run `R/biostat_data_pre.R`

## Perform postive-only model fitting

1. Usage instructions:
```
	res <- LZ_c_solnp_r(<df>,
		iter = 1000,
		tol = 1e-8,
		v = c(0.2, 0.3, 0.4, 0.5),
		vv = seq(0, 1, 0.1))
```

## IRB Compliance
University of Pennsylvania IRB #827260
http://www.med.upenn.edu/dac/irb-compliance.html

## License
Copyright (c) 2019 Daniel Herman, Xiruo Ding, Lingjiao Zhang, Jinbo Chen
Released under the MIT License:
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

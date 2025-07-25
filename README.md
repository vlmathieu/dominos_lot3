# The DOMINOS Project
This repository present the workflow for survey data analysis of the DOMINOS project, lot 3.

## Folder structure
The folder is organized as follows:

```bash
├── README.md
├── resources
│   └── inhouse
│       └── results-survey857139.csv
├── results
└── workflow
    ├── Snakefile
    ├── envs
    ├── rules
    ├── sandbox
    └── scripts
```


README.md provides information on the repository structuration and explains the data analysis workflow.
 
The workflow code goes into a subfolder `workflow`, while the configuration is stored in a subfolder `config`. Inside of the workflow subfolder, the central `Snakefile` marks the entrypoint of the workflow (it will be automatically discovered when running snakemake from the root of above structure). In addition to the central `Snakefile`, rules are stored in a modular way, using the subfolder `workflow/rules`. Such modules should end with `.smk`, the recommended file extension of Snakemake. Further, scripts are stored in a subfolder `workflow/scripts`. Conda environments are stored in the subfolder `workflow/envs` (they are kept as finegrained as possible to improve transparency and maintainability).

All output files generated in the workflow are stored under `results`, unless they are rather retrieved `resources`, in which case they should be stored under resources. The latter subfolder also contains small resources that shall be delivered along with the workflow via git.

# To do list

- [ ] Update likert_desc to:
    - [ ] plot barplots by reverse and not reverse questions
    - [ ] plot barplots by categories (economics, comfort, environment)
- [ ] Better select cronbach alpha results to display
- [ ] Create rule, script, and environment for data cleaning
- [ ] Create rule, script, and environment for data processing for SEM
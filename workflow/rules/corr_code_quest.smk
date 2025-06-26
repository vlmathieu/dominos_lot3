rule corr_code_quest:
    input:
        'resources/inhouse/results_survey857139_code_quest.csv'
    output:
        'results/output/code_quest.csv'
    log:
        'workflow/logs/corr_code_quest.log'
    threads: 1
    conda:
        '../envs/data_science_basics.yaml'
    script: 
        '../scripts/corr_code_quest.R'
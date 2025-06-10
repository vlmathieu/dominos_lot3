rule exploring_data:
    input:
        'resources/inhouse/results-survey857139.csv'
    log:
        'workflow/logs/exploring_data.log'
    threads: 1
    conda:
        '../envs/exploring_data.yaml'
    script:
        '../scripts/exploring_data.R'
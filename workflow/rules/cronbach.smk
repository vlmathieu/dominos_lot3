rule cronbach:
    input:
        'resources/inhouse/results_survey857139_code.csv',
        'results/output/code_quest.csv'
    output:
        'results/plots/cronbach_alpha/cronbach_alpha.tex'
    params:
        att = config['attitudes_codes']
    log:
        'workflow/logs/cronbach.log'
    threads: 1
    conda:
        '../envs/cronbach.yaml'
    script: 
        '../scripts/cronbach.R'
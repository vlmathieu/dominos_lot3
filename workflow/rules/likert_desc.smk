rule likert_desc:
    input:
        'resources/inhouse/results_survey857139_code.csv',
        'results/output/code_quest.csv'
    output:
        expand('results/plots/likert_desc_{att}.{ext}',
               att = config['attitudes_codes'],
               ext = ['png', 'svg'])
    params:
        att = config['attitudes_codes'],
        ext = ['png', 'svg']
    log:
        'workflow/logs/likert_desc.log'
    threads: 1
    conda:
        '../envs/likert.yaml'
    script: 
        '../scripts/likert_desc.R'
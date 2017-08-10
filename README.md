# Control Chart for Hospital Performance

Demonstration: https://sppandlkk.shinyapps.io/XMR_control_chart_for_efficiency_and_effectiveness/.

## Introduction
This is an application that can help hospitals monitor their performance. It tracks two things: adverse outcome and cost, which people usualy refer as effectiveness and efficiency. Outcomes or costs are rolled up by month and then get plotted together sequencially. Control limits are set up using 3-sigma to identify outlier. Case level information are also provided at the end with the option to include diagnosis and procedure codes.

## Input data
The program expect to see an episode level data with the following information:
1. Surgery information: 
    * surgical group index: grp
    * procedure date: procDate
    * procedure codes: proc_#
    * diagnosis codes: diag_#
    * poa information (binary): poa_#
2. Provider information: 
    * hosptial: hospID
    * physician: physID
3. Adverse Outcome
    * observed (binary): obsAdverseOutcome
    * predicted (probability): preAdverseOutcome
4. Cost
    * observed: obsCost
    * predicted: preCost

You need to use your own model to create the predictied values. Model creation/selection is not the focus of this app. In my demonstraion example, I generated my own fake data. Please see GenerateData.R for how the data was generated.
    
## Risk adjustment
Risk adjustment was done by first standardizing the predicted values, then, for each provider, take the sum of observed divided by sum of predicted, multiplying by the grand average observed in the whole data set. It is used as the variables to monitor in control chart.

## Moving range control chart
Moving range control chart was developed to monitor both adverse outcomes and costs. More details can be found here: https://en.wikipedia.org/wiki/Shewhart_individuals_control_chart.

## How to interpret
The control chart is doing on the aggregated level, meaning each dot represents all the cases in that month. Potential outliers will be marked (in red) on the plot. You can click the marked dot to see a more details plot (on case level). Once a dot is selected, the table (at the bottom) will also be trimmed down to those in the selected timeframe. You also also print out the diagnosis and procedure codes information. You can also highlight the POA diagnosis codes to differentiate complication and comorbidity .

# Control Chart for Hospital Performance

Demonstration: https://sppandlkk.shinyapps.io/XMRControlChart/.

## Introduction
This is an application that can help hospitals monitor their performance in terms of the quality of the care provided. It tracks two things: adverse outcome and cost, which people usualy refer to as effectiveness and efficiency. Outcomes (or costs) are rolled up monthly, and then get plotted together in sequential order. Control limits are set by using 3-sigma to identify outlier. Case level information are also provided at the bottom with the option to include diagnosis and procedure codes.

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

## How to interpret the chart
The control chart is doing on the aggregated level, meaning that each dot represents all the cases during each month. Potential outliers will be marked (in red) on the plot. You can click the marked dot to see a more detailed plot (by each case). Once a dot is selected, the table (at the bottom) will also be filtered down to those in the selected timeframe. You can print out the diagnosis and procedure codes information. Moreover, the POA information can be highlighted on the corresponding diagnosis codes to differentiate complication and comorbidity .

# Bayesian Network Visualization Dashboard

The Bayesian Network Dashboard is a tool developed in R which allows researchers to apply Bernoulli conjugate priors with alpha/beta hyperparameters as it relates to Bayes theorem all within a dynamic, clean user interface. If you would like to follow along with the demonstration, an up-to-date version of this application is hosted at https://jamie-sgro.shinyapps.io/beysian-network-0-3-0/. To begin, select ‘Load Network’ and select the demo dataset called ‘coronary’. This dataset comes from the BnLearn R Package that contains data on probable risk factors for coronary thrombosis, comprising data from 1841 men.

![file](https://github.com/jamie-sgro/bayes-net-standalone/blob/master/images/file.png)

Once loaded, the Network tab visualized the current existing Bayesian network created by users. This following structure was learned from Min-Maxed normalized Hill Climber algorithm applied to a log-likelihood function. Selecting a node, such as P. Work, populates a conditional probability table indicating the likelihood that an observation regularly smokes or engages in strenuous physical work.

![file](https://github.com/jamie-sgro/bayes-net-standalone/blob/master/images/cpt.png)

The application applies a recursive Bayes theorem algorithm to calculate any number of parent nodes connecting to a child node. As a result, users are able to add prior probability distributions to inform the posterior distribution of the node, and subsequently, the conditional probability table. To do so, simply updated the Alpha Beta hyperparameters for that node in the field below the ‘Prior Beta Distribution’ section. In the following example, the user has calibrated a 10/50 prior which is reflected in the new distribution found in the ‘Graph’ tab.

![file](https://github.com/jamie-sgro/bayes-net-standalone/blob/master/images/distribution.png)

To continue modifying the network, press the edit button on the ‘Network’ tab to add new edges and delete current edges. Lastly, new machine learned networks can be automatically constructed by pressing the ‘Learn Network’ button, with all the available network parameters listed under the ‘Settings’ tab.
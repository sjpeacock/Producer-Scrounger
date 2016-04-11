# Data supporting the article "An asymmetric producer-scrounger game: body size and the social foraging behavior of coho salmon"## AbstractA tension between cooperation and conflict characterizes the behavioral dynamics of many social species. The foraging benefits of group living include increased efficiency and reduced need for vigilance, but social foraging can also encourage theft of captured prey from conspecifics. The payoffs of stealing prey from others (scrounging) versus capturing prey (producing) may depend on the frequency of each foraging strategy in the group, but also on an individual’s ability to steal. Our experiments on the foraging behavior of juvenile coho salmon (*Oncorhynchus kisutch*) revealed that relatively smaller coho within a group acted primarily as producers, took longer to handle prey, and were therefore more likely to be targeted by scroungers than larger coho. Further, the frequency of scrounging was higher when groups contained individuals of different sizes. These observations motivated the development of a theoretical model of phenotype-limited producer-scrounger dynamics, in which rates of stealing were structured by the size of both the producer and scrounger. Simulations of the model confirmed that relatively large predators should tend to be scroungers while smaller predators should be producers. Contrary to previous models, we also found that under certain conditions, producer and scrounger strategies should coexist for both large and small phenotypes. Large scroungers tended to receive the highest payoff, suggesting that producer-scrounger dynamics may result in an uneven distribution of benefits among group members that – under the right conditions – could entrench social positions of dominance. Our model provides new insights that advance the study of asymmetric games in social foraging theory.  **Key words:** kleptoparasitism, game theory, coho salmon, social foraging## Metadata### Experimental methodsSee publication for complete experimental methods.  Briefly, we observed the social foraging behaviour of juvenile coho salmon preying on pink and chum salmon fry in experimental net pens. Specific behaviors recorded included:* strikes (predator rapidly lunges at prey, whether or not it results in capture)* captures (predator successfully gets prey in mouth)* theft attempts (one predator rapidly lunges at the prey in another predator’s mouth)* thefts (successful theft attempts; see video in Supplementary Data)* prey escapes (predator loses prey, but not to a thief)  We noted the size of the predator involved in each behavior as small, medium or large, relative to the other individuals in the group.   These data are the results of two studies. Study 1 was part of a larger experiment investigating the impact of sea louse parasites on prey preference of coho salmon (see Peacock et al. 2015 Ecosphere [doi: 10.1890/ES15-00337.1](http://www.esajournals.org/doi/full/10.1890/ES15-00337.1)). Study 2 was less extensive, but designed specifically to investigate the effects of predator size on producer-scrounger dynamics.###Data files:  1. `Study1_coho_size.csv`The fork length and body depth of coho predators used in Study 1A and Study 1B, measured after the completion of all predation experiments. Column headings are:    1. `year` - the year the experiment was conducted (2013 or 2014, only the 2014 experiments are relevant to the above paper).    2. `location` – the approximate location in the Broughton Archipelago where coho predators were captured by beach seine. See supplemental information of [Peacock et al. (2015 Ecosphere)](http://www.esajournals.org/doi/full/10.1890/ES15-00337.1) for a map showing locations.    3. `exp_num` – for the last experiments with each cohort of coho, we were able to track the size of coho coming out of each experiment.  However, for most experiments we do not have that information (NA) because we measured coho only after all trials were completed to avoid stressing predators that may be used in future trials.    4. `fish_num` – consequetive numbering of coho measured in each cohort.    5. `length` – forklength (to the nearest 5 mm) measured from the tip of the snout to the fork in the tail.    6. `depth` – body depth (to the nearest mm) measured at the widest part of the coho.  2. `Study1_observations.csv`The raw observations of coho behavior from Study 1. Column headings are:    1. `date` - date of the observations in dd-MMM in 2014    2. `start_time` - the time of day that the 1 hour observation period started, in 24 hour clock    3. `expt_num` - the experiment number corresponding to summary data in Study1_summary.csv    4. `net_pen` - which of the two net pens the experiment used (A or B). Net pens were identical in their dimensions and material.    5. `treatment` - the sea lice treatment (Lice or NoLice) corresponding to the Peacock et al. (2015) study.    6. `obs_num` - the observation number for that experiment.    7. `elapsed time` - the time since the start_time that the observation was made in mm:ss.s. (Blanks are missing values.)    8. `time_between` - time between successive observations in mm:ss.s    9. `obs` - the behaviour observed, classified as strike (S), capture (C), successful theft (T), attempted thefts (AT), pursuit (P), and escaped/lost (L). There are four records of (A) from Expt 1B, but we are unsure what this behavior was, so these observations were disregarded in the analysis.    9. `by` - the size of the coho displaying the behavior or, in the case of thefts and attempted thefts, the size of the coho scrounger/size of target. Sizes are small (S), medium (M), and large (L). Sometimes, we observed a frenzied "group strike", which is noted as (G).    10. `on` - whether the target of the behaviour was an individual (I) or group (G) of prey.    12. `comments` - any notes about the experiment or behavior. If we were able to track repeated behaviors involving the same prey, the obs_num of the previous behavior is noted.  3. `Study1_summary.csv`A summary of the number of predators and prey involved in each experiment. Column headings are:    1. `exp` - the experiment number    2. `trial` - the unique trial number, where each experiment consisted of two trials, one in each net pen.    3. `net_pen` - which of the two net pens the experiment used (A or B). Net pens were identical in their dimensions and material.    4. `treatment` - the sea lice treatment (Lice or NoLice) corresponding to the Peacock et al. (2015) study.    5. `time` - the length of the experiment in hours.  Note that the behavioral observations were made over the first hour of the experiment only.    6. `pink0` - the starting number of pink salmon prey.    7. `chum0` - the starting number of chum salmon prey.    8. `pink.consumed` - the number of pink salmon consumed during the experiment.    9. `chum.consumed` - the number of chum salmon consumed during the experiment.  4. `Study2_observations.csv`    The behavioral observations from the second study focusing on size of coho in smaller groups. Column headings are:    1. `expt_num` - experiment number **for Study 2** (not corresponding to exp_num in Study 1)    2. `num_groups` - whether there was a single size class in the experiment (1) or mixed-size classes (2).    3. `groups_compared` - ??    4. `date` - date of the observations in dd-MMM in 2014    5. `start_time` - the time of day that the 1 hour observation period started, in 24 hour clock    6. `obs_num` - the observation number for that experiment.    7. `time_between` - time between successive observations in mm:ss.s    8. `obs` - the behaviour observed, classified as strike (S), capture (C), successful theft (T), attempted thefts (AT), pursuit (P), escaped/lost (L), or consumed (CON; fully down the hatch).    9. `by` - the size of the coho displaying the behavior defined as small (S), large (M), group (G) or (in the case of single size trials) individual (I).    10. `on` - the size of the target of the behaviour classified as small (S), large (M), group (G) or (in the case of single size trials) individual (I).    11. `ref_num` - reference number from field notes.    12. `comments` - any notes, including reference to previous observations in connection.  ###Code files:  1. `observations_per_trial.R`    * Reads in the `Study1_observations.csv` and summarizes the number of observations per fish per trial for analysis.2. `GLMM_fitting.R`    * Calls `observations_per_trial.R` to compile the data from raw observations.    * Fits the GLMMS for response strike rate, capture rate, proportion of strikes that were successful, attempted theft rate, theft rate, proportion of thefts that were successful, rate of being a target of theft, rate of prey escapes, all with and without coho size as a factor.    * Compares models and gives table 2 and figure 13. `Study2_fig3.R`    * Reads in the `Study2_observations.csv` and produces Figure 3.4. `model_functions.R`    * Functions to compute the equilirbium payoffs and change in the frequency of scrounging for both the simple model and the structured model.    * Called by `model_numerical_analysis.R`5. `model_numerical_analysis.R`    * Code for the analysis of the simple and the phenotype-limited models.    * Calls `model_functions.R`    * Produces Figures 4-7
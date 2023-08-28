# Physical-statistical method to populate an ensemble of regional climate simulations

The tools and their evaluation are described in Jourdain et al. (in preparation).

Note that all RCM files (inputs and outputs) are in the form of a climatology and anomalies with respect to this climatology.

This work package includes:

* **extrapolate\_SMB\_ROF\_new.py** : used to derive the _a_ and _b_ coefficients for the accumulation and melt exponential fits. [_Has been run on the PROTECT server_]

* **calculate\_extended\_RCM\_climatology\_from\_model\_to\_other\_model.sh** : used to calculate the climatology of constructed pseudo-RCM simulations.

* **calculate\_extended\_RCM\_data\_from\_model\_to\_other\_model.sh** : used to calculate anomalies of a pseudo-RCM simulation driven by another CMIP model.

* **calculate\_extended\_RCM\_data\_from\_model\_to\_other\_model\_2200.sh** : same as previous but for the runs going until 2200.

* **calculate\_extended\_RCM\_data\_from\_scenarA\_to\_scenarB.sh** : used to calculate anomalies of a pseudo-RCM simulation in another scenario.

* **calculate\_extended\_RCM\_data\_back\_to\_1850.sh** : used to extend from 1850 to 1980 based on the RCM over present-day.

* **calculate\_extended\_RCM\_data\_until\_2200.sh** : used to extend from 2100 to 2200 based on the RCM over 2080-2100.

* **calculate\_extended\_RCM\_climatology\_from\_member\_to\_other\_member.sh** : used to calculate the climatology of constructed pseudo-RCM simulations.

* **calculate\_extended\_RCM\_data\_from\_member\_to\_other\_member.sh** : used to calculate anomalies of a pseudo-RCM simulation in other members of the same model-scenario.

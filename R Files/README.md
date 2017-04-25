# In this repository:

1. `German(Overall).R`---This file is for generating the time-series approval data of **_CDU/CSU_** and **_FDP_** after **2000**. Furthermore, this file can also generating the overall distribution of the approval data of **_CDU/CSU_** and **_FDP_**.

2. `German(Overall).R.ipynb`---This file provides the results of `German(Overall).R` through notebook.

3. `UK(Overall).R`---This file is for generating the time-series approval data of **_the Consevative_** and **_the Labour_** after **2000**. Furthermore, this file can also generating the overall distribution of the approval data of **_the Labour_**.

4. `UK(Overall).R.ipynp`---This file provides the results of `UK(Overall).R` through notebook.

5. Use `ErrorTerm Generation.R` to generate the error terms for each party before moving on. **Note:** The error term in the original data sets is: $\epsilon\sim N(\mu=0,\sigma=3.394)$. However, since the **FDP**'s poll is low during the time period this project examines, I instead set the **sampling error** as the standard error of the error term, which means $\epsilon\sim N(\mu=0,\sigma=\sqrt{\frac{p(1-p)}{600}})$, where $p$ is the average approval rate within a given election cycle.

#!/usr/bin/env python
# coding: utf-8

# # ANOVA over all DBS configurations for a specific walking test

# What we are roughly doing in this notebook:
# ```
# for each paradigm (fast, free,...):
#    for each patient:
#       for each gait parameter (stride legnt, gait speed, ...):
#          if there are significant differences between 130, 100, OFF, ... according to anova:
#             n_patients_significant[gait parameter] += 1
# ```
# 

# In[1]:


import sys
from tqdm.notebook import tqdm

sys.path.append("../")
from helper.util import reshape_to_multicolumn
from helper.anova import anova, extract_significant_p, conclude_results
from typing import Tuple, Dict
import pprint

import numpy as np
import pandas as pd

#%load_ext lab_black


# ## Select DBS configurations to use

# In[2]:


configs = {
    "frequencies": ["OFF", "030", "085", "130"],
    "intensities": ["OFF", "033", "066", "100"],
    "pulse_widths": ["OFF", "040", "090"],
}
dbs_parameter = "frequencies"
selected_configs = configs[dbs_parameter]
print(
    f"Configurations that will be used for ANOVA: {dbs_parameter} ({', '.join(selected_configs)})"
)
print(
    f"We are using the frequencies here, but we can do the same for pulse width or intensity, if necessary."
)


# ## Load data (not visible in html)

# In[3]:


stride_params = pd.read_csv("../../data/stride_params.csv")
stride_params.drop("time_stamp_s", axis=1, inplace=True)

df = stride_params.set_index("configuration")
df = df.loc[selected_configs]
df = df.reset_index()
config = df["configuration"]
df.drop("configuration", axis=1, inplace=True)
df.insert(1, "configuration", config)

stride_params = df

stride_params[
    (stride_params["patient_id"] == "Pat_03")
    & (stride_params["configuration"] == "040")
]
stride_params.drop("preferred", axis=1, inplace=True)
available_gait_parameters = stride_params.columns[5:]
strides_dict = {
    patient: stride_params[stride_params["patient_id"] == patient]
    for patient in set(stride_params["patient_id"])
}


# ## Calculate anovas (not visible in html)

# In[4]:


results_anova = {}
anova_significant = {}
unprocessed = []
paradigms = list(set(sorted(stride_params["test"])))
with tqdm(
    total=len(paradigms) * len(available_gait_parameters), file=sys.stdout
) as pbar:
    # handled_cases = 0
    pbar.set_description(f"Processing...")
    for paradigm in paradigms:
        results_anova[paradigm] = {}
        anova_significant[paradigm] = {}
        for gait_para in available_gait_parameters:
            pbar.update(1)
            results, n_a = anova(strides_dict, paradigm, gait_para)
            results_anova[paradigm][gait_para] = results
            unprocessed.extend(list(n_a))


# Not for all patients all tests have been recorded, so there are some for which there are no gait parameters
# pprint.pprint("Not processed because no strides detected: ")
# pprint.pprint(set(unprocessed))


# # Display results

# ## Show one of the resulting dataframes
# 

# In[5]:


# Here you can replace "free" by any of "slow", "normal", "fast", "tug_one", "tug_two" to get results for another test paradigm.
paradigm = "free"
df = conclude_results(results_anova[paradigm], p_value_limit=0.05)
df


# Note: Actually I would not expect differences in turning angle, but there are some.
#     Let's take a closer look, what might be going on. First, we want to find out for which patient there was the significant difference.
#     Therefor, we get the four patients for which the p-value was <0.05 in the anova:

# In[6]:


extract_significant_p(results_anova["free"]["turning_angle_deg"], p_value_limit=0.05)


# Now, let's see what's going on with the gait parameters:

# In[7]:


patient = "Pat_19"
paradigm = "free"

df = strides_dict[patient].set_index("test")
gait_paradigm = df.loc[paradigm]
gait_paradigm.groupby("configuration").mean()["turning_angle_deg"]


# Potentially the patient turned into different directions in the different DBS configurations?

# ## Explaining what is going to be shown next
# Next, we show the average number of patients for which we found a significance according to anova. It is averaged over all gait parameters for one specific gait test paradigm (e.g. "free"). Example:

# In[8]:


df = pd.DataFrame(
    data=[10, 20],
    columns=["n_patients_significant"],
    index=["Stride length", "Stride time"],
)
print(
    f"For this, on average gait parameters of {df['n_patients_significant'].mean()} patients reach significance according to anova.\n"
    "Of course if e.g. stride length was significant for 5 patients and stride time as well for 5 patients, these could actually be 5 different patients in both cases. We ignore this for now.\n"
)
df


# In[9]:


print(
    f"For this, on average gait parameters of {df['n_patients_significant'].mean()} patients reach significance according to anova. "
    f"In total we analyzed {len(available_gait_parameters)} gait parameters."
    "Of course if e.g. stride length was significant for 5 patients and stride time as well for 5 patients, these could actually 5 different patients in both cases.\n"
)


# ## Show the conclusion of results

# In[10]:


print(
    "For explanation where these values come from, please see the cell above this one (and its output).\n"
)
for paradigm in paradigms:
    df = conclude_results(results_anova[paradigm], p_value_limit=0.05)
    # display(df)
    mean = df.mean()["n_patients_significant"]
    print(
        f"For '{paradigm}':"
        f"  On average, gait parameters of {mean:.1f} patients ({mean/23*100:.1f}%) reach significance according to anova."
    )


# In[ ]:





import numpy as np
import pandas as pd
from scipy.stats import f_oneway

from typing import Dict, Tuple, Set


def extract_significant_p(df: pd.DataFrame, p_value_limit: float):
    """Return a df, which replaces values that are above p_value_limit with `None`"""
    return (
        df.loc(axis=1)[f"p-value"]
        .where(df[f"p-value"] < p_value_limit)
        .dropna(axis=0, how="all")
    )


def _calculate_anova(data: pd.DataFrame) -> Tuple:
    """Calculate one-way anova using each column as a different measurement."""
    parameter = [column for column in data.columns if column != "configuration"][0]
    data_ = [
        data[data["configuration"] == configuration][parameter].T.to_numpy()
        for configuration in set(data["configuration"])
    ]
    return f_oneway(*data_)


def anova(
    dataset: Dict, gait_test: str, gait_parameter: str
) -> Tuple[pd.DataFrame, Set]:
    """Calculat a one-way anova for a single gait test and gait parameter.

    Parameters
    ----------
    dataset
        A dictionary, where the keys are descriptions for different subjects. The values are dataframes, which have a
        pd.MultiIndex as columns. The first level describes the test paradigm, e.g. "slow" / "fast". The second level
        describes the DBS configureation, e.g. "130", "100", "OFF". The third level is the gait parameter,
        e.g. stride length.

    gait_test
        Used to select the first level of the columns

    gait_parameter
        Used to select the thrid level of the columns

    Returns
    -------
    d
        A dictionary where the keys are equal to the passed argument `dataset`. The values are dataframes,
        where the columns correspond to the two feet and the rows are different gait parameters. The values are anova
        p-values between all DBS configurations and the OFF state for this specific `gait_test`
    """
    anova_dict = {}
    anova_df = pd.DataFrame()
    not_evaluated = []
    for patient, patient_data in dataset.items():
        anova_dict[patient] = {"LeftFoot": (None, None), "RightFoot": (None, None)}
        for foot in set(patient_data["foot"]):
            missing_condition = None
            foot_data = patient_data[
                (patient_data["foot"] == foot) & (patient_data["test"] == gait_test)
            ][[gait_parameter, "configuration"]]

            possible_configurations = {
                "030",
                "033",
                "040",
                "066",
                "085",
                "090",
                "100",
                "130",
                "OFF",
            }
            actual_configurations = set(foot_data["configuration"])
            missing_configurations = possible_configurations - actual_configurations
            if missing_configurations:
                not_evaluated.append(
                    " ".join([gait_test, patient, *missing_configurations, foot])
                )

                if len(missing_configurations) > (len(possible_configurations) - 2):
                    print(
                        "Not evaluating this foot, because to few configurations available."
                    )
                    continue
                # print(set(foot_data.columns) - set(foot_data_valid.columns))
            anova_dict[patient][foot] = _calculate_anova(foot_data)
        row = pd.DataFrame(
            index=[patient],
            columns=pd.MultiIndex.from_arrays(
                [["p-value"] * 2, ["LeftFoot", "RightFoot"]]
            ),
            data=[
                [
                    anova_dict[patient]["LeftFoot"][1],
                    anova_dict[patient]["RightFoot"][1],
                ]
            ],
        )
        anova_df = pd.concat([anova_df, row])
    return anova_df, set(not_evaluated)


def conclude_results(
    all_results: pd.DataFrame,
    p_value_limit: float
) -> pd.DataFrame:
    anova_overview = pd.DataFrame()
    significant_results = {}
    for gait_parameter in all_results.keys():
        significant_results[gait_parameter] = extract_significant_p(
            all_results[gait_parameter], p_value_limit=p_value_limit
        )
        data = [
            len(all_results[gait_parameter]),
            len(significant_results[gait_parameter]),
            significant_results[gait_parameter].count().sum(),
        ]
        columns = ["n_patients", "n_patients_significant", "n_feet_significant"]
        anova_overview = pd.concat(
            [
                anova_overview,
                pd.DataFrame(data=[data], columns=columns, index=[gait_parameter]),
            ]
        )
    return anova_overview

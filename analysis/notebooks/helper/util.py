import pandas as pd

def reshape_to_multicolumn(data: pd.DataFrame) -> pd.DataFrame:
    """Split column names like `fast_030` into one level `fast` and an additional level `030`."""
    cols = data.columns.get_level_values(level=0)
    cols_paradigms = []
    cols_configurations = []
    for col in cols:
        if not "tug" in col:
            paradigm = col.split("_")[0].split(" ")[-1]
            configuration = col.split("_")[1]
        else:
            paradigm = "_".join(col.split(" ")[2].split("_")[0:2])
            configuration = col.split("_")[-1]
        cols_paradigms.append(paradigm)
        cols_configurations.append(configuration)
    cols_para = data.columns.get_level_values(level=1)
    new_cols = pd.MultiIndex.from_arrays([cols_paradigms, cols_configurations, cols_para])
    data.columns = new_cols
    return data


def reshape_to_single_column(data: pd.DataFrame) -> pd.DataFrame:
    """Fuse columns like `fast` and `030` to a single level `fast_030`."""
    df = data.copy()
    cols_test = [f"{col[0]}_{col[1]}" for col in df.columns]
    cols_parameters = df.columns.get_level_values(level=2)
    cols = pd.MultiIndex.from_arrays([cols_test, cols_parameters])
    df.columns = cols
    return df

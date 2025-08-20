# read the 'brazil_values.csv' file
# i want to test how the age of the respondent influence their attitude toward economic situation in their home country, their attitude toward us and china, and their satisfaction with democracy in their home country
# # for this, i want to use linear regression
# there is also a column named 'weight' in the file, which is the weight of the respondent, i want to use this column as a weight for the regression
# firstly, regress 'satisfaction_value' on 'age', 'fav_us_value' on 'age', 'fav_china_value' on 'age', and 'econ_value' on 'age' and print the summary of the regression results
# secondly, multiple the 'satisfaction_value' by 'weight', 'fav_us_value' by 'weight', 'fav_china_value' by 'weight', and 'econ_value' by 'weight'
# and regress the new columns on 'age' and print the summary of the regression results
# put out all the regression results in a csv file named 'regression_results.csv'
import pandas as pd
import statsmodels.api as sm
from statsmodels.formula.api import ols
# Load the data
df = pd.read_csv('brazil_values.csv')
# Define a function to perform regression and return summary
def perform_regression(dependent_var, df, weight_col=None):
    if weight_col:
        model = ols(f"{dependent_var} ~ age", data=df, weights=df[weight_col])
    else:
        model = ols(f"{dependent_var} ~ age", data=df)
    results = model.fit()
    return results.summary()
# Perform regressions without weights
satisfaction_summary = perform_regression('satisfaction_value', df)
fav_us_summary = perform_regression('fav_us_value', df)
fav_china_summary = perform_regression('fav_china_value', df)
econ_summary = perform_regression('econ_value', df)
# Perform regressions with weights
satisfaction_weighted_summary = perform_regression('satisfaction_value', df, weight_col='weight')
fav_us_weighted_summary = perform_regression('fav_us_value', df, weight_col='weight')
fav_china_weighted_summary = perform_regression('fav_china_value', df, weight_col='weight')
econ_weighted_summary = perform_regression('econ_value', df, weight_col='weight')
# Collect all summaries into a dictionary
regression_results = {
    'satisfaction_summary': satisfaction_summary,
    'fav_us_summary': fav_us_summary,
    'fav_china_summary': fav_china_summary,
    'econ_summary': econ_summary,
    'satisfaction_weighted_summary': satisfaction_weighted_summary,
    'fav_us_weighted_summary': fav_us_weighted_summary,
    'fav_china_weighted_summary': fav_china_weighted_summary,
    'econ_weighted_summary': econ_weighted_summary
}
# Save the regression results to a CSV file
results_df = pd.DataFrame({
    'summary': [str(regression_results[key]) for key in regression_results]
})
results_df.to_csv('regression_results.csv', index=False)
# Print the summaries to the console
print(satisfaction_summary)
print(fav_us_summary)
print(fav_china_summary)
print(econ_summary)
print(satisfaction_weighted_summary)
print(fav_us_weighted_summary)
print(fav_china_weighted_summary)
print(econ_weighted_summary)
# Note: The summaries are printed to the console and saved to a CSV file.





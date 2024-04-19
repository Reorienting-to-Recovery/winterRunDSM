1. Modeling Straying Rates: The paper aims to model straying rates based on multiple factors. This aligns closely with your goal of increasing your model's straying capacity and could provide you with valuable approaches or parameters to consider.
2. Management Practices and Environmental Factors: The paper will examine how different management actions and environmental conditions affect straying and stock dynamics. These could be additional variables or scenarios to include in your model.
3. Variable and Changing Climate: The focus on adaptability and resilience in changing climate conditions could offer you insights into how to build a more dynamic and robust model that can adapt to various environmental conditions.

Beta regression model uses the following:
- Source Hatchery: The hatchery from where the fish were released.
- Release Region: Whether the fish were released in the bay or upstream.
- Transport Distance: Both absolute and scaled between 0 and 1 by hatchery.
- Release Day and Month: Specific day and month when the fish were released.
- Fish Size and Life Stage: Size and life stage of the fish at the time of release.
- Release Group Size: The size of the group of fish being released.
- Run Size: Combined escapement in the natal spawning ground and hatchery for the return year
- Return Age: Age of the fish at the time of their return.
- Run Year: Year of the fish's return.
- Natal Stream Flows: During both release (April–May) and return (October–November) periods, both absolute and normalized to each other within the year.
- Flow Discrepancy: Difference between release and return flows.
- Regional Temperature: Temperature at the time of the fish's return.
- Pacific Decadal Oscillation (PDO): During the return year, a climate index associated with salmon demography.
- PDO Discrepancy: Difference between release and return year indices for PDO.
- Delta Cross Channel Open Days: Number of days the water conveyance structure was open in October of the return year.

Relevant part of the model:

```r
base = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + Bay
               + Xchannel + flow.1011 + flow.J45R + flow_discrep + rel_month2 + delta_temp.1011
               + mean_PDO_retn + runsize, link = "logit",  weights = log(tot_N_rec+1), data = data2)

```

Here, the response variable (stray_ind) is being transformed using a custom function transf to account for zeros and ones. The predictor variables include:

- hatchery * dist_hatch: Interaction between the hatchery and the distance from hatchery to release site
- run_year: Year of the run
- age: Age of the fish
- Total_N: Release batch size
- Bay: Bay condition or characteristic
- Xchannel: Number of days the channel is open in October
- flow.1011: Flow rate in October-November
- flow.J45R: Relative spring flow rate
- flow_discrep: Flow discrepancy
- rel_month2: Month of release (based on the year)
- delta_temp.1011: Delta temperature in October-November
- mean_PDO_retn: Mean Pacific Decadal Oscillation of return year
- runsize: Size of the fish run
- The model uses the "logit" link function and logarithmically transforms the tot_N_rec + 1 as weights.


another relevant part of the code:

```r
###########################################################################
###########################################################################

## GOLDEN FINAL MODEL!!!

###########################################################################
###########################################################################

finalmod.p = betareg(transf(stray_ind) ~ hatchery*dist_hatch + run_year + age + Total_N + rel_month
                      + flow.1011 + flow_discrep + mean_PDO_retn, link = "probit",weights = log(tot_N_rec+1), data = data2)

summary(finalmod.p)
table = as.matrix(coeftest(finalmod.p))
write.csv(table,"stray model coefficients2.csv")

# Call:
#   betareg(formula = transf(stray_ind) ~ hatchery * dist_hatch + run_year + age + Total_N +
#             rel_month + flow.1011 + flow_discrep + mean_PDO_retn, data = data2, weights = log(tot_N_rec +
#                                                                                                 1), link = "probit")
#
# Standardized weighted residuals 2:
#   Min     1Q Median     3Q    Max
# -7.125 -1.248 -0.027  1.145 12.043
#
# Coefficients (mean model with probit link):
#   Estimate Std. Error z value             Pr(>|z|)
# (Intercept)                  -0.92539    0.02417  -38.29 < 0.0000000000000002 ***
#   hatcheryfeather              -0.07425    0.08591   -0.86               0.3875
# hatcherymerced                0.27062    0.05457    4.96   0.0000007082868437 ***
#   hatcherymokelumne             0.61255    0.03165   19.35 < 0.0000000000000002 ***
#   hatcherynimbus                0.20339    0.06300    3.23               0.0012 **
#   dist_hatch                    0.65842    0.01425   46.21 < 0.0000000000000002 ***
#   run_year                      0.10789    0.01579    6.83   0.0000000000084301 ***
#   age                           0.08352    0.01065    7.84   0.0000000000000045 ***
#   Total_N                       0.02046    0.00982    2.08               0.0372 *
#   rel_month                    -0.04493    0.01391   -3.23               0.0012 **
#   flow.1011                    -0.39884    0.02509  -15.90 < 0.0000000000000002 ***
#   flow_discrep                  0.07853    0.01020    7.70   0.0000000000000140 ***
#   mean_PDO_retn                -0.07982    0.01313   -6.08   0.0000000011959918 ***
#   hatcheryfeather:dist_hatch    0.14669    0.06476    2.26               0.0235 *
#   hatcherymerced:dist_hatch     0.44187    0.05631    7.85   0.0000000000000042 ***
#   hatcherymokelumne:dist_hatch  0.21116    0.09876    2.14               0.0325 *
#   hatcherynimbus:dist_hatch    -0.23781    0.05215   -4.56   0.0000051048207779 ***
#
#   Phi coefficients (precision model with identity link):
#   Estimate Std. Error z value            Pr(>|z|)
# (phi)   2.6916     0.0638    42.2 <0.0000000000000002 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Type of estimator: ML (maximum likelihood)
# Log-likelihood: 5.35e+03 on 18 Df
# Pseudo R-squared: 0.467
# Number of iterations: 34 (BFGS) + 3 (Fisher scoring)

```


## flow data in the model
- flow1: Contains data where Month == 10 (presumably October)
- flow2: Contains data where Month is either 10 or 11 (October or November)
- flow3: Contains data where Month is in 9, 10, or 11 (September, October, or November)
- flow4: Contains data where Month is in 4 or 5 (April or May)
- flow5: Contains data where Month is in 3, 4, 5, or 6 (March to June)
- Here are the flow variables generated and what they likely represent:

- flow.10: Mean flow for October for each hatchery and year.
- flow.1011: Mean flow for October and November for each hatchery and year. This is probably what "flow.1011" in the regression model represents.
- flow.1011R: Relative mean flow for October and November, normalized within each watershed.
- flow.911: Mean flow for September, October, and November.
- flow.J45: Mean flow for April and May.
- flow.J45R: Relative mean flow for April and May, normalized within each watershed.
- flow.J36: Mean flow for May.


## Data cleaning script
### Features and Corresponding Code Lines

1. **Return Age (`return_age`)**: Calculated based on the `recovery_date` and `release_date`.
    - Code: `df['return_age'] = df['recovery_date'] - df['release_date']`
    
2. **Stray vs. Non-Stray (`stray`)**: Based on the hatchery and recovery locations.
    - Code: `df['stray'] = np.where(df['recovery_location'] != df['hatchery_location'], 1, 0)`
    
3. **Counts (`count`, `count_adj`)**: Aggregated and summarized based on different criteria like age, hatchery, and stray status.
    - Code: `df_grouped = df.groupby(['age', 'hatchery', 'stray']).agg({'count': 'sum'}).reset_index()`
    
4. **Estimated Numbers (`estimated_number`, `estimated_number_adj`)**: Summarized and sometimes filled with default values.
    - Code: `df['estimated_number'].fillna(default_value, inplace=True)`
    
5. **Ocean Distance (`dist_hatchR`)**: Joined from an additional dataset and potentially normalized.
    - Code: `df = pd.merge(df, dist_data, on=['hatchery', 'recovery_location'], how='left')`
    
6. **Flow Data**: Additional flow data is joined into the dataset.
    - Code: `df = pd.merge(df, flow_data, on='release_date', how='left')`
    
7. **Various Aggregate Metrics**: For different hatcheries and other groupings.
    - Code: `df_metrics = df.groupby('hatchery').agg({'count': 'sum', 'estimated_number': 'mean'}).reset_index()`
    
8. **Other Variables**: Such as `Excl`, `release_age`, `Hatchery`, `return_year`, etc., which are either read from the data files or modified during the data preparation.
    - Code: `df['Excl'] = df.apply(lambda row: some_function(row['some_column']), axis=1)`

import pandas as pd
import numpy as np
import jdcal

def generate_trappingtable(data):
    """Generates master table of when each plot has been sampled or missed. 
    Input: Pandas Dataframe with the containing the following columns
    plot - plot number
    period - portal project period code, unique for each month of trapping
    yr - year sample occured
    mo - month sample occurred
    dy - day sample occurred
    
    Outputs: New pandas dataframe with only the following columns
    yr - year in input data
    mo - month in input data
    dy - day in input data
    period - unique period code in input data
    plot - plot in input data   
    sampled - indicator of whether plot was sampled at that time. 1= yes, 0=no
    JulianDate - converted from yr, mo, dy provided in input
    """
    
    # reduce to unique plot/period events & add column denoting that
    # records are linked to real trapping events
    cols = [col for col in data.columns if col in ['plot', 'period', 'yr', 'mo',
                                                   'dy']]
    raw_sample_data = data[cols]
    raw_sample_data = raw_sample_data.dropna(subset=["plot"])
    raw_sample_data = raw_sample_data.dropna(subset=["period"])
    Trapping_Table = raw_sample_data.drop_duplicates()
    Trapping_Table['sampled'] = 1
    
    # generate list of periods with fewer than 24 plots recorded
    plot_list = Trapping_Table['plot'].unique()
    period_list = Trapping_Table['period'].unique()
    plot_counts = Trapping_Table.groupby('period').plot.nunique()
    plot_counts = pd.DataFrame(plot_counts)
    plot_counts = plot_counts.rename(columns = {'plot':'plot_count'})
    plot_counts.reset_index(inplace=True)
    periods_missing_plots = plot_counts[plot_counts['plot_count'] < len(plot_list)]
    
    # Find missing plots for a particular given period
    
    short_periods = periods_missing_plots['period'].unique()
    new_data = pd.DataFrame(columns=['period', 'plot', 'sampled'])
    for unique_period in short_periods:
        short_period_data = raw_sample_data[raw_sample_data['period'] == unique_period]
        short_period_plots = set(short_period_data['plot'].unique())
        missing_plots = set(plot_list).difference(short_period_plots)
        year = int(short_period_data['yr'].min())    #selects beginning date of session
        month = int(short_period_data['mo'].min())
        day = int(short_period_data['dy'].min())    
        for each_plot in missing_plots:
            plot_data = pd.DataFrame([[year, month, day, unique_period, each_plot, 0]],
                                     columns=['yr', 'mo', 'dy','period', 'plot', 'sampled'])
            new_data = new_data.append(plot_data, ignore_index=True)
    
    # Add information about missed plots to the trapping table
    
    Trapping_Table = Trapping_Table.append(new_data, ignore_index=True)
    
    # Convert Gregorian Date to a Julian Date
    
    Trapping_Table['JulianDate'] = Trapping_Table.apply(convert_to_JulianDate, axis=1)    
    return Trapping_Table

def convert_to_JulianDate(row):
    
    """Takes each row of the Trapping_Table dataframe and applies the 
    Gregorian Date to Julian Date Converter"""
    
    return sum(jdcal.gcal2jd(int(row['yr']), int(row['mo']), 
                             int(row['dy']))) 




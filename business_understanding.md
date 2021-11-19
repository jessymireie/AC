# Business Understanding

## Analysis of requirements with the end user
The bank in question is from the Czech Republic, as are its clients. The records are from the 1990's - the loans from the training set date from 1993 until 1996 whilst the testing dataset contains loan requests exclusively from the year 1997.
Using the bank data stored of its clients, the bank is looking to determine if a client is predisposed to have their loan request approved. From the bank's perspective, this probably would be approached using differents method. Based on our reasearch, several aspects come to play, such as: 
- credit, salary and employment history;
- crash flow and past expenses;
- owned goods and collaterals;
- character and personal information;
- characteristics of the loan request (duration, amount, etc).

## Definition of business goals
The bank's main focus would be predicting beforehand which loan request should be accepted or not - this decision would relay on trying to understand which client's would be able to pay the requested loan amount in full and on time. The motivation behind only partaking in successful loans, would be to minimize the bank's losses (decrease its current deficit). The bank's current success rate regarding loan's outcomes is at 85.97561%. The desired result would be a 100% success rate. Although obtaining that result would be virtually impossible, the goal here would be to get as close to 100 as possible.


## Translation of business goals into data mining goals

The main goal is to find a model that will predict accurately whether a loan request will be approved by the bank or not. Using the data provided by the bank on its clients, we can conduct 2 different studies. Firstly, a descriptive approach would help us to better understand the current trends. Given the past experiences the bank has had with loans, we could look for patterns or factors that show a tendency in results (whether the loan should have been approved or not). This purely informative review would both help understand what led to the previous results and give a starting point for the second study to be conducted: one with a predictive end goal. A predictive research using data mining would look for anomalies, patterns and correlations within the large data set provided by the bank to predict outcomes, in this case loan approvals.

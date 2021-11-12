loans_successful <- dplyr::filter(dpersonal, status == 1)
loans_unsuccessful <- dplyr::filter(dpersonal, status == -1)

print("Successful loans")
print(summarise(loans_successful, nbWomen = sum(gender == "F"), nbMen = sum(gender == "M"), avgAge = mean(age), 
                avgSalary = mean(average_salary), avgUnemploymentRate95 = mean(unemployment95, na.rm=TRUE),
                avgUnemploymentRate96 = mean(unemployment96), avgCrimeRate95 = mean(crime95, na.rm=TRUE), avgCrimeRate96 = mean(crime96)))

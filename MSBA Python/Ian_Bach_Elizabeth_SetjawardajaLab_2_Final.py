"""
Created on Fri Mar 22 09:59:01 2024

@author: Ian Bach and Elizabeth Setjawardaja
"""

#Called function to determine user's monthly payment based on credit score
def monthly_payment_percentage (passedCreditScore, passedMonthlyGrossIncome):
    if int(passedCreditScore) < 530:
        #15% of gross monthly income
        monthly_payment = float(passedMonthlyGrossIncome) * float(0.15)
    elif int(passedCreditScore) < 590:
        #15% of gross monthly income
        monthly_payment = float(passedMonthlyGrossIncome) * float(0.15)
    elif int(passedCreditScore) < 640:
        #17% of gross monthly income
        monthly_payment = float(passedMonthlyGrossIncome) * float(0.17)
    elif int(passedCreditScore) < 721:
        #20% of gross monthly income
        monthly_payment = float(passedMonthlyGrossIncome) * float(0.20)
    else:
        #30% of gross monthly income
        monthly_payment = float(passedMonthlyGrossIncome) * float(0.30)
    return str(monthly_payment)

#Called function to calculate the maximum allowable principal loan
def principal_loan(monthlypayment,interest,monthlength):
    principal_loan_result = (monthlypayment/interest)*(1-(1+interest)**(-monthlength))
    return str(principal_loan_result)

#Main Program
#During the process user will input the following 4 items
#Input Annual Gross Amount
continue_pre_approval = 'yes'
while continue_pre_approval.lower() == 'yes':
    annual_gross_income = input("Please input your annual gross income. ")
    #Monthly Gross Income
    monthly_gross_income = float(annual_gross_income) / int(12)
    #Input Length of Loan 
    length_of_loan = input("Please input the desired length of your auto loan (in year). ")
    length_of_loan_monthly = int(length_of_loan) * int(12)
    #Input Interest Rate Monthly
    interest_rate = input("Please input the annual interest rate (in percentage and do not use symbol). ")
    interest_rate_monthly = float(interest_rate) / int(1200)
    #Input Credit Score
    credit_score = input("Please input your current credit score. ")
    continue_pre_approval = input("Do you have more pre-approvals to process? (yes/no): ")
    
#After the process monthly payment and maximum allowable principal amount will be printed
print()
print()
print()

#Print Monthly payment for auto loan
monthly_payment_value = float(monthly_payment_percentage(credit_score,monthly_gross_income))
print("The monthly payment for the auto loan is ${:.2f}.".format(monthly_payment_value))

print()

#Print Principal Loan Amount
principal_loan_value = float(principal_loan (monthly_payment_value,interest_rate_monthly,length_of_loan_monthly))
print("The principal loan amount is ${:.2f}.".format(principal_loan_value))

#End the pre approval calculator
print("Thank you for using the auto loan pre-approval calculator.")

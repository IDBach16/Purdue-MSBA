
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 10 11:36:16 2024

@author: IBach
"""

total_msrp = 0
total_cost = 0
total_potential_gross_profit = 0
total_adjusted_gross_profit = 0

#####################################################################

#Car1
print("Enter details for car #1:")
car_name1 = input("Car name 1 (e.g., Red 1969 Triumph TR6): ")
msrp1 = int(input("MSRP: "))
cost1 = int(input("Cost: "))
print("Type (0=domestic, 1=import): ")
car_type_code1 = int(input())

# Determine car type
car_type1 = "domestic" if car_type_code1 == 0 else "import"

#Calculate gross profit and adjusted gross profit
gross_profit1 = msrp1 - cost1
adjusted_gross_profit1 = gross_profit1 * (0.9825 if car_type_code1 == 1 else 1)


####################################################################
#Car2
print("Enter details for car #2:")
car_name2 = input("Car name 2 (e.g., Red 1969 Triumph TR6): ")
msrp2 = int(input("MSRP: "))
cost2 = int(input("Cost: "))
print("Type (0=domestic, 1=import): ")
car_type_code2 = int(input())

# Determine car type
car_type2 = "domestic" if car_type_code2 == 0 else "import"

#Calculate gross profit and adjusted gross profit
gross_profit2 = msrp2 - cost2
adjusted_gross_profit2 = gross_profit2 * (0.9825 if car_type_code2 == 1 else 1)


######################################################################
#Car3
print("Enter details for car #3:")
car_name3 = input("Car name 3 (e.g., Red 1969 Triumph TR6): ")
msrp3 = int(input("MSRP: "))
cost3 = int(input("Cost: "))
print("Type (0=domestic, 1=import): ")
car_type_code3 = int(input())

# Determine car type
car_type3 = "domestic" if car_type_code3 == 0 else "import"

#Calculate gross profit and adjusted gross profit
gross_profit3 = msrp3 - cost3
adjusted_gross_profit3 = gross_profit3 * (0.9825 if car_type_code3 == 1 else 1)


#########################################################################
#Car4
print("Enter details for car #4:")
car_name4 = input("Car name 4 (e.g., Red 1969 Triumph TR6): ")
msrp4 = int(input("MSRP: "))
cost4 = int(input("Cost: "))
print("Type (0=domestic, 1=import): ")
car_type_code4 = int(input())

# Determine car type
car_type4 = "domestic" if car_type_code4 == 0 else "import"

#Calculate gross profit and adjusted gross profit
gross_profit4 = msrp4 - cost4
adjusted_gross_profit4 = gross_profit4 * (0.9825 if car_type_code4 == 1 else 1)

#########################################################################
#Car5
print("Enter details for car #5:")
car_name5 = input("Car name 5 (e.g., Red 1969 Triumph TR6): ")
msrp5 = int(input("MSRP: "))
cost5 = int(input("Cost: "))
print("Type (0=domestic, 1=import): ")
car_type_code5 = int(input())

# Determine car type
car_type5 = "domestic" if car_type_code5 == 0 else "import"

#Calculate gross profit and adjusted gross profit
gross_profit5 = msrp5 - cost5
adjusted_gross_profit5 = gross_profit5 * (0.9825 if car_type_code5 == 1 else 1)

################################################################################

#Totals for all cars
total_cost = sum([cost1, cost2, cost3, cost4, cost5])
total_msrp = sum([msrp1, msrp2, msrp3, msrp4, msrp5])
total_potential_gross_profit = sum([gross_profit1, gross_profit2, gross_profit3, gross_profit4, gross_profit5])
total_adjusted_gross_profit = sum([adjusted_gross_profit1, adjusted_gross_profit2, adjusted_gross_profit3, adjusted_gross_profit4, adjusted_gross_profit5])

##############################################################################

#Totals Car 1
print("Details for Car 1:")
print("Car Name:", car_name1)
print("MSRP:", msrp1)
print("Cost:", cost1)
print("Potential Gross Profit:", gross_profit1)
print("Adjusted Gross Profit:", adjusted_gross_profit1)

#Totals Car 2
print("Details for Car 2:")
print("Car Name:", car_name2)
print("MSRP:", msrp2)
print("Cost:", cost2)
print("Potential Gross Profit:", gross_profit2)
print("Adjusted Gross Profit:", adjusted_gross_profit2)

#Totals Car 3
print("Details for Car 3:")
print("Car Name:", car_name3)
print("MSRP:", msrp3)
print("Cost:", cost3)
print("Potential Gross Profit:", gross_profit3)
print("Adjusted Gross Profit:", adjusted_gross_profit3)

#Totals Car 4
print("Details for Car 4:")
print("Car Name:", car_name4)
print("MSRP:", msrp4)
print("Cost:", cost4)
print("Potential Gross Profit:", gross_profit4)
print("Adjusted Gross Profit:", adjusted_gross_profit4)

#Totals Car 5
print("Details for Car 5:")
print("Car Name:", car_name5)
print("MSRP:", msrp5)
print("Cost:", cost5)
print("Potential Gross Profit:", gross_profit5)
print("Adjusted Gross Profit:", adjusted_gross_profit5)

#Total
print("Totals for the shipment:")
print("Total MSRP:", total_msrp)
print("Total Cost:", total_cost)
print("Total Potential Gross Profit:", total_potential_gross_profit)
print("Total Adjusted Gross Profit:", total_adjusted_gross_profit)



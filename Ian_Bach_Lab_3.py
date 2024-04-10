# -*- coding: utf-8 -*-
"""
Created on Wed Apr 10 12:08:12 2024

@author: IBach
"""
# creating list called allAgents[] with all Agent Names listed
allAgents = ['Angela Wright', 'Roberto Major', 'Mary Jones']
# creating nested dictionary called allCars{} with data about all cars in the inventory and also their activities/actions associated with them
allCars = {'Spider': {'agentID': 2, 'oil change': 2, 'fixes': 1, 'waiver':1, 'bodywork incidents':1},
           'Triangle': {'agentID': 1, 'oil change': 6, 'upgrades': 3},
           'Shamrock': {'agentID': 0, 'fixes': 6, 'upgrades': 2},
           'Marty': {'agentID': 2, 'fixes':5, 'bodywork incidents':2}}

# action count function: this function is used to count the actions of all cars, the user will be asked which actions to count
# the allCars dictionary and the lookupAction (entered by user in main code) will be passed
def actionCount (passed_allCars, passed_lookupAction):
  # the count of action is defined and set as 0
  totalAction = 0
  # call the keys and values of the passed_allCars dicitionary and get the items of the dictionary associated with the key and value
  for k, v in passed_allCars.items():
    # equation calculates the total count of actions, it gets the value for each key and adds them up
    totalAction = totalAction + v.get(passed_lookupAction, 0)
  # returns the count of total actions for this specific action in the allCars dictionary
  return totalAction

#main program starts
# create variable for lookupAction and ask the user which action they want to look up
lookupAction = input('Which action do you want to lookup?: ')
result = actionCount(allCars, lookupAction)
print('Total of ' + lookupAction + ' ------> ' + str(result))

# create variable which car the user wants to look up
specificCar = input('Which car do you want to lookup?: ')
# create variabel which action the user wants to look up for the earlier entered car they want to check
specificAction = input('Which action do you want to look up for ' + specificCar +'?: ')

# call the keys and values of the allCars dicitionary and get the items of the dictionary associated with the key and value
for k, v in allCars.items():
  # if statement created, determining that if the key (referred to in the allCars dictionary) is the same value as the specifcCar (which is an input above), then the loop will begin.
  if k == specificCar:
    # define variable and call the value for this specific car with the key agentID, if no agent listed then return 0
    getAgentID = v.get('agentID', 0)
    # use the above created variable with the AgentID for this specific car. Call the list allAgents and find the Agent Name with this agent ID
    getAgentName = allAgents[int(getAgentID)]
    # define variable and call the value for specific car and specific action defined earlier from the dictionary allCars, if not action listed then return 0
    actionforCar = v.get(specificAction, 0)
    # print all gathered information about the specific car, the amount of actions need to be done for the specific action required and the name of the agent
    print(specificCar + ' had ' + str(actionforCar) + ' ' + specificAction + ' and the agent is ' + allAgents[getAgentID] + '.')
    # if the action the user wants to look up is "oil change" and the value is larger then 5, the user will get a warning to let the mechanic check the engine
    if specificAction == 'oil change' and actionforCar > 5:
      # print warning if above statement is true
      print('Warning: You should probably have the mechanic check the engine for ' + specificCar + '.')
      break
    else:
      break
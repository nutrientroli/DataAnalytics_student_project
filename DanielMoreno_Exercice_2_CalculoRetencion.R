
calculateRetentionOnDay = function(vector,dayToCalculate) {
  
  #dayToCalculate = 1      # Martes
  dayToCalculate = dayToCalculate      # Martes
  
  day0Index = 1
  day0Index = which(vector > 0)
  if (is.null(day0Index) == FALSE) {
    day0Index = min(day0Index)
  }
  
  print(day0Index)
  
  retentionDay =(vector[day0Index + dayToCalculate ] / vector[day0Index]) * 100
  
  return (retentionDay)
}
calculateChurnOnDay = function(retention) {
  
  churnRateDay = 100 - retention
  # churnRateDay[churnRateDay < 0] = churnRateDay * -1
  
  return(churnRateDay)
}

# Compute retention and churn rate at day1, and day 4, considering the list of users per day that follows:

l1 = c(1,1,1,1,2,2)
l2 = c(2,2,2,2,6,6)
l3 = c(3,3,4,6,9,9)
l4 = c(4,4,6,9,11,11)
l5 = c(5,6,9,11,13,13)
l6 = c(6,8,11,13,15,15)
l7 = c(7,9,13,14,17,17)
l8 = c(8,11,14,15,18,18)
l9 = c(9,12,15,17,20,20)
l10 = c(10,13,16,18,21,22)
l11 = c(0,14,17,19,22,24)
l12 = c(0,15,18,20,24,25)
l13 = c(0,16,19,21,25,26)
l14 = c(0,0,20,20,26,27)
l15 = c(0,0,21,23,27,29)
l16 = c(0,0,22,24,28,30)
l17 = c(0,0,0,25,29,31)
l18 = c(0,0,0,26,30,32)
l19 = c(0,0,0,27,31,33)
l20 = c(0,0,0,28,32,34)
l21 = c(0,0,0,29,33,35)
l22 = c(0,0,0,0,0,36)
l23 = c(0,0,0,0,0,0)



myMat = rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23)
colnames(myMat) = c("Mond","Tue","Wed","Thurs","Frid","Sat")
rownames(myMat) = c("Sample_1","Sample_2","Sample_3","Sample_4","Sample_5","Sample_6","Sample_7","Sample_8","Sample_9","Sample_10","Sample_11","Sample_12","Sample_13","Sample_14","Sample_15","Sample_16","Sample_17","Sample_18","Sample_19","Sample_20","Sample_21","Sample_22", "Sample_23")
myMat 


# Calculate the Retention rate for day 1 and day 4

# Como no entiendo como trabajar con matrices he hecho la formulacion para trabajar con vectores
# cuando entienda como trabajr con matrices lo adabpare para que utilice la tabla entera

# 1a Remove all the 0 values to get the table lined up
# NOT SUCCESFULL
#myMat
#refinedMat =  myMat[myMat != 0] # exclude the 0 on the new matrix PORQUE ME DEVUELVE VALORES TAN  RRIJUOS
#refinedMat

# 1B smart operation

# DAY 1 matrix ---------------------------------------- //

# retentionDay
# Create a loop iterating throught the matrix

acumulatedRetentionDay1 = c(0)
acumulatedRetentionDay4 = c(0)
acumulatedChurnRateDay1 = c(0)
acumulatedChurnRateDay4 = c(0)
retentionOnDay1 = c(0)
retentionOnDay4 = c(0)
churnRateDay1 = c(0)
churnRateDay4 = c(0)

for(row in 1:nrow(myMat)) {
  
  # retention -------------------------------------------------- //
  retentionOnDay1 = calculateRetentionOnDay(myMat[row,],1)
  if (is.na(retentionOnDay1) == FALSE) {
    acumulatedRetentionDay1  = acumulatedRetentionDay1 + retentionOnDay1
  }
  # print(paste("Retention on day 1 ", retentionOnDay1))
  
  if (row == nrow(myMat)) {
    acumulatedRetentionDay1 = acumulatedRetentionDay1 / nrow(myMat)
    print("The retention on day 1 is") 
    print(acumulatedRetentionDay1)
  }
  
  # churn rate ---------------------------------------- //
  churnRateDay1 =  calculateChurnOnDay(retentionOnDay1)
  # print(paste("Churn rate on day 1" , churnRateDay1))
  
  if (is.na(churnRateDay1) == FALSE) {
    acumulatedChurnRateDay1  = acumulatedChurnRateDay1 + churnRateDay1
  }
  
  if (row == nrow(myMat)) {
    acumulatedChurnRateDay1 = acumulatedChurnRateDay1 / nrow(myMat)
    print("The churn rate on day 1 is") 
    print(acumulatedChurnRateDay1)
  }
  
}

for(row in 1:nrow(myMat)) {
 
  retentionOnDay4 = calculateRetentionOnDay(myMat[row,],4)
  
  if (is.na(retentionOnDay4) == FALSE) {
    acumulatedRetentionDay4  = acumulatedRetentionDay4 + retentionOnDay4
  }
  #print(paste("Retention on day 4 ", retentionOnDay4))

  if (row == nrow(myMat)) {
    acumulatedRetentionDay4 = acumulatedRetentionDay4 / nrow(myMat)
    print("The retention on day 4 is") 
    print(acumulatedRetentionDay4)
  }
  
  # churn rate ---------------------------------------- //
  churnRateDay4 =  calculateChurnOnDay(retentionOnDay4)
  # print(paste("Churn rate on day 1" , churnRateDay1))
  
  if (is.na(churnRateDay4) == FALSE) {
    acumulatedChurnRateDay4  = acumulatedChurnRateDay4 + churnRateDay4
  }
  
  if (row == nrow(myMat)) {
    acumulatedChurnRateDay4 = acumulatedChurnRateDay4 / nrow(myMat)
    print("The churn rate on day 4 is") 
    print(acumulatedChurnRateDay4)
  }
  
}







# DAY 1 ----------------------------------------------- //

retentionDay1 = calculateRetentionOnDay(l1,1)

# DAY 4 ----------------------------------------------- //

retentionDay4 = calculateRetentionOnDay(l1,4)

# dayToCalculate = 4      # Viernes

# day0Index = which(l1 > 0)
# day0Index = min(day0Index)
# day0Index

# retentionDay4 = l1[dayToCalculate + 1] / l1[day0Index] * 100
# retentionDay4

# Calculate the churn rate for the day 1 and 4

churnRateDay1 = calculateChurnOnDay(retentionDay1)
churnRateDay4 = calculateChurnOnDay(retentionDay4)

churnRateDay1
churnRateDay4

#
#churnRateDay1 = 100 - retentionDay1
#churnRateDay1[churnRateDay1 < 0] = churnRateDay1 * -1
#churnRateDay1

#churnRateDay4 = 100 - retentionDay4 
#churnRateDay4[churnRateDay4 < 0] = churnRateDay4 * -1
#churnRateDay4

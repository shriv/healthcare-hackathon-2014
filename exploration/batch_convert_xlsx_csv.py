## Python script modidied from http://superuser.com/questions/192306/xls-to-csv-on-mac

import xlrd
import csv

filename = 'PCA_Apr_14.xlsx'
book = xlrd.open_workbook(str('/Users/shrividya/Downloads/nhs-prescriptions-costs-aug13-aug14/'+filename))

# Assuming the fist sheet is of interest 
sheet = book.sheet_by_index(0)

# Many options here to control how quotes are handled, etc.
csv_filename = str(filename.split('.')[0]+'.csv')
csvWriter = csv.writer(open(csv_filename, 'w'), delimiter=',') 

for i in range(sheet.nrows -1):
    csvWriter.writerow(sheet.row_values(i+1))

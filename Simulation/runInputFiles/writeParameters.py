#require python 3.6 or above
# usage
# python writeParameters.py -i <paramExampleFile> -p <paraList> -n <run number>
import csv,random
from optparse import OptionParser
import sqlite3 as lite
import numpy as np
import sys
import os.path
from scipy import stats
from scipy.optimize import curve_fit

parser = OptionParser()
parser.add_option("-p", "--parameters", dest="paraList",\
					help="csv file for all the parameter combinations", metavar="FILE")
parser.add_option("-i", "--input", dest="paramExampleFile",\
					help="input template file", metavar="FILE")
parser.add_option("-n", "--number", dest="outNumber",type = "int",\
					help="the run number")
parser.add_option("-s", "--seasonality", dest="seasonality",action = "store_true",\
					help="whether running seasonality or not", default = False)
parser.add_option("-r", "--rep", dest="repNumber",type = "int",\
					help="define how many replications of control is run")
parser.add_option("-x", "--prefix", dest="prefix",type = "string",\
					help="prefix for output filenames")
                  
(options, args) = parser.parse_args()


if __name__ == "__main__":
	#step 1 read in parameter combination file
	allParamFile = open(options.paraList,newline = '')
	allParam = csv.DictReader(allParamFile)
	
	#step 2 read in input file template
	prototype = open(options.paramExampleFile,"r").read()
	
	#iterate through the list and find the correct run number
	for row in allParam:
	  if os.path.isfile(row['DAILY_BITING_RATE_DISTRIBUTION']):
	    f = open(row['DAILY_BITING_RATE_DISTRIBUTION'], newline = '')
	    db = csv.reader(f)
	    dbVec = []
	    for y in db:
	      dbVec.append(y[1])
	  if row['No'] == str(options.outNumber):
	    for r in range(options.repNumber):
	      if options.seasonality:
	        row['DAILY_BITING_RATE_DISTRIBUTION'] = ','.join(dbVec)
	      else:
	        row['DAILY_BITING_RATE_DISTRIBUTION'] = []
	      row['RANDOM_SEED'] = random.randint(1,10000000000)
	      row['SAMPLE_DB_FILENAME'] = options.prefix + "_" + row['No'] + "_r" + str(r) + "_sd.sqlite"
	      hspVec2=[180,300]
	      row['HOST_SAMPLING_PERIOD'] = hspVec2
	      out = open(options.prefix + "_" + row['No'] + "_r" + str(r) +  "_input.py", "w")
	      out.write(prototype.format(**row))
	      out.close()
	    break

import os
import json
import csv
import re
from operator import itemgetter

#saving to directory
DIR ='data' 

#file title config (also allows for capitalised version)
edgeFileName = "matrix"
nodeFileName = "bank"

#global Variables
sectorKey = 'Sector'
nameKey = 'Node'
colorKey = 'Color'
decimalSeparator = "."
scalingForValues = 1

def gettimestamp(filename):
    thestring = filename
    timeformat = re.compile("20\d\d\d\d")
    result = timeformat.search(thestring)
    if result:
        return (result.string[result.start():result.end()])

def cleaned_input(file):
    try:
        csv_input = csv.reader(file,delimiter='|')
        data=[]
        for idx,row in enumerate(csv_input):
                        for item in row:
                            item.replace('"','')
                        cleanedRow= row[0].split(',')
                        if idx == 0:
                            keyArray = cleanedRow
                        else:
                            valueArray = cleanedRow
                            dictRow = {}
                            for idx1,value in enumerate(valueArray):
                                dictRow[keyArray[idx1]] = value
                            #add_date(dictRow,edgeDates)
                            data.append(dictRow)
    except TypeError:
        return []
    return data

def add_date(row,dateString):
    year=dateString[0:4]

    if dateString[4]== '0':
        month=dateString[5]
    else:
        month=dateString[4:6]
    
    formattedDate=year+'/'+month
    dateID=int(year)*12+int(month)

    row['date']=formattedDate
    row['dateID']=dateID
    return

def add_assetName(row,assetName):
    row['asset']=assetName
    return

def add_regionName(row,regionName):
    row['region']=regionName
    return

def read_file(file,filename,region,asset):
    data = cleaned_input(file)

    for row in data:
        add_date(row,gettimestamp(filename))
        add_assetName(row,asset.name)
        add_regionName(row,region.name)

    return data

def fix_decimals(values):
    dotValues=[]
    for val in values:
        val = str(val)
        val = val.replace(decimalSeparator,".")
        dotValues.append(val)
    return dotValues

def check_forZeros(row,categoryKeys):
    allZeros=True
    for key in categoryKeys:
        if (row[key]>0):
            allZeros=False
            return allZeros
    return allZeros

def set_row(inputRowRaw,keys,categoryKeys):

    #to list
    inputRow=list(inputRowRaw.values())
    
    #create output
    outputRow={}
    outputRow['id']=str(inputRow[0])
    outputRow['name']=str(inputRow[keys.index(nameKey)])
    outputRow['sector']=str(inputRow[keys.index(sectorKey)])
    outputRow['date']=inputRowRaw['date']
    outputRow['dateID']=inputRowRaw['dateID']
    outputRow['region']=inputRowRaw['region']
    outputRow['asset']=inputRowRaw['asset']

    #adding categories
    for key in categoryKeys:
        val = str(inputRowRaw[key])
        val = val.replace(decimalSeparator,".",1)
        try:
            outputRow[key]=float(val)/scalingForValues
        except ValueError or TypeError:
            outputRow[key]=0
    return outputRow

def set_keys():
    return ['Node','Sector','time','Assets','Liabilities','date','dateID']  

def set_categories():
    categoryKeys = ['Assets','Liabilities','no value']
    return categoryKeys

def set_dates(nodes):
    unsortedDates=[]
    unique=[]
    for node in nodes:
        if node['dateID'] not in unique:
            unsortedDates.append({'date': node['date'], 'dateID': node['dateID']})
        unique.append(node['dateID'])
    dates = sorted(unsortedDates, key=itemgetter('dateID')) 
    return dates

def set_sectors(nodes):
    sectors=[]
    unique=[]
    for node in nodes:
        if node['sector'] not in unique:
            sectors.append({'sector': node['sector']})
        unique.append(node['sector'])
    sectors.append({'sector': 'all'})
    return sectors

def set_names(nodes):
    banks=[]
    unique=[]
    for node in nodes:
        if node['id'] not in unique:
            banks.append({'id': node['id']})
        unique.append(node['id'])
    return banks

def transform_nodes(nodes,keys):
    categoryKeys = set_categories()
    transformedNodes=[]    
    for idx, row in enumerate(nodes):
        outputRow = set_row(row,keys,categoryKeys[:-1])
        allZeros = check_forZeros(outputRow,categoryKeys)
        #don't add unspecified ids or nodes for which all numeric values = 0
        if outputRow['id'] is not '' and allZeros is False:
            transformedNodes.append(outputRow)
    return transformedNodes

def transform_edges(edges):
    transformedEdges=[]

    for row in edges:
        rowValues=list(row.values())
        rowValues=fix_decimals(rowValues)
        rowKeys=list(row.keys())

        #source is the 0 column
        source=rowValues[0]
        region=row['region']
        asset=row['asset']
        idx=1

        #skipping over the first column
        while idx < len(rowKeys)-1:
            #all targets start with '_'
            if '_' in rowKeys[idx]:
                target = rowKeys[idx][1:]
                #only add edges not connecting node to itself
                try:
                    float(rowValues[idx])
                except ValueError:
                    return []
                if source != target and float(rowValues[idx]) != 0: 
                    transformedEdges.append({'region': region, 'asset': asset, 'from': source, 'to': target, 'absValue': int(float(rowValues[idx])/scalingForValues), 'date': row['date'], 'dateID': row['dateID']})
            idx+=1
    return transformedEdges

def upload_files():
    path=os.path.abspath("C:/Users/Philippa/Desktop/Econ research/Whom to whom/Data")
    edges=[]
    nodes=[]
    #iterator 1 for all regions
    with os.scandir(path) as it1:
        for region in it1:
            if not region.name.startswith('.'):
                #iterator 2 for all assets
                with os.scandir(region) as it2:
                    for asset in it2:
                        if not asset.name.startswith('.'):
                            #iterator for individual files
                            for filename in os.listdir(asset):
                                filepath=os.path.join(region,asset, filename)
                                with open(filepath) as file:
                                    #collecting edge files
                                    if edgeFileName in filename or edgeFileName.capitalize() in filename:
                                        edges = edges + read_file(file,filename,region,asset)
                                    if nodeFileName in filename or nodeFileName.capitalize() in filename:
                                        nodes = nodes + read_file(file,filename,region,asset) 

    edges=transform_edges(edges)
    nodes=transform_nodes(nodes,set_keys())
    
    #OUTPUT CHECK
    for row in edges[1:10]:
        print(row)
    for row in nodes[1:10]:
        print(row)
   
    data = [edges,nodes]
    return data

def save_data():
    # data[0]=edges, data[1]=nodes 
    edges,nodes = upload_files()
    categoryKeys = set_categories()
    dates = set_dates(nodes)
    sectors = set_sectors(nodes)
    banks = set_names(nodes)
    data = {"edges": edges,"nodes":nodes, "categoryKeys": categoryKeys, "dates": dates, "sectors":sectors,"banks":banks}
    
    filename = 'data.json'
    with open (filename,'w') as file:
        json.dump(data,file)

    '''
    filename = 'edges.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(edges,file)

    filename = 'nodes.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(nodes,file)

    filename = 'dates.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(dates,file)

    filename = 'sectors.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(sectors,file)

    filename = 'banks.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(sectors,file)
    
    filename = 'categoryKeys.json'
    filepath = os.path.join(DIR, filename)
    with open (filepath,'w') as file:
        json.dump(categoryKeys,file)
    '''
if __name__ == '__main__':
   save_data()
   
   
   
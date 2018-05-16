const compareVals = (a,b) => {
    if (a['value'] < b['value'])
      return -1;
    if (a['value'] > b['value'])
      return 1;
    return 0;
  }

const sortOnValue = (dictArr,value) => {
  return dictArr.sort((a,b) => {
    return a[value]-b[value]
  })
}  

const unique = (dictArr,value) => {
  let all = dictArr.map((item) => {return item[value]})
  return all.filter((item, pos) => {
  return all.indexOf(item) == pos
  })
}
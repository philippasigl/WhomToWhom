"use strict";

var _xHighlighted = 0
var _xHighlighted2 = 0
var _ratio = 0
var _xHighlightedName

//black,green,red,grey
const COLORS = { 'unchanged' :'#222233', 'increased' : '#4DC19C', 'decreased' : '#F89570', 'no_info': '#778899'}
const BLURRED_LINK_OPACITY = 0.3
const FOCUSED_LINK_OPACITY = 0.6
//https://www.hexdictionary.com/common-colors
const SECTOR_COLORS = ['#000077', '#008877','#0055ee', '#00ddcc', '#00cc77']
const EXTENDED_COLORS = [
  '#19CDD7', //turc
  '#DDB27C',
  '#88572C',
  '#FF991F', //light orange
  '#F15C17', //dark orange
  '#223F9A', //blue
  '#DA70BF', //pink
  '#4DC19C', //green
  '#12939A', //turk
  '#B7885E', //light brown
  '#FFCB99', //orange/sand
  '#F89570', //light red
  '#E79FD5', //pink
  '#89DAC1' //light green
]

const setOptions = () => { 
  const OPTIONS = {
        //detect container resizing and redraw
        autoResize: true,
        height: '100%',
        width: '100%',
        physics: {
          enabled: false,
          hierarchicalRepulsion: {nodeDistance: 400}
        },
        layout: { 
          hierarchical: { 
            direction: 'LR',
            levelSeparation: 600,
            nodeSpacing: 1000 
          },
          randomSeed: undefined,
          improvedLayout: true
        },
        interaction: {
          dragNodes: false,
          selectConnectedEdges: true,
          multiselect: true
        },
        edges: {
          font: {face: 'Roboto'},
          color: {color:'lightgrey'},
          arrows: {to: {enabled: true}},
          smooth: {
            type: 'curvedCW',
            forceDirection: 'horizontal',
            roundness: 0.4
          },
          scaling: {min: _edgeRange[0], max: _edgeRange[1]}
        },
        nodes: {
          font: {color: 'white', face: 'Roboto'},
          borderWidth: 0,
          color: {border: 'white', background: '#89DAC1', highlight: {background: '#FF991F'}},
          shape: 'circle',
          margin: 10,
          scaling: {min: _nodeRange[0], max: _nodeRange[1], label: {min: _nodeRange[0], max: _nodeRange[1]}}
        },
        //currently unused. range slider for node and edge sizes implemented with nouislider
        configure: {
          enabled: false,
          filter: function (option, path) {
            return path.indexOf('nodes') !== -1 && path.indexOf('scaling') !== -1;
          }
        },
        /*
        physics: {
         stabilization: true,
         forceAtlas2Based: {
              gravitationalConstant: -500,
              centralGravity: 0.01,
              springLength: 100,
              springConstant: 0,
              damping: 0.08
         },
          "minVelocity": 0.75,
          "maxVelocity": 100,
          "solver": "forceAtlas2Based",
          "timestep": 1
        }
        */
      }
    //console.log(OPTIONS)  
    return OPTIONS
}

const hideZeroValues = () => {
  //_edges.map((edge) => {if (edge.value==0) edge.hidden=true; else edge.hidden=false;})
  _nodes.map((node) => {if (node.value==0) node.hidden=true; else node.hidden=false;})
}

//UNUSED
const nodeScale = (nodes) => {
  return 20/nodes.length
}

const nodeColorsBySector = () => {
    var colors = SECTOR_COLORS
    var len = sectors.length/COLORS.length
    if (len>1) {
      for (var i=0; i< len+1; i++) {
        colors = colors.concat(colors)
      }
    }
    var cols = {}
    sectors.map((sector, idx) => cols[sector.sector]= colors[idx]) 
    _nodes.map((node) => { node.color = {background: cols[node.sector]}; return node})
    network.setData({nodes: _nodes, edges: _edges})
}

const edgeColorsByTrend = () => {
    _edges.map((edge) => {edge.color = {color: COLORS[edge['trend']], opacity: 0.2, highlight: COLORS[edge['trend']]}; return edge})
    network.setData({nodes: _nodes, edges: _edges})
}

const edgeColorsUniform = () => {
    //var nodesArray = objectToArray(network.body.data.nodes._data);
    //var edgesArray = objectToArray(network.body.data.edges._data);
    //edgesArray = edgesArray.map((edge) => { edge.color = {color: 'lightgrey', opacity: 0.2, highlight: '#FF991F'}; return edge})
    _edges.map((edge) => { edge.color = {color: 'lightgrey', opacity: 0.2, highlight: '#FF991F'}; return edge})
    network.setData({nodes: _nodes, edges: _edges})
}

const edgeRank = () => {
  let values = _edges.map((edge) => { return edge.value})
  _edges.sort(compareVals)
  _edges.map((edge,idx) => {
      edge['rank']=idx/_edges.length
      edge['absRank']=idx
  })
}

const edgeSelectBySize = (cutOffPoint) => {
  _edges.map((edge) => {
    if (edge.rank<cutOffPoint) edge.hidden=true
    else if (edge.value == 0 ) edge.hidden=true 
    else edge.hidden=false
  })
}

const edgeHighlightLargest = () => {
  _edges.map((edge) => {if (edge.absRank>_edges.length-4) { edge.color = {color: '#FF991F', opacity: 1}; return edge} })
  network.setData({nodes: _nodes, edges: _edges})
}

const herfindahl = () => {
  let uniqueNodes = unique(_nodes,"id")
  let sumByNode = uniqueNodes.map((node) => {
    let total=0
    _edges.map((edge) => { if (edge.from==node || edge.to==node) total=total+edge.absValue})
    return total
  })
  let totalSum=0
  sumByNode.map((item) => {totalSum=totalSum+item})
  let shares = sumByNode.map((item) => { return item/totalSum})
  let herf=0
  shares.map((share) => herf=herf+share*share)
  let result = herf.toPrecision(4)*100
  return result.toFixed(2)
 // _nodes((node) => {if (node)})
}

const setRatio = () => network.on("select", function (param) {
  if (param.edges.length==0 && param.nodes.length==0) return
  //get the selected item and check whether node or edge is highlighted
  let selection = network.getSelection()
  let val
  let name

  //if the selection is an edge
  if (selection.nodes.length == 0) {
    let arr = network.body.edges[selection.edges[0]]
    val = arr.options.value
    name = arr.options.from + " to " + arr.options.to
  }  
  //if the selection is a node
  else {
    let arr = network.body.nodes[selection.nodes[0]]
    val = arr.options.value
    name = arr.options.name
  }
  if (_xHighlighted == 0) {
      _xHighlighted = val
      _xHighlightedName = name
      document.getElementById('ratio-name1').innerHTML = _xHighlightedName
      document.getElementById('ratio-name2').innerHTML = ''
      document.getElementById('ratio-value').innerHTML = ''
  }
  else {
      _ratio = _xHighlighted/val
      _xHighlighted2 = val
      //_xHighlighted = 0
      let formRatio = (_ratio*100).toFixed(1) + '%'
      document.getElementById('ratio-value').innerHTML = formRatio
      document.getElementById('ratio-name2').innerHTML = name
  }
  if (_xHighlighted !=0 && _xHighlighted2 != 0) {
      _xHighlighted=0
      _xHighlighted2=0
  }
  //console.log(d)
})

const setCoords = () => {
  network.moveNode('govt',1000,200)
  network.moveNode('hh',1000,-200)
  network.moveNode('icpf',0,500)
  network.moveNode('iv',0,-500)
  network.moveNode('ofi',0,0)
  network.moveNode('mfi',-1000,0)
  network.moveNode('row',1000,800)
  network.moveNode('nfc',1000,-800)
}
"use strict";

var network
var container
var exportButton
var nodeSlider
//var edgeSlider
var edgeSlider2
var edgeChangeSlider
var selectedEdgeIDs
//boolean for the history switch
//var historyIsOn = false
var nodeCoords
var nodeColor
var edgeColorIsOn
var edgeColorLargestIsOn
var nodeName
var nodeSizeKey
var edgeSizeKey
var coreSector
//saving the non historical positions, so switch can be turned off
//var nohist_network
var date
var region
var asset
var herfindahlVal
var _sector
var _nodes
var _edges
var _options
var _nodeRange
var _edgeRange
var _comparisonDate
var _edgeCutoff
var _innerCircleRadius
var _outerCirclesRadius
var _innerOuterRadius
//var date

const init = () => {
    container = document.getElementById('network')
    exportButton = document.getElementById('export_button')
    nodeSlider = document.getElementById('nodeSlider')
    //edgeSlider = document.getElementById('edgeSlider')  
    edgeSlider2 = document.getElementById('edgeSlider2')
    edgeChangeSlider = document.getElementById('edgeChangeSlider')
    herfindahlVal = document.getElementById('herfindahl')
    nodeCoords = 'default'
    nodeColor = 'default'
    nodeName = 'actual'
    nodeSizeKey = 'no value'
    edgeSizeKey = 'absolute'
    edgeColorIsOn = false
    edgeColorLargestIsOn = false
    date = dates[dates.length-1].dateID
    asset = 'total'
    region = 'Euro Area'
    _sector = 'all'
    _nodeRange = [20,200]
    _edgeRange = [0.3,30]
    //initialised to last and second to last value
    _comparisonDate = dates[dates.length-1].dateID
    let edgeRange = [0.3,30]
    let nodeRange = [20,200]
    _edgeCutoff = 0.1
    _innerCircleRadius = 400
    _outerCirclesRadius = 100
    _innerOuterRadius = 600
    setSlider(nodeSlider,_nodeRange,nodeRange)
    //setSlider(edgeSlider,_edgeRange,edgeRange)
    setEdgeRankSlider(edgeSlider2,_edgeCutoff)
    setEdgeChangeSlider(edgeChangeSlider,dates)
    //construct_network()
    draw()
}

const draw = () => { 
    console.log("comp date",_comparisonDate)
    //if (typeof date !== 'undefined')  {
    //   document.getElementById("hist_network_select").checked = false;
    //   historyIsOn = false;
    //}
   
    //subset nodes and edges by date; required to prevent duplicate id error
    var data = select_data()

    //set minimum scale for nodes
    //_nodeRange[0] = _nodeRank
    //options are defined in options.js 
    _options = setOptions()
    //set levels
    set_levels()
    //draw current network
    network = new vis.Network(container, data, _options)

    //set network structure
    //select_nodeCoord()
    //if (historyIsOn) set_history()
    //set change
    set_change()
    //set colors
    select_nodeColor()
    if (edgeColorIsOn) set_edge_color()
    else unset_edge_color()
    if (edgeColorLargestIsOn) edgeHighlightLargest()
    if (edgeColorIsOn == false && edgeColorLargestIsOn == false) unset_edge_color()
    
    //set node labels
    select_nodeName()
    //set category for node and edge sizes
    select_nodeSizeKey()
    select_edgeSizeKey()
    //set tooltips
    set_tooltips()
    //calculate the relative size for the edges
    edgeRank()
    
    edgeSelectBySize(_edgeCutoff)
    //hide edges with 0 values
    hideZeroValues()
    //display nodes for selected sector only
    select_nodesBySector()
    //set highlighting options (which depend on the chosen color options)
    setHighlight()
    unsetHighlight()
    herfindahlVal.innerText=herfindahl()
    
}

const set_levels = () => {
    _nodes.map((node) => { 
        if (node.id=='mfi') {node.level=0}
        else if (node.id=='iv' || node.id=='icpf' || node.id=='ofi') {node.level=1}
        else node.level=2
    })
}

const set_tooltips = () => {
    _edges.map((edge) => {edge.title= edge.from+" to "+ edge.to + " value " + edge.value})
    _nodes.map((node) => {node.title= node.label+ " value "+ node.value})
}

const exportImage2 = () => {
    let dateLabel = (Math.floor(date/12)).toString() + (date % 12).toString()
    let canvas = document.getElementsByTagName('canvas')
    let nodeProps = 'n_' + nodeName + '_' + nodeSizeKey + '_' + nodeColor
    let edgeProps = 'e_' + '_'+ edgeSizeKey + '_'+ edgeColorIsOn
    let netProps = 'net_' + nodeCoords + '_' + _sector
    let canvas_filename = dateLabel + ' ' + netProps + ' ' + nodeProps + ' ' + edgeProps
        canvas[0].toBlobHD(function(blob) {
            saveAs(
                  blob
                , (canvas_filename) + ".png"
            );
        }, "image/png");
}

//------NETWORK PROPERTIES------//
const switch_region = () => {
    region = document.getElementById('region').value
    draw()
}

const switch_asset = () => {
    asset = document.getElementById('asset').value
    draw()
}

const switch_date = () => {
    date = document.getElementById('date').value
    draw()
}

const select_data = () => {
    _nodes = nodes.filter((node) => node.dateID == date && node.region == region && node.asset == asset)
    _edges = edges.filter((edge) => edge.dateID == date && edge.region == region && edge.asset == asset)
   
    var data = {nodes: _nodes, edges: _edges}

    return data
}

const switch_nodesBySector = () => {
    _sector = document.getElementById('sector').value
    draw()
}

const select_nodesBySector = () => {
    if (_sector != 'all') {
        _nodes.map((node) => { 
            if (node['sector']!=_sector) {
                node.hidden=true
            }
            else {node.hidden=false}
        })
    }
    else _nodes.map((node) => { node.hidden=false })
    network.setData({nodes: _nodes, edges: _edges}) 
} 

const setHighlight = () => network.on("selectNode", function () {
    selectedEdgeIDs = network.getSelectedEdges()
    selectedEdgeIDs.map((id) => {network.body.edges[id].options.color.opacity=0.8} )
    //selectedEdgeIDs.map((id) => {network.body.edges[id].options.color.opacity=0.8} )
    if (edgeColorIsOn == false) {
        selectedEdgeIDs.map((id) => {network.body.edges[id].options.color.opacity=1, network.body.edges[id].options.color.highlight= '#FF991F'})
    }
})

const unsetHighlight = () => network.on("deselectNode", function () {
    selectedEdgeIDs.map((id) => {network.body.edges[id].options.color.opacity=0.3})
    selectedEdgeIDs=[]
    selectedEdgeIDs.map((id) => {network.body.edges[id].options.color.highlight=undefined})
})

//------NODE PROPERTIES------//
const switch_nodeName = () => {
    nodeName = document.getElementById('nodeName').value
    draw()
}

const select_nodeName = () => {
    if (nodeName == 'actual') _nodes.map((node) => node.label=node.name)
    else if (nodeName == 'anonymised') _nodes.map((node,idx) => node.label="Item "+ idx.toString())
    else if (nodeName == 'none') _nodes.map((node) => node.label="        ")
}

const switch_nodeSizeKey = () => {
    nodeSizeKey = document.getElementById('nodeSize').value
    //select_nodeSizeKey()
    draw()
}

const select_nodeSizeKey = () => {
    let nodeSize  
    _nodes.map((node) => {
        nodeSizeKey == 'no value' ? nodeSize = 1 : nodeSize = parseInt(node[nodeSizeKey])
        node.value=nodeSize
        node.title=nodeSize
    })
    var data = {nodes: _nodes, edges: _edges}
    network.setData({nodes: _nodes, edges: _edges})
}

const set_nodeRange = () => nodeSlider.noUiSlider.on('change', (values) => {
        _nodeRange[0]=parseInt(values[0])
        _nodeRange[1]=parseInt(values[1])
        draw()
})

const switch_nodeColor = () => {
    nodeColor = document.getElementById('nodeColor').value
    draw()
}

const select_nodeColor = () => {
    if (nodeColor == 'default') nodeColorsBySector()
    //else if (nodeColor == 'custom') nodeColorsCustom()
    else nodeColorsUniform()
    network.setData({nodes: _nodes, edges: _edges})
}

//------EDGE PROPERTIES------//
const switch_edgeSizeKey = () => {
    edgeSizeKey = document.getElementById('edgeSize').value
    draw()
}

const select_edgeSizeKey = () => {
    let edgeSize  
    _edges.map((edge) => {
        if (edgeSizeKey == 'absolute') edgeSize = edge['absValue']
        else edgeSize = edge['change'] 
        edge.value=edgeSize
        edge.title=edgeSize
    })
    var data = {nodes: _nodes, edges: _edges}
    network.setData({nodes: _nodes, edges: _edges})
}

const set_change = () => {
    console.log(edges)
    let edgeSize  
    _edges.map((edge) => {
        let compEdge = edges.find(it => {
            return it['dateID']==_comparisonDate && it['asset']==edge['asset'] && it['from']==edge['from'] && it['to']==edge['to']
        })
        compEdge == undefined ? edgeSize = 0 : edgeSize = edge['absValue'] - compEdge['absValue']

        if (edgeSize > 0) edge['trend'] == 'increased'
        if (edgeSize == 0) edge['trend'] == 'unchanged'
        if (edgeSize < 0) edge['trend'] == 'decreased' 
        else edge['trend'] == 'none'
        console.log("1 ",edge)
        console.log("2 ",compEdge)
        edge.change=edgeSize
    })
    var data = {nodes: _nodes, edges: _edges}
    network.setData({nodes: _nodes, edges: _edges})
}

//UNUSED since scale roughly know and not very effective
/*
const set_edgeRange = () => edgeSlider.noUiSlider.on('change', (values) => {
    _edgeRange[0]=parseInt(values[0])
    _edgeRange[1]=parseInt(values[1])
    draw()
})
*/

const set_edgeChangeValue = () => edgeChangeSlider.noUiSlider.on('change',(values) => {
    _comparisonDate=parseFloat(values[0])
    draw()
})

const set_edgeCutoff = () => edgeSlider2.noUiSlider.on('change', (values) => {
    _edgeCutoff = parseFloat(values[0])
    draw()
})

const switch_edge_color  = () => {
    if (edgeColorIsOn) {
        edgeColorIsOn = false
        //unset_edge_color()
    }
    else {
        edgeColorIsOn = true
        //set_edge_color()
    }
    draw()
}

const switch_edge_color_largest  = () => {
    if (edgeColorLargestIsOn) {
        edgeColorLargestIsOn = false
        //unset_edge_color()
    }
    else {
        edgeColorLargestIsOn = true
        //edgeHighlightLargest()
        
    }
    draw()
}

const set_edge_color = () => {
    edgeColorsByTrend()
}

const unset_edge_color = () => {
    edgeColorsUniform()
}
//------SLIDERS--------------------------//
//slider for scales
const setSlider = (sliderID,startValues,rangeValues) => {
    noUiSlider.create(sliderID, {
    start: startValues,
    range: {
        'min': [  rangeValues[0] ],
        'max': [ rangeValues[1] ]
    },
    connect: true
    })
}
//slider determining which edges will be shown according to percentile rank
const setEdgeRankSlider = (sliderID,startValue) => {
    noUiSlider.create(sliderID, {
    start: startValue,
    range: {
        min: [0],
        max: [1]
    }
    })
}
//slider determining which edges will be shown according to percentile rank
const setEdgeChangeSlider = (sliderID,rangeValues) => {
    console.log(rangeValues)
    console.log(rangeValues[rangeValues.length-1])
    noUiSlider.create(sliderID, {
    start: rangeValues[rangeValues.length-2].dateID,
    step: 3,
    range: {
        min: rangeValues[0].dateID,
        max: rangeValues[rangeValues.length-1].dateID
    }
    })
}
//------EXECUTE AND EVENT LISTENERS------//
//in index.html
//init()
//set_nodeRange()
//set_edgeRange()    
//set_edgeCutoff()
//------CURRENTLY UNUSED------//
/*
exportNetwork = () => {
    var nodes = objectToArray(network.getPositions())
    str = (JSON.stringify(nodes))
    saveNetwork(str)
}

switch_history = () => {
    if (historyIsOn) {
        historyIsOn = false;
        unset_history();
    }
    else {
        historyIsOn = true;
        set_history()
    }
    draw()
}

set_history = () => {
    nohist_network = network.getPositions();
    var keys = network.body.nodeIndices
    //to ensure nodes are not rearranged
    network.physics.stabilized = false
    
    //set nodes and edges to respective values
    keys.map((key,idx) => {
        network.body.nodes[key].x = hist_network[idx].x;
        network.body.nodes[key].y = hist_network[idx].y;
    })
}

unset_history = () => {
    var keys = network.body.nodeIndices
    //to ensure nodes are not rearranged
    network.physics.stabilized = false
    //set nodes and edges to respective values
    keys.map((key,idx) => {
        network.body.nodes[key].x = nohist_network[key].x;
        network.body.nodes[key].y = nohist_network[key].y;
    })    
}

saveNetwork = (exportValue) => { 
    fetch('http://127.0.0.1:5000/save', {
    method: 'POST',
    headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json',
    },
    body: exportValue
    })
}

getBase64Image = () => {
    canvas = document.getElementsByTagName('canvas')
    dataURL = canvas[0].toDataURL('image/png',0.5)
    return dataURL.replace(/^data:image\/(png);base64,/, "")
}

//currently unused; sends the image back to the server and saves it in the temp file
exportImage = () => {
    let imgData = JSON.stringify(getBase64Image())
    //console.log("img",imgData)
    fetch('http://127.0.0.1:5000/save', {
        method: 'POST',
        headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        },
        body: imgData
        }).then((response) => {
            if (response.ok) { return response}
            throw new Error('Network response was not ok.');
        })
}

objectToArray = (obj) => {
    var arr = []
    Object.keys(obj).map((key) => {
        obj[key].id = key
        arr.push(obj[key])
    }
    )
    return arr
}
*/
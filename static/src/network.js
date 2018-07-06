"use strict";

var network
var container
var exportButton
var nodeSlider
var edgeSlider2
var selectedEdgeIDs
var nodeCoords
var edgeColorIsOn
var edgeColorLargestIsOn
var nodeSizeKey
var edgeSizeKey
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

const init = () => {
    container = document.getElementById('network')
    exportButton = document.getElementById('export_button')
    nodeSlider = document.getElementById('nodeSlider')
    //edgeSlider = document.getElementById('edgeSlider')  
    edgeSlider2 = document.getElementById('edgeSlider2')
    //edgeChangeSlider = document.getElementById('edgeChangeSlider')
    herfindahlVal = document.getElementById('herfindahl')
    //nodeCoords = 'default'
    nodeSizeKey = 'Assets'
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
    setSlider(nodeSlider,_nodeRange,nodeRange)
    setEdgeRankSlider(edgeSlider2,_edgeCutoff)

    draw()
}

const draw = () => { 
    //subset nodes and edges by date; required to prevent duplicate id error
    var data = select_data()

    //options are defined in options.js 
    _options = setOptions()

    //set levels (network is hierarchical from left to right)
    set_levels()

    //draw current network
    network = new vis.Network(container, data, _options)

    //save positions
    network.storePositions()
    //set change in edges for given reference period
    set_change()

    //set colors
    nodeColorsBySector()
    if (edgeColorIsOn) set_edge_color()
    else unset_edge_color()
    if (edgeColorLargestIsOn) edgeHighlightLargest()
    if (edgeColorIsOn == false && edgeColorLargestIsOn == false) unset_edge_color()
    
    //set node labels
    set_nodeName()

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
    setRatio()
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
    _edges.map((edge) => {edge.title= edge.from+" to "+ edge.to + ": € " + edge.value + "bn"})
    _nodes.map((node) => {node.title= node.label+ ": € "+ node.value + "bn"})
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
const set_nodeName = () => {
    _nodes.map((node) => node.label=node.name)
}

const switch_nodeSizeKey = () => {
    nodeSizeKey = document.getElementById('nodeSize').value
    //select_nodeSizeKey()
    draw()
}

const select_nodeSizeKey = () => {
    let nodeSize  
    _nodes.map((node) => {
        nodeSize = parseInt(node[nodeSizeKey])
        node.value=nodeSize
        node.title=nodeSize
    })
    var data = {nodes: _nodes, edges: _edges}
    network.setData({nodes: _nodes, edges: _edges})
    console.log(network)
}

const set_nodeRange = () => nodeSlider.noUiSlider.on('change', (values) => {
        _nodeRange[0]=parseInt(values[0])
        _nodeRange[1]=parseInt(values[1])
        draw()
})

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

const switch_comparisonDate = () => {
    _comparisonDate = document.getElementById('comparisonDate').value
    draw()
}

const set_change = () => {
    let edgeSize  
    _edges.map((edge) => {
        let compEdge = edges.find(it => {
            return it['dateID']==_comparisonDate && it['region']==edge['region'] && it['asset']==edge['asset'] && it['from']==edge['from'] && it['to']==edge['to']
        })
        compEdge == undefined ? edgeSize = 0 : edgeSize = edge['absValue'] - compEdge['absValue']

        if (edgeSize > 0) edge['trend'] = 'increased'
        else if (edgeSize == 0) edge['trend'] = 'unchanged'
        else if (edgeSize < 0) edge['trend'] = 'decreased' 
        else edge['trend'] = 'none'
        //negative edgeSizes inverted to ensure scaling works
        if (edgeSize < 0) edgeSize = (-1)*edgeSize

        edge['change']=edgeSize
    })
    var data = {nodes: _nodes, edges: _edges}
    network.setData({nodes: _nodes, edges: _edges})
}

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

//------CURRENTLY UNUSED------//
/*
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

/*
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
*/
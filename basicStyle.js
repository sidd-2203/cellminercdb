[
  {"selector":"node", "css": {
    "border-width": "2px",
    "content": "data(name)",
    "background-color":"grey",
    "border-color":"black"
  }},
  {"selector":"node[avgValues <= 0]", "css": {
       "background-color": "mapData(avgValues, -10, 0, blue, white)"
  }},
  {"selector":"node[avgValues > 0]", "css": {
       "background-color": "mapData(avgValues, 0, 10, white, red)"
  }},
  
  {"selector":"node[nodeType = 'FAMILY']","css":{
    "shape":"rectangle",
    "background-color":"white",
    "border-color":"grey"
  }
  },
  {"selector":"node[nodeType = 'GENE']","css":{
    "shape":"round-rectangle"
  }
  },
  {"selector":"node[nodeType = 'PROCESS']","css":{
    "border-color":"white"
  }
  },
  {"selector":"node[nodeType = 'COMPARTMENT']","css":{
    "shape":"barrel",
    "border-width":"4px",
    "background-color":"white" 
  }
  },
  {"selector":"node[nodeType = 'COMPLEX']","css":{
    "shape":"cut-rectangle",
    "background-color":"white"
  }},
  
  
  {"selector": "node:selected", "css": {
       "overlay-opacity": 0.6,
       "overlay-color": "gray"
  }},
   
  
  {"selector":"edge","css":{
    "curve-style":"bezier"}
  },

  {"selector": "edge[interaction='INHIBITS']", "css": {
    "line-color": "red",
    "target-arrow-shape": "tee",
    "target-arrow-color": "red"
  }},
  {"selector": "edge[interaction='ACTIVATES']", "css": {
    "line-color": "green",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "green"
  }},
  {"selector": "edge[interaction='BINDS']", "css": {
    "line-color": "black",
    "target-arrow-color": "black"
  }},
  {"selector": "edge[interaction='INDUCES']", "css": {
    "line-color": "green",
    "line-style":"dashed",
    "target-arrow-shape": "triangle",
    "target-arrow-color": "green"
  }},
  {"selector": "edge[interaction='REPRESSES']", "css": {
    "line-color": "red",
    "line-style": "dashed",
    "target-arrow-shape": "tee",
    "target-arrow-color": "red"
  }}
]
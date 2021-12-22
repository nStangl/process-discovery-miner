import * as React from 'react';
import CytoscapeComponent from 'react-cytoscapejs';
import testElements from './transitions2.json';
import dagre from 'cytoscape-dagre';
import cytoscape from 'cytoscape';
// import cytoscapesvg from 'cytoscape-svg'; does not offer @types --> either define myself or use png


export default function Graph() {

    // Do not forget to load the layout extension!!
    cytoscape.use(dagre);

    const layout = { name: 'dagre',
                    rankDir: 'LR',
                    spacingFactor: 0.8, 
                    nodeDimensionsIncludeLables: true };

    return <CytoscapeComponent 
                elements={ testElements }
                style={ { width: '900px', height: '550px' } }
                stylesheet={[
                    {
                        selector: 'node',
                        style: {
                            'width': 20,
                            'height': 20,
                            'label': 'data(id)',
                            'shape': 'ellipse'                            
                        }
                    },
                    {
                        selector: 'node[shape]',
                        style: {
                            'shape': (el) => el.data('shape') ?? 'star',
                            'text-valign': ((el: any) => el.data('shape') == 'rectangle' ? 'center' : 'bottom'),
                            'text-halign': 'center'
                        }
                    },
                    {
                        selector: 'edge',
                        style: {
                            'width': 1,
                            'line-color': '#666',
                            'target-arrow-color': '#ccc',
                            'target-arrow-shape': 'triangle',
                            'curve-style': 'bezier'
                        }
                    }
                ]}
                layout={ layout }
                wheelSensitivity={ 0.7 }
            />
}
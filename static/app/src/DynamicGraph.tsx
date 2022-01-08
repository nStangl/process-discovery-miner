import * as React from "react";
import CytoscapeComponent from "react-cytoscapejs";
import testElements from "./transitions.json";
import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";
import transitions from "./transitions.json";

export default function DynamicGraph() {
  cytoscape.use(dagre);

  const cy = cytoscape({
    container: document.getElementById("testGraphId"),

    elements: transitions,

    style: [
      {
        selector: "node",
        style: {
          width: 20,
          height: 20,
          label: "data(id)",
          shape: "ellipse",
        },
      },
      {
        selector: "node[shape]",
        style: {
          shape: (el) => el.data("shape") ?? "star",
          "text-valign": (el: any) =>
            el.data("shape") == "rectangle" ? "center" : "bottom",
          "text-halign": "center",
        },
      },
      {
        selector: "edge",
        style: {
          width: 1,
          "line-color": "#666",
          "target-arrow-color": "#ccc",
          "target-arrow-shape": "triangle",
          "curve-style": "bezier",
        },
      },
    ],
  });

  console.log(cy.container());

  return <div>id="cy"</div>;
}

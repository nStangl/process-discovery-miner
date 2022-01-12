import React, { useState } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import testElements from "./myfile.json";
import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";
import { ElementDefinition } from "cytoscape";
import SpinStretch from "react-cssfx-loading/lib/SpinStretch";
import Alert from "@mui/material/Alert";
import AlertTitle from "@mui/material/AlertTitle";
import Box from "@mui/material/Box";

type GraphProps = {
  postBody: string;
  miner: string;
};
type GraphState = {
  apiResponse: any;
  receivedResponse: Boolean;
  responseError: string;
};
export default class GraphClass extends React.Component<
  GraphProps,
  GraphState
> {
  constructor(props: any) {
    super(props);

    this.state = {
      apiResponse: null,
      receivedResponse: false,
      responseError: "",
    };
  }

  componentDidMount() {
    const requestOptions = {
      method: "POST",
      headers: { "Content-Type": "application/xml" },
      body: this.props.postBody,
    };
    const apiURL: string = window.location.href + "api/v1/" + this.props.miner;

    fetch(apiURL, requestOptions)
      .then((response) => response.json())
      .then((data) =>
        this.setState({
          apiResponse: data.graph,
          receivedResponse: true,
          responseError: "",
        })
      )
      .catch((error) => {
        this.setState({
          apiResponse: null,
          receivedResponse: true,
          responseError: error.toString(),
        });
        console.log(
          "There was an error fetching from the API. Made request to: " + apiURL
        );
      });
  }

  render() {
    const { apiResponse, receivedResponse, responseError } = this.state;

    if (!receivedResponse) {
      return (
        <div
          style={{ width: "100%", display: "flex", justifyContent: "center" }}
        >
          <Box
            width="200px"
            height="200px"
            display="flex"
            alignItems="center"
            justifyContent="center"
          >
            <SpinStretch color="#002f6c" width="90%" height="90%" />
          </Box>
        </div>
      );
    }

    if (receivedResponse && responseError === "") {
      cytoscape.use(dagre);
      const layout = {
        name: "dagre",
        rankDir: "LR",
        spacingFactor: 0.8,
        nodeDimensionsIncludeLables: true,
      };

      return (
        <div
          style={{ width: "100%", display: "flex", justifyContent: "center" }}
        >
          <CytoscapeComponent
            elements={CytoscapeComponent.normalizeElements(apiResponse)}
            style={{
              width: "900px",
              height: "550px",
              border: "2px dashed grey",
            }}
            stylesheet={[
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
            ]}
            layout={layout}
            wheelSensitivity={0.7}
          />
        </div>
      );
    } else {
      return (
        <div style={{ display: "flex", justifyContent: "center" }}>
          <Alert severity="error">
            <AlertTitle>Error</AlertTitle>
            An error occured while trying to fetch data from the API:{" "}
            {responseError}
          </Alert>
        </div>
      );
    }
  }
}

import React, { useState } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";
import { ElementDefinition } from "cytoscape";
import SpinStretch from "react-cssfx-loading/lib/SpinStretch";
import Alert from "@mui/material/Alert";
import AlertTitle from "@mui/material/AlertTitle";
import Box from "@mui/material/Box";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableContainer from "@mui/material/TableContainer";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";
import Paper from "@mui/material/Paper";
import { Pie } from "react-chartjs-2";
import { Chart as ChartJS, ArcElement, Tooltip, Legend } from "chart.js";
import { TabsContextValue } from "@mui/base";
import { PieChart } from "./PieChart";

type GraphProps = {
  postBody: string;
  miner: string;
};

type GraphState = {
  apiResponse: any;
  response: APIResponse;
  receivedResponse: boolean;
  responseError: string;
};

// type definitons to conveniently store the api reponses
type APIResponse = AlphaMinerReponse | null;

type AlphaMinerReponse = {
  graph: cytoscape.ElementDefinition[];
  traceCount: Array<TraceCountLine>;
  alphaminersets: AlphaminerSets;
  footprintmatrix: FootprintMatrix;
};

type RegionMinerResponse = {};

type FootprintMatrix = {
  dim: number;
  row: Array<string>;
  fields: Array<Array<string>>;
};

type TraceCountLine = [count: number, trace: Array<string>];

// TODO: Check if not change to tuple as TCL ^
type Transition = {
  from: Array<string>;
  to: Array<string>;
};

type AlphaminerSets = {
  tl: Array<string>;
  ti: Array<string>;
  to: Array<string>;
  xl: Array<Transition>;
  yl: Array<Transition>;
};

export default class GraphClass extends React.Component<
  GraphProps,
  GraphState
> {
  constructor(props: any) {
    super(props);

    this.state = {
      apiResponse: null,
      response: null,
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
      .then((data) => this.handleFetch(data))
      .catch((error) => {
        this.setState({
          apiResponse: null,
          receivedResponse: true,
          responseError: error.toString(),
          response: null,
        });
        console.log(
          "There was an error fetching from the API. Made request to: " + apiURL
        );
      });
  }

  handleFetch(responseJSON: any): void {
    if (this.props.miner == "alphaminer") {
      let fpm: FootprintMatrix = responseJSON.footprintmatrix;
      let graph = responseJSON.graph;
      let tracecount: Array<TraceCountLine> = responseJSON.traceCount;
      let ams: AlphaminerSets = responseJSON.alphaminersets;

      this.setState({
        apiResponse: responseJSON.graph,
        receivedResponse: true,
        responseError: "",
        response: {
          graph: CytoscapeComponent.normalizeElements(graph),
          traceCount: tracecount,
          alphaminersets: ams,
          footprintmatrix: fpm,
        },
      });
    } else if (this.props.miner == "regionminer") {
    }
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
      console.log("Reponse obj");
      console.log(this.state.response);

      return (
        <div
          style={{ width: "100%", display: "flex", justifyContent: "center" }}
        >
          <div>{this.displayCytoGraph(this.state.response?.graph!)}</div>
          <div>
            {this.displayFootprintmatrixOrError(
              this.state.response?.footprintmatrix
            )}
          </div>
          <div>
            {this.displayTraceCountPieChart(this.state.response?.traceCount)}
          </div>
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

  parseReponseToAPIReponse(minertype: string): APIResponse {
    return null;
  }

  // Create a table representing the footprintmatrix
  displayFootprintmatrixOrError(fpm: FootprintMatrix | undefined) {
    if (fpm === undefined) {
      console.log("Footprintmatrix undefined");
      return null;
    }
    return (
      <TableContainer component={Paper}>
        <Table sx={{ minWidth: 650 }} aria-label="simple table">
          <TableHead>
            <TableRow>
              <TableCell>Footprint Matrix</TableCell>
              {fpm.row.map((field) => (
                <TableCell align="right">{field}</TableCell>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {fpm.fields.map((line, index) => (
              <TableRow
                key={fpm.row[index]}
                sx={{ "&:last-child td, &:last-child th": { border: 0 } }}
              >
                <TableCell component="th" scope="row">
                  {fpm.row[index]}
                </TableCell>
                {line.map((field) => (
                  <TableCell align="right">{field}</TableCell>
                ))}
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
    );
  }

  displayTraceCountPieChart(tc: Array<TraceCountLine> | undefined) {
    if (tc === undefined) {
      console.log("traceCount is undefined");
      return null;
    }

    var traces: string[] = [];
    var counts: number[] = [];

    tc.forEach((line) => {
      traces.push("<" + line[1].join(",") + ">");
      counts.push(line[0]);
    });

    return PieChart(traces, counts);
  }

  static defaultLayout: any = {
    name: "dagre",
    rankDir: "LR",
    spacingFactor: 0.8,
    nodeDimensionsIncludeLables: true,
  };

  displayCytoGraph(
    graph: cytoscape.ElementDefinition[],
    layout: cytoscape.LayoutOptions | undefined = GraphClass.defaultLayout
  ) {
    cytoscape.use(dagre);

    return (
      <CytoscapeComponent
        elements={this.state.response?.graph!}
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
    );
  }
}

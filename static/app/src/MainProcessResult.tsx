import React, { useState } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import dagre from "cytoscape-dagre";
import cytoscape, { ExportOptions } from "cytoscape";
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
import { PieChart } from "./PieChart";
import Grid from "@mui/material/Grid";
import Divider from "@mui/material/Divider";
import Button from "@mui/material/Button";
import ButtonGroup from "@mui/material/ButtonGroup";
import Paper from "@mui/material/Paper";
import Typography from "@mui/material/Typography";
import { saveAs } from "file-saver";

type ProcessResultProps = {
  postBody: string;
  miner: string;
};

type ProcessResultState = {
  response: APIResponse;
  responseError: string;
  cyRef: cytoscape.Core | null;
};

// type definitons to conveniently store the api reponses
type APIResponse = AlphaMinerReponse | null;

type AlphaMinerReponse = {
  graph: cytoscape.ElementDefinition[];
  traceCount: Array<TraceCountLine>;
  alphaminersets: AlphaminerSets;
  footprintmatrix: FootprintMatrix;
};

// Not implemented yet
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

export default class MainProcessResult extends React.Component<
ProcessResultProps,
ProcessResultState
> {
  constructor(props: any) {
    super(props);

    this.state = {
      response: null,
      responseError: "",
      cyRef: null,
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
          responseError: error.toString(),
          response: null,
          cyRef: null,
        });
        console.log(
          "There was an error fetching from the API. Made request to: " + apiURL
        );
      });
  }

  // Perform type casting and parse graph
  handleFetch(responseJSON: any): void {
    if (this.props.miner == "alphaminer") {
      let fpm: FootprintMatrix = responseJSON.footprintmatrix;
      let graph = responseJSON.graph;
      let tracecount: Array<TraceCountLine> = responseJSON.traceCount;
      let ams: AlphaminerSets = responseJSON.alphaminersets;

      this.setState({
        responseError: "",
        response: {
          graph: CytoscapeComponent.normalizeElements(graph),
          traceCount: tracecount,
          alphaminersets: ams,
          footprintmatrix: fpm,
        },
        cyRef: null,
      });
    } else if (this.props.miner == "regionminer") {
    }
  }

  render() {
    const { response, responseError } = this.state;

    // Loading animation...
    if (response === null) {
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

    // Display result
    if (response !== null && responseError === "") {
      return (
        <Box sx={{ flexGrow: 1, marginTop: "10px" }}>
          <Box sx={{ display: "flex", justifyContent: "center" }}>
            {this.displayCytoGraph(this.state.response?.graph!)}
          </Box>
          <Box sx={{ display: "flex", justifyContent: "center" }}>
            <ButtonGroup variant="outlined" aria-label="outlined button group">
              <Button onClick={this.handleResetCy}>Reset View</Button>
              <Button onClick={this.handleSavePNG}>Save as PNG</Button>
              <Button onClick={this.handleSaveJSON}>Save as JSON</Button>
            </ButtonGroup>
          </Box>

          <Grid
            container
            rowSpacing={3}
            columnSpacing={1}
            justifyContent="space-evenly"
            alignItems="center"
          >
            <Grid item xs={12} md={12} lg={12} justifyContent="center"></Grid>

            <Grid item xs={12}>
              <Divider flexItem sx={{ borderBottomWidth: 3 }} />
            </Grid>

            <Grid item xs="auto">
              <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
                Footprint matrix
              </Typography>
              {this.displayFootprintmatrix(
                this.state.response?.footprintmatrix
              )}
            </Grid>

            <Grid item xs="auto">
              <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
                Trace Occurences
              </Typography>
              {this.displayTraceCountPieChart(this.state.response?.traceCount!)}
            </Grid>
          </Grid>
        </Box>
      );
    }
    // Error message
    else {
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

  handleResetCy = () => {
    if (this.state.cyRef !== null) {
      this.state.cyRef.reset();
    } else {
      console.log("Cannot reset view");
    }
  };

  handleSaveJSON = () => {
    if (this.state.cyRef !== null) {
      var json = this.state.cyRef.json();
      var jsonBlob = new Blob([JSON.stringify(json)], {
        type: "application/json",
      });

      saveAs(URL.createObjectURL(jsonBlob), "graph_data.json");
    } else {
      console.log("Cannot save JSON");
    }
  };

  handleSavePNG = () => {
    if (this.state.cyRef !== null) {
      const options: cytoscape.ExportBlobOptions = {
        output: "blob",
        full: true,
      };
      var fileBlob = this.state.cyRef.png(options);
      var blobURL = URL.createObjectURL(fileBlob);

      saveAs(blobURL, "cytograph.png");
    } else {
      console.log("Cannot save PNG");
    }
  };

  // Create a table representing the footprintmatrix
  displayFootprintmatrix(fpm: FootprintMatrix | undefined) {
    if (fpm === undefined) {
      console.log("Footprintmatrix undefined");
      return null;
    }
    return (
      <TableContainer component={Paper}>
        <Table sx={{ minWidth: "auto" }} aria-label="simple table">
          <TableHead>
            <TableRow>
              <TableCell></TableCell>
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

  // Extract data and create Pie Chart to display trace count
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

  // Default cytoscape layout if no other is specified
  static defaultLayout: any = {
    name: "dagre",
    rankDir: "LR",
    spacingFactor: 0.8,
    // Does include long labels in dimension
    // remove if not desired
    nodeDimensionsIncludeLabels: true,
  };

  // Display and return the Cytoscape canvas to show the graph
  displayCytoGraph(
    graph: cytoscape.ElementDefinition[],
    layout: cytoscape.LayoutOptions | undefined = MainProcessResult.defaultLayout
  ) {
    // Tell cytoscape to use the layout. Don't forget!
    cytoscape.use(dagre);

    return (
      <CytoscapeComponent
        cy={(cyElement) => {
          if (this.state.cyRef === null) {
            this.setState({
              cyRef: cyElement,
            });
          }
        }}
        elements={graph}
        style={{
          background: "#fff",
          width: "960px",
          height: "550px",
          border: "4px dashed rgba(0, 0, 0, 0.12)",
          alignItems: "center",
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

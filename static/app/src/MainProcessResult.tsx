import React, { useState } from "react";
import CytoscapeComponent from "react-cytoscapejs";
import dagre from "cytoscape-dagre";
import cytoscape, { ExportOptions } from "cytoscape";
import SpinStretch from "react-cssfx-loading/lib/SpinStretch";
import Alert from "@mui/material/Alert";
import AlertTitle from "@mui/material/AlertTitle";
import Box from "@mui/material/Box";
import Container from "@mui/material/Container";
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
import AlphaMinerSetsAccordion from "./AlphaMinerSetAccordion";
import ArrowBackIosIcon from "@mui/icons-material/ArrowBackIos";
import Statistics from "./Statistics";

type ProcessResultProps = {
  postBodyPromise: Promise<string>;
  miner: string;
};

type ProcessResultState = {
  response: APIResponse;
  responseError: string;
  cyRef: cytoscape.Core | null;
};

// type definitons to conveniently store the api reponses
type APIResponse = AlphaMinerResponse | AlphaPlusMinerResponse | null;

type AlphaMinerResponse = {
  graph: cytoscape.ElementDefinition[];
  traceCount: Array<TraceCountLine>;
  eventCount: Array<EventCountLine>;
  alphaminersets: AlphaminerSets;
  footprintmatrix: FootprintMatrix;
  traceStatistics: Statistics;
  eventStatistics: Statistics;
};

// extend AlphaMinerReponse by additional field
type AlphaPlusMinerResponse = AlphaMinerResponse & {
  loopsWithNeighbours: Array<LoopWithNeighbour>;
};

// Not implemented yet
type RegionMinerResponse = {};

type FootprintMatrix = {
  dim: number;
  row: Array<string>;
  fields: Array<Array<string>>;
};

export type Statistics = {
  distinctOccurences: number;
  totalOccurences: number;
  avgLengthAll: number;
  maxLength: number;
  avgLengthDistinct: number;
  minLength: number;
  mostFrequent: string;
  leastFrequent: string;
};

// This is a tuple which is defined as a array with 2 elements
type TraceCountLine = [count: number, trace: Array<string>];

type EventCountLine = [count: number, event: string];

export type LoopWithNeighbour = [left: string, loop: string, right: string];

export type Transition = [from: Array<string>, to: Array<string>];

export type AlphaminerSets = {
  tl: Array<string>;
  ti: Array<string>;
  to: Array<string>;
  xl: Array<Transition>;
  yl: Array<Transition>;
};

export class MainProcessResult extends React.Component<
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

  // will be called after the component is displayed
  componentDidMount() {
    const readFileAndPostToAPI = async () => {
      const content = await this.props.postBodyPromise;

      const requestOptions = {
        method: "POST",
        headers: { "Content-Type": "application/xml" },
        body: content,
      };
      // Create URL for API call
      const apiURL: string =
        window.location.href + "api/v1/" + this.props.miner;

      // fetch, if success update state to trigger re-render
      fetch(apiURL, requestOptions)
        .then((response) => response.json())
        .then((data) => this.handleFetch(data))
        .catch((error) => {
          console.log("An error occured");
          console.log(error);
          this.setState({
            responseError: error.toString(),
            response: null,
            cyRef: null,
          });
          console.log(this.state.responseError);
          console.log(
            "There was an error fetching from the API. Made request to: " +
              apiURL
          );
        });
    };

    readFileAndPostToAPI();
  }

  // Perform type casting from API response and parse graph
  // update state when done
  handleFetch(responseJSON: any): void {
    if (
      this.props.miner === "alphaminer" ||
      this.props.miner === "alphaplusminer"
    ) {
      let fpm: FootprintMatrix = responseJSON.footprintmatrix;
      let graph = responseJSON.graph;
      let tracecount: Array<TraceCountLine> = responseJSON.traceCount;
      let eventcount: Array<EventCountLine> = responseJSON.eventCount;
      let ams: AlphaminerSets = responseJSON.alphaminersets;
      let traceStats: Statistics = responseJSON.traceStatistics;
      let eventStats: Statistics = responseJSON.eventStatistics;

      // alphaplusminer additionally returns Array<LoopWithNeighbour>
      if (this.props.miner === "alphaplusminer") {
        let loopsWNeighbours: Array<LoopWithNeighbour> =
          responseJSON.loopsWithNeighbours;

        this.setState({
          responseError: "",
          response: {
            graph: CytoscapeComponent.normalizeElements(graph),
            traceCount: tracecount,
            eventCount: eventcount,
            alphaminersets: ams,
            footprintmatrix: fpm,
            loopsWithNeighbours: loopsWNeighbours,
            traceStatistics: traceStats,
            eventStatistics: eventStats,
          },
          cyRef: null,
        });
      } else {
        this.setState({
          responseError: "",
          response: {
            graph: CytoscapeComponent.normalizeElements(graph),
            traceCount: tracecount,
            eventCount: eventcount,
            alphaminersets: ams,
            footprintmatrix: fpm,
            traceStatistics: traceStats,
            eventStatistics: eventStats,
          },
          cyRef: null,
        });
      }
    } else if (this.props.miner == "regionminer") {
      // Not implemented yet
    }
  }

  render() {
    const { response, responseError } = this.state;

    // Loading animation...
    if (response === null && responseError === "") {
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
            <SpinStretch color="#002f6c" width="80%" height="80%" />
          </Box>
        </div>
      );
    }

    // Display result
    if (response !== null && responseError === "") {
      return (
        <Box sx={{ flexGrow: 1, marginTop: "20px", justifyContent: "center"}}>
          <Box sx={{ display: "flex", justifyContent: "center" }}>
            {this.displayCytoGraph(this.state.response?.graph!)}
          </Box>
          <Box sx={{ display: "flex", justifyContent: "center" }}>
            <ButtonGroup variant="outlined" aria-label="outlined button group">
              <Button
                onClick={this.handleUploadNewLog}
                startIcon={<ArrowBackIosIcon />}
              >
                Upload new Log
              </Button>
              <Button onClick={this.handleResetCy}>Reset View</Button>
              <Button onClick={this.handleSavePNG}>Save as PNG</Button>
              <Button onClick={this.handleSaveJSON}>Save as JSON</Button>
            </ButtonGroup>
          </Box>

          <Grid
            container
            rowSpacing={1}
            columnSpacing={1}
            justifyContent="space-evenly"
            alignItems="center"
          >

            <Grid item xs={12}>
              <Divider flexItem sx={{ borderBottomWidth: 3 }} />
            </Grid>

            <Grid item md={3}>
              <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
                Trace Occurences
              </Typography>
              {this.displayTraceCountPieChart(this.state.response?.traceCount!)}
            </Grid>
            <Grid item md={3}>
              <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
                Event Occurences
              </Typography>
              {this.displayEventCountPieChart(this.state.response?.eventCount!)}
            </Grid>
          </Grid>
          <Container sx={{ width: "auto", marginY: "20px" }}>
            <Statistics isTrace stats={this.state.response?.traceStatistics!} />
          </Container>
          <Container sx={{ maxWidth: "960px", marginY: "20px" }}>
            <Statistics
              isTrace={false}
              stats={this.state.response?.eventStatistics!}
            />
          </Container>

          <Container sx = {{ marginY: "20px", maxWidth: "960px" }}>
            <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
              Footprint matrix
            </Typography>
            <Box>
              {this.displayFootprintmatrix(
                this.state.response?.footprintmatrix
              )}
            </Box>
          </Container>

          <Container
            sx={{ minWidth: "800px", width: "auto", maxWidth: "960px", marginTop: "20px", marginBottom: "20px" }}
          >
            <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
              Alpha miner sets
            </Typography>
            {this.displayAlphaMinerSets()}
          </Container>
        </Box>
      );
    }
    // Error message
    else {
      return (
        <div
          style={{
            display: "flex",
            justifyContent: "center",
            marginTop: "20px",
            marginBottom: "10px",
          }}
        >
          <Alert severity="error">
            <AlertTitle>Error</AlertTitle>
            An error occured while trying to fetch data from the API:{" "}
            {this.state.responseError}
          </Alert>
        </div>
      );
    }
  }

  handleUploadNewLog = () => {
    window.location.reload();
  };

  // Handle for 'Reset View' button
  handleResetCy = () => {
    if (this.state.cyRef !== null) {
      this.state.cyRef.reset();
    } else {
      console.log("Cannot reset view");
    }
  };

  // Handle for 'Save as JSON'
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

  // Handle for 'Save as PNG'
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
        <Table sx={{ minWidth: "auto"}} aria-label="simple table">
          <TableHead>
            <TableRow>
              <TableCell></TableCell>
              {
                // Fill in values in left-most column
                fpm.row.map((field) => (
                  <TableCell align="center">{field}</TableCell>
                ))
              }
            </TableRow>
          </TableHead>
          <TableBody>
            {
              /* Dynamically create all required Rows and fill with values */
              fpm.fields.map((line, index) => (
                /* Fill in top field for each row */
                <TableRow
                  key={fpm.row[index]}
                  sx={{ "&:last-child td, &:last-child th": { border: 0 } }}
                >
                  {/* Fill in top field for each row */}
                  <TableCell component="th" scope="row" align="center">
                    {fpm.row[index]}
                  </TableCell>
                  {line.map((field) => (
                    // fill in values for row
                    <TableCell align="center">{field}</TableCell>
                  ))}
                </TableRow>
              ))
            }
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
    // separate traces and counts for the PieChart
    var traces: string[] = [];
    var counts: number[] = [];

    tc.forEach(([count, trace]) => {
      traces.push("<" + trace.join(",") + ">");
      counts.push(count);
    });

    return PieChart(traces, counts, 0.4, 1, true);
  }

  displayEventCountPieChart(eventlines: Array<EventCountLine> | undefined) {
    if (eventlines === undefined) {
      console.log("eventCount is undefined");
      return null;
    }

    // separate counts and events for PieChart
    var counts: number[] = [];
    var events: string[] = [];

    eventlines.forEach(([count, event]) => {
      counts.push(count);
      events.push(event);
    });

    return PieChart(events, counts, 0, 0.6, true);
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
    layout:
      | cytoscape.LayoutOptions
      | undefined = MainProcessResult.defaultLayout
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

  // Returns the Alpha Miner sets
  displayAlphaMinerSets() {
    const x = this.state.response;

    if (this.isAlphaPlusMinerResponse(x)) {
      return AlphaMinerSetsAccordion(x.alphaminersets, x.loopsWithNeighbours);
    } else if (x !== null) {
      return AlphaMinerSetsAccordion(x.alphaminersets);
    }
  }

  // check if APIResponse is AlphaMinerReponse, because Typescript has some very poor language design
  isAlphaPlusMinerResponse(x: APIResponse): x is AlphaPlusMinerResponse {
    return (x as AlphaPlusMinerResponse).loopsWithNeighbours !== undefined;
  }

  static testStatistics: Statistics = {
    distinctOccurences: 4,
    totalOccurences: 8,
    avgLengthAll: 3.375,
    maxLength: 6,
    avgLengthDistinct: 3.75,
    minLength: 2,
    mostFrequent: "<a,b,c>",
    leastFrequent: "<a,b,b,b,b,c>",
  };
}

import React, { useState } from "react";
import AppBar from "@mui/material/AppBar";
import AccountTreeIcon from "@mui/icons-material/AccountTree";
import CssBaseline from "@mui/material/CssBaseline";
import Toolbar from "@mui/material/Toolbar";
import Grid from "@mui/material/Grid";
import Box from "@mui/material/Box";
import { createTheme, ThemeProvider } from "@mui/material/styles";
import { Typography } from "@mui/material";
import { InfoDialog, AboutDialog } from "./Dialog";
import FileUpload from "./FileUpload";
import {
  DropzoneAreaBase,
  DropzoneAreaBaseClasses,
  FileObject,
} from "material-ui-dropzone";
import AttachFile from "@material-ui/icons/AttachFile";
import Graph from "./Graph";
import {
  ConstructionOutlined,
  RepeatOneSharp,
  UploadFile,
} from "@mui/icons-material";
import { convertToObject } from "typescript";
import CytoscapeComponent from "react-cytoscapejs";
import testElements from "./myfile.json";
import dagre from "cytoscape-dagre";
import cytoscape from "cytoscape";
import { ElementDefinition } from "cytoscape";
import GraphClass from "./GraphClass";

type MyProps = {};
type MyState = {
  files: FileObject[];
  dataString: string;
};

export default class MinerClass extends React.Component<MyProps, MyState> {
  constructor(props: any) {
    super(props);

    this.handleAdd = this.handleAdd.bind(this);
    this.handleDelete = this.handleDelete.bind(this);

    this.state = {
      files: [],
      dataString: "",
    };
  }

  componentDidMount() {}

  componentWillUnmount() {}

  static regxRemoveMD: RegExp = /^data:.*?\/.*?;base64,/;

  handleAdd = (newFiles: FileObject[]) => {
    this.setState({ files: [newFiles[0]] });
  };

  handleDelete = (deleted: FileObject) => {
    this.setState({ files: this.state.files.filter((f) => f !== deleted) });
  };

  render() {
    if (this.state.files === undefined || this.state.files.length < 1) {
      return (
        <div
          style={{ width: "100%", display: "flex", justifyContent: "center" }}
        >
          <Box
            sx={{
              display: "flex",
              justifyContent: "center",
              p: 1,
              m: 1,
              width: "60%",
            }}
          >
            <DropzoneAreaBase
              fileObjects={this.state.files}
              //Icon={fileAttachIcon}
              dropzoneText={"Upload .xes or .xml file here"}
              onAdd={this.handleAdd}
              onDelete={this.handleDelete}
              onAlert={(message, variant) =>
                console.log(`${variant}: ${message}`)
              }
              acceptedFiles={[".xml", ".xes"]}
              filesLimit={1}
              maxFileSize={104857600} // max 100 MB
            />
          </Box>
        </div>
      );
    } else {
      // A File has been uploaded

      let dataString = "";
      try {
        if (typeof this.state.files[0].data === "string") {
          if (this.state.files[0].data.match(MinerClass.regxRemoveMD)) {
            dataString = this.state.files[0].data.replace(
              MinerClass.regxRemoveMD,
              ""
            );
          }
          dataString = this.b64_to_utf8(dataString);
        }
      } catch (e) {
        console.log(
          "An error has occured while trying to decode the file. Check if the content of your file has UTF-8 encoding!"
        );
        dataString = "";
      }

      if (dataString === "") {
        // TODO Handle error by giving proper message!
      }

      return <GraphClass postBody={dataString} miner={"alphaminer"} />;
    }
  }

  //  curl -X POST http://localhost:3000/api/v1/alphaminer -H "Content-Type: application/xml" -H "Accept: application/xml" --data @L1.xes > result.txt
  /*
    Encodes a Base64 string to UTF-8 string. Unicode characters are escaped and should work.
    Taken from: https://developer.mozilla.org/en-US/docs/Glossary/Base64#solution_1_%E2%80%93_escaping_the_string_before_encoding_it
    Note: It does use the deprecated escape() function, but it should be more performant than a regex approach as proposed in above link.
    */
  b64_to_utf8(str: string): string {
    return decodeURIComponent(escape(window.atob(str)));
  }
}

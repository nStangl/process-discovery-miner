import React, { useState } from "react";
import Box from "@mui/material/Box";
import { DropzoneAreaBase, FileObject } from "material-ui-dropzone";
import { ToggleButton, ToggleButtonGroup, Typography } from "@mui/material";

import { MainProcessResult } from "./MainProcessResult";

type MainUploadProps = {};
type MainUploadState = {
  files: FileObject[];
  dataString: string;
  miner: string;
};

export default class MainUpload extends React.Component<
  MainUploadProps,
  MainUploadState
> {
  constructor(props: any) {
    super(props);

    this.handleAdd = this.handleAdd.bind(this);
    this.handleDelete = this.handleDelete.bind(this);

    this.state = {
      files: [],
      dataString: "",
      miner: "alphaminer",
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

  handleMinerSelect = (
    event: React.MouseEvent<HTMLElement>,
    newMiner: string
  ) => {
    if (this.state.miner != newMiner) this.setState({ miner: newMiner });
  };

  render() {
    if (this.state.files === undefined || this.state.files.length < 1) {
      return (
        <div>
          <Box sx={{display: "flex", justifyContent: "center", marginTop: "20px"}}>
            <ToggleButtonGroup
              color="primary"
              value={this.state.miner}
              exclusive
              onChange={this.handleMinerSelect}
            >
              <ToggleButton value="alphaminer">Alpha Miner</ToggleButton>
              <ToggleButton value="alphaplusminer">Alpha+ Miner</ToggleButton>
              <ToggleButton value="regionminer" disabled>Region Miner</ToggleButton>
            </ToggleButtonGroup>
          </Box>
          <Box sx={{display: "flex", justifyContent: "center", width: "100%"}}>
            <Box
              sx={{
                display: "flex",
                justifyContent: "center",
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
          </Box>
        </div>
      );
    } else {
      // A File has been uploaded

      let dataString = "";
      try {
        if (typeof this.state.files[0].data === "string") {
          if (this.state.files[0].data.match(MainUpload.regxRemoveMD)) {
            dataString = this.state.files[0].data.replace(
              MainUpload.regxRemoveMD,
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

      return (
        <MainProcessResult postBody={dataString} miner={this.state.miner} />
      );
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
/*

*/

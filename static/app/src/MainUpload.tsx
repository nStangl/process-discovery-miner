import React, { useState } from "react";
import Box from "@mui/material/Box";
import { DropzoneAreaBase, FileObject } from "material-ui-dropzone";
import { ToggleButton, ToggleButtonGroup, Typography } from "@mui/material";

import { MainProcessResult } from "./MainProcessResult";

type MainUploadProps = {};
type MainUploadState = {
  files: FileObject[];
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
      miner: "alphaplusminer",
    };
  }

  handleAdd = (newFiles: FileObject[]) => {
    this.setState({ files: [newFiles[0]] });
  };

  handleDelete = (deleted: FileObject) => {
    this.setState({ files: this.state.files.filter((f) => f !== deleted) });
  };

  // Options are exclusive and one has to be selected at all times.
  handleMinerSelect = (
    event: React.MouseEvent<HTMLElement>,
    newMiner: string
  ) => {
    if (newMiner !== null) this.setState({ miner: newMiner });
  };

  // Render the buttons and the upload window
  render() {
    if (this.state.files === undefined || this.state.files.length < 1) {
      return (
        <div>
          <Box
            sx={{
              display: "flex",
              justifyContent: "center",
              marginTop: "20px",
            }}
          >
            <ToggleButtonGroup
              color="primary"
              value={this.state.miner}
              exclusive
              onChange={this.handleMinerSelect}
            >
              <ToggleButton value="alphaminer">Alpha Miner</ToggleButton>
              <ToggleButton value="alphaplusminer">Alpha+ Miner</ToggleButton>
            </ToggleButtonGroup>
          </Box>
          <Box
            sx={{ display: "flex", justifyContent: "center", width: "100%" }}
          >
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
      const postBodyPromise: Promise<string> = this.state.files[0].file.text();
      
      return (
        <MainProcessResult postBodyPromise={postBodyPromise} miner={this.state.miner} />
      );
      
    }
  }
}

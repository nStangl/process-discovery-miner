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
} from "material-ui-dropzone";
import AttachFile from "@material-ui/icons/AttachFile";
import Graph from "./Graph";
import testJSONFile from "./transitions.json";
import MinerClass from "./MinerClass";

const theme = createTheme();

export default function Miner() {
  return (
    <React.Fragment>
      <MinerAppBar />

      <MinerClass />
    </React.Fragment>
  );
}

function MinerAppBar() {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <AppBar position="static">
        <Toolbar>
          <AccountTreeIcon sx={{ mr: 2 }} />
          <Typography variant="h5" component="div" sx={{ flexGrow: 1 }}>
            Alpha Miner
          </Typography>

          <InfoDialog />

          <AboutDialog />
        </Toolbar>
      </AppBar>
    </Box>
  );
}

function UploadAndGraph() {
  const [files, setFiles] = useState<any[]>([]);

  const isFileUploaded: Boolean =
    files === undefined || files.length < 1 ? false : true;

  const handleAdd = (newFiles: any) => {
    setFiles([newFiles]);
    console.log(newFiles);
  };

  const handleDelete = (deleted: any) => {
    setFiles(files.filter((f) => f !== deleted));
  };

  if (!isFileUploaded) {
    return (
      <div style={{ width: "100%", display: "flex", justifyContent: "center" }}>
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
            fileObjects={files}
            //Icon={fileAttachIcon}
            dropzoneText={"Upload .xes or .xml file here"}
            onAdd={handleAdd}
            onDelete={handleDelete}
            onAlert={(message, variant) =>
              console.log(`${variant}: ${message}`)
            }
            //acceptedFiles={['.xml', '.xes']}
            filesLimit={1}
            maxFileSize={104857600} // max 100 MB
          />
        </Box>
      </div>
    );
  } else {
    return (
      <div
        style={{ width: "100%", display: "flex", justifyContent: "center" }}
      ></div>
    );
  }
}

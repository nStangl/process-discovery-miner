import React, { useState } from "react";
import { DropzoneAreaBase } from "material-ui-dropzone";
import AttachFile from "@material-ui/icons/AttachFile";
import { createStyles, makeStyles } from "@material-ui/core/styles";
import Graph from "./Graph";

const Home = () => {
  const useStyles = makeStyles((theme) =>
    createStyles({
      previewChip: {
        minWidth: 160,
        maxWidth: 210,
      },
    })
  );

  const classes = useStyles();

  const [files, setFiles] = useState([]);

  const handleAdd = (newFiles: any) => {
    setFiles(newFiles);
  };

  const handleDelete = (deleted: any) => {
    setFiles(files.filter((f) => f !== deleted));
  };

  return (
    <DropzoneAreaBase
      fileObjects={files}
      //Icon={fileAttachIcon}
      dropzoneText={"Upload .xes or .xml file here"}
      onAdd={handleAdd}
      onDelete={handleDelete}
      onAlert={(message, variant) => console.log(`${variant}: ${message}`)}
      acceptedFiles={[".xml", ".xes"]}
      filesLimit={1}
      maxFileSize={104857600} // max 100 MB
    />
  );
};

export default Home;

import * as React from "react";
import Button from "@mui/material/Button";
import { styled } from "@mui/material/styles";
import Dialog from "@mui/material/Dialog";
import DialogTitle from "@mui/material/DialogTitle";
import DialogContent from "@mui/material/DialogContent";
import IconButton from "@mui/material/IconButton";
import CloseIcon from "@mui/icons-material/Close";
import Typography from "@mui/material/Typography";
import Link from "@mui/material/Link";
import Divider from "@mui/material/Divider";
import Box from "@mui/material/Box";

export { InfoDialog, AboutDialog };
export interface DialogTitleProps {
  id: string;
  children?: React.ReactNode;
  onClose: () => void;
}

const BootstrapDialog = styled(Dialog)(({ theme }) => ({
  "& .MuiDialogContent-root": {
    padding: theme.spacing(2),
  },
  "& .MuiDialogActions-root": {
    padding: theme.spacing(1),
  },
}));

const BootstrapDialogTitle = (props: DialogTitleProps) => {
  const { children, onClose, ...other } = props;

  return (
    <DialogTitle sx={{ m: 0, p: 2 }} {...other}>
      {children}
      {onClose ? (
        <IconButton
          aria-label="close"
          onClick={onClose}
          sx={{
            position: "absolute",
            right: 8,
            top: 8,
            color: (theme) => theme.palette.grey[500],
          }}
        >
          <CloseIcon />
        </IconButton>
      ) : null}
    </DialogTitle>
  );
};

function InfoDialog() {
  const [open, setOpen] = React.useState(false);

  const handleClickOpen = () => {
    setOpen(true);
  };
  const handleClose = () => {
    setOpen(false);
  };

  return (
    <div>
      <Button
        variant="outlined"
        onClick={handleClickOpen}
        style={{ color: "#fff" }}
      >
        Help
      </Button>
      <BootstrapDialog
        onClose={handleClose}
        aria-labelledby="customized-dialog-title"
        open={open}
      >
        <BootstrapDialogTitle
          id="customized-dialog-title"
          onClose={handleClose}
        >
          How to use
        </BootstrapDialogTitle>
        <DialogContent dividers sx={{ p: 2 }}>
          <Box sx={{ fontWeight: "medium", fontSize: 16, m: 1 }}>
            1. Select the process discovery algorithm.{" "}
          </Box>

          <Box sx={{ fontWeight: "medium", fontSize: 16, m: 1 }}>
            2. Upload a log file as a .xml or .xes file. See{" "}
            <Link
              href="https://lehre.bpm.in.tum.de/~pm-prak/datasets/"
              target="_blank"
            >
              here
            </Link>{" "}
            for examples on how the log is supposed to be structured.
          </Box>

          <Box sx={{ fontWeight: "medium", fontSize: 16, m: 1 }}>
            3. The alpha miner then processes the log and returns a petri net.
          </Box>

          <Typography gutterBottom>
            The graph is interactive and can be saved as a JSON or PNG. Note
            that it also saves the layout.
          </Typography>
          <Divider />
          <Typography gutterBottom>
            The backend currently supports files with a maximum size up to 50
            MB. This could easily be changed in the settings.
          </Typography>
        </DialogContent>
      </BootstrapDialog>
    </div>
  );
}

function AboutDialog() {
  const [open, setOpen] = React.useState(false);

  const handleClickOpen = () => {
    setOpen(true);
  };
  const handleClose = () => {
    setOpen(false);
  };

  return (
    <div>
      <Button
        variant="outlined"
        onClick={handleClickOpen}
        style={{ color: "#fff" }}
      >
        About
      </Button>
      <BootstrapDialog
        onClose={handleClose}
        aria-labelledby="customized-dialog-title"
        open={open}
      >
        <BootstrapDialogTitle
          id="customized-dialog-title"
          onClose={handleClose}
        >
          About
        </BootstrapDialogTitle>
        <DialogContent dividers>
          <Typography gutterBottom>
            This is an implementation of easy to use process discovery
            algorithms. It currently supports the Alpha Miner and the Alpha+
            Miner.
            <br />
            If you are unsure which mining alogrithm to choose, take the Alpha+
            Miner.
            <br />
            The Alpha Miner is unable to handle short loops, which the Alpha+ Miner can handle.
            <br />
            <br />
            The implementation of the Alpha Miner is based on [1].
            <br />
            The implementation of the Alpha+ Miner is mainly based on [2].
            <br />
            <br />
            This project was made for my Bachelor internship 'Implementation of
            Process Mining Algorithms: Transformative Business Knowledge'.
            <br />
            The project is made with React.js, Material UI and Typescript. As
            well as Haskell and Yesod for the webserver.
          </Typography>
          <Divider />
          <Typography gutterBottom>
            <Box sx={{ fontWeight: "medium", fontSize: 16, m: 1 }}>
              Mentionable Sources{" "}
            </Box>
            [1]: Van Der Aalst, Wil. Process Mining, Data Science in Action
            [2016]
            <br />
            [2]: ML Wiki.{" "}
            <Link
              href="https://http://mlwiki.org/index.php/Alpha_Algorithm#The_Alpha_Plus_Algorithm"
              target="_blank"
            >
              The Alpha Plus Algorithm
            </Link>
            <br />
            Favicon:{" "}
            <Link
              href="https://www.flaticon.com/free-icon/pickaxe_663361"
              target="_blank"
            >
              source
            </Link>{" "}
          </Typography>
        </DialogContent>
      </BootstrapDialog>
    </div>
  );
}

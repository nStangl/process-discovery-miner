import * as React from "react";
import Button from "@mui/material/Button";
import { styled } from "@mui/material/styles";
import Dialog from "@mui/material/Dialog";
import DialogTitle from "@mui/material/DialogTitle";
import DialogContent from "@mui/material/DialogContent";
import DialogActions from "@mui/material/DialogActions";
import IconButton from "@mui/material/IconButton";
import CloseIcon from "@mui/icons-material/Close";
import Typography from "@mui/material/Typography";
import Link from "@mui/material/Link";
import Divider from '@mui/material/Divider';

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
        <DialogContent dividers>
          <Typography gutterBottom>
            To use the alpha miner upload a log file either as a .xml or .xes
            file. See{" "}
            <Link
              href="https://lehre.bpm.in.tum.de/~pm-prak/datasets/"
              target="_blank"
            >
              here
            </Link>{" "}
            for an example on the expected log structure.
            <br />
            The alpha miner then processes the log and returns a petri net.
          </Typography>
          <Divider />
          <Typography gutterBottom>
            The alpha miner then processes the log and returns a petri net.
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
            This is an implementation of a alpha miner as a easy to use web service. The implementation is based on [1].
            <br />

          </Typography>
          <Divider />
          <Typography gutterBottom>
            [1]: Van Der Aalst, Wil. Process Mining, Data Science in Action [2016]
            <br />
            Favicon source: <Link
              href="https://www.flaticon.com/free-icon/pickaxe_663361"
              target="_blank"
            >
              here
            </Link>{" "} 
          
          </Typography>
        </DialogContent>
      </BootstrapDialog>
    </div>
  );
}

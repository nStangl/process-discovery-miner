import * as React from "react";
import Typography from "@mui/material/Typography";
import Link from "@mui/material/Link";
import Box from "@mui/material/Box";
import Paper from "@mui/material/Paper";
import { styled } from "@mui/material/styles";
import { ThemeProvider } from "@mui/material/styles";
import theme from "./theme";
import AppBar from "@mui/material/AppBar";
import Toolbar from "@mui/material/Toolbar";
import AccountTreeIcon from "@mui/icons-material/AccountTree";
import { InfoDialog, AboutDialog } from "./Dialog";
import { CssBaseline } from "@mui/material";
import MainUpload from "./MainUpload";

export default function App() {
  return (
    <ThemeProvider theme={theme}>
      <CssBaseline>
        <Box sx={{ flexGrow: 1 }}>
          <CustomAppBar />
          <MainUpload />
          <Copyright />
        </Box>
      </CssBaseline>
    </ThemeProvider>
  );
}

function Copyright() {
  return (
    <Typography variant="body2" color="text.secondary" align="center">
      {"Copyright © "}
      <Link color="inherit" href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">
        Bachelor internship implementation
      </Link>{" "}
      {new Date().getFullYear()}.
    </Typography>
  );
}

const Item = styled(Paper)(({ theme }) => ({
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: "center",
  color: theme.palette.text.secondary,
}));

function CustomAppBar() {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <AppBar position="static">
        <Toolbar>
          <AccountTreeIcon sx={{ mr: 2 }} />
          <Typography variant="h5" component="div" sx={{ flexGrow: 1 }}>
            Alpha+ Miner
          </Typography>

          <InfoDialog />

          <AboutDialog />
        </Toolbar>
      </AppBar>
    </Box>
  );
}

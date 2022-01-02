import * as React from 'react';
import Typography from '@mui/material/Typography';
import Link from '@mui/material/Link';
import Container from '@mui/material/Typography';
import Box from '@mui/material/Box';
import Grid from '@mui/material/Grid';
import Paper from '@mui/material/Paper';
import { styled } from '@mui/material/styles';

import Miner from './Miner';
import { CssBaseline } from '@mui/material';

export default function App() {
  return (
    <CssBaseline>
      <Box sx={{ flexGrow: 1}}>
        <Miner/>
        
        <Copyright/>
      </Box>
    </CssBaseline>
  );
}


function Copyright() {
  return (
    <Typography variant="body2" color="text.secondary" align="center">
      {'Copyright © '}
      <Link color="inherit" href="https://www.youtube.com/watch?v=dQw4w9WgXcQ">
        Very Cool Website
      </Link>{' '}
      {new Date().getFullYear()}.
    </Typography>
      );
}

const Item = styled(Paper)(({ theme }) => ({
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.secondary,
}));


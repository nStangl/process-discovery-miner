import { createTheme } from '@mui/material/styles';
import { red } from '@mui/material/colors';

// A custom theme for this app
const theme = createTheme({
    palette: {
      background: {
        default: "#eceff1" //"#222222"
      },
      primary: {
        light: '#819ca9',
        main: '#546e7a',
        dark: '#29434e',
        contrastText: '#fff',
      },
      secondary: {
        light: '#4f83cc',
        main: '#01579b',
        dark: '#002f6c',
        contrastText: '#fff',
      },
    },
  });

export default theme;
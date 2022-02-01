import { Statistics } from "./MainProcessResult";
import Container from "@mui/material/Container";
import Box from "@mui/material/Box";
import Grid from "@mui/material/Grid";
import Paper from "@mui/material/Paper";
import Typography from "@mui/material/Typography";
import Divider from "@mui/material/Divider";
import Table from "@mui/material/Table";
import TableBody from "@mui/material/TableBody";
import TableCell from "@mui/material/TableCell";
import TableContainer from "@mui/material/TableContainer";
import TableHead from "@mui/material/TableHead";
import TableRow from "@mui/material/TableRow";

type StatisticsProps = {
  isTrace: boolean;
  stats: Statistics;
};

export default function Statistics({
  isTrace,
  stats,
}: StatisticsProps): JSX.Element {
  const {
    distinctOccurences,
    totalOccurences,
    avgLengthAll,
    maxLength,
    avgLengthDistinct,
    minLength,
    mostFrequent,
    leastFrequent,
  } = stats;
  const field = isTrace ? "Trace" : "Event";

  return (
    <Box>
      <Typography variant="h6" component="div">
        {field} Statistics
      </Typography>
      <TableContainer component={Paper}>
        <Table sx={{ minWidth: "auto" }} aria-label="simple table">
          <TableHead>
            <TableRow>
              <TableCell align="center">Total {field}s</TableCell>
              <TableCell align="center">Distinct {field}s</TableCell>
              <TableCell align="center">Min Length</TableCell>
              <TableCell align="center">Avg Length All</TableCell>
              <TableCell align="center">Avg Length Dist.</TableCell>
              <TableCell align="center">Max Length</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            <TableRow>
              <TableCell align="center">{totalOccurences}</TableCell>
              <TableCell align="center">{distinctOccurences}</TableCell>
              <TableCell align="center">{minLength}</TableCell>
              <TableCell align="center">{avgLengthAll}</TableCell>
              <TableCell align="center">{avgLengthDistinct}</TableCell>
              <TableCell align="center">{maxLength}</TableCell>
            </TableRow>
          </TableBody>
        </Table>
        {/*
        <Table>
          <TableHead>
            <TableRow>
              <TableCell align="left">Least Frequent</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            <TableRow>
              <TableCell align="left">{leastFrequent}</TableCell>
            </TableRow>
          </TableBody>
        </Table>
        <Table>
          <TableHead>
            <TableRow>
              <TableCell align="left">Most Frequent</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            <TableRow>
              <TableCell align="left">{mostFrequent}</TableCell>
            </TableRow>
          </TableBody>
        </Table>
        */}
      </TableContainer>
    </Box>
  );

  /*
  return (
    <div>
      <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
        {title}
      </Typography>
      <Box component={Paper}>
        <div>
          <div>
            Total Traces
            <Divider variant="middle"/>
            {totalOccurences}
          </div>
          <div>
            Min length
            <Divider variant="middle"/>
            {minLength}
          </div>
          <div>
            Avg length all
            <Divider variant="middle"/>
            {avgLengthAll}
          </div>
        </div>
        
        <div>
          <div>
            Distinct Traces
            <Divider variant="middle"/>
            {distinctOccurences}
          </div>
          <div>
            Max length
            <Divider variant="middle"/>
            {maxLength}
          </div>
          <div>
            Avg length distinct
            <Divider variant="middle"/>
            {avgLengthDistinct}
          </div>
        </div>
      </Box>
    </div>
  );
  */
}

/*

return (
    <TableContainer component={Paper}>
      <Table arai-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell align='center'>Total Traces</TableCell>
            <TableCell align='center'>Distinct Traces</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          <TableRow>
            <TableCell align='center'>3</TableCell>
            <TableCell align='center'>4</TableCell>
          </TableRow>
        </TableBody>
      </Table>
      <Table arai-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell align='center'>Min length</TableCell>
            <TableCell align='center'>Max length</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          <TableRow>
            <TableCell align='center'>1</TableCell>
            <TableCell align='center'>3</TableCell>
          </TableRow>
        </TableBody>
      </Table>
      <Table arai-label="simple table">
        <TableHead>
          <TableRow>
            <TableCell align='center'>avg length all</TableCell>
            <TableCell align='center'>avg length district</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          <TableRow>
            <TableCell align='center'>1</TableCell>
            <TableCell align='center'>3</TableCell>
          </TableRow>
        </TableBody>
      </Table>
    </TableContainer>


*/

/*
The idea and implementation is based on
https://codenebula.io/javascript/frontend/dataviz/2019/04/18/automatically-generate-chart-colors-with-chart-js-d3s-color-scales/
*/

import React from "react";
import { Chart as ChartJS, ArcElement, Tooltip, Legend } from "chart.js";
import { Pie } from "react-chartjs-2";
import * as d3 from "d3";

type ColorRangeInfo = {
  colorStart: number;
  colorEnd: number;
  useEndAsStart: boolean;
};

ChartJS.register(ArcElement, Tooltip, Legend);

export function PieChart(labels: string[], data: number[], colorStart: number, colorEnd: number, endAsStart: boolean) {
  let n = labels.length;
  // Change colorScale method and colorRangeInfo parameters
  // to change the color palette
  const colorScale = d3.interpolateRdYlBu;
  const colorRangeInfo = {
    colorStart: colorStart,
    colorEnd: colorEnd,
    useEndAsStart: endAsStart,
  };
  var labels2 = labels.map((label) => chunkSubstr(label, 60));

  // generate colors
  var COLORS = interpolateColors(n, colorScale, colorRangeInfo);

  const chartData = {
    labels: labels,
    datasets: [
      {
        labels: labels,
        data: data,
        backgroundColor: COLORS,
      },
    ],
  };
  const chartOptions = {
  }

  return <Pie data={chartData} options={chartOptions} />;
}

function calculatePoint(
  i: number,
  intervalSize: number,
  colorRangeInfo: ColorRangeInfo
) {
  var { colorStart, colorEnd, useEndAsStart } = colorRangeInfo;
  return useEndAsStart
    ? colorEnd - i * intervalSize
    : colorStart + i * intervalSize;
}

/* Must use an interpolated color scale, which has a range of [0, 1] */
function interpolateColors(
  dataLength: number,
  colorScale: (t: number) => string,
  colorRangeInfo: ColorRangeInfo
): string[] {
  var { colorStart, colorEnd } = colorRangeInfo;
  var colorRange = colorEnd - colorStart;
  var intervalSize = colorRange / dataLength;
  var i, colorPoint;
  var colorArray = [];

  for (i = 0; i < dataLength; i++) {
    colorPoint = calculatePoint(i, intervalSize, colorRangeInfo);
    colorArray.push(colorScale(colorPoint));
  }

  return colorArray;
}

// Taken from: https://stackoverflow.com/questions/7033639/split-large-string-in-n-size-chunks-in-javascript
function chunkSubstr(str: string, size: number): string[] {
  const numChunks = Math.ceil(str.length / size)
  const chunks = new Array(numChunks)

  for (let i = 0, o = 0; i < numChunks; ++i, o += size) {
    chunks[i] = str.substr(o, size)
  }

  return chunks
}
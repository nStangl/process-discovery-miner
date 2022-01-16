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

export function PieChart(labels: string[], data: number[]) {
  let n = labels.length;
  // Change colorScale method and colorRangeInfo parameters
  // to change the color palette
  const colorScale = d3.interpolateRdYlBu;
  const colorRangeInfo = {
    colorStart: 0.4,
    colorEnd: 1,
    useEndAsStart: true,
  };

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

  return <Pie data={chartData} />;
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

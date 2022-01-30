import * as React from "react";
import Accordion from "@mui/material/Accordion";
import AccordionDetails from "@mui/material/AccordionDetails";
import AccordionSummary from "@mui/material/AccordionSummary";
import Typography from "@mui/material/Typography";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import Latex from "react-latex-next";
import {
  Transition,
  AlphaminerSets,
  LoopWithNeighbour,
} from "./MainProcessResult";

export default function AlphaMinerSetsAccordion(
  ams: AlphaminerSets,
  loopsWNeigh?: Array<LoopWithNeighbour>
) {

  const isLoops: boolean = (loopsWNeigh === null || loopsWNeigh === undefined) ? false : true;

  const { tl, ti, to, xl, yl } = ams;

  // Description for the corresponding sets in KaTeX
  const tex_tl_desc =
    "$\\lbrace t \\in T \\mid \\exists_{\\sigma \\in L} t \\in \\sigma \\rbrace$";
  const tex_ti_desc =
    "$\\lbrace t \\in T \\mid \\exists_{\\sigma \\in L} t = first(\\sigma) \\rbrace$";
  const tex_to_desc =
    "$\\lbrace t \\in T \\mid \\exists_{\\sigma \\in L} t = last(\\sigma) \\rbrace$";
  const tex_xl_desc =
    "$\\lbrace (A,B) \\mid A \\subseteq T_L \\land A \\ (not =) \\ \\emptyset \\land B \\subseteq T_L \\land B \\ (not =) \\ \\emptyset \\land \\forall_{a \\in A} \\forall_{b \\in B} a \\rarr_L b $ $\\land \\forall_{a_{1},a_{2} \\in A} a_{1} \\# a_{2} \\land \\forall_{b_{1},b_{2} \\in B} b_{1} \\# b_{2} \\rbrace$";
  const tex_yl_desc =
    "$\\lbrace (A,B) \\in X_L \\mid \\forall_{(A',B') \\in X_L} A \\subseteq A' \\land B \\subseteq B' \\rArr (A,B) = (A',B') \\rbrace$";

  return (
    <div>
      <Typography>
        L is an event log over <Latex>{"$T \\subseteq \\Alpha$."}</Latex>
      </Typography>
      <Typography variant="caption">
        (There is a bug in <Latex>{"$\\KaTeX$"}</Latex> which results in 'not
        equal' not rendering correctly.)
      </Typography>
      <Accordion>
        <AccordionSummary
          expandIcon={<ExpandMoreIcon />}
          aria-controls="panel1bh-content"
          id="panel1bh-header"
        >
          <Typography sx={{ width: "10%", flexShrink: 0 }}>
            <Latex>{"$T_L$"}</Latex>
          </Typography>
          <Typography sx={{ color: "text.secondary" }}>
            <Latex>{tex_tl_desc}</Latex>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography>{tl.join()}</Typography>
        </AccordionDetails>
      </Accordion>
      <Accordion>
        <AccordionSummary
          expandIcon={<ExpandMoreIcon />}
          aria-controls="panel2bh-content"
          id="panel2bh-header"
        >
          <Typography sx={{ width: "10%", flexShrink: 0 }}>
            <Latex>{"$T_I$"}</Latex>
          </Typography>
          <Typography sx={{ color: "text.secondary" }}>
            <Latex>{tex_ti_desc}</Latex>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography>{ti.join()}</Typography>
        </AccordionDetails>
      </Accordion>
      <Accordion>
        <AccordionSummary
          expandIcon={<ExpandMoreIcon />}
          aria-controls="panel3bh-content"
          id="panel3bh-header"
        >
          <Typography sx={{ width: "10%", flexShrink: 0 }}>
            <Latex>{"$T_O$"}</Latex>
          </Typography>
          <Typography sx={{ color: "text.secondary" }}>
            <Latex>{tex_to_desc}</Latex>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography>{to.join()}</Typography>
        </AccordionDetails>
      </Accordion>
      <Accordion>
        <AccordionSummary
          expandIcon={<ExpandMoreIcon />}
          aria-controls="panel4bh-content"
          id="panel4bh-header"
        >
          <Typography sx={{ width: "10%", flexShrink: 0 }}>
            <Latex>{"$X_L$"}</Latex>
          </Typography>
          <Typography sx={{ color: "text.secondary" }}>
            <Latex>{tex_xl_desc}</Latex>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography>
            {xl.map((t) => prettyPrintTransition(t)).join(", ")}
          </Typography>
        </AccordionDetails>
      </Accordion>
      <Accordion>
        <AccordionSummary
          expandIcon={<ExpandMoreIcon />}
          aria-controls="panel4bh-content"
          id="panel4bh-header"
        >
          <Typography sx={{ width: "10%", flexShrink: 0 }}>
            <Latex>{"$Y_L$"}</Latex>
          </Typography>
          <Typography sx={{ color: "text.secondary" }}>
            <Latex>{tex_yl_desc}</Latex>
          </Typography>
        </AccordionSummary>
        <AccordionDetails>
          <Typography>
            {yl.map((t) => prettyPrintTransition(t)).join(", ")}
          </Typography>
        </AccordionDetails>
      </Accordion>
      {isLoops ? (
        <Accordion>
          <AccordionSummary
            expandIcon={<ExpandMoreIcon />}
            aria-controls="panel4bh-content"
            id="panel4bh-header"
          >
            <Typography sx={{ width: "10%", flexShrink: 0 }}>
              Loops
            </Typography>
            <Typography sx={{ color: "text.secondary" }}>
              <Latex>{"Structured like (left neighbour, loop, right neighbour)"}</Latex>
            </Typography>
          </AccordionSummary>
          <AccordionDetails>
            <Typography>
              {loopsWNeigh?.map((l) => prettyPrintLoop(l)).join(", ")}
            </Typography>
          </AccordionDetails>
        </Accordion>
      ) : (
        <div></div>
      )}
    </div>
  );
}

function prettyPrintTransition([from, to]: Transition): string {
  return "({" + from.join() + "},{" + to.join() + "})";
}

function prettyPrintLoop([left, loop, right]: LoopWithNeighbour): string {
  return "(" + left + ", " + loop + ", " + right + ")";
}

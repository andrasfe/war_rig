/**
 * Batch mermaid diagram validation via mermaid.parse().
 *
 * Reads JSON from stdin:  { "diagrams": ["flowchart TD\n  A-->B", ...] }
 * Writes JSON to stdout:  { "results": [{ "valid": true }, { "valid": false, "error": "..." }] }
 */

import { JSDOM } from "jsdom";
import mermaid from "mermaid";

// jsdom globals required by mermaid's browser-oriented code
const dom = new JSDOM("<!DOCTYPE html><html><body></body></html>");
global.document = dom.window.document;
global.window = dom.window;
global.DOMParser = dom.window.DOMParser;

mermaid.initialize({ startOnLoad: false });

const chunks = [];
for await (const chunk of process.stdin) {
  chunks.push(chunk);
}

const input = JSON.parse(Buffer.concat(chunks).toString("utf-8"));
const results = [];

for (const diagram of input.diagrams) {
  try {
    await mermaid.parse(diagram);
    results.push({ valid: true });
  } catch (err) {
    results.push({ valid: false, error: String(err?.message ?? err) });
  }
}

process.stdout.write(JSON.stringify({ results }));

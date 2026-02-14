/**
 * Batch mermaid diagram validation via mermaid.parse().
 *
 * Reads JSON from stdin:  { "diagrams": ["flowchart TD\n  A-->B", ...] }
 * Writes JSON to stdout:  { "results": [{ "valid": true }, { "valid": false, "error": "..." }] }
 */

import { JSDOM } from "jsdom";

// Set up minimal jsdom globals BEFORE importing mermaid.
// mermaid.parse() only needs syntax validation (no rendering/DOMPurify).
const dom = new JSDOM("<!DOCTYPE html><html><body></body></html>");
global.document = dom.window.document;
global.window = dom.window;
global.DOMParser = dom.window.DOMParser;

// Dynamic import so globals are visible at module-load time
const { default: mermaid } = await import("mermaid");
mermaid.initialize({ startOnLoad: false, suppressErrorRendering: true });

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

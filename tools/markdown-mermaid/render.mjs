import { render } from 'pele';
import { pathToFileURL } from 'node:url';

// Resolve variables before handing SVG to librsvg.  Keep this inside tags:
// a diagram label may itself contain text such as "var(--pele-bg)".
function resolveCss(value, variables, seen = new Set()) {
  let output = '';
  let position = 0;
  for (;;) {
    const start = value.indexOf('var(', position);
    if (start === -1) return output + value.slice(position);
    let end = start + 4;
    let depth = 1;
    let quote = '';
    for (; end < value.length && depth; end++) {
      const char = value[end];
      if (char === '\\') { end++; continue; }
      if (quote) { if (char === quote) quote = ''; continue; }
      if (char === '"' || char === "'") quote = char;
      else if (char === '(') depth++;
      else if (char === ')') depth--;
    }
    if (depth) throw new Error('Unclosed CSS variable in SVG');
    const expression = value.slice(start + 4, end - 1);
    const comma = expression.indexOf(',');
    const name = (comma === -1 ? expression : expression.slice(0, comma)).trim();
    const fallback = comma === -1 ? 'initial' : expression.slice(comma + 1).trim();
    const replacement = seen.has(name) ? fallback : (variables[name] ?? fallback);
    output += value.slice(position, start)
      + resolveCss(replacement, variables, new Set([...seen, name]));
    position = end;
  }
}

function escapeAttribute(value) {
  return String(value).replaceAll('&', '&amp;').replaceAll('"', '&quot;')
    .replaceAll('<', '&lt;').replaceAll('>', '&gt;');
}

export function staticSvg(svg, colors = {}) {
  const base = Object.fromEntries(Object.entries(colors)
    .map(([name, value]) => [name, escapeAttribute(value)]));
  const scopes = [base];
  return svg.replace(/<(\/?)([\w:-]+)\b[^>]*>/g, (tag, closing) => {
    if (closing) { scopes.pop(); return tag; }
    const variables = Object.create(scopes.at(-1));
    const style = /\bstyle="([^"]*)"/.exec(tag)?.[1] ?? '';
    for (const [, name, value] of style.matchAll(/(?:^|;)\s*(--[\w-]+)\s*:\s*([^;]*)/g)) {
      variables[name] = value;
    }
    if (!tag.endsWith('/>')) scopes.push(variables);
    return tag.replace(/([\w:-]+)="([^"]*)"/g, (attribute, name, value) => {
      if (name === 'style') {
        value = value.replace(/(?:^|;)\s*--[\w-]+\s*:[^;]*/g, '').replace(/^;+/, '');
      }
      // Text in data-* and accessibility attributes is user content, not CSS.
      if (/^(data-|aria-)|^(id|class|role|xmlns|href|xlink:href)$/.test(name)) {
        return attribute;
      }
      return `${name}="${resolveCss(value, variables)}"`;
    });
  });
}

export function renderBatch({ diagrams, options = {}, colors = {} }) {
  return {
    results: diagrams.map(({ key, source }) => {
      try {
        const { svg, width, height } = render(source, {
          ...options, responsive: false, autoDirection: false,
          links: false, images: false,
        });
        return { key, svg: staticSvg(svg, colors), width, height };
      } catch (error) {
        return { key, error: error.message ?? String(error) };
      }
    }),
  };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    process.stdin.setEncoding('utf8');
    let input = '';
    for await (const chunk of process.stdin) input += chunk;
    process.stdout.write(JSON.stringify(renderBatch(JSON.parse(input))));
  } catch (error) {
    process.stderr.write(`${error.message ?? error}\n`);
    process.exitCode = 1;
  }
}

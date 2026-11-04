import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import { renderBatch, staticSvg } from './render.mjs';

test('CSS variables are resolved with inheritance, local scopes and nested fallbacks', () => {
  const svg = '<svg style="--ink:var(--pele-fg,#111);--paint:var(--missing,rgb(1,2,3))" fill="var(--ink)">'
    + '<g style="--ink:#abc" stroke="var(--ink)"><path fill="var(--paint)"/></g>'
    + '<text fill="var(--ink)" aria-label="var(--ink)">var(--ink)</text></svg>';
  const result = staticSvg(svg, { '--pele-fg': '#eee' });
  assert.match(result, /<svg style="" fill="#eee">/);
  assert.match(result, /<g style="" stroke="#abc">/);
  assert.match(result, /<path fill="rgb\(1,2,3\)"\/>/);
  assert.match(result, /<text fill="#eee" aria-label="var\(--ink\)">var\(--ink\)<\/text>/);
});

test('Pele renders Chinese labels, several diagram types and per-diagram errors', () => {
  const sources = [
    'flowchart LR\n A["读取文档"] --> B["SVG 显示"]',
    'sequenceDiagram\n participant 用户\n participant Emacs\n 用户->>Emacs: 打开文档',
    'classDiagram\n Animal <|-- Duck\n Animal : +age int',
    'mindmap\n root((阅读))\n  文本\n  图表',
    'flowchart LR\n A[unfinished',
  ];
  const { results } = renderBatch({
    diagrams: sources.map((source, key) => ({ key, source })),
    colors: { '--pele-fg': '#f8f8f2', '--pele-line': '#eee', '--pele-surface': '#383830' },
    options: { fontFamily: 'Microsoft YaHei', maxWidth: 700 },
  });
  for (const result of results.slice(0, -1)) {
    assert.equal(result.error, undefined);
    assert.ok(result.width > 0 && result.height > 0);
    assert.match(result.svg, /^<svg xmlns=/);
    assert.doesNotMatch(result.svg, /var\(|foreignObject|--[\w-]+:/);
    assert.match(result.svg, /fill="#f8f8f2"/);
  }
  assert.match(results[0].svg, /读取文档/);
  assert.match(results[0].svg, /stroke="#eee"/);
  assert.match(results[0].svg, /fill="#383830"/);
  assert.ok(results.at(-1).error);
  assert.equal(results.at(-1).svg, undefined);
});

test('stdin/stdout protocol preserves Unicode and shell-like labels', () => {
  const source = 'flowchart TD\n A["中文 $HOME $(echo test) `code`"] --> B["完成"]';
  const process = spawnSync(globalThis.process.execPath,
    [fileURLToPath(new URL('./render.mjs', import.meta.url))], {
      input: JSON.stringify({ diagrams: [{ key: 'one', source }] }), encoding: 'utf8',
    });
  assert.equal(process.status, 0, process.stderr);
  const result = JSON.parse(process.stdout).results[0];
  assert.match(result.svg.replace(/<[^>]*>/g, ' ').replace(/\s+/g, ' '),
    /中文 \$HOME \$\(echo test\) `code`/);
  assert.equal(result.key, 'one');
});

test('malformed input fails through stderr instead of returning a partial SVG', () => {
  const process = spawnSync(globalThis.process.execPath,
    [fileURLToPath(new URL('./render.mjs', import.meta.url))], { input: '{', encoding: 'utf8' });
  assert.equal(process.status, 1);
  assert.equal(process.stdout, '');
  assert.ok(process.stderr.length);
});

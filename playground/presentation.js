import { bindPlotTooltip, createPlotViews } from './full-size-plot.js';

const pageSize = 10;
const grid = Array.from({length:31}, (_,index) => (index / 5).toFixed(1));

function columns(kind, rows, stdout) {
  const labels = stdout.filter(row => row.kind === 'sample').map(row => row.label);
  switch (kind) {
    case 'poisson': return ['Count', 'Frequency'];
    case 'regression': return ['Draw', 'f(0)', 'f(6)', ...grid.map(x => `Predictive y(${x})`)];
    case 'gp': case 'wiener': case 'piecewise': return ['Draw', ...grid.map(x => `f(${x})`)];
    case 'clustering': return ['Point', 'x', 'y', 'Cluster', 'Centre x', 'Centre y', 'Sigma'];
    case 'irm': return ['Person id', ...labels];
    case 'program': return ['Program', 'x', 'f(x)'];
    case 'ibp': return ['Country id', ...Array.from({length: rows[0].length - 1}, (_,index) => `Feature ${index + 1}`)];
    case 'graph': return ['Vertex', ...Array.from({length: rows.length}, (_,index) => String(index + 1))];
    case 'mondrian': return ['Block', 'x lower', 'x upper', 'y lower', 'y upper', 'Value'];
    default: throw new Error(`Unknown model: ${kind}`);
  }
}

export function createPresentation(root, plotter) {
  const tables = new Map();
  const plotViews = createPlotViews(root, plotter);
  function showScrollHint(section) {
    const region = section.querySelector('.numeric-scroll');
    section.querySelector('[data-scroll-hint]').hidden = !(region.clientWidth > 0 &&
      (region.scrollWidth > region.clientWidth || region.scrollHeight > region.clientHeight));
  }
  const observer = new ResizeObserver(entries => {
    for (const entry of entries) showScrollHint(entry.target.closest('[data-numeric-model]'));
  });
  function render(kind) {
    const state = tables.get(kind);
    if (!state) return;
    const table = root.querySelector(`#${kind}-table`);
    const head = document.createElement('tr');
    for (const title of state.rowNames ? [kind === 'irm' ? 'Person' : 'Country', ...state.columns] : state.columns) {
      const cell = document.createElement('th'); cell.scope = 'col'; cell.textContent = title; head.append(cell);
    }
    table.tHead.replaceChildren(head);
    const start = state.page * pageSize;
    const fragment = document.createDocumentFragment();
    const clusters = kind === 'clustering' ? [...new Set(state.rows.map(row => row[3]))] : null;
    for (const [offset, row] of state.rows.slice(start, start + pageSize).entries()) {
      const line = document.createElement('tr');
      if (state.rowNames) {
        const cell = document.createElement('th'); cell.scope = 'row'; cell.textContent = state.rowNames[start + offset]; line.append(cell);
      }
      const values = kind === 'graph' || kind === 'program' ? [row[0] + 1, ...row.slice(1)]
        : kind === 'clustering' ? row.map((value, index) => index === 3 ? clusters.indexOf(value) + 1 : value) : row;
      for (const value of values) { const cell = document.createElement('td'); cell.textContent = String(value); line.append(cell); }
      fragment.append(line);
    }
    table.tBodies[0].replaceChildren(fragment);
    const section = root.querySelector(`#${kind}-numbers`);
    section.querySelector('[data-page-range]').textContent = `Rows ${start + 1}–${Math.min(start + pageSize, state.rows.length)} of ${state.rows.length}`;
    section.querySelector('[data-previous]').disabled = state.page === 0;
    section.querySelector('[data-next]').disabled = start + pageSize >= state.rows.length;
    section.querySelector('[data-pagination]').hidden = state.rows.length <= pageSize;
    showScrollHint(section);
  }
  const handlers = [];
  const tooltipHandlers = [];
  for (const kind of ['irm', 'ibp']) {
    const svg = root.querySelector(`#${kind}-plot`);
    if (!svg) continue;
    const tooltip = document.createElement('div');
    tooltip.setAttribute('popover', 'auto');
    tooltip.setAttribute('role', 'tooltip');
    tooltip.dataset.plotTooltip = '';
    svg.parentElement.append(tooltip);
    tooltipHandlers.push({controller:bindPlotTooltip(svg, tooltip), tooltip});
  }
  for (const section of root.querySelectorAll('[data-numeric-model]')) {
    observer.observe(section.querySelector('.numeric-scroll'));
    const kind = section.dataset.numericModel;
    for (const [selector, delta] of [['[data-previous]', -1], ['[data-next]', 1]]) {
      const button = section.querySelector(selector);
      button.onclick = () => { const state = tables.get(kind); if (state) { state.page += delta; render(kind); } };
      handlers.push(button);
    }
  }
  return {
    show(kind, rows, stdout) {
      const rowNames = ['irm', 'ibp'].includes(kind) ? stdout.filter(row => row.kind === 'sample').map(row => row.label) : null;
      tables.set(kind, {rows, columns:columns(kind, rows, stdout), rowNames, page:0});
      render(kind);
      if (kind === 'poisson') {
        plotter.draw(kind, {rows});
        root.querySelector('#poisson-description').textContent = 'Observed frequencies from 10,000 Poisson simulations.';
      }
    },
    completePlot(kind) { plotViews.complete(kind); },
    dispose() { observer.disconnect(); for (const button of handlers) button.onclick = null; for (const {controller, tooltip} of tooltipHandlers) { controller.abort(); tooltip.remove(); } tables.clear(); plotViews.dispose(); },
  };
}

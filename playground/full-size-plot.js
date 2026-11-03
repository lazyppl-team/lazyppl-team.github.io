const presentationProperties = [
  'fill', 'fill-opacity', 'stroke', 'stroke-width', 'stroke-opacity',
  'stroke-linecap', 'stroke-linejoin', 'stroke-dasharray', 'stroke-dashoffset',
  'stroke-miterlimit', 'fill-rule', 'clip-rule', 'font-family', 'font-size',
  'font-weight', 'font-style', 'font-variant', 'letter-spacing', 'word-spacing',
  'text-anchor', 'dominant-baseline', 'alignment-baseline', 'opacity', 'color',
];

export function bindPlotTooltip(svg, tooltip) {
  const controller = new AbortController();
  const show = event => {
    const cell = event.target.closest('rect[data-plot-cell]');
    if (!cell || !svg.contains(cell)) return;
    tooltip.textContent = cell.getAttribute('aria-label');
    if (!tooltip.matches(':popover-open')) tooltip.showPopover();
  };
  svg.addEventListener('pointerover', event => { if (event.pointerType === 'mouse') show(event); }, {signal:controller.signal});
  svg.addEventListener('pointerout', event => {
    if (event.pointerType === 'mouse' && tooltip.matches(':popover-open')) tooltip.hidePopover();
  }, {signal:controller.signal});
  svg.addEventListener('click', show, {signal:controller.signal});
  svg.addEventListener('focusin', show, {signal:controller.signal});
  svg.addEventListener('focusout', () => { if (tooltip.matches(':popover-open')) tooltip.hidePopover(); }, {signal:controller.signal});
  return controller;
}

function fullSizeDocument(svg) {
  const clone = svg.cloneNode(true);
  const originals = [svg, ...svg.querySelectorAll('*')];
  const copies = [clone, ...clone.querySelectorAll('*')];
  originals.forEach((element, index) => {
    const style = getComputedStyle(element);
    for (const property of presentationProperties) copies[index].style.setProperty(property, style.getPropertyValue(property));
  });
  clone.setAttribute('data-full-plot', '');
  clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
  clone.setAttribute('width', svg.getAttribute('width'));
  clone.setAttribute('height', svg.viewBox.baseVal.height);
  clone.style.display = 'block';
  clone.style.position = 'static';
  clone.style.left = 'auto';
  const background = getComputedStyle(document.body).backgroundColor;
  clone.style.backgroundColor = background;
  const documentView = document.implementation.createHTMLDocument(svg.querySelector('title').textContent);
  const viewport = documentView.createElement('meta');
  viewport.name = 'viewport';
  viewport.content = 'width=device-width, initial-scale=1';
  const fonts = documentView.createElement('link');
  fonts.rel = 'stylesheet';
  fonts.href = new URL('../brand/fonts.css', import.meta.url).href;
  documentView.head.append(viewport, fonts);
  documentView.body.style.margin = '0';
  documentView.body.style.backgroundColor = background;
  const bodyStyle = getComputedStyle(document.body);
  documentView.body.style.color = bodyStyle.color;
  documentView.body.style.fontFamily = bodyStyle.fontFamily;
  documentView.body.style.minWidth = `${svg.getAttribute('width')}px`;
  const heading = documentView.createElement('h1');
  heading.textContent = svg.querySelector('title').textContent;
  heading.style.cssText = 'font-size:22px;line-height:1.3;margin:16px;max-width:min(688px,calc(100vw - 32px))';
  heading.style.fontFamily = getComputedStyle(document.documentElement).getPropertyValue('--font-display');
  const caption = documentView.createElement('p');
  caption.textContent = svg.querySelector('desc').textContent;
  caption.style.cssText = 'font-size:14px;line-height:1.5;margin:16px;max-width:min(688px,calc(100vw - 32px))';
  documentView.body.append(heading, caption, clone);
  const tooltip = svg.parentElement.querySelector('[data-plot-tooltip]');
  if (tooltip) {
    const copy = tooltip.cloneNode(false);
    const style = getComputedStyle(tooltip);
    for (const property of ['position', 'top', 'right', 'bottom', 'left', 'margin', 'max-inline-size', 'padding',
      'border', 'border-radius', 'box-sizing', 'background-color', 'color', 'font-family', 'font-size', 'font-weight',
      'line-height', 'pointer-events']) copy.style.setProperty(property, style.getPropertyValue(property));
    documentView.body.append(copy);
    documentView.body.dataset.fullSizePlot = '';
    const script = documentView.createElement('script');
    script.type = 'module';
    script.src = import.meta.url;
    documentView.head.append(script);
  }
  const legend = svg.parentElement.querySelector('.plot-legend');
  if (legend) {
    const copy = legend.cloneNode(true);
    copy.style.cssText = 'display:flex;flex-wrap:wrap;gap:12px 24px;font-size:14px;list-style:none;margin:16px;padding:0;max-width:min(688px,calc(100vw - 32px))';
    copy.style.fontFamily = getComputedStyle(legend).fontFamily;
    copy.style.color = getComputedStyle(legend).color;
    [...copy.children].forEach(item => { item.style.cssText = 'display:flex;align-items:center;gap:8px'; });
    [...legend.querySelectorAll('span')].forEach((swatch, index) => {
      const style = getComputedStyle(swatch), target = copy.querySelectorAll('span')[index];
      for (const property of ['display','width','height','background-color','background-image','border-top','border-radius']) {
        target.style.setProperty(property, style.getPropertyValue(property));
      }
    });
    documentView.body.append(copy);
  }
  return `<!doctype html>${documentView.documentElement.outerHTML}`;
}

if (document.body?.hasAttribute('data-full-size-plot')) {
  const tooltip = document.querySelector('[data-plot-tooltip]');
  const viewport = window.visualViewport;
  const place = () => {
    tooltip.style.left = `${viewport.offsetLeft + viewport.width - 16}px`;
    tooltip.style.top = `${viewport.offsetTop + viewport.height - 16}px`;
    tooltip.style.maxInlineSize = `${Math.max(0, viewport.width - 32)}px`;
  };
  tooltip.style.right = 'auto';
  tooltip.style.bottom = 'auto';
  tooltip.style.width = 'max-content';
  tooltip.style.transform = 'translate(-100%, -100%)';
  viewport.addEventListener('resize', place);
  viewport.addEventListener('scroll', place);
  window.addEventListener('scroll', place);
  place();
  bindPlotTooltip(document.querySelector('svg[data-full-plot]'), tooltip);
}

export function createPlotViews(root, plotter) {
  const views = new Map();
  function refresh(kind, view) {
    if (!view.completed) return;
    const svg = plotter.snapshot(kind);
    const documentText = fullSizeDocument(svg);
    svg.remove();
    const url = URL.createObjectURL(new Blob([documentText], { type: 'text/html;charset=utf-8' }));
    const previous = view.url;
    view.url = url;
    view.link.href = url;
    if (previous) URL.revokeObjectURL(previous);
  }
  for (const link of root.querySelectorAll('[data-full-plot]')) {
    const kind = link.dataset.fullPlot;
    const view = { link, url: null, completed: false };
    link.onclick = () => refresh(kind, view);
    views.set(kind, view);
  }
  const observer = new MutationObserver(() => {
    for (const [kind, view] of views) refresh(kind, view);
  });
  observer.observe(document.documentElement, { attributes: true, attributeFilter: ['class'] });
  return {
    complete(kind) {
      const view = views.get(kind);
      view.completed = true;
      refresh(kind, view);
      view.link.hidden = false;
    },
    dispose() {
      observer.disconnect();
      for (const view of views.values()) {
        view.link.onclick = null;
        if (view.url) URL.revokeObjectURL(view.url);
      }
      views.clear();
    },
  };
}

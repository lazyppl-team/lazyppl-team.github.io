window.addEventListener('load', () => {
  const plot = document.querySelector('.plotly-graph-div');
  if (!plot || !window.Plotly || !plot.data) throw new Error('The graph could not be themed');
  const points = [], lines = [], surfaces = [];
  plot.data.forEach((trace, index) => {
    if (trace.type === 'surface') surfaces.push(index);
    else if (trace.mode === 'markers') points.push(index);
    else if (trace.mode === 'lines') lines.push(index);
  });
  const media = matchMedia('(prefers-color-scheme: dark)');
  let parentRoot;
  try { if (window.parent !== window) parentRoot = window.parent.document.documentElement; }
  catch { /* An external embedding page cannot expose its theme. */ }
  const apply = async () => {
    const dark = parentRoot ? parentRoot.classList.contains('dark') : media.matches;
    const ink = dark ? '#E8EEF6' : '#002147';
    const background = dark ? '#12233A' : '#EBF0F6';
    document.body.style.backgroundColor = background;
    const gradient = ['#002147', dark ? '#5CD5CE' : '#0B9A9C'];
    await Plotly.restyle(plot, { 'marker.color': ink }, points);
    await Plotly.restyle(plot, { 'line.color': ink }, lines);
    await Plotly.restyle(plot, { colorscale: [[[0, gradient[0]], [1, gradient[1]]]], opacity: dark ? 0.35 : 0.25 }, surfaces);
    await Plotly.relayout(plot, { paper_bgcolor: background, plot_bgcolor: background, 'scene.bgcolor': background,
      'modebar.color': ink, 'modebar.activecolor': dark ? '#5CD5CE' : '#002147' });
  };
  media.addEventListener('change', apply);
  if (parentRoot) new MutationObserver(apply).observe(parentRoot, { attributes: true, attributeFilter: ['class'] });
  void apply();
});

import { decode } from './protocol.js';
import { createPresentation } from './presentation.js';
import { rowsFromRecords } from './site-records.js';

function mountPlayground(root, workerUrl, plotter) {
let disposed = false;
const presentation = createPresentation(root, plotter);
let worker;
let busy = false;
const seed = root.querySelector('#seed');
const buttons = [...root.querySelectorAll('button[id$="-run"]')];
let lastKind;
const retryButtons = [...root.querySelectorAll('[data-retry]')];
const offerRetry = value => { for (const button of retryButtons) button.hidden = !value; };
const setBusy = value => {
  busy=value;
  root.dispatchEvent(new CustomEvent('playground-busy', { detail: value }));
  for (const button of [...buttons,...root.querySelectorAll('input,select,[data-original],[data-model]')]) button.disabled=value;
  for (const button of root.querySelectorAll('[data-cancel],#cancel')) button.disabled=!value;
};

function createWorker() {
  try {
    worker = new Worker(workerUrl,{type:'module'});
  } catch (error) {
    worker = undefined;
    const output = root.querySelector('section:not([hidden]) [role="status"]');
    output.textContent = `Could not run: ${String(error)}`;
    output.dataset.state = 'error';
    setBusy(false);
    offerRetry(true);
    return;
  }
  const currentWorker = worker;
  worker.onerror = event => {
    if (disposed || worker !== currentWorker) return;
    currentWorker.terminate();
    worker = undefined;
    finishError(event.message || 'Worker could not be started.');
  };
  worker.onmessage = ({data}) => {
    if (disposed || worker !== currentWorker) return;
    if (data.error) return finishError(data.error);
    try {
    const output = root.querySelector(`#${data.kind}-result`);
    decode(data.kind, { count: Number(output.dataset.count) }, data.records);
    const rows = rowsFromRecords(data.kind, data.records);
    root.querySelector(`#${data.kind}-data`).textContent=JSON.stringify(rows);
    presentation.show(data.kind, rows, data.records);
    const elapsed=data.records.find(row=>row.kind==='timing').elapsed_ms;
    if(data.kind==='regression') {
      const observations=data.records.filter(row=>row.kind==='observation').map(row=>[row.x,row.y]);
      const samples=data.records.filter(row=>row.kind==='sample');
      plotter.draw('regression',{rows,observations,samples});
      root.querySelector('#plot-description').textContent=`Seven observations, 1,000 posterior functions and a 95% posterior predictive interval. Observation noise ${output.dataset.noise}.`;
      output.textContent=`1,000 posterior functions, observation noise ${output.dataset.noise}.\n${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else if (['gp','wiener','piecewise'].includes(data.kind)) {
      const observations=data.records.filter(row=>row.kind==='observation').map(row=>[row.x,row.y]);
      const samples=data.records.filter(row=>row.kind==='sample');
      plotter.draw(data.kind,{rows,observations,samples});
      root.querySelector(`#${data.kind}-description`).textContent=`${rows.length} posterior functions and seven observations, on a grid from zero to six.`;
      output.textContent=`${rows.length} posterior functions at 31 positions.\n10,000 burn-in steps; one retained function per 1,000 steps.\n${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else if (data.kind==='clustering') {
      const clusterCount=new Set(rows.map(row=>row[3])).size;
      plotter.draw('clustering',{rows});
      root.querySelector('#clustering-description').textContent=`Ten observations assigned to ${clusterCount} ${clusterCount === 1 ? 'cluster' : 'clusters'}. ${clusterCount>8?'Clusters from eight onwards share Other.':'Clusters are numbered.'}`;
      output.textContent=`Highest-scoring assignment from ${Number(output.dataset.count).toLocaleString('en-GB')} inference steps.\n${clusterCount} ${clusterCount===1?'cluster':'clusters'}. ${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else if(['irm','ibp'].includes(data.kind)) {
      const labels=data.records.filter(row=>row.kind==='sample').map(row=>row.label);
      const columns=data.kind==='irm'?labels:Array.from({length:rows[0].length-1},(_,i)=>String(i+1));
      plotter.draw(data.kind,{rows,labels});
      root.querySelector(`#${data.kind}-description`).textContent=data.kind==='irm'?`${labels.join(', ')}. Rows and columns identify people; cell values are probabilities of sharing a group.`:`${labels.join(', ')}. Rows identify countries and columns identify features. A one indicates membership.`;
      output.textContent=data.kind==='irm'?`Pairwise group probabilities from ${Number(output.dataset.count).toLocaleString('en-GB')} inference steps.\n${Math.round(elapsed).toLocaleString('en-GB')} ms.`:`${columns.length} inferred features in the highest-scoring assignment from ${Number(output.dataset.count).toLocaleString('en-GB')} steps.\n${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else if(data.kind==='program') {
      const expressions=data.records.filter(row=>row.kind==='expression').map(row=>row.expression);
      const observations=data.records.filter(row=>row.kind==='observation').map(row=>[row.x,row.y]);
      plotter.draw('program',{rows,observations});
      root.querySelector('#program-description').textContent=`${expressions.length} inferred programs and seven observations.${expressions.length > 8 ? ' Programs from eight onwards share Other.' : ''}`;
      output.textContent=`${expressions.length} posterior ${expressions.length === 1 ? 'program' : 'programs'}, with 1,000 burn-in steps and 1,000 steps between programs.\n${Math.round(elapsed).toLocaleString('en-GB')} ms. Expressions (rounded constants):\n${expressions.map((text,i)=>`${i+1}. ${text}`).join('\n')}`;
    } else if(data.kind==='graph') {
      plotter.draw('graph',{rows});
      root.querySelector('#graph-description').textContent=`A sampled graph on ${rows.length} numbered vertices, in a circular layout.`;
      const {matches,theta}=data.records.find(row=>row.kind==='graph_stat');
      output.textContent=`Final draw after ${Number(output.dataset.count).toLocaleString('en-GB')} steps.\n${matches} of 105 vertex pairs match the observed graph.\nDistance parameter ${Number(theta).toFixed(3)} radians. ${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else if(data.kind==='mondrian') {
      plotter.draw('mondrian',{rows});
      root.querySelector('#mondrian-description').textContent=`A partition of the unit square into ${rows.length} rectangular blocks, with independently sampled values.`;
      output.textContent=`${rows.length} blocks at lifetime ${Number(output.dataset.count).toLocaleString('en-GB')}.\n${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    } else output.textContent=`10,000 samples in ${Math.round(elapsed).toLocaleString('en-GB')} ms.`;
    presentation.completePlot(data.kind);
    output.dataset.runMs=data.runMs;
    output.dataset.kernelMs=elapsed;
    output.dataset.finishedMs=performance.now();
    output.dataset.state='done';
    setBusy(false);
    } catch (error) { finishError(error.message ?? String(error)); }
  };
}
function finishError(message, retryable = true) {
  if (disposed) return;
  const outputs = [...root.querySelectorAll('pre[data-state="running"]')];
  if (!outputs.length) outputs.push(root.querySelector('section:not([hidden]) [role="status"]'));
  for(const output of outputs) {
    output.textContent=`Could not run: ${message}`;
    output.dataset.state='error';
  }
  setBusy(false);
  offerRetry(retryable);
}
function run(kind) {
  if(disposed || busy || !worker || !seed.reportValidity()) return;
  const noise=root.querySelector('#noise');
  if(kind==='regression' && !noise.reportValidity()) return;
  const count=root.querySelector(`#${kind}-count`);
  if(count && !count.reportValidity()) return;
  const output=root.querySelector(`#${kind}-result`);
  lastKind=kind;offerRetry(false);
  output.textContent='Running…';output.dataset.state='running';
  output.dataset.noise=noise.value;
  if(count) output.dataset.count=count.value;
  setBusy(true);
  const args=[kind,seed.value];
  if(kind==='regression') args.push(noise.value);
  else if(count) args.push(count.value);
  worker.postMessage({kind,args});
}

root.querySelector('#poisson-run').onclick=()=>run('poisson');
root.querySelector('#regression-run').onclick=()=>run('regression');
root.querySelector('#piecewise-run').onclick=()=>run('piecewise');
root.querySelector('#gp-run').onclick=()=>run('gp');
root.querySelector('#wiener-run').onclick=()=>run('wiener');
root.querySelector('#clustering-run').onclick=()=>run('clustering');
root.querySelector('#irm-run').onclick=()=>run('irm');
root.querySelector('#program-run').onclick=()=>run('program');
root.querySelector('#ibp-run').onclick=()=>run('ibp');
root.querySelector('#graph-run').onclick=()=>run('graph');
root.querySelector('#mondrian-run').onclick=()=>run('mondrian');
const cancelComputation=()=>{
  worker?.terminate();finishError('Computation cancelled.',false);createWorker();
};
// A worker that failed keeps failing (its WebAssembly download is awaited once), so Retry starts a new one and runs the last model again.
const retryComputation=()=>{
  worker?.terminate();offerRetry(false);createWorker();
  if(lastKind) run(lastKind);
};
for(const button of retryButtons) button.onclick=retryComputation;
for(const button of root.querySelectorAll('[data-cancel],#cancel')) button.onclick=cancelComputation;
createWorker();
setBusy(false);

return () => {
  disposed = true;
  if (worker) {
    worker.onmessage = null;
    worker.onerror = null;
    worker.terminate();
  }
  for (const button of root.querySelectorAll('button[id$="-run"],[data-cancel],#cancel,[data-retry]')) button.onclick = null;
  presentation.dispose();
  plotter.dispose();
};
}

globalThis.LazyPPLPlayground = { mount: mountPlayground };

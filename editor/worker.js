import {ConsoleStdout, File, OpenFile, PreopenDirectory, WASI} from '/wasi/index.js';
import {DyLDBrowserHost, main} from './dyld.mjs';

let run;
const send = (type, text) => postMessage({type, text});
self.onmessage = async ({data}) => {
  try {
    if (data.type === 'init') {
      const rootfs = new PreopenDirectory('/', []);
      // The in-memory WASI filesystem does not preserve archive timestamps.
      const extractor = new WASI(['bsdtar.wasm', '-x', '-m'], [], [
        new OpenFile(new File(new Uint8Array(data.archive), {readonly:true})),
        ConsoleStdout.lineBuffered(text => send('stdout', text)),
        ConsoleStdout.lineBuffered(text => send('stderr', text)), rootfs
      ], {debug:false});
      const result = await WebAssembly.instantiateStreaming(fetch('./bsdtar.wasm'),
        {wasi_snapshot_preview1:extractor.wasiImport});
      const exitCode = extractor.start(result.instance);
      if (exitCode !== 0) throw new Error(`Filesystem extraction exited ${exitCode}`);
      const dyld = await main({
        rpc:new DyLDBrowserHost({rootfs,
          stdout:text => send('stdout', text), stderr:text => send('stderr', text)}),
        searchDirs:data.runtime.searchDirs,
        mainSoPath:'/tmp/libplayground001.so',
        args:['libplayground001.so', '+RTS', '-c', '-RTS'], isIserv:false
      });
      run = await dyld.exportFuncs.myMain('/tmp/hslib/lib');
      send('ready', 'Ready');
    } else if (data.type === 'run') {
      const start = performance.now();
      try {
        if (!Array.isArray(data.args) || data.args.length > 1
            || !data.args.every(arg => typeof arg === 'string' && /^-?\d+$/.test(arg))) {
          throw new Error('Pass at most one integer seed as a program argument');
        }
        await run('', data.source, JSON.stringify(data.args));
        postMessage({type:'done', ok:true, elapsedMs:performance.now()-start});
      } catch (error) {
        postMessage({type:'done', ok:false, error:String(error), elapsedMs:performance.now()-start});
      }
    }
  } catch (error) { send('fatal', String(error)); }
};
postMessage({type:'booted'});

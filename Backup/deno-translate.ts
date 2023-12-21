import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"
import deapl from 'npm:deapl';
// import translate from 'https://cdn.skypack.dev/deapl/dist/index.js';

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {

    const data = JSON.parse(message);
    const sentence = data[1][0];
    const callback = data[1][1];
    const translation = await deapl.default(sentence, {
        sourceLanguage: 'en',
        targetLanguage: 'zh-CN',
    });
    bridge.evalInEmacs(`(${callback} "${translation}")`);
}

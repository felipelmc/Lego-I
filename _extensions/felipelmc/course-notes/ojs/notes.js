// Course Notes · utilitários para simulações em OJS.
//
//   import {scheme, palette, plotStyle, rng, mean, sd, corr, ols} from "/_extensions/felipelmc/course-notes/ojs/notes.js"
//
//   mode = scheme()          // "light" | "dark", reage ao botão de tema
//   pal  = palette(mode)     // cores do tema atual (lidas das variáveis --cn-*)
//   Plot.plot({style: plotStyle, ...})

// Gera "light"/"dark" agora e a cada troca de tema.
export async function* scheme() {
  let wake;
  const obs = new MutationObserver(() => wake && wake());
  obs.observe(document.body, { attributes: true, attributeFilter: ["class"] });
  try {
    let last;
    for (;;) {
      const s = document.body.classList.contains("quarto-dark") ? "dark" : "light";
      if (s !== last) yield (last = s);
      await new Promise((r) => (wake = r));
    }
  } finally {
    obs.disconnect();
  }
}

// Paleta do tema atual. O argumento só existe para criar dependência reativa em OJS.
export function palette(_mode) {
  const cs = getComputedStyle(document.body);
  const v = (n) => cs.getPropertyValue(`--cn-${n}`).trim();
  return {
    accent: v("accent"),
    link: v("link"),
    ink: v("ink"),
    ink2: v("ink-2"),
    ink3: v("ink-3"),
    line: v("line"),
    lineStrong: v("line-strong"),
    surface: v("surface"),
    bg: v("bg"),
    award: v("award"),
    danger: v("danger"),
    ramp: [1, 2, 3, 4, 5].map((i) => v(`d${i}`)),
  };
}

export const plotStyle = {
  fontFamily: "Geist Mono, ui-monospace, monospace",
  fontSize: "11px",
  background: "transparent",
  overflow: "visible",
};

// Gerador pseudoaleatório com semente (mulberry32) e sorteios comuns.
export function rng(seed = 42) {
  let a = seed >>> 0;
  const unif = () => {
    a |= 0; a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  let spare = null;
  const normal = (mu = 0, sigma = 1) => {
    if (spare !== null) { const s = spare; spare = null; return mu + sigma * s; }
    let u, v, s;
    do { u = 2 * unif() - 1; v = 2 * unif() - 1; s = u * u + v * v; } while (s >= 1 || s === 0);
    const m = Math.sqrt((-2 * Math.log(s)) / s);
    spare = v * m;
    return mu + sigma * u * m;
  };
  const bernoulli = (p) => (unif() < p ? 1 : 0);
  const exponential = (rate = 1) => -Math.log(1 - unif()) / rate;
  const int = (n) => Math.floor(unif() * n);
  return { unif, normal, bernoulli, exponential, int };
}

export const mean = (xs) => xs.reduce((s, x) => s + x, 0) / xs.length;
export const sd = (xs) => {
  const m = mean(xs);
  return Math.sqrt(xs.reduce((s, x) => s + (x - m) ** 2, 0) / (xs.length - 1));
};
export function corr(xs, ys) {
  const mx = mean(xs), my = mean(ys);
  let sxy = 0, sxx = 0, syy = 0;
  for (let i = 0; i < xs.length; i++) {
    const dx = xs[i] - mx, dy = ys[i] - my;
    sxy += dx * dy; sxx += dx * dx; syy += dy * dy;
  }
  return sxy / Math.sqrt(sxx * syy);
}

// MQO com intercepto. X: array de linhas (cada linha um array de preditores). Retorna os coeficientes.
export function ols(X, y) {
  const n = y.length, k = X[0].length + 1;
  const XtX = Array.from({ length: k }, () => new Array(k).fill(0));
  const Xty = new Array(k).fill(0);
  for (let i = 0; i < n; i++) {
    const row = [1, ...X[i]];
    for (let a = 0; a < k; a++) {
      Xty[a] += row[a] * y[i];
      for (let b = 0; b < k; b++) XtX[a][b] += row[a] * row[b];
    }
  }
  return solve(XtX, Xty);
}

function solve(A, b) {
  const n = b.length, M = A.map((r, i) => [...r, b[i]]);
  for (let c = 0; c < n; c++) {
    let p = c;
    for (let r = c + 1; r < n; r++) if (Math.abs(M[r][c]) > Math.abs(M[p][c])) p = r;
    [M[c], M[p]] = [M[p], M[c]];
    for (let r = 0; r < n; r++) {
      if (r === c) continue;
      const f = M[r][c] / M[c][c];
      for (let k = c; k <= n; k++) M[r][k] -= f * M[c][k];
    }
  }
  return M.map((r, i) => r[n] / r[i]);
}

// Formata números em pt-BR.
export const fmt = (x, d = 2) =>
  Number.isFinite(x) ? x.toLocaleString("pt-BR", { minimumFractionDigits: d, maximumFractionDigits: d }) : "–";

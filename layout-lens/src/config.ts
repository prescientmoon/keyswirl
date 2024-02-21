import {
  Arguments,
  Config,
  KeyboardKey,
  KeySymbol,
  Layout,
  PredefinedLayout,
  PredefinedLayoutName,
  SpecialSymbols,
  LayoutMeasurements,
  LayoutColorscheme,
  ChordConfig,
  ElementLayout,
} from "./types";
import split_3x5_2 from "./layouts/split_3x5_2";
import alpha_staggered_double_switch from "./layouts/alpha_staggered_double_switch";

const defaultMeasurements: LayoutMeasurements = {
  keySize: 60,
  keyPadding: 2,
  keyCornerRadius: 5,
  keyStrokeWidth: 1.5,
};

const defaultColorscheme: LayoutColorscheme = {
  keyFill: "#ffffff",
  keyStroke: "#000000",
  mainLayerColor: "black",
  tlLayerColor: "blue",
  trLayerColor: "red",
  blLayerColor: "purple",
};

const defaultElementLayout: ElementLayout = {
  mainToChordsGap: 10,
  imagePadding: 20,
  groupsPerRow: 2,
  groupPadding: 20,
};

function parseSymbol(s: string) {
  const special = SpecialSymbols[s];
  const isNumber = String(parseInt(s)) == s;
  if (isNumber || special === undefined) return s;
  return special as SpecialSymbols;
}

export function parseConfig(input: string): Config {
  const parsed = JSON.parse(input);

  const layout = PredefinedLayoutName[
    parsed.layout as string
  ] as PredefinedLayoutName;

  if (layout === undefined) {
    throw `Cannot find layout ${parsed.layout}`;
  }

  return {
    keys: (parsed.keys as string[][]).map((k) => k.map(parseSymbol)),
    chords: ((parsed.chords as ChordConfig[][]) || []).map((group) =>
      group.map((chord) => ({
        ...chord,
        input: chord.input.map((k) => parseSymbol(k as string)),
        output: parseSymbol(chord.output as string),
      })),
    ),
    colorscheme: { ...defaultColorscheme, ...parsed.colorscheme },
    measurements: { ...defaultMeasurements, ...parsed.measurements },
    elementLayout: {
      ...defaultElementLayout,
      ...parsed.elementLayout,
    },
    layout,
  };
}

function key(
  main: KeySymbol,
  tlLayer: KeySymbol = "",
  trLayer: KeySymbol = "",
  blLayer: KeySymbol = "",
): KeyboardKey {
  return { main, tlLayer, trLayer, blLayer };
}

const layouts: Record<PredefinedLayoutName, PredefinedLayout> = {
  [PredefinedLayoutName.split_3x5_2]: split_3x5_2,
  [PredefinedLayoutName.alpha_staggered_double_switch]:
    alpha_staggered_double_switch,
};

export function makeLayout(config: Config): Layout {
  const predefined = layouts[config.layout];
  return {
    keys: config.keys.map((k) => key(...(k as Arguments<typeof key>))),
    colorscheme: config.colorscheme,
    measurements: config.measurements,
    elementLayout: config.elementLayout,
    chords: config.chords,
    visual: predefined.visual,
    size: predefined.size,
  };
}

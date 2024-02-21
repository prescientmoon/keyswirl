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
} from "./types";
import split_3x5_2 from "./layouts/split_3x5_2";
import alpha_staggered_double_switch from "./layouts/alpha_staggered_double_switch";

const defaultMeasurements: LayoutMeasurements = {
  imagePadding: 20,
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

export function parseConfig(input: string): Config {
  const parsed = JSON.parse(input);

  const layout = PredefinedLayoutName[
    parsed.layout as string
  ] as PredefinedLayoutName;

  if (layout === undefined) {
    throw `Cannot find layout ${parsed.layout}`;
  }

  return {
    keys: (parsed.keys as string[][]).map((k) =>
      k.map((s) => {
        const special = SpecialSymbols[s];
        const isNumber = String(parseInt(s)) == s;
        if (isNumber || special === undefined) return s;
        return special as SpecialSymbols;
      }),
    ),
    colorscheme: { ...defaultColorscheme, ...parsed.colorscheme },
    measurements: { ...defaultMeasurements, ...parsed.measurements },
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
    visual: predefined.visual,
    size: predefined.size,
  };
}

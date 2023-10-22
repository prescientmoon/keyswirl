import {
  Arguments,
  Config,
  KeyboardKey,
  KeySymbol,
  Layout,
  PredefinedLayout,
  PredefinedLayoutName,
  SpecialSymbols,
} from "./types";
import split_3x5_2 from "./layouts/split_3x5_2";

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
        if (special === undefined) return s;
        return special as SpecialSymbols;
      }),
    ),
    colorscheme: parsed.colorscheme,
    imagePadding: parsed.imagePadding,
    keySize: parsed.keySize,
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
};

export function makeLayout(config: Config): Layout {
  const predefined = layouts[config.layout](config.keySize);
  return {
    keys: config.keys.map((k) => key(...(k as Arguments<typeof key>))),
    colorscheme: config.colorscheme,
    imagePadding: config.imagePadding,
    keySize: config.keySize,
    visual: predefined.visual,
    size: predefined.size,
  };
}

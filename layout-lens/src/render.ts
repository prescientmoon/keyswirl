import { tag, px } from "./svg";
import {
  KeyboardKey,
  KeySymbol,
  Layout,
  LayoutColorscheme,
  SpecialSymbols,
  VisualKey,
} from "./types";

function textContents(input: KeySymbol): string {
  if (input === SpecialSymbols.TL || input === SpecialSymbols.TR) return "■";

  return input;
}

function renderKey(
  visual: VisualKey,
  key: KeyboardKey,
  colorscheme: LayoutColorscheme,
  keySize: number,
) {
  const centerX = visual.position[0] + visual.size[0] / 2;
  const centerY = visual.position[1] + visual.size[1] / 2;
  const textAttribs = {
    "text-anchor": "middle",
    "dominant-baseline": "middle",
    "font-family": "Helvetica",
  };

  const textColor = (input: KeySymbol, _default: string): string => {
    if (input === SpecialSymbols.TL) return colorscheme.tlLayerColor;
    if (input === SpecialSymbols.TR) return colorscheme.trLayerColor;
    return _default;
  };

  return tag(
    "g",
    {
      transform:
        visual.angle && visual.angle !== 0
          ? `rotate(${visual.angle}, ${centerX}, ${centerY})`
          : undefined,
    },
    [
      tag("rect", {
        width: px(visual.size[0]),
        height: px(visual.size[1]),
        x: visual.position[0],
        y: visual.position[1],
        fill: colorscheme.keyFill,
        stroke: colorscheme.keyStroke,
        "stroke-width": px(2),
      }),
      tag(
        "text",
        {
          x: centerX,
          y: centerY,
          textLength: px(keySize / 2),
          fill: textColor(key.main, colorscheme.mainLayerColor),
          ...textAttribs,
        },
        textContents(key.main),
      ),
      tag(
        "text",
        {
          x: visual.position[0] + visual.size[0] / 6,
          y: visual.position[1] + visual.size[1] / 6,
          fill: textColor(key.tlLayer, colorscheme.tlLayerColor),
          "font-size": "66%",
          ...textAttribs,
        },
        textContents(key.tlLayer),
      ),
      tag(
        "text",
        {
          x: visual.position[0] + (9 * visual.size[0]) / 10,
          y: visual.position[1] + visual.size[1] / 6,
          fill: textColor(key.trLayer, colorscheme.trLayerColor),
          "font-size": "66%",
          ...textAttribs,
          "text-anchor": "end",
        },
        textContents(key.trLayer),
      ),
      tag(
        "text",
        {
          x: visual.position[0] + visual.size[0] / 10,
          y: visual.position[1] + (5 * visual.size[1]) / 6,
          fill: textColor(key.blLayer, colorscheme.blLayerColor),
          "font-size": "66%",
          ...textAttribs,
          "text-anchor": "start",
        },
        textContents(key.blLayer),
      ),
    ].join("\n"),
  );
}

export function renderLayout(layout: Layout) {
  return tag(
    "svg",
    {
      viewBox: [
        -layout.imagePadding,
        -layout.imagePadding,
        2 * layout.imagePadding + layout.size[0],
        2 * layout.imagePadding + layout.size[1],
      ].join(" "),
      xmlns: "http://www.w3.org/2000/svg",
      "xmlns:xlink": "http://www.w3.org/1999/xlink",
    },
    layout.visual
      .map((key, index) =>
        renderKey(key, layout.keys[index], layout.colorscheme, layout.keySize),
      )
      .join("\n"),
  );
}

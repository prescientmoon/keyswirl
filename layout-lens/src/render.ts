import { tag, px } from "./svg";
import * as L from "./layout";
import * as V from "./vec2";
import {
  KeyboardKey,
  KeySymbol,
  Layout,
  LayoutColorscheme,
  LayoutMeasurements,
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
  measurements: LayoutMeasurements,
) {
  const withPadding = {
    position: V.add(visual.position, measurements.keyPadding),
    size: V.add(visual.size, -2 * measurements.keyPadding),
  };

  const centerX = visual.position[0] + visual.size[0] / 2;
  const centerY = visual.position[1] + visual.size[1] / 2;
  const textAttribs = {
    "text-anchor": "middle",
    "dominant-baseline": "central",
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
          : "",
    },
    [
      tag("rect", {
        width: px(withPadding.size[0]),
        height: px(withPadding.size[1]),
        x: withPadding.position[0],
        y: withPadding.position[1],
        rx: measurements.keyCornerRadius,
        fill: colorscheme.keyFill,
        stroke: colorscheme.keyStroke,
        "stroke-width": px(measurements.keyStrokeWidth),
      }),
      tag(
        "text",
        {
          x: centerX,
          y: centerY,
          textLength: px(withPadding.size[1] / 2),
          fill: textColor(key.main, colorscheme.mainLayerColor),
          ...textAttribs,
        },
        textContents(key.main),
      ),
      tag(
        "text",
        {
          x: withPadding.position[0] + withPadding.size[0] / 6,
          y: withPadding.position[1] + withPadding.size[1] / 6,
          fill: textColor(key.tlLayer, colorscheme.tlLayerColor),
          "font-size": "66%",
          ...textAttribs,
          "text-anchor": "start",
        },
        textContents(key.tlLayer),
      ),
      tag(
        "text",
        {
          x: withPadding.position[0] + (9 * withPadding.size[0]) / 10,
          y: withPadding.position[1] + withPadding.size[1] / 6,
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
          x: withPadding.position[0] + withPadding.size[0] / 10,
          y: withPadding.position[1] + (5 * withPadding.size[1]) / 6,
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
        -layout.measurements.imagePadding,
        -layout.measurements.imagePadding,
        2 * layout.measurements.imagePadding +
          layout.size[0] * layout.measurements.keySize,
        2 * layout.measurements.imagePadding +
          layout.size[1] * layout.measurements.keySize,
      ].join(" "),
      xmlns: "http://www.w3.org/2000/svg",
      "xmlns:xlink": "http://www.w3.org/1999/xlink",
    },
    layout.visual
      .map((key, index) =>
        renderKey(
          L.scaleVisual(key, layout.measurements.keySize),
          layout.keys[index],
          layout.colorscheme,
          layout.measurements,
        ),
      )
      .join("\n"),
  );
}

import { children, tag, px } from "./svg";
import * as L from "./layout";
import * as V from "./vec2";
import {
  ChordConfig,
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

interface KeyRenderingFlags {
  stroke: boolean;
  text: boolean;
}

function applyKeyPadding(
  key: VisualKey,
  measurements: LayoutMeasurements,
): { position: V.Vec2; size: V.Vec2 } {
  return {
    position: V.add(key.position, measurements.keyPadding),
    size: V.add(key.size, -2 * measurements.keyPadding),
  };
}

function keyCenter(key: VisualKey): V.Vec2 {
  const centerX = key.position[0] + key.size[0] / 2;
  const centerY = key.position[1] + key.size[1] / 2;

  return [centerX, centerY];
}

const textAttribs = {
  "text-anchor": "middle",
  "dominant-baseline": "central",
  "font-family": "Helvetica",
};

function textColor(
  colorscheme: LayoutColorscheme,
  input: KeySymbol,
  _default: string,
): string {
  if (input === SpecialSymbols.TL) return colorscheme.tlLayerColor;
  if (input === SpecialSymbols.TR) return colorscheme.trLayerColor;
  return _default;
}

function renderKey(
  visual: VisualKey,
  key: KeyboardKey,
  colorscheme: LayoutColorscheme,
  measurements: LayoutMeasurements,
  flags: KeyRenderingFlags,
) {
  const withPadding = applyKeyPadding(visual, measurements);
  const center = keyCenter(visual);
  const keySize = measurements.keySize - 2 * measurements.keyPadding;

  const textOverlays = flags.text
    ? [
        tag(
          "text",
          {
            x: center[0],
            y: center[1],
            textLength: px(withPadding.size[1] / 2),
            fill: textColor(colorscheme, key.main, colorscheme.mainLayerColor),
            ...textAttribs,
          },
          textContents(key.main),
        ),
        tag(
          "text",
          {
            x: withPadding.position[0] + keySize / 10,
            y: withPadding.position[1] + keySize / 6,
            fill: textColor(colorscheme, key.tlLayer, colorscheme.tlLayerColor),
            "font-size": "66%",
            ...textAttribs,
            "text-anchor": "start",
          },
          textContents(key.tlLayer),
        ),
        tag(
          "text",
          {
            x: withPadding.position[0] + (9 * keySize) / 10,
            y: withPadding.position[1] + keySize / 6,
            fill: textColor(colorscheme, key.trLayer, colorscheme.trLayerColor),
            "font-size": "66%",
            ...textAttribs,
            "text-anchor": "end",
          },
          textContents(key.trLayer),
        ),
        tag(
          "text",
          {
            x: withPadding.position[0] + keySize / 10,
            y: withPadding.position[1] + (5 * keySize) / 6,
            fill: textColor(colorscheme, key.blLayer, colorscheme.blLayerColor),
            "font-size": "66%",
            ...textAttribs,
            "text-anchor": "start",
          },
          textContents(key.blLayer),
        ),
      ]
    : [];

  return tag(
    "g",
    {
      transform:
        visual.angle && visual.angle !== 0
          ? `rotate(${visual.angle}, ${center[0]}, ${center[1]})`
          : "",
    },
    children(
      tag("rect", {
        width: px(withPadding.size[0]),
        height: px(withPadding.size[1]),
        x: withPadding.position[0],
        y: withPadding.position[1],
        rx: measurements.keyCornerRadius,
        fill: colorscheme.keyFill,
        stroke: colorscheme.keyStroke,
        "stroke-width": px(flags.stroke ? measurements.keyStrokeWidth : 0),
      }),
      ...textOverlays,
    ),
  );
}

function keyCorners(
  key: VisualKey,
  measurements: LayoutMeasurements,
): V.Vec2[] {
  const withPadding = applyKeyPadding(key, measurements);
  return [
    withPadding.position,
    V.add(withPadding.position, [withPadding.size[0], 0]),
    V.add(withPadding.position, withPadding.size),
    V.add(withPadding.position, [0, withPadding.size[1]]),
  ];
}

function renderChordShape(
  first: VisualKey,
  second: VisualKey,
  chord: ChordConfig,
  measurements: LayoutMeasurements,
  colorscheme: LayoutColorscheme,
) {
  if (first.position[0] > second.position[0])
    return renderChordShape(second, first, chord, measurements, colorscheme);

  const multi = (...steps: string[]) => steps.join(" ");
  const moveTo = (to: V.Vec2) => `M ${to.join(" ")}`;
  const lineTo = (to: V.Vec2) => `L ${to.join(" ")}`;

  const firstCorners = keyCorners(first, measurements);
  const secondCorners = keyCorners(second, measurements);
  const firstCenter = keyCenter(first);
  const secondCenter = keyCenter(second);
  const middle = V.scale(V.add(firstCenter, secondCenter), 0.5);

  const halfPath = (b: V.Vec2, c: V.Vec2, d: V.Vec2) => {
    if ((b[1] - c[1]) * (b[0] - d[0]) > 0) return multi(lineTo(b), lineTo(d));
    else return multi(lineTo(c), lineTo(d));
  };

  const dottedIndicator = (key: VisualKey) => {
    const withPadding = applyKeyPadding(key, measurements);
    const center = keyCenter(key);
    const radius = Math.min(...withPadding.size) / 7.5;
    return tag("circle", {
      cx: center[0],
      cy: center[1],
      r: radius,
      fill: "gray",
      stroke: "gray",
      "fill-opacity": 0.3,
      "stroke-opacity": 0.7,
      "stroke-width": px(measurements.keyStrokeWidth),
      "stroke-dasharray": (radius * 2 * Math.PI) / 12,
    });
  };

  return tag(
    "g",
    {},
    children(
      tag("path", {
        fill: chord.fill,
        stroke: chord.stroke,
        "stroke-width": px(measurements.keyStrokeWidth),
        d: multi(
          moveTo(firstCorners[0]),
          halfPath(firstCorners[1], secondCorners[0], secondCorners[1]),
          lineTo(secondCorners[2]),
          halfPath(secondCorners[3], firstCorners[2], firstCorners[3]),
          "Z", // close path
        ),
      }),
      dottedIndicator(first),
      dottedIndicator(second),
      tag(
        "text",
        {
          x: middle[0],
          y: middle[1],
          "font-size": `${(chord.fontSizeModifier || 1) * 70}%`,
          fill: textColor(
            colorscheme,
            chord.output,
            colorscheme.mainLayerColor,
          ),
          ...textAttribs,
          "dominant-baseline": "middle",
        },
        textContents(chord.output),
      ),
    ),
  );
}

export function renderLayout(layout: Layout) {
  const totalWidth = layout.size[0] * layout.measurements.keySize;
  const chordKeyScalingFactor =
    (totalWidth / layout.elementLayout.groupsPerRow -
      layout.elementLayout.groupPadding * 2) /
    totalWidth;

  const chordRowCount = Math.ceil(
    layout.chords.length / layout.elementLayout.groupsPerRow,
  );

  const totalHeight =
    layout.elementLayout.mainToChordsGap +
    layout.measurements.keySize *
      layout.size[1] *
      (1 + chordRowCount / layout.elementLayout.groupsPerRow);

  const widthPerChord = totalWidth / layout.elementLayout.groupsPerRow;

  // {{{ Render main keys
  const mainKeys = layout.visual.map((key, index) =>
    renderKey(
      L.scaleVisual(key, layout.measurements.keySize),
      layout.keys[index],
      layout.colorscheme,
      layout.measurements,
      {
        text: true,
        stroke: true,
      },
    ),
  );
  // }}}
  // {{{ Render chord groups
  const chordMeasurements = L.scaleMeasurements(
    layout.measurements,
    chordKeyScalingFactor,
  );
  const chords = layout.chords.map((group, index) => {
    const normalKeys = layout.visual.map((key, index) => {
      const keyLabels = layout.keys[index];

      if (group.findIndex((c) => c.input.includes(keyLabels.main)) !== -1)
        return "";

      return renderKey(
        L.scaleVisual(key, chordMeasurements.keySize),
        keyLabels,
        { ...layout.colorscheme, keyFill: "gray" },
        chordMeasurements,
        {
          text: false,
          stroke: false,
        },
      );
    });

    const chordShapes = group.map((chord) => {
      return renderChordShape(
        L.scaleVisual(
          L.findKeyByLabel(layout, chord.input[0])!,
          chordMeasurements.keySize,
        ),
        L.scaleVisual(
          L.findKeyByLabel(layout, chord.input[1])!,
          chordMeasurements.keySize,
        ),
        chord,
        chordMeasurements,
        layout.colorscheme,
      );
    });

    return tag(
      "g",
      {
        transform: `translate(${
          (index % layout.elementLayout.groupsPerRow) * widthPerChord +
          layout.elementLayout.groupPadding +
          (index + layout.elementLayout.groupsPerRow > layout.chords.length
            ? ((layout.elementLayout.groupsPerRow -
                (layout.chords.length % layout.elementLayout.groupsPerRow)) *
                widthPerChord) /
              2
            : 0)
        } ${
          (Math.floor(index / layout.elementLayout.groupsPerRow) *
            layout.measurements.keySize *
            layout.size[1]) /
            layout.elementLayout.groupsPerRow +
          layout.elementLayout.groupPadding
        })`,
      },
      children(...normalKeys, ...chordShapes),
    );
  });
  // }}}
  // {{{ Put everything together
  return tag(
    "svg",
    {
      viewBox: [
        -layout.elementLayout.imagePadding,
        -layout.elementLayout.imagePadding,
        2 * layout.elementLayout.imagePadding + totalWidth,

        2 * layout.elementLayout.imagePadding + totalHeight,
      ].join(" "),
      xmlns: "http://www.w3.org/2000/svg",
      "xmlns:xlink": "http://www.w3.org/1999/xlink",
    },
    children(
      ...mainKeys,
      tag(
        "g",
        {
          transform: `translate(0 ${
            layout.size[1] * layout.measurements.keySize +
            layout.elementLayout.mainToChordsGap
          })`,
        },
        children(...chords),
      ),
    ),
  );
  // }}}
}

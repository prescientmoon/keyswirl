#!/usr/bin/env node
import * as fs from "fs";

type Vec2 = [number, number];

interface VisualKey {
  position: Vec2;
  size: Vec2;
  angle?: number;
}

type VisualLayout = VisualKey[];

interface KeyboardKey {
  main: string;
  tlLayer: string;
  trLayer: string;
  blLayer: string;
}

interface LayoutColorscheme {
  keyFill: string;
  keyStroke: string;
  mainLayerColor: string;
  tlLayerColor: string;
  trLayerColor: string;
  blLayerColor: string;
}

interface Layout {
  visual: VisualLayout;
  keys: KeyboardKey[];
  colorscheme: LayoutColorscheme;
  padding: number;
  size: Vec2;
}

function indent(amount: number, text: string) {
  return text
    .split("\n")
    .map((l) => " ".repeat(amount) + l)
    .join("\n");
}

function tag(
  name: string,
  attributes: Record<string, string | number | undefined>,
  children: string = ""
) {
  const attributeString = Object.entries(attributes)
    .map(([k, v]) => `${k}="${v}"`)
    .join(" ");

  const result = [
    `<${name}${attributeString === "" ? "" : ` ${attributeString}`}>`,
    indent(2, children),
    `</${name}>`,
  ]
    .filter((l) => l.trim() !== "")
    .join("\n");

  return result;
}

function px(value: number) {
  return `${value}px`;
}

function textContents(input: string): string {
  if (input === "TR" || input === "TL") return "■";
  return input;
}

function renderKey(
  visual: VisualKey,
  key: KeyboardKey,
  colorscheme: LayoutColorscheme
) {
  const centerX = visual.position[0] + visual.size[0] / 2;
  const centerY = visual.position[1] + visual.size[1] / 2;
  const textAttribs = {
    "text-anchor": "middle",
    "dominant-baseline": "middle",
    "font-family": "Helvetica",
  };

  const textColor = (input: string, _default: string): string => {
    if (input === "TL") return colorscheme.tlLayerColor;
    if (input === "TR") return colorscheme.trLayerColor;
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
        textContents(key.main)
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
        textContents(key.tlLayer)
      ),
      tag(
        "text",
        {
          x: visual.position[0] + (9 * visual.size[0]) / 10,
          y: visual.position[1] + visual.size[1] / 6,
          fill: textColor(key.trLayer, colorscheme.trLayerColor),
          "font-size": "66%",
          ...textAttribs,
          "text-anchor": "end"
        },
        textContents(key.trLayer)
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
        textContents(key.blLayer)
      ),
    ].join("\n")
  );
}

function renderLayout(layout: Layout) {
  return tag(
    "svg",
    {
      viewBox: [
        -layout.padding,
        -layout.padding,
        2 * layout.padding + layout.size[0],
        2 * layout.padding + layout.size[1],
      ].join(" "),
    },
    layout.visual
      .map((key, index) =>
        renderKey(key, layout.keys[index], layout.colorscheme)
      )
      .join("\n")
  );
}

const outPath = process.argv[2];

// ========== Layout generation
const keySize = 50;
const keySizeVec: Vec2 = [50, 50];
function visualKey(at: Vec2, angle: number = 0): VisualKey {
  return { position: at, size: keySizeVec, angle };
}

function add(x: Vec2, y: Vec2): Vec2 {
  return [x[0] + y[0], x[1] + y[1]];
}

function col(at: Vec2): VisualLayout {
  return [
    visualKey(at),
    visualKey(add(at, [0, keySize])),
    visualKey(add(at, [0, 2 * keySize])),
  ];
}

function radians(deg: number): number {
  return (deg / 180) * Math.PI;
}

function neg(v: Vec2): Vec2 {
  return [-v[0], -v[1]];
}

function thumbs(
  at: Vec2,
  reverse: boolean,
  thumbRotation = reverse ? -15 : 15
): VisualLayout {
  // Distance between thumb key centers
  const factor = keySize;
  const offset: Vec2 = [
    Math.cos(radians(thumbRotation)) * factor,
    Math.sin(radians(thumbRotation)) * factor,
  ];

  const result = [
    visualKey(at, thumbRotation),
    visualKey(add(at, reverse ? neg(offset) : offset), thumbRotation),
  ];

  if (reverse) result.reverse();

  return result;
}

function cols(at: Vec2, cols: Vec2[]): VisualLayout {
  return cols
    .map((self, index) => col(add(at, add(self, [index * keySize, 0]))))
    .flat();
}

function key(
  main: string,
  tlLayer = "",
  trLayer = "",
  blLayer = ""
): KeyboardKey {
  return { main, tlLayer, trLayer, blLayer };
}

const block: Vec2[] = [
  [0, keySize],
  [0, keySize / 2],
  [0, 0],
  [0, keySize / 2],
  [0, keySize],
];

const layout: Layout = {
  colorscheme: {
    keyFill: "#ffffff",
    keyStroke: "#000000",
    mainLayerColor: "black",
    tlLayerColor: "blue",
    trLayerColor: "red",
    blLayerColor: "purple",
  },
  visual: [
    cols([0, 0], block),
    thumbs([keySize * 3.5, keySize * 4.5], false),
    thumbs([keySize * 7.5, keySize * 4.5], true),
    cols([7 * keySize, 0], block),
  ].flat(),
  keys: [
    [
      key("Q", "!", "1", "f1"),
      key("A", "(", "6", "f6"),
      key("Z", ")", "", "f11"),
    ],
    [
      key("W", "@", "2", "f2"),
      key("S", "[", "7", "f7"),
      key("X", "]", "", "f12"),
    ],
    [key("E", "#", "3", "f3"), key("D", "{", "8", "f8"), key("C", "}")],
    [key("R", "$", "4", "f4"), key("F", "&lt", "9", "f9"), key("V", "&gt")],
    [key("T", "%", "5", "f5"), key("G", ";", "0", "f10"), key("B", "")],
    [key("TR", "", ""), key("␣", "", "")],
    [key("⇧", "", ""), key("TL", "", "")],
    [key("Y", "^", ""), key("H", "-", "◄", "😱"), key("N", "?", "")],
    [
      key("U", "&", "", "🔊"),
      key("J", "_", "▼", "🔉"),
      key("M", "/", "", "🔇"),
    ],
    [key("I", "*", "", "🔆"), key("K", "=", "▲", "🔅"), key(",", "\\", "")],
    [key("O", "~", ""), key("L", "+", "►"), key(".", "|", "")],
    [key("P", "`", "del"), key(":", "", ""), key('"', "'", "")],
  ].flat(),
  padding: 20,
  size: [keySize * 12, keySize * 6],
};

fs.writeFileSync(outPath, renderLayout(layout));

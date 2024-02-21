import * as L from "../layout";
import type { PredefinedLayout } from "../types";
import type { Vec2 } from "../vec2";

// 3x5 block
const block: Vec2[] = [
  [0, 1],
  [0, 0.5],
  [0, 0],
  [0, 0.5],
  [0, 1],
];

const layout: PredefinedLayout = {
  visual: [
    L.cols([0, 0], block),
    L.thumbs([3.5, 4.5], false),
    L.thumbs([7.5, 4.5], true),
    L.cols([7, 0], block),
  ].flat(),
  size: [12, 6],
};

export default layout;

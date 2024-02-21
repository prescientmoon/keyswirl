import * as L from "../layout";
import type { PredefinedLayout } from "../types";
import type { Vec2 } from "../vec2";

// 3x5 block
const block: Vec2[] = [
  [0, 0],
  [0, 0],
  [0, 0],
  [0, 0],
  [0, 0],
];

// per-column offsets
const offsets: Vec2[] = [
  [0, 0],
  [0.25, 0],
  [0.75, 0],
];

const layout: PredefinedLayout = {
  visual: [
    L.cols([0, 0], block, offsets),
    L.visualKey([1.75, 3.25]),
    L.visualKey([2.75, 3.25], [5, 1]),
    L.visualKey([7.75, 3.25]),
    L.cols([5, 0], block, offsets),
  ].flat(),
  size: [10.8, 4],
};

export default layout;

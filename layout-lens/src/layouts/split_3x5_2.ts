import * as L from "../layout";
import type { PredefinedLayout } from "../types";
import type { Vec2 } from "../vec2";

const layout: PredefinedLayout = (keySize) => {
  // 3x5 block
  const block: Vec2[] = [
    [0, keySize],
    [0, keySize / 2],
    [0, 0],
    [0, keySize / 2],
    [0, keySize],
  ];

  return {
    visual: [
      L.cols([0, 0], block, keySize),
      L.thumbs([keySize * 3.5, keySize * 4.5], false, keySize),
      L.thumbs([keySize * 7.5, keySize * 4.5], true, keySize),
      L.cols([7 * keySize, 0], block, keySize),
    ].flat(),
    size: [keySize * 12, keySize * 6],
  };
};

export default layout;

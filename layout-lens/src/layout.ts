import { VisualKey, VisualLayout } from "./types";
import * as V from "./vec2";

function visualKey(at: V.Vec2, keySize: number, angle: number = 0): VisualKey {
  return { position: at, size: [keySize, keySize], angle };
}

function col(at: V.Vec2, keySize: number): VisualLayout {
  return [
    visualKey(at, keySize),
    visualKey(V.add(at, [0, keySize]), keySize),
    visualKey(V.add(at, [0, 2 * keySize]), keySize),
  ];
}

function radians(deg: number): number {
  return (deg / 180) * Math.PI;
}

export function thumbs(
  at: V.Vec2,
  reverse: boolean,
  keySize: number,
  thumbRotation = reverse ? -15 : 15,
): VisualLayout {
  // Distance between thumb key centers
  const factor = keySize;

  const offset: V.Vec2 = [
    Math.cos(radians(thumbRotation)) * factor,
    Math.sin(radians(thumbRotation)) * factor,
  ];

  const result = [
    visualKey(at, keySize, thumbRotation),
    visualKey(
      V.add(at, reverse ? V.neg(offset) : offset),
      keySize,
      thumbRotation,
    ),
  ];

  if (reverse) result.reverse();

  return result;
}

export function cols(
  at: V.Vec2,
  cols: V.Vec2[],
  keySize: number,
): VisualLayout {
  return cols.flatMap((self, index) =>
    col(V.add(at, V.add(self, [index * keySize, 0])), keySize),
  );
}

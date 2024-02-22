import type {
  KeySymbol,
  Layout,
  LayoutMeasurements,
  VisualKey,
  VisualLayout,
} from "./types";
import * as V from "./vec2";

export function visualKey(
  position: V.Vec2,
  size: V.Vec2 = [1, 1],
  angle: number = 0,
): VisualKey {
  return { position, size, angle };
}

function offsetMany(keys: VisualLayout, offsets: V.Vec2[]): VisualLayout {
  return keys.map((key, index) => {
    const offset = offsets[index] || [0, 0];
    key.position = V.add(key.position, offset);
    return key;
  });
}

function col(at: V.Vec2): VisualLayout {
  return [
    visualKey(at),
    visualKey(V.add(at, [0, 1])),
    visualKey(V.add(at, [0, 2])),
  ];
}

function radians(deg: number): number {
  return (deg / 180) * Math.PI;
}

export function thumbs(
  at: V.Vec2,
  reverse: boolean,
  thumbRotation = reverse ? -15 : 15,
): VisualLayout {
  const offset: V.Vec2 = [
    Math.cos(radians(thumbRotation)),
    Math.sin(radians(thumbRotation)),
  ];

  const result = [
    visualKey(at, undefined, thumbRotation),
    visualKey(
      V.add(at, reverse ? V.neg(offset) : offset),
      undefined,
      thumbRotation,
    ),
  ];

  if (reverse) result.reverse();

  return result;
}

export function cols(
  at: V.Vec2,
  cols: V.Vec2[],
  offsets: V.Vec2[] = [],
): VisualLayout {
  return cols.flatMap((self, index) =>
    offsetMany(col(V.add(at, V.add(self, [index, 0]))), offsets),
  );
}

export function scaleVisual(visual: VisualKey, amount: number): VisualKey {
  return {
    angle: visual.angle,
    position: V.scale(visual.position, amount),
    size: V.scale(visual.size, amount),
  };
}

export function scaleMeasurements(
  measurements: LayoutMeasurements,
  amount: number,
): LayoutMeasurements {
  return {
    keySize: measurements.keySize * amount,
    keyPadding: measurements.keyPadding * amount,
    keyCornerRadius: measurements.keyCornerRadius * amount,
    keyStrokeWidth: measurements.keyStrokeWidth * amount,
  };
}

export function findKeyByLabel(
  layout: Layout,
  label: KeySymbol,
): VisualKey | null {
  for (let i = 0; i < layout.keys.length; i++) {
    if (layout.keys[i].main === label) return layout.visual[i];
  }

  return null;
}

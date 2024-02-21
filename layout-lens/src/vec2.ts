export type Vec2 = [number, number];

export function add(x: Vec2, y: number | Vec2): Vec2 {
  const z = typeof y === "number" ? [y, y] : y;
  return [x[0] + z[0], x[1] + z[1]];
}

export function neg(v: Vec2): Vec2 {
  return [-v[0], -v[1]];
}

export function scale(v: Vec2, amount: number): Vec2 {
  return [v[0] * amount, v[1] * amount];
}

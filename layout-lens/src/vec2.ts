export type Vec2 = [number, number];

export function add(x: Vec2, y: Vec2): Vec2 {
  return [x[0] + y[0], x[1] + y[1]];
}

export function neg(v: Vec2): Vec2 {
  return [-v[0], -v[1]];
}

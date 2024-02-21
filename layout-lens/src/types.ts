import type { Vec2 } from "./vec2";

/** Returns the arguments of a given function type  */
export type Arguments<T extends (...args: any) => any> = T extends (
  ...args: infer U
) => any
  ? U
  : never;

export interface VisualKey {
  position: Vec2;
  size: Vec2;
  angle?: number;
}

export type VisualLayout = VisualKey[];

export enum SpecialSymbols {
  TL,
  TR,
}

export type KeySymbol = SpecialSymbols | string;

export interface KeyboardKey {
  main: KeySymbol;
  tlLayer: KeySymbol;
  trLayer: KeySymbol;
  blLayer: KeySymbol;
}

export interface LayoutColorscheme {
  keyFill: string;
  keyStroke: string;
  mainLayerColor: string;
  tlLayerColor: string;
  trLayerColor: string;
  blLayerColor: string;
}

export enum PredefinedLayoutName {
  split_3x5_2,
  alpha_staggered_double_switch,
}

export interface LayoutMeasurements {
  imagePadding: number;
  keySize: number;
  keyPadding: number;
  keyCornerRadius: number;
  keyStrokeWidth: number;
}

export interface Config {
  keys: KeySymbol[][];
  layout: PredefinedLayoutName;
  colorscheme: LayoutColorscheme;
  measurements: LayoutMeasurements;
}

export type PredefinedLayout = {
  visual: VisualLayout;
  size: Vec2;
};

export interface Layout extends PredefinedLayout {
  keys: KeyboardKey[];
  colorscheme: LayoutColorscheme;
  measurements: LayoutMeasurements;
}

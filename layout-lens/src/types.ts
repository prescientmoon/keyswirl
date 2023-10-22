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

export interface Layout {
  visual: VisualLayout;
  keys: KeyboardKey[];
  colorscheme: LayoutColorscheme;
  imagePadding: number;
  size: Vec2;
  keySize: number;
}

export enum PredefinedLayoutName {
  split_3x5_2,
}

export interface Config {
  keys: KeySymbol[][];
  colorscheme: LayoutColorscheme;
  imagePadding: number;
  keySize: number;
  layout: PredefinedLayoutName;
}

export type PredefinedLayout = (keySize: number) => {
  visual: VisualLayout;
  size: Vec2;
};

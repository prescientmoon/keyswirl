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

export interface LayoutMeasurements {
  keySize: number;
  keyPadding: number;
  keyCornerRadius: number;
  keyStrokeWidth: number;
}

export enum PredefinedLayoutName {
  split_3x5_2,
  alpha_staggered_double_switch,
}

export type ChordName = string;

export interface ChordConfig {
  input: KeySymbol[];
  output: KeySymbol;
  fill: string;
  stroke?: string;
  fontSizeModifier?: number;
}

export interface ElementLayout {
  groupsPerRow: number;
  groupPadding: number;
  imagePadding: number;
  mainToChordsGap: number;
}

export interface Config {
  keys: KeySymbol[][];
  chords: ChordConfig[][];
  layout: PredefinedLayoutName;
  colorscheme: LayoutColorscheme;
  measurements: LayoutMeasurements;
  elementLayout: ElementLayout;
}

export type PredefinedLayout = {
  visual: VisualLayout;
  size: Vec2;
};

export interface Layout extends PredefinedLayout {
  colorscheme: LayoutColorscheme;
  measurements: LayoutMeasurements;
  elementLayout: ElementLayout;

  keys: KeyboardKey[];
  chords: ChordConfig[][];
}

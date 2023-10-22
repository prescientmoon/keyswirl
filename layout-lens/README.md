# Layout-lens

> NOTE: this project cannot yet render combos, which will change soon

This is a quickly-thrown-together set of scripts for generating SVG previews of keyboard layouts. For example configurations check out any config in the `keyboards` directory of this repository. To run this on your config simply do

```sh
nix run github:mateiadrielrafael/keyswirl#layout-lens my-config.json out.svg
```

## Technical details

The code isn't very well written (i.e.: no error handling, only contains the features I needed myself, etc). I'd rewrite this in a better language given the motivation, but the current version does the job just fine. If you want to contribute a layout preset, add it to [./src/layouts](./src/layouts) and then modify the enum in [./src/types.ts](./src/types.ts) to know about it's existence.

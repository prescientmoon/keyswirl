#!/usr/bin/env node
import * as fs from "fs";
import { renderLayout } from "./render";
import { makeLayout, parseConfig } from "./config";

const inPath = process.argv[2];
const outPath = process.argv[3];

const input = fs.readFileSync(inPath, "utf8");
const layout = makeLayout(parseConfig(input));

fs.writeFileSync(outPath, renderLayout(layout));

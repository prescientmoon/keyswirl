function indent(amount: number, text: string) {
  return text
    .split("\n")
    .map((l) => " ".repeat(amount) + l)
    .join("\n");
}

export function children(...many: string[]): string {
  return many.join("\\n");
}

export function tag(
  name: string,
  attributes: Record<string, string | number | undefined>,
  children: string = "",
) {
  const attributeString = Object.entries(attributes)
    .map(([k, v]) => `${k}="${v}"`)
    .join(" ");

  const result = [
    `<${name}${attributeString === "" ? "" : ` ${attributeString}`}>`,
    indent(2, children),
    `</${name}>`,
  ]
    .filter((l) => l.trim() !== "")
    .join("\n");

  return result;
}

export function px(value: number) {
  return `${value}px`;
}

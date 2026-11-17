const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "Getting Started",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        {
          type: "category",
          label: "Reference",
          collapsed: false,
          link: { type: "doc", id: "reference/index" },
          items: [
            "reference/json-encoder",
            "reference/json-decoder",
            "reference/json-codec",
            "reference/json-field-codecs",
            "reference/built-in-instances",
            "reference/json-codec-configuration",
            "reference/annotations",
            "reference/zio-json-macros",
            "reference/errors",
            "reference/json-ast",
            "reference/yaml"
          ]
        },
        {
          type: "category",
          label: "Tutorial",
          collapsed: false,
          items: ["tutorial/getting-started", "tutorial/model-an-api-payload"]
        },
        {
          type: "category",
          label: "How-to Guides",
          collapsed: false,
          link: { type: "doc", id: "how-to/index" },
          items: [
            "how-to/encoding",
            "how-to/decoding",
            "how-to/manual-instances",
            "how-to/third-party-types",
            "how-to/custom-map-keys",
            "how-to/low-level-codecs",
            "how-to/validate-on-decode",
            "how-to/field-names",
            "how-to/sealed-traits",
            "how-to/optional-and-default-fields",
            "how-to/scala-3-derivation",
            "how-to/generic-types",
            "how-to/edit-json-with-cursors",
            "how-to/load-yaml-config",
            "how-to/streaming",
            "how-to/golden-testing",
            "how-to/generate-case-classes"
          ]
        },
        {
          type: "category",
          label: "Explanation",
          collapsed: false,
          items: [
            "explanation/how-it-works",
            "explanation/scala-2-vs-scala-3",
            "explanation/decoder-strictness",
            "explanation/wire-format",
            "explanation/performance",
            "explanation/security"
          ]
        },
        {
          type: "category",
          label: "Interop",
          collapsed: true,
          link: { type: "doc", id: "interop/index" },
          items: [
            "interop/enumeratum",
            "interop/http4s",
            "interop/refined",
            "interop/scalaz-7x"
          ]
        }
      ]
    }
  ]
};

module.exports = sidebars;

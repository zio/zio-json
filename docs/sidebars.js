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
            "reference/json-codec-configuration",
            "reference/errors",
            "reference/annotations",
            "reference/json-ast",
            "reference/yaml"
          ]
        },
        {
          type: "category",
          label: "Tutorial",
          collapsed: false,
          items: ["tutorial/getting-started"]
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
            "how-to/validate-on-decode",
            "how-to/field-names",
            "how-to/sealed-traits",
            "how-to/optional-and-default-fields",
            "how-to/streaming",
            "how-to/golden-testing"
          ]
        },
        {
          type: "category",
          label: "Explanation",
          collapsed: false,
          items: [
            "explanation/how-it-works",
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

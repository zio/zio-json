const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "ZIO JSON",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "getting-started",
        "decoding",
        "encoding",
        "configuration",
        "manual-instances",
        "performance",
        "security",
        {
          type: "category",
          label: "Interop",
          link: { type: "doc", id: "interop/index" },
          collapsed: true,
          items: [
            "interop/akka-http",
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

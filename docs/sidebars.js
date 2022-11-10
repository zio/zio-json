const sidebars = {
  sidebar: [
    "index",
    "decoding",
    "encoding",
    {
      type: "category",
      label: "Interop",
      link: { type: "doc", id: "interop/index" },
      collapsed: true,
      items: [
        "interop/akka-http",
        "interop/http4s",
        "interop/refined",
        "interop/scalaz-7x"
      ]
    }
  ]
};

module.exports = sidebars;

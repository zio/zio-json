# ZIO JSON brand assets

Exported from the ZIO JSON brand sheet. All text is converted to outlines (Sora and JetBrains Mono), so no fonts are needed to open the SVGs.

## Which file to use

| Need | File |
| --- | --- |
| Logo on a light page (README, docs header) | `svg/zio-json-lockup-light.svg` |
| Logo on a dark page | `svg/zio-json-lockup-dark.svg` |
| Square logo, app icon | `svg/zio-json-symbol.svg` (violet tile) |
| Square logo on violet or busy grounds | `svg/zio-json-symbol-dark.svg` (ink tile) |
| Avatar (GitHub, Slack, X, Discord) | `png/zio-json-avatar-1024.png` (full-bleed; sites crop it) |
| The bare mark, no tile | `svg/zio-json-mark-light.svg`, `svg/zio-json-mark-dark.svg` |
| One-colour print, stamps, engraving | `svg/zio-json-mark-mono-ink.svg`, `-mono-white.svg`, `-mono-violet.svg` |
| README banner | `png/readme-banner-light.png`, `png/readme-banner-dark.png` |
| Link previews | `png/social-og-*.png` (1200 × 630), `png/social-x-*.png` (1200 × 600), `png/social-github-*.png` (1280 × 640) |

Rules from the sheet: never set the tile beside a full "ZIO JSON" (the lockup already has the Z), keep a quarter of the tile's width clear around it, use the small cut below 24 px, and don't recolour, stretch, rotate or add effects.

## Favicon set

`favicon/favicon.svg` is the hand-drawn 16 px small cut. `favicon/favicon.ico` holds 16 (the small cut), 32 and 48.

```html
<link rel="icon" href="/favicon.ico" sizes="16x16 32x32 48x48">
<link rel="icon" href="/favicon.svg" type="image/svg+xml">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">
```

For a web app manifest, use `icon-192.png` and `icon-512.png` (purpose `any`) and `icon-maskable-512.png` (purpose `maskable`).

## README banner that follows the viewer's theme

```html
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="brand/png/readme-banner-dark.png">
  <img alt="ZIO JSON: Fast and secure JSON for ZIO" src="brand/png/readme-banner-light.png" width="600">
</picture>
```

## Colours

| Name | Hex | Use |
| --- | --- | --- |
| Violet | `#6B4FD8` | The tile, values in the mark, links, primary buttons |
| Lilac | `#C4B9EF` | Values on the violet tile only |
| Violet 400 | `#9784E4` | Violet on dark grounds |
| Ink | `#101521` | Text, keys in the lockup, dark surfaces |
| Snow | `#F4F5F8` | Panels on light, text on dark |

The taglines and card copy are proposals for the maintainers to confirm.

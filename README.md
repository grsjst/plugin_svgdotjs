# A SWISH Plugin for SVG.js (plugin_svgdotjs)

Plugin SVG.js is a SWISH plugin (https://github.com/SWI-Prolog/SWISH) to render SVG using SVG.js (https://svgjs.com/) 

## Installation

These instructions assume:
- You have installed SWISH (further referred to as `$SWISH_DIR`)
- A \*nix like OS (but it may work for others)

Download the plugin to your preferred location (further referred to as `$PLUGIN_SVGDOTJS`)

```bash
git pull https://github.com/grsjst/plugin_svgdotjs.git
```

Download the svgdot.js libraries and plugins to ./js/node_modules (using `yarn` it will directly download in the correct dir)

```bash
yarn add @svgdotjs/svg.js @svgdotjs/svg.panzoom.js @svgdotjs/svg.filter.js 
```

Create a symbolic link from `$PLUGIN_SVGDOTJS/plugin_svgdotjs.pl` to `$SWISH_DIR/config-available`
```bash
cd $PLUGIN_SVGDOTJS
ln -s ./plugin_svgdotjs  $SWISH_DIR/config-available/.
```

Enable the plugin in SWISH by making the plugin available in `config-enabled` (create if if it doesn't exist)

```bash
cd $SWISH_DIR/config-enabled
ln -s ../config-available/plugin_svgdotjs .
```

Then relaunch SWISH (or type `make` in swipl console)

## Usage

In your SWISH programme add the directive: 

```swipl
:- use_rendering(svgdotjs).   % creates a default canvas 300x300 (see Options for more details)
```

## Examples

Some examples are available in `$PLUGIN_SVGDOTJS/examples` that you need to copy to your SWISH programme


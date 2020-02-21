# A SWISH Plugin for SVG.js (plugin_svgdotjs)

Plugin SVG.js is a SWISH plugin (https://github.com/SWI-Prolog/SWISH) to render SVG using SVG.js (https://svgjs.com/) 

## Installation

These instructions assume:
- You have installed SWISH (further referred to as `$SWISH_DIR`)
- A \*nix like OS (but it may work for others)

Download the plugin to your preferred location (further referred to as `$PLUGIN_SVGDOTJS`)

```bash
git clone https://github.com/grsjst/plugin_svgdotjs.git
```

Download the svgdot.js libraries and plugins to ./js/node_modules (using `yarn` it will directly download in the correct dir)

```bash
cd $PLUGIN_SVGDOTJS
yarn add @svgdotjs/svg.js @svgdotjs/svg.panzoom.js @svgdotjs/svg.filter.js 
```

Edit `$PLUGIN_SVGDOTJS/plugin_svgdotjs.pl` and edit the following line to set `$PLUGIN_SVGDOTJS` 


```swipl
% $PLUGIN_SVGDOTJS should be set to the root of PLUGIN_SVGDOTJS
user:file_search_path(plugin_svgdotjs, $PLUGIN_SVGDOTJS)
```

Create a symbolic link (or copy) from `$PLUGIN_SVGDOTJS/plugin_svgdotjs.pl` to `$SWISH_DIR/config-available`
```bash
cd $SWISH_DIR/config-enabled
ln -s $PLUGIN_SVGDOTJS/plugin_svgdotjs.pl .
```

Enable the plugin in SWISH by making the plugin available in `config-enabled` (create if it doesn't exist)

```bash
cd $SWISH_DIR/config-enabled
ln -s ../config-available/plugin_svgdotjs.pl .
```

Then relaunch SWISH (or type `make` in swipl console)

## Usage

In your SWISH programme add the directive: 

```swipl
:- use_rendering(svgdotjs).   % creates a default canvas 300x300 (see Options for more details)
```

As an example, run the following query to render a rectangle:

```swipl
X = [rect{x:10,y:10,width:100,height:100}].
```

The PLUGIN_SVGDOTJS renderer accepts a List of Dicts representing basic SVG shapes. Currently supported are:
- group,
- rect,
- circle,
- ellipse,
- line,
- polyline,
- polygon,
- path,
- text,
- textPath,
- image

See the SVG reference for the attributes supported (see https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Basic_Shapes)

The `d` attribute (path and textPath) used to denote a PathString (e.g. `"M20,230 Q40,205 50,230 T90,230"`) also accepts a more Prolog friendly format (e.g `['M'20,230,'Q',40,205,50,230,'T',90,230]`)

Similarily for the `transform` attribute (see https://svgjs.com/docs/3.0/manipulating/#transforming) 

## Examples

Some basic examples are available in `$PLUGIN_SVGDOTJS/examples` that you need to copy to your SWISH programme. `$PLUGIN_SVGDOTJS/util` contains a utility to convert an SVG file to a Prolog friendly version.

## Files

```
plugin_svgdotjs.pl - loads the svgdotjs renderer in SWISH`
render/svgdotjs.pl - the SVGDOTJS renderer
web/js/plugin_svgdotjs/plugin_svg.js - sets requirejs paths, and adds interaction elements (input boxes) to the SWISH results pane
web/js/plugin_svgdotjs/canvas_svg.js - object that is returned to SVGDOTJS renderer
```


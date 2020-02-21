requirejs.config({
    urlArgs: "ts="+new Date().getTime(),  /* prevent caching during development */
    baseUrl: '/js/plugin_svgdotjs/',
    paths: {
        svgjs: '../../node_modules/@svgdotjs/svg.js/dist/svg',
        svgjs_filter: '../../node_modules/@svgdotjs/svg.filter.js/dist/svg.filter',
        svgjs_panzoom: '../../node_modules/@svgdotjs/svg.panzoom.js/dist/svg.panzoom'
    },
    shim:
    { 
        svgjs:
        { deps:["jquery"],
            init: function ($) {
            return SVG;
            }
        },
        svgjs_filter:
        { deps:["svgjs"],
            init: function (SVG) {
                return SVG;
            }
        },
        svgjs_panzoom:
        { deps:["svgjs"],
            init: function (SVG) {
                return SVG;
            }
        },
    }
});

define(

    //The array of dependencies
    ["jquery","canvas_svg","laconic"],

    //The function to execute when all dependencies have loaded. The
    //arguments to this function are the array of dependencies mentioned
    //above.
    function ($,r_canvas) {
        //console.log("from svgjs_plugin.js");

        var canvas_factory = {

            create: function(element, options) {
                var id = $(element).attr("id");
                console.log(`Create canvas #${id}`);
                console.log(options);

                var x = options.x != undefined ? options.x : 0;
                var y = options.y != undefined ? options.y : 0;
                var width = options.width != undefined ? options.width : 300;
                var height = options.height != undefined ? options.height : 300;
                var frame = options.frame != undefined ? options.x : 0;
                var transform = options.transform != undefined ? options.transform : "";
                var n_shapes = options.n_shapes != undefined ? options.n_shapes : 0;

                var r_canvas_obj = new r_canvas(element,x,y,width,height,transform,n_shapes); 
                r_canvas_obj.create_frame();
                r_canvas_obj.apply_transform(transform);
                
                var div_controls = $.el.div({class : "canvas-controls"});
                this.create_panzoom_control(div_controls, r_canvas_obj);
                this.create_tooltip_control(div_controls, r_canvas_obj);

                $(element).append(div_controls);
                $(element).data("r_canvas",r_canvas_obj);

                return r_canvas_obj;
            },

            create_tooltip_control(div_controls, r_canvas) {
                var enable_tooltip = $.el.input({id:'tooltip', type:'checkbox'});
                var label_tooltip = $.el.label({for:'tooltip'},"Tooltip");
                var span_tooltip = $.el.span({class : "enable-tooltip"}, enable_tooltip, label_tooltip);
                $(div_controls).append(span_tooltip);
                
                enable_tooltip.addEventListener('change', function(e) {
                    if(e.target.checked) {
                        r_canvas.rshapes.each(function(i,children) {r_canvas.enable_tooltip(r_canvas,this)});
                    } else {
                        r_canvas.rshapes.each(function(i,children) {r_canvas.disable_tooltip(r_canvas,this)});
                    }
                });
            },

            create_panzoom_control(div_controls, r_canvas) {
                 var panZoomOpts = {zoomEnabled: true, controlIconsEnabled: true, fit: true, center: true, maxZoom: 5};
                 var enable_panzoom = $.el.input({id:'panzoom', type:'checkbox'});
                 var label_panzoom =$.el.label({for:'panzoom'},"Pan/Zoom");
                 var span_panzoom = $.el.span({class : "enable-panzoom"}, enable_panzoom, label_panzoom);
                 $(div_controls).append(span_panzoom);

                 $(enable_panzoom).data("r_canvas",r_canvas);
        
                 enable_panzoom.addEventListener('change', function(e) {
                    if(e.target.checked) {
                        r_canvas.enable_panzoom();
                    } else {
                        r_canvas.disable_panzoom();
                    }
                });
            }
        };
        return canvas_factory;
    }
);

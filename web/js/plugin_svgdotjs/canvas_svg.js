define(

    //The array of dependencies
    ["jquery", "svgjs_filter","svgjs_panzoom"],

    //The function to execute when all dependencies have loaded. The
    //arguments to this function are the array of dependencies mentioned
    //above.
    function ($, SVG) {
        //console.log("from svgjs_canvas.js");

        var r_canvas_constructor = function(element,x,y,width,height,transform,n_shapes) {
            //console.log("from r_canvas_constructor");

            var paper = SVG().addTo(element).size("100%",height);
            var id = $(element).attr("id");
            var rshapes = paper.group();
            
            var r_canvas = {
            "id": id,
            "x" : x,
            "y" : y,
            "width" : width,
            "height" : height,
            "transform" : transform,
            "paper" : paper,
            "rshapes" : rshapes,
            "n_created" : 0,
            "n_shapes" : n_shapes,

            create_frame() {
                frame = this.paper.rect(this.width,this.height).attr({x:this.x,y:this.y,stroke: "SlateGray", fill:"WhiteSmoke"}).back();

                frame.filterWith(function(add) {
                  var blur = add.offset(5, 5).in(add.$sourceAlpha).gaussianBlur(5)
                  add.blend(add.$source, blur);
                  this.size('200%','200%').move('-50%', '-50%');
                });

                this.frame = frame;
            },

            add_shapes: function(shapes) {
                var r_shapes = [];
                shapes.forEach(shape => {
                    var r_shape = this.create_rshape(shape);
                    r_shapes.push(r_shape);
                    this.rshapes.add(r_shape);
                });
                return r_shapes;
            },

            create_rshape : function(shape) {                
                var rshape;
                var t = shape.transform; // may be undefined

                // process attributes (if needed)
                if(typeof t == 'object') {
                    t = shape.transform;    // t is a tranfrom spec as used by svg.js
                    delete shape.transform; // remove as the classic svg transform attr is expected to be a string     
                };

                // create element - for most shape types an skeleton element is created that is instantiated by setting its attributes
                // however, group, text and textpath require a special approach
                var f_create = this.paper[shape.type];
                if(shape.type == "group") {
                    rshape = this.paper.group();
                    var children_rshapes = this.add_shapes(shape.children);
                    children_rshapes.forEach(child => rshape.add(child));
                    rshape.attr(shape);
                } else if(shape.type == "text") { 
                    rshape = this.paper.text(shape.text).attr(shape);    // text contents cannot be set using attr
                } else if(shape.type == "textPath") {
                    rshape = this.paper.textPath(shape.text, shape.d).parent();   // textPath is child of enclosing text element
                    rshape.attr(shape); // add attributes (inherited by textPath)
                } else if(f_create != undefined) {
                    rshape = f_create.apply(this.paper, [shape]);
                } else {
                    console.log(`${shape.type} is not a known shape`);
                }

                if(t != undefined) {
                    rshape = rshape.transform(t);
                }
                this.n_created += 1;
                console.log(`Created ${(shape.id != undefined ? shape.id : "")} (${shape.type}) #${this.n_created}/${this.n_shapes} shapes`);

                return rshape;
            },

            apply_viewbox : function() {
                this.paper.viewbox(this.x-5, this.y-5, this.width+10, this.height+10, false);
                $(this.paper.node).attr("preserveAspectRatio","xMidYMid");
            },

            apply_transform : function(transform) {
                this.rshapes.transform(transform);
            },

            test_mouse : function(rshape) {
                var point = this.paper.circle(5);
                rshape.mousemove(function(e) {
                    var p = point.point(e.clientX, e.clientY);
                    point.attr({cx:p.x, cy:p.y,"pointer-events":"none"});
                });
            },

            enable_panzoom : function() {
                //console.log("enable panzoom");
                var options = {zoomMin: 0.5, zoomMax: 5};
                this.paper.panZoom(options);
            },

            disable_panzoom : function() {
                //console.log("disable panzoom");
                this.paper.panZoom(false);
                this.apply_viewbox();
            },

            enable_tooltip : function(r_canvas, rshape) {
                //console.log(`add tip for ${rshape} ${r_canvas.id}`);
                var glow = rshape.clone();
                r_canvas.paper.add(glow);
                var sw = rshape.attr("stroke-width") + 2;
                glow.attr({opacity:0,"stroke-width":sw});
                glow.front();
                glow.transform(r_canvas.transform);
                $(rshape).data("glow",glow); // to be able to remove the handler

                var msg = rshape.attr("tooltip");
                var txt = r_canvas.paper.text(msg).attr({fill:"##333333", stroke:"#333333"});
                var bb = txt.bbox();
                var r = r_canvas.paper.rect(bb.width + 6,bb.height + 6,5).attr({x:bb.x - 3,y:bb.y - 3,fill:"#fff",stroke:"#333333"});
                var tip = paper.group();
                tip.attr({"pointer-events":"none"});
                tip.add(r);
                tip.add(txt);
                tip.hide()

                var frame = r_canvas.frame;

                fin = function(e) {
                    var glow = this;
                    var p = frame.point(e.clientX, e.clientY);
                    tip.move(p.x,p.y);
                    glow.attr({opacity:0.7});
                    tip.show();
                };

                fout = function(e) {
                    var glow = this;
                    tip.hide();
                    glow.attr({opacity:0});
                }
                glow.mouseenter(fin);
                glow.mouseleave(fout);
            },

            disable_tooltip : function(r_canvas,rshape) {
                //console.log(`remove tip for ${rshape}`);
                var glow = $(rshape).data("glow");
                glow.mouseenter(null);
                glow.mouseleave(null);
            }
          
        };   // r_canvas   

        r_canvas.apply_viewbox();
        return r_canvas;

        }; // r_canvas_constructor

        return r_canvas_constructor;
    }
);
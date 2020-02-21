:- module(swish_svgdotjs,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/json)).
:- use_module(library(http/js_write)).
:- use_module(swish(lib/render)).
:- use_module(library(dcg/basics)).

:- use_module(library(debug)).


:- debug(svgdotjs).
:- register_renderer(svgdotjs, "Render shapes in SVG using SVG.js").

:- debug(svgdotjs,"SVG.js renderer loaded",[]).

/** <module> SVG.js shapes renderer

Renders shapes (rect, cirlce, path, ...) using SVG.js

*/

%%	term_rendering(+Shapes:list(dict), +Vars, +Options)//
%
%	Renders the spefied Shapes in SVG.js (see <https://svgjs.com/>).  
% 	Options processed: 
% 		- x(int), and y(int) sets the top-left x-coordinate of the canvas
% 		- width(int), hand eight(int) set the extents of the canvas,
% 		- transform(dict) transforms all shapes on the canvas (this can be used to transform domain values to a "visible" range)
% 	Shapes is a list of Dicts with a Tag that corresponds to a SVG Shape
%	supported: group,rect,circle,ellipse,line,polyline,path,text,textPath,image
%
term_rendering(Shapes, _Vars, Options) -->
	{ 
		%debug(svgdotjs,"try svgdotjs renderer",[]),
		Types = [group,rect,circle,ellipse,line,polyline,polygon,path,text,textPath,image],
		DefaultOptions = _{x:0,y:0,width:300, height:300, transform:""},
		is_list(Shapes),
		forall(member(Shape,Shapes),(is_dict(Shape,Type),member(Type,Types))),!,
		debug(svgdotjs,"use svgdotjs renderer",[]),
		to_svgdotjs(Shapes,ShapeSpecs),
		debug(svgdotjs,"specs:~p",[ShapeSpecs]),
		dict_options(DictOptions,Options),
		NOptions = DefaultOptions.put(DictOptions).put(n_shapes,N),
		gensym(canvas,Id), % for debugging
		count_shapes(ShapeSpecs,N),
		debug(svgdotjs,"creating  ~w, containing #~w shapes, options: ~w",[Id,N,NOptions])
	},
		html(div([ class('render-svg-bbox'),
		   'data-render'('Shapes using SVG.js')
		 ],
		 [ div([id=Id],[]),
		   \js_script({|javascript(ShapeSpecs,NOptions)||
(function() {
  if ( $.ajaxScript ) {
  	//console.log("from svgdotjs.pl");
  	
    var div = $.ajaxScript.parent().find("div")[0];
    require(['plugin_svgdotjs/plugin_svg'], function(canvas_factory) {
    	var canvas = canvas_factory.create(div,NOptions);
    	canvas.add_shapes(ShapeSpecs);
	    });
  }
})();
		  |})
		 ])).

to_svgdotjs([],[]).
to_svgdotjs([Shape|Shapes],[ShapeSpecPost|ShapeSpecs]) :-
	pre_process_shape(Shape,ShapePre),
	process_shape(ShapePre,ShapeSpec),
	post_process_shape(ShapeSpec,ShapeSpecPost),
	to_svgdotjs(Shapes,ShapeSpecs).

process_shape(Shape,Spec) :-
	is_dict(Shape,Type),
	Type = group,!,
	to_svgdotjs(Shape.children,ChildrenSpecs),
	Spec = Shape.put(_{type:Type, children:ChildrenSpecs}).

process_shape(Shape,Shape).

pre_process_shape(Shape,NShape) :-
	is_dict(Shape,Type),
	Default = Type{tooltip:Msg},
	shape_message(Shape,Msg),
	NShape = Default.put(Shape).

post_process_shape(Shape,NShape) :-
	Default = _{type:Type},
	is_dict(Shape,Type),
	NShape = Shape.put(Default).

count_shapes([],0).
count_shapes([Shape|Shapes],N) :-
	is_dict(Shape,Type),
	Type = group,!,
	count_shapes(Shape.children,L),
	count_shapes(Shapes,M),
	N is L + M + 1.

count_shapes([_Shape|Shapes],N) :-
	count_shapes(Shapes,M),
	N is M + 1.

shape_message(Shape,Mesg) :-
	phrase(shape_msg(Shape),Codes),
	string_codes(Mesg,Codes).

shape_msg(Shape) --> id(Shape),msg_with_title(Shape),!.
shape_msg(Shape) --> id(Shape),msg(Shape),!.
id(Shape) --> {Id = Shape.get(id),format(string(StrId),"~w : ",[Id])},!,StrId.
id(_) --> "".
msg_with_title(Shape) --> {Title = Shape.get(title)},msg(Shape)," - ",Title.
msg(Group) --> {is_dict(Group,group),length(Group.children,N), format(string(AttrStr),"group(#~w children)",[N])},AttrStr.
msg(Rect) --> {is_dict(Rect,rect)},"rect","(", xy_coord(Rect.x,Rect.y),",",attrs([Rect.width,Rect.height]), ")".
msg(Circle) --> {is_dict(Circle,circle)},"circle","(", xy_coord(Circle.cx,Circle.cy),",",attrs([Circle.r]), ")".
msg(Ellipse) --> {is_dict(Ellipse,ellipse)},"ellipse","(", xy_coord(Ellipse.cx,Ellipse.cy),",",attrs([Ellipse.rx,Ellipse.ry]), ")".
msg(Line) --> {is_dict(Line,line)},"line","(", "[",xy_coords([Line.x1,Line.y1,Line.x2,Line.y2]),")".
msg(PolyLine) --> {is_dict(PolyLine,polyline)},"polyline","(", xy_coords(PolyLine.points),")".
msg(Polygon) --> {is_dict(Polygon,polygon)},"polygon","(", xy_coords(Polygon.points),")".
msg(Path) --> {is_dict(Path,path),format(string(AttrStr),"path(~w)",[Path.d])},AttrStr.
msg(Text) --> {is_dict(Text,text),string_length(Text.text,N),format(string(TxtStr),"#~w chars",[N])},"text","(",xy_coord(Text.x,Text.y)," - ",TxtStr,")".
msg(TextPath) --> {is_dict(TextPath,textPath),string_length(TextPath.text,N),format(string(TxtStr),"textPath(~w - #~w chars)",[TextPath.d,N])},TxtStr.
msg(Image) --> {is_dict(Image,image)},"image","(", xy_coord(Image.x,Image.y),",",attrs([Image.width,Image.height]), ")".
msg(UnknownShape) --> {is_dict(UnknownShape,Type), format(string(Msg),"~w isn't supported yet",[Type])},Msg.
xy_coords([X,Y]) --> xy_coord(X,Y).
xy_coords([X,Y|XYCoords]) --> xy_coord(X,Y),"-",xy_coords(XYCoords).
xy_coord(X,Y) --> "[",attr(X),",",attr(Y),"]".
attrs([]) --> "".
attrs([Attr]) --> attr(Attr).
attrs([Attr|Attrs]) --> attr(Attr), ",", attrs(Attrs).
attr(Attr) --> {format(string(AttrStr),"~w",[Attr])},AttrStr.





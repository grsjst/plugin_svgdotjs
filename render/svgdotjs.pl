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
		% debug(svgdotjs,"try svgdotjs renderer",[]),
		Types = [group,rect,circle,ellipse,line,polyline,path,text,textPath,image],
		DefaultOptions = _{x:0,y:0,width:300, height:300, transform:""},
		is_list(Shapes),
		forall(member(Shape,Shapes),(is_dict(Shape,Type),member(Type,Types))),!,
		debug(svgdotjs,"use svgdotjs renderer",[]),
		to_svgdotjs(Shapes,ShapeSpecs),
		debug(svgdotjs,"specs:~p",[ShapeSpecs]),
		dict_options(DictOptions,Options),
		NOptions = DefaultOptions.put(DictOptions),
		gensym(canvas,Id), % for debugging
		length(ShapeSpecs,N),
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
    require(['svgdotjs_plugin/svgdotjs_plugin'], function(canvas_factory) {
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


shape_message(Shape,Mesg) :-
	phrase(shape_msg(Shape),Codes),
	string_codes(Mesg,Codes).

shape_msg(Shape) --> msg_with_title(Shape),!.
shape_msg(Shape) --> msg(Shape),!.
msg_with_title(Shape) --> {Title = Shape.get(title)},msg(Shape)," - ",Title.
msg(Group) --> {is_dict(Group,group),length(Group.children,N), format(string(AttrStr),"group(#~w children)",[N])},AttrStr.
msg(Rect) --> {is_dict(Rect,rect)},"rect","(", xy_coord(Rect.x,Rect.y),",",attrs([Rect.width,Rect.height]), ")".
msg(Circle) --> {is_dict(Circle,circle)},"circle","(", xy_coord(Circle.cx,Circle.cy),",",attrs([Circle.r]), ")".
msg(Ellipse) --> {is_dict(Ellipse,ellipse)},"ellipse","(", xy_coord(Ellipse.cx,Ellipse.cy),",",attrs([Ellipse.rx,Ellipse.ry]), ")".
msg(Line) --> {is_dict(Line,line)},"line","(", "[",xy_coords([Line.x1,Line.y1,Line.x2,Line.y2]),")".
msg(PolyLine) --> {is_dict(PolyLine,polyline)},"polyline","(", xy_coords(PolyLine.points),")".
msg(Path) --> {is_dict(Path,path),format(string(AttrStr),"path(~w)",[Path.d])},AttrStr.
msg(Text) --> {is_dict(Text,text),string_length(Text.text,N),format(string(TxtStr),"#~w chars",[N])},"text","(",xy_coord(Text.x,Text.y)," - ",TxtStr,")".
msg(TextPath) --> {is_dict(TextPath,textPath),string_length(TextPath.text,N),format(string(TxtStr),"#~w chars",[N])},"textPath","(",xy_coord(TextPath.x,TextPath.y)," - ",TxtStr,")".
msg(Image) --> {is_dict(Image,image)},"image","(", xy_coord(Image.x,Image.y),",",attrs([Image.width,Image.height]), ")".
msg(UnknownShape) --> {is_dict(UnknownShape,Type), format(string(Msg),"~w isn't supported yet",[Type])},Msg.
xy_coords([X,Y]) --> xy_coord(X,Y).
xy_coords([X,Y|XYCoords]) --> xy_coord(X,Y),"-",xy_coords(XYCoords).
xy_coord(X,Y) --> "[",attr(X),",",attr(Y),"]".
attrs([]) --> "".
attrs([Attr]) --> attr(Attr).
attrs([Attr|Attrs]) --> attr(Attr), ",", attrs(Attrs).
attr(Attr) --> {format(string(AttrStr),"~w",[Attr])},AttrStr.

% "M490.667,0H21.333C9.551,0,0,9.551,0,21.333v469.333C0,502.449,9.551,512,21.333,512h469.333c11.782,0,21.333-9.551,21.333-21.333V21.333C512,9.551,502.449,0,490.667,0z M469.333,469.333H42.667V42.667h426.667V469.333z"
parse_path(String,Path) :-
	atom_codes(String,Codes),
    phrase(path(Path),Codes,[]),!.

path([]) --> "".
path(Segements) --> segment(Segment),path(Segements0),{append(Segment,Segements0,Segements)}.

segment([Cmd]) --> close(Cmd),whites.
segment([Cmd|Coords]) --> command(Cmd),coords(Coords).
command(LetterAtom) --> [Letter],{code_type(Letter, alpha),!,atom_codes(LetterAtom,[Letter])}. 
coords([Number]) --> number(Number).
coords([Number|Numbers]) --> number(Number),sep,coords(Numbers).
close(z) --> "z".
close(z) --> "Z".
sep --> whites.
sep --> whites,",",whites.






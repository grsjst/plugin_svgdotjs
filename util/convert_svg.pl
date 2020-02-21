:- use_module(library(sgml)).
:- use_module(svg_path_parser).


% Types = [svg,g,rect,circle,ellipse,line,polyline,polygon,path,text,textPath,image],

svg_convert(SVG,Shapes) :-
	string(SVG),!,
    open_string(SVG,StrIn),
    do_svg_convert(StrIn,Shapes).

svg_convert(InSpec,OutSpec) :-
	setup_call_cleanup(
        open_any(InSpec,read,In,CloseIn,[encoding(utf8)]),
        do_svg_convert(In,Shapes),
        close_any(CloseIn)),
	length(Shapes,N),writeln(N),
	setup_call_cleanup(
        open_any(OutSpec,write,Out,CloseOut,[encoding(utf8)]),
        forall(member(Shape,Shapes),write_term(Out,shape(Shape),[fullstop(true),quoted(true)])),
        close_any(CloseOut)),
	writeln(done).

 do_svg_convert(StreamIn,Shapes) :-
    load_xml(StreamIn,Elements,[]),
    findall(Shape,(member(Element,Elements),element_to_shape(Element,Shape)),Shapes).

element_to_shape(element(Type,Atts,Children),NShape) :-
	member(Type,[svg,g]),!,
    findall(ChildShape,(member(Child,Children),element_to_shape(Child,ChildShape)),ChildShapes),
    convert_atts(Atts,Shape),
    is_dict(Shape,group),
    NShape = Shape.put(children,ChildShapes).

element_to_shape(element(Type,Atts,_Children),Shape) :-
    member(Type,[rect,circle,ellipse,line,polyline,polygon,path,text,textPath,image]),!,
    convert_atts(Atts,Shape),
    is_dict(Shape,Type).

element_to_shape(element(Type,_,_),_) :-
    writeln(unknown:Type),fail.

convert_atts([],_{}).
convert_atts([Att|Atts],NAttsDict) :-
    convert_att(Att,AttDict),
    convert_atts(Atts,AttsDict),
    NAttsDict = AttsDict.put(AttDict).

convert_att(Key=Value,AttDict) :-
    Key = d,!,
    parse_path(Value,Path),
    AttDict = _{}.put(Key,Path).

convert_att(Key=Value,AttDict) :-
    Key = points,!,
    parse_points(Value,Points),
    AttDict = _{}.put(Key,Points).

convert_att(Key=Value,AttDict) :-
    atom_number(Value,Number),!,
    AttDict = _{}.put(Key,Number).

convert_att(Key=Value,AttDict) :-
    AttDict = _{}.put(Key,Value).

convert([],[]).
convert([Shape|Shapes],[NShape|NShapes]) :-
    convert_shape(Shape,NShape),
    convert(Shapes,NShapes).

convert_shape(Shape,NShape) :-
    is_dict(Shape),
    PathStr = Shape.d,
    parse_path(PathStr,Path),
    NShape = Shape.put(d,Path).



:- module(svg_path_parser,
	  [ parse_path/2,			% +String, -List
	  parse_points/2			% +String, -List
	  ]).

:- use_module(library(dcg/basics)).
:- use_module(library(apply_macros)).
:- use_module(library(debug)).

:- portray_text(true).

:- debug(svg_path_parser).


/** <module> svg_path_parser

	Parses an SVG path into a list 

	https://www.w3.org/TR/SVG/paths.html#PathDataBNF
*/


parse_points(String,Points) :-
	atom_codes(String,Codes),
    phrase(coordinate_sequence(Points),Codes,[]),!.

% "M20,230 Q40,205 50,230 T90,230"
parse_path(String,Path) :-
	atom_codes(String,Codes),
    phrase(svg_path(Path),Codes,[]),!.

% svg_path::= wsp* moveto? (moveto drawto_command*)?
svg_path(Segments) --> wsp, moveto_drawto_command(Segments).
svg_path(Segments) --> wsp, moveto(SegmentA),moveto_drawto_command(SegmentB),{append(SegmentA,SegmentB,Segments)}.

moveto_drawto_command(Segments) --> moveto(SegmentA),drawto_command_star(SegmentB),{append(SegmentA,SegmentB,Segments)}.

drawto_command_star([]) --> "".
drawto_command_star(Segments) --> wsp,drawto_command(SegmentA),!,drawto_command_star(SegmentB),{append(SegmentA,SegmentB,Segments)}.

% drawto_command::=
%     moveto
%     | closepath
%     | lineto
%     | horizontal_lineto
%     | vertical_lineto
%     | curveto
%     | smooth_curveto
%     | quadratic_bezier_curveto
%     | smooth_quadratic_bezier_curveto
%     | elliptical_arc
drawto_command(Segment) --> moveto(Segment).
drawto_command(Segment) --> closepath(Segment).
drawto_command(Segment) --> lineto(Segment).
drawto_command(Segment) --> horizontal_lineto(Segment).
drawto_command(Segment) --> vertical_lineto(Segment).
drawto_command(Segment) --> curveto(Segment).
drawto_command(Segment) --> smooth_curveto(Segment).
drawto_command(Segment) --> quadratic_bezier_curveto(Segment).
drawto_command(Segment) --> smooth_quadratic_bezier_curveto(Segment).
drawto_command(Segment) --> elliptical_arc(Segment).

% otherwise parsing error
drawto_command(_Segment) --> remainder(RemC),{atom_codes(RemA,RemC),syntax_error(RemA),fail}.

% moveto::=
%     ( "M" | "m" ) wsp* coordinate_pair_sequence
moveto([MA|Coords]) --> [MC],{atom_codes(MA,[MC]),memberchk(MA,[m,'M'])},wsp,coordinate_pair_sequence(Coords).

% closepath::=
%     ("Z" | "z")
closepath([ZA]) --> [ZC],{atom_codes(ZA,[ZC]),memberchk(ZA,[z,'Z'])}.

% lineto::=
%     ("L"|"l") wsp* coordinate_pair_sequence
lineto([LA|Coords]) --> [LC],{atom_codes(LA,[LC]),memberchk(LA,[l,'L'])},wsp,coordinate_pair_sequence(Coords).

% horizontal_lineto::=
%     ("H"|"h") wsp* coordinate_sequence
horizontal_lineto([HA|Coords]) --> [HC],{atom_codes(HA,[HC]),memberchk(HA,[h,'H'])},wsp,coordinate_sequence(Coords).

% vertical_lineto::=
%     ("V"|"v") wsp* coordinate_sequence
vertical_lineto([VA|Coords]) --> [VC],{atom_codes(VA,[VC]),memberchk(VA,[v,'V'])},wsp,coordinate_sequence(Coords).

%vcurveto::=
%    ("C"|"c") wsp* curveto_coordinate_sequence
curveto([CA|Coords]) --> [CC],{atom_codes(CA,[CC]),memberchk(CA,[c,'C'])},wsp,curveto_coordinate_sequence(Coords).

% smooth_curveto::=
%     ("S"|"s") wsp* smooth_curveto_coordinate_sequence
smooth_curveto([SA|Coords]) --> [SC],{atom_codes(SA,[SC]),memberchk(SA,[s,'S'])},wsp,smooth_curveto_coordinate_sequence(Coords).

% quadratic_bezier_curveto::=
%     ("Q"|"q") wsp* quadratic_bezier_curveto_coordinate_sequence
quadratic_bezier_curveto([QA|Coords]) --> [QC],{atom_codes(QA,[QC]),memberchk(QA,[q,'Q'])},wsp,quadratic_bezier_curveto_coordinate_sequence(Coords).

% smooth_quadratic_bezier_curveto::=
%     ("T"|"t") wsp* coordinate_pair_sequence
smooth_quadratic_bezier_curveto([TA|Coords]) --> [TC],{atom_codes(TA,[TC]),memberchk(TA,[t,'T'])},wsp,coordinate_pair_sequence(Coords).

% elliptical_arc::=
%     ( "A" | "a" ) wsp* elliptical_arc_argument_sequence   
elliptical_arc([AA|Coords]) --> [AC],{atom_codes(AA,[AC]),memberchk(AA,[a,'A'])},wsp,elliptical_arc_argument_sequence(Coords).

% curveto_coordinate_sequence::=
%     coordinate_pair_triplet
%     | (coordinate_pair_triplet comma_wsp? curveto_coordinate_sequence)
curveto_coordinate_sequence(Coords) --> coordinate_pair_triplet(CoordsA),comma_wsp,curveto_coordinate_sequence(CoordsB),{append(CoordsA,CoordsB,Coords)}.
curveto_coordinate_sequence(Coords) --> coordinate_pair_triplet(Coords).

% smooth_curveto_coordinate_sequence::=
%     coordinate_pair_double
%     | (coordinate_pair_double comma_wsp? smooth_curveto_coordinate_sequence)
smooth_curveto_coordinate_sequence(Coords) --> coordinate_pair_double(CoordsA),comma_wsp,smooth_curveto_coordinate_sequence(CoordsB),{append(CoordsA,CoordsB,Coords)}.
smooth_curveto_coordinate_sequence(Coords) --> coordinate_pair_double(Coords).

% quadratic_bezier_curveto_coordinate_sequence::=
%     coordinate_pair_double
%     | (coordinate_pair_double comma_wsp? quadratic_bezier_curveto_coordinate_sequence)
quadratic_bezier_curveto_coordinate_sequence(Coords) --> coordinate_pair_double(CoordsA),comma_wsp,quadratic_bezier_curveto_coordinate_sequence(CoordsB),{append(CoordsA,CoordsB,Coords)}.
quadratic_bezier_curveto_coordinate_sequence(Coords) --> coordinate_pair_double(Coords).

% elliptical_arc_argument_sequence::=
%     elliptical_arc_argument
%     | (elliptical_arc_argument comma_wsp? elliptical_arc_argument_sequence)
elliptical_arc_argument_sequence(Coords) --> elliptical_arc_argument(CoordsA),comma_wsp,elliptical_arc_argument_sequence(CoordsB),{append(CoordsA,CoordsB,Coords)}.
elliptical_arc_argument_sequence(Coords) --> elliptical_arc_argument(Coords).

% elliptical_arc_argument::=
%     number comma_wsp? number comma_wsp? number comma_wsp
%     a_flag comma_wsp? a_flag comma_wsp? coordinate_pair
elliptical_arc_argument([N,M,O,LAF,SW,X,Y]) --> 
	number(N),comma_wsp,number(M),comma_wsp,number(O),comma_wsp, a_flag(LAF),comma_wsp,a_flag(SW),comma_wsp,coordinate_pair(X,Y).

% coordinate_pair_double::=
%     coordinate_pair comma_wsp? coordinate_pair
coordinate_pair_double([X1,Y1,X2,Y2]) --> coordinate_pair(X1,Y1),comma_wsp,coordinate_pair(X2,Y2).

%coordinate_pair_triplet::=
%    coordinate_pair comma_wsp? coordinate_pair comma_wsp? coordinate_pair
coordinate_pair_triplet([X1,Y1,X2,Y2,X3,Y3]) --> coordinate_pair(X1,Y1),comma_wsp,coordinate_pair(X2,Y2),comma_wsp,coordinate_pair(X3,Y3).

% coordinate_pair_sequence::=
%     coordinate_pair | (coordinate_pair comma_wsp? coordinate_pair_sequence)
coordinate_pair_sequence([X,Y|Coords]) --> coordinate_pair(X,Y), comma_wsp, coordinate_pair_sequence(Coords).
coordinate_pair_sequence([X,Y]) --> coordinate_pair(X,Y).

% coordinate_sequence::=
%     coordinate | (coordinate comma_wsp? coordinate_sequence)
coordinate_sequence([X|Coords]) --> coordinate(X),comma_wsp,coordinate_sequence(Coords).
coordinate_sequence([X]) --> coordinate(X).

% coordinate_pair::= coordinate comma_wsp? coordinate
coordinate_pair(X,Y) --> coordinate(X), comma_wsp, coordinate(Y).

% coordinate::= sign? number
coordinate(X) --> number(X).
coordinate(X) --> "-.",integer(I),{format(atom(A),"-0.~w",[I]),atom_number(A,X)}. % -.10 -> -0.10.
coordinate(X) --> ".",integer(I),{format(atom(A),"0.~w",[I]),atom_number(A,X)}. % .10 -> 0.10. 


a_flag(0) --> "0".
a_flag(1) --> "1".

comma_wsp --> wsp,",",wsp.
comma_wsp --> wsp.

wsp --> blanks.


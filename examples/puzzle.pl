:- use_rendering(svgdotjs, [width(210),height(210),transform:_{origin: [0,0],scale:30}]).
:- use_module(library(clpfd)).

example(Shapes) :-
    BB = rect{id:bb, x:0,y:0,width: 7,height: 7,fill:"none"},
	A = rect{id:a, x:_,y:_,width:3,height:3,fill: "red",'fill-opacity':0.7},
    B = rect{id:b, x:_,y:_,width:3,height:2,fill: "blue",'fill-opacity':0.7},
    C = rect{id:c, x:_,y:_,width:4,height:1,fill: "yellow",'fill-opacity':0.7},
	D = rect{id:d, x:_,y:_,width:1,height:5,fill: "green",'fill-opacity':0.7},
    E = rect{id:e, x:_,y:_,width:2,height:4,fill: "orange",'fill-opacity':0.7},
    F = rect{id:f, x:_,y:_,width:2,height:2,fill: "purple",'fill-opacity':0.7},
	G = rect{id:g, x:_,y:_,width:1,height:6,fill: "cyan",'fill-opacity':0.7},
    H = rect{id:h, x:_,y:_,width:7,height:1,fill: "magenta",'fill-opacity':0.7},
    Shapes = [A,B,C,D,E,F,G,H],
    term_variables(Shapes,CVars),
    CVars ins 0..50,
	all_contained(BB,Shapes),
    no_overlap(Shapes),
    label(CVars).

all_contained(BB,L) :-
    maplist(contains(BB),L).

contains(B,N) :-
    B.x #=< N.x,
    N.x + N.width #=< B.x + B.width,
    B.y #=< N.y,
    N.y + N.height #=< B.y + B.height.

no_overlap([]).
no_overlap([N|T]) :-
    maplist(no_overlap(N),T),
    no_overlap(T).

no_overlap(N,M) :-
    N.x + N.width #=< M.x.

no_overlap(N,M) :-
    M.x + M.width #=< N.x.

no_overlap(N,M) :-
    N.y + N.height #=< M.y.

no_overlap(N,M) :-
    M.y + M.height #=< N.y.
     

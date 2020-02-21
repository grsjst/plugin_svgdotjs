:- use_rendering(svgdotjs,[width(200),height(250)]).

% https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Basic_Shapes
shapes([
       rect{tooltip:"my tip",fill:transparent,height:30,stroke:black,'stroke-width':5,width:30,x:10,y:10},
       rect{fill:transparent,height:30,rx:10,ry:10,stroke:black,'stroke-width':5,width:30,x:60,y:10},
       circle{cx:25,cy:75,fill:transparent,r:20,stroke:red,'stroke-width':5},
       ellipse{cx:75,cy:75,fill:transparent,rx:20,ry:5,stroke:red,'stroke-width':5},
       line{stroke:orange,'stroke-width':5,x1:10,x2:50,y1:110,y2:150},
       polyline{fill:transparent,points:[60,110,65,120,70,115,75,130,80,125,85,140,90,135,95,150,100,145],stroke:orange,'stroke-width':5},
       polygon{fill:transparent,points:[50,160,55,180,70,180,60,190,65,205,50,195,35,205,40,190,30,180,45,180],stroke:green,'stroke-width':5},
       path{d:['M',20,230,'Q',40,205,50,230,'T',90,230],fill:none,stroke:blue,'stroke-width':5}
       ]).
shapes([
       text{x: 10,y: 5,text: "Hello World!"},         
       textPath{d:['M',20,20,'C',80,60,100,40,120,20],text: "A curve... A curve..."}
       ]).
shapes([
       image{x: 5,y: 5,width: 190,height: 200,href:"https://picsum.photos/190"}
       ]).

example(Shapes) :-
	shapes(Shapes).
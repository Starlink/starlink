input boxes

 
prologues := 1;
% TeX-style name for Times-Roman
defaultfont := "rptmr";

u := 1.5cm;
vu := 32pt;  % vertical unit

def starttool(expr toolname) =
  z0 = (0,0);
  x1-x0 = x2-x1 = 2u;
  y0 = y1 = y2;

  boxit.tool(toolname);
  tool.c = z1;
  drawboxed(tool);
enddef;

beginfig(1);
  starttool("autoheader");

  x0t = x1t = x0;
  (y0t + y1t)/2 = y0;
  y1t - y0t = 1vu;

  label.lft("starconf.m4", z1t);
  label.lft("configure.ac", z0t);
  drawarrow (z0t--tool.c) cutafter bpath tool;
  drawarrow (z1t--tool.c) cutafter bpath tool;

  drawarrow (tool.c--z2) cutbefore bpath tool;
  label.rt("config.h.in", z2);
endfig;

beginfig(2);
  starttool("autoconf");

  x0t = x1t = x0;
  (y0t + y1t)/2 = y0;
  y1t - y0t = 1vu;

  label.lft("starconf.m4", z1t);
  label.lft("configure.ac", z0t);
  drawarrow (z0t--tool.c) cutafter bpath tool;
  drawarrow (z1t--tool.c) cutafter bpath tool;

  drawarrow (tool.c--z2) cutbefore bpath tool;
  label.rt("configure", z2);
endfig;

beginfig(3);
  starttool("automake");

  x0t = x1t = x0;
  (y0t + y1t)/2 = y0;
  y1t - y0t = 1vu;

  label.lft("configure.ac", z1t);
  label.lft("Makefile.am",  z0t);
  drawarrow (z0t--tool.c) cutafter bpath tool;
  drawarrow (z1t--tool.c) cutafter bpath tool;

  drawarrow (tool.c--z2) cutbefore bpath tool;
  label.rt("Makefile.in", z2);
endfig;

beginfig(4);
  starttool("./configure");

  x0in = x1in = x2in = x0;
  (y0in + y1in + y2in)/3 = y0;
  y1in-y0in = y2in-y1in = 1vu;

  x4 = x1;
  y4 - y1 = 2vu;

  label.lft("Makefile.in", z2in);
  label.lft("config.h.in", z1in);
  label.lft("...others.in...", z0in);
  drawarrow (z0in--tool.c) cutafter bpath tool;
  drawarrow (z1in--tool.c) cutafter bpath tool;
  drawarrow (z2in--tool.c) cutafter bpath tool;

  label.top("(System)", z4);
  drawarrow (z4--tool.c) cutafter bpath tool;

  x0o = x1o = x2o = x2;
  (y0o + y1o + y2o)/3 = y2;
  y1o-y0o = y2o-y1o = 1vu;

  drawarrow (tool.c--z0o) cutbefore bpath tool;
  drawarrow (tool.c--z1o) cutbefore bpath tool;
  drawarrow (tool.c--z2o) cutbefore bpath tool;
  label.rt("Makefile", z2o);
  label.rt("config.h", z1o);
  label.rt("...others...", z0o);
endfig;

% see metapost manual, example 52
vardef cuta(suffix a,b) expr p =
  drawarrow p cutbefore bpath.a cutafter bpath.b;
  point .5*length p of p
enddef;

beginfig(5);
  numeric hsep, vsep;
  vsep = 2cm;

  y1 = y2 = (y3+y4)/2;
  y3-y4 = y5-y6 = vsep;
  y4 = (y5+y6)/2;
  x2-x1 = x4-x2 = x6-x4 = hsep;
  x3 = x4;
  x5 = x6;
  x6-x1 = 10cm;                 % specifies hsep

  boxit.a("c:r:a");
  boxit.b("c:r+1:a");
  boxit.c("c+1:0:a+1");
  boxit.d("c+1:0:0");

  circleit.qa("i/f unchanged?");  qa.dx=qa.dy;
  circleit.qb("i/f compatible?"); qb.dx=qb.dy;

  a.c = z1;
  qa.c = z2;
  b.c = z3;
  qb.c = z4;
  c.c = z5;
  d.c = z6;

  drawboxed(a,b,c,d,qa,qb);

  drawarrow a.c..qa.c cutbefore bpath.a cutafter bpath.qa;
  label.top("yes", cuta(qa,b)  qa.c..b.c);
  label.bot("no",  cuta(qa,qb) qa.c..qb.c);
  label.top("yes", cuta(qb,c)  qb.c..c.c);
  label.bot("no",  cuta(qb,d)  qb.c..d.c);
endfig;
    
end

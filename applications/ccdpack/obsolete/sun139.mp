
input boxes;
prologues:=1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
beginfig(4);

defaultfont:="Times-Roman";
save gap, u;

w = 12bp;
h = 10bp;
u = 4bp;
gap = 1/2u;
frac = 1/3;

color boxinside;
boxinside = 1/1024[white,black];

textsize = 1.4;
defaultscale := textsize;

def bend(expr a, patha, b, pathb, mult, theta) =
   drawarrow a {dir (mult*((angle (b-a)) - theta) + theta)} .. b
             cutbefore patha cutafter pathb shifted (gap*unitvector (a-b));
enddef;

def bendy(expr a,patha,b,pathb)=bend(a,patha,b,pathb,1-frac,-90); enddef;
def bendo(expr a,patha,b,pathb)=bend(a,patha,b,pathb,1+frac,-90); enddef;
def straight(expr a,patha,b,pathb)=bend(a,patha,b,pathb,1,-90); enddef;

% def bendy(expr a, patha, b, pathb) =
%    drawarrow a {dir ((1-frac)*((angle (b - a)) + 90) - 90)} .. b
%              cutbefore patha cutafter pathb shifted (gap*unitvector (a-b));
% enddef;
%
% def bendo(expr a, patha, b, pathb) =
%    drawarrow a {dir ((1+frac)*((angle (b - a)) + 90) - 90)} .. b
%              cutbefore patha cutafter pathb shifted (gap*unitvector (a-b));
% enddef;


boxit.astimp("ASTIMP");
boxit.wcsedit("WCSEDIT");
boxit.findobj("FINDOBJ");
boxit.findoff("FINDOFF");
boxit.pairndf("PAIRNDF");
boxit.ccdalign("CCDALIGN");
boxit.register("REGISTER");
boxit.wcsreg("WCSREG");
boxit.astexp("ASTEXP");
boxit.tranndf("TRANNDF");
boxit.drizzle("DRIZZLE");
boxit.makemos("MAKEMOS");

def allboxes = astimp, wcsedit, findobj, findoff, pairndf, ccdalign,
               register, wcsreg, astexp, tranndf, drizzle, makemos   
enddef;

forsuffixes t = allboxes:
    t.dx = t.dy = 3pt;
endfor;


( 1, 1 ) = (xpart findobj.w, ypart makemos.s );

findobj.c - findoff.c = (-1w, 4h);
pairndf.c - register.c = (0, 10h);
register.c - z1 = (-9w, 5h);
astimp.w - wcsedit.e = wcsedit.w - ccdalign.e = (2w, 0);
ccdalign.w - pairndf.e = pairndf.w - findobj.e = (1w, 0);
z1 - z2 = (0, 9h);

wcsreg.c = 1/2[z1, z2] - (6w, 0);
astexp.c = z2 + (9w, 0);
drizzle.c - tranndf.c = (10w, 0);

1/2[tranndf.c, drizzle.c] = z2 - (0, 5h) 
                          = makemos.c + (0, 5h);

forsuffixes t=findobj, pairndf, ccdalign, wcsedit, astimp:
   pair t.desc;
   t.desc = t.n + (0, h);
endfor;

pair over, object.grp, geom.grp;
over = (0,h);
object.grp - pairndf.desc =
geom.grp - 1/2[astimp.desc, wcsedit.desc] = 3over;

defaultfont := "Helvetica";
defaultscale := 0.8textsize;

label("automatic", findobj.desc);
label("semi-automatic", pairndf.desc);
label("manual", ccdalign.desc);
label("imported", astimp.desc);
label("manual", wcsedit.desc);

label("Object matching", object.grp);
label("Geometry information", geom.grp);

drawoptions(withpen pencircle scaled 1bp);
draw (findobj.desc -- ccdalign.desc) shifted 2over;
draw (wcsedit.desc -- astimp.desc) shifted 2over;
% draw ((0,0) -- -over) shifted object.grp;
% draw ((0,0) -- -over) shifted geom.grp;
forsuffixes t=findobj, pairndf, ccdalign, wcsedit, astimp:
   draw t.desc + over -- t.desc + 2over;
endfor;
drawoptions();


path dot[], dot[].out;
dot1 = fullcircle scaled 1u shifted z1;
dot1.out = fullcircle scaled 2u shifted z1;
dot2 = fullcircle scaled 1u shifted z2;
dot2.out = fullcircle scaled 2u shifted z2;
fill dot1;
fill dot2;


bendy( astimp.c, bpath astimp, z1, dot1.out );
bendy( wcsedit.c, bpath wcsedit, z1, dot1.out );
bendy( findobj.c, bpath findobj, findoff.c, bpath findoff );
bendy( findoff.c, bpath findoff, register.c, bpath register );
bendy( pairndf.c, bpath pairndf, register.c, bpath register );
bendy( ccdalign.c, bpath ccdalign, register.c, bpath register );
bendy( register.c, bpath register, z1, dot1.out );

bend( z1, dot1.out, z2, dot2.out, 1-1/2frac, 0 );

bendo( z1, dot1.out, wcsreg.c, bpath wcsreg );
bendy( wcsreg.c, bpath wcsreg, z2, dot2.out );


straight( z2, dot2.out, astexp.c, bpath astexp );
drawoptions(dashed evenly scaled 3);
bend( astexp.c, bpath astexp, astimp.c, bpath astimp, 1+frac, 90 );
drawoptions(); 


bendo( z2, dot2.out, tranndf.c, bpath tranndf );
bendo( z2, dot2.out, drizzle.c, bpath drizzle );
bendy( tranndf.c, bpath tranndf, makemos.c, bpath makemos );
bendy( drizzle.c, bpath drizzle, makemos.c, bpath makemos );


forsuffixes t = allboxes:
   fill bpath t withcolor boxinside;
endfor;

drawboxed( allboxes );

endfig;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
beginfig(3); 

defaultfont:="Courier";
defaultscale:=1.6;

path ccd[]a, ccd[]b, ccd[]c;
pair ccd[]a.orig, ccd[]b.orig, ccd[]c.orig;
pair ccd[]orig;
pair squodge;
color grey[],yellow;

save gap, u;

u := 80bp;
long = 2 short = 2u;
gap = 1/10u;
squodge = (1/4long, 1/2short);
yellow = (1,1,0);

ccd1.dir = 0;
ccd2.dir = ccd1.dir + 180;
ccd2.orig - ccd1.orig = (2long+2gap, -2short-gap) + squodge;
ccd1.orig = (0, long+gap) + (gap, gap);

grey1 = .4[white,yellow];
grey2 = .8[white,yellow];

pair a.orig, b.orig, c.orig;
path a, b, c;
x0a = x3a = 0;
y0a = y1a = 0;
x1a - x0a = x2a - x3a = short;
y0a - y3a = y1a - y2a = long;
a = (z0a -- z1a -- z2a -- z3a -- cycle);
b = a rotated 90 shifted (short+gap,-short);
c = b shifted (0,-short-gap);
a.orig = z0a;
b.orig = z0a + (short+gap,0);
c.orig = b.orig - (0,short+gap);

forsuffixes t=1,2:
   forsuffixes s=a,b,c:
      ccd.t.s = s rotated ccd.t.dir shifted ccd.t.orig;
      ccd.t.s.orig = s.orig rotated ccd.t.dir shifted ccd.t.orig;
   endfor;
endfor;

% forsuffixes t=1,2:
%    x0t = x3t = xpart ccd.t.orig;
%    y0t = y1t = ypart ccd.t.orig;
%    x1t - x0t = x2t - x3t = short;
%    y0t - y3t = y1t - y2t = long;
%    ccd.t.a = (z0t -- z1t -- z2t -- z3t -- cycle) 
%                   shifted (-ccd.t.orig) rotated ccd.t.dir shifted ccd.t.orig;
%    ccd.t.b = (ccd.t.a shifted (-ccd.t.orig)
%                       rotated 90
%                       shifted (ccd.t.orig+(short+gap,-short)));
%    ccd.t.c = (ccd.t.b shifted (0, -gap-short));
%    ccd.t.a.orig = ccd.t.orig;
%    ccd.t.b.orig = ccd.t.orig + (short+gap, 0);
%    ccd.t.c.orig = ccd.t.b.orig - (0, short+gap);
% endfor;

forsuffixes t=1,2:
   fill ccd.t.a withcolor grey1;
   fill ccd.t.b withcolor grey1;
   fill ccd.t.c withcolor grey1;
endfor;

fill buildcycle(ccd1b, ccd2c) withcolor grey2;
fill buildcycle(ccd1c, ccd2b) withcolor grey2;
fill buildcycle(ccd1b, ccd2b) withcolor grey2;

forsuffixes t=1,2:
   draw ccd.t.a;
   draw ccd.t.b;
   draw ccd.t.c;
endfor;

label.lrt("1a", ccd1a.orig);
label.lrt("1b", ccd1b.orig);
label.lrt("1c", ccd1c.orig);
label.ulft("2a", ccd2a.orig);
label.ulft("2b", ccd2b.orig);
label.ulft("2c", ccd2c.orig);


endfig;

end


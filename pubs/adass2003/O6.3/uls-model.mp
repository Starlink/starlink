
% Figures for O6-3
% $Id$

input boxes
%prologues := 1;                % uncomment to produce directly-viewable output

numeric u, a, e;
u := 10mm;

defaultfont := "rptmr";         % Times-Roman in (local?) TeX-speak


def cloud(expr centre, a, b) =
  begingroup
    % See eg. http://mathworld.wolfram.com/Ellipse.html
    pair i[], o[];
    numeric steps, hwidth, inc, thetadev, rdev;
    hwidth := 0.2u;             % outer/inner ellipse has semi-major axis of
                                % a +/- hwidth
    steps := 16;                % number of bumps on cloud
    inc := 360/steps;
    thetadev := inc/16;         % max deviation of theta
    rdev := a/8;                % ...and r

    for j=0 upto steps-1: 
      theta := j*inc + uniformdeviate(thetadev)-thetadev/2;
      r := 1/sqrt((cosd(theta)/(a-hwidth))**2 + (sind(theta)/(b-hwidth))**2)
           + uniformdeviate(rdev)-rdev/2;
      i[j] := (r*cosd(theta), r*sind(theta)) shifted centre;
      theta := (j+0.5)*inc + uniformdeviate(thetadev)-thetadev/2;
      r := 1/sqrt((cosd(theta)/(a+hwidth))**2 + (sind(theta)/(b+hwidth))**2)
           + uniformdeviate(rdev)-rdev/2;
      o[j] := (r*cosd(theta), r*sind(theta)) shifted centre;
    endfor
    i[steps] = i[0];

    pickup pencircle scaled 1pt;
    for j=0 upto steps-1: 
      draw i[j]{i[j]-centre}..o[j]..{centre-i[j+1]}i[j+1];
    endfor
  endgroup
enddef;

defaultscale := 2.5;

beginfig(1);

  boxhgap := 6u;
  boxvgap := 2.5u;
  interim ahlength := 0.5u;
  interim defaultdx := 0.4u;
  interim defaultdy := 0.4u;

  boxit.u("User");
  boxit.l("Language");
  boxit.s("System");
  ypart u.c = ypart l.c = ypart s.c;
  ypart u.s = ypart l.s;        % "User" has no descenders, make box same depth
  xpart s.c - xpart l.c = xpart l.c - xpart u.c = boxhgap;
  drawboxed(u,l,s);

  cloud(u.c shifted(0,boxvgap), 2u, 1.2u);
  label("model", u.c shifted(0,boxvgap));
  cloud(l.c shifted(0,boxvgap), 2u, 1.2u);
  label("model", l.c shifted(0,boxvgap));
  cloud(s.c shifted(0,boxvgap), 2u, 1.2u);
  label("model", s.c shifted(0,boxvgap));

  drawdblarrow u.e--l.w;
  drawdblarrow l.e--s.w;
 
  boxit.ut("astronomer, programmer");
  boxit.lt("GUI, API, protocol");
  boxit.st("application, library, service");

%Either...
if true:
  ypart ut.c = ypart lt.c = ypart st.c = ypart s.c - 1.5boxvgap;
  xpart lt.c = xpart l.c;
  xpart ut.e = xpart u.w;
  xpart st.w = xpart s.e;
  drawboxed(ut, lt, st);

  drawarrow ut.e{right} .. {up}u.s  dashed evenly scaled 2;
  drawarrow lt.n .. l.s      dashed evenly scaled 2;
  drawarrow st.w{left} .. {up}s.s  dashed evenly scaled 2;

else:
% ...or
  ypart ut.c = ypart st.c = ypart s.c - 1.5boxvgap;
  ypart lt.c = ypart st.c - boxvgap;
  xpart lt.c = xpart l.c;
  xpart st.c - xpart lt.c = xpart lt.c - xpart ut.c = 1.5 boxhgap;
  drawboxed(ut, lt, st);

  drawarrow ut.n{up} .. {up}u.s  dashed evenly scaled 2;
  drawarrow lt.n .. l.s      dashed evenly scaled 2;
  drawarrow st.n{up} .. {up}s.s  dashed evenly scaled 2;
fi
endfig;

end

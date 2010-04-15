/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdmoon(double qfoo,double qbar[6])
#define qbaz 3155760000.0
#define Q0 4.2635212653763e-5
#define qfobar 1949.9997904423
{double q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,
qdog,qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,
qDAd,q9,Q10,Q11,q12,Q13,Q14,qdisk,p,Q15,q16,q17,QEMPTY,q18,
QFULL,qfast,qsmall,QBIG,QOK,QHELLO,QBYE,QMAGIC,q19,q20,
qobSCUrE,QSPEED,qIndex,Q21,qbill,q22,q23,qjoe,qemacs;int q24
,QVI;static double qrms=270.434164,QfbI=481267.8831,Qcia=-
0.001133,Q25=0.0000019;double Q26,QNASA;static double QERR=
358.475833,Q27=35999.0498,Q28=-0.000150,qgoogle=-0.0000033;
double q29,QYahoO;static double Q30=296.104608,qtrick=
477198.8491,q31=0.009192,Q32=0.0000144;double QHINT,Q33;
static double Q34=350.737486,QBLAcK=445267.1142,Q35=-
0.001436,q36=0.0000019;double q37,q38;static double q39=
11.250889,qred=483202.0251,QgreEN=-0.003211,QYELLOW=-
0.0000003;double q40,QBLUE;static double QMAGENTA=259.183275
,QCyaN=-1934.1420,Q41=0.002078,QWHITE=0.0000022;double
qclinton,q42;static double qbUsh=-0.002495,q43=-0.00000752;
double Q44,Q45,q46,qJFK;static double Q47=0.000233,q48=51.2,
Q49=20.2;static double Q50=-0.001778;static double q51=
0.000817;static double q52=0.002011;static double q53=
0.003964,qSEx=346.560,q54=132.870,qbar_foo=-0.0091731;static
 double qbar_bar=0.001964;static double qbar_baz=0.002541;
static double q55=0.001964;static double QbAR_FObAR=-
0.024691;static double q56=-0.004328,Q57=275.05,Q58=-2.30;
static double Q59=0.0004664;static double qbaR_FoOBaR=
0.0000754;struct Q60{double Qbar_fOBaZ;int q61;int Q62;int
qbar_foobaz;int q63;int q64;};static struct Q60 Q65[]={{
6.288750,0,1,0,0,0},{1.274018,0,-1,2,0,0},{0.658309,0,0,2,0,
0},{0.213616,0,2,0,0,0},{-0.185596,1,0,0,0,1},{-0.114336,0,0
,0,2,0},{0.058793,0,-2,2,0,0},{0.057212,-1,-1,2,0,1},{
0.053320,0,1,2,0,0},{0.045874,-1,0,2,0,1},{0.041024,-1,1,0,0
,1},{-0.034718,0,0,1,0,0},{-0.030465,1,1,0,0,1},{0.015326,0,
0,2,-2,0},{-0.012528,0,1,0,2,0},{-0.010980,0,-1,0,2,0},{
0.010674,0,-1,4,0,0},{0.010034,0,3,0,0,0},{0.008548,0,-2,4,0
,0},{-0.007910,1,-1,2,0,1},{-0.006783,1,0,2,0,1},{0.005162,0
,1,-1,0,0},{0.005000,1,0,1,0,1},{0.004049,-1,1,2,0,1},{
0.003996,0,2,2,0,0},{0.003862,0,0,4,0,0},{0.003665,0,-3,2,0,
0},{0.002695,-1,2,0,0,1},{0.002602,0,1,-2,-2,0},{0.002396,-1
,-2,2,0,1},{-0.002349,0,1,1,0,0},{0.002249,-2,0,2,0,2},{-
0.002125,1,2,0,0,1},{-0.002079,2,0,0,0,2},{0.002059,-2,-1,2,
0,2},{-0.001773,0,1,2,-2,0},{-0.001595,0,0,2,2,0},{0.001220,
-1,-1,4,0,1},{-0.001110,0,2,0,2,0},{0.000892,0,1,-3,0,0},{-
0.000811,1,1,2,0,1},{0.000761,-1,-2,4,0,1},{0.000717,-2,1,0,
0,2},{0.000704,-2,1,-2,0,2},{0.000693,1,-2,2,0,1},{0.000598,
-1,0,2,-2,1},{0.000550,0,1,4,0,0},{0.000538,0,4,0,0,0},{
0.000521,-1,0,4,0,1},{0.000486,0,2,-1,0,0}};static int
qbar_quux=(sizeof Q65/sizeof(struct Q60));static struct Q60
QBaR_fRed[]={{5.128189,0,0,0,1,0},{0.280606,0,1,0,1,0},{
0.277693,0,1,0,-1,0},{0.173238,0,0,2,-1,0},{0.055413,0,-1,2,
1,0},{0.046272,0,-1,2,-1,0},{0.032573,0,0,2,1,0},{0.017198,0
,2,0,1,0},{0.009267,0,1,2,-1,0},{0.008823,0,2,0,-1,0},{
0.008247,-1,0,2,-1,1},{0.004323,0,-2,2,-1,0},{0.004200,0,1,2
,1,0},{0.003372,-1,0,-2,1,1},{0.002472,-1,-1,2,1,1},{
0.002222,-1,0,2,1,1},{0.002072,-1,-1,2,-1,1},{0.001877,-1,1,
0,1,1},{0.001828,0,-1,4,-1,0},{-0.001803,1,0,0,1,1},{-
0.001750,0,0,0,3,0},{0.001570,-1,1,0,-1,1},{-0.001487,0,0,1,
1,0},{-0.001481,1,1,0,1,1},{0.001417,-1,-1,0,1,1},{0.001350,
-1,0,0,1,1},{0.001330,0,0,-1,1,0},{0.001106,0,3,0,1,0},{
0.001020,0,0,4,-1,0},{0.000833,0,-1,4,1,0},{0.000781,0,1,0,-
3,0},{0.000670,0,-2,4,1,0},{0.000606,0,0,2,-3,0},{0.000597,0
,2,2,-1,0},{0.000492,-1,1,2,-1,1},{0.000450,0,2,-2,-1,0},{
0.000439,0,3,0,-1,0},{0.000423,0,2,2,1,0},{0.000422,0,-3,2,-
1,0},{-0.000367,1,-1,2,1,1},{-0.000353,1,0,2,1,1},{0.000331,
0,0,4,1,0},{0.000317,-1,1,2,1,1},{0.000306,-2,0,2,-1,2},{-
0.000283,0,1,0,3,0}};static int qBar_doG=(sizeof QBaR_fRed/
sizeof(struct Q60));static struct Q60 qbar_cat[]={{0.950724,
0,0,0,0,0},{0.051818,0,1,0,0,0},{0.009531,0,-1,2,0,0},{
0.007843,0,0,2,0,0},{0.002824,0,2,0,0,0},{0.000857,0,1,2,0,0
},{0.000533,-1,0,2,0,1},{0.000401,-1,-1,2,0,1},{0.000320,-1,
1,0,0,1},{-0.000271,0,0,1,0,0},{-0.000264,1,1,0,0,1},{-
0.000198,0,-1,0,2,0},{0.000173,0,3,0,0,0},{0.000167,0,-1,4,0
,0},{-0.000111,1,0,0,0,1},{0.000103,0,-2,4,0,0},{-0.000084,0
,2,-2,0,0},{-0.000083,1,0,2,0,1},{0.000079,0,2,2,0,0},{
0.000072,0,0,4,0,0},{0.000064,-1,1,2,0,1},{-0.000063,1,-1,2,
0,1},{0.000041,1,0,1,0,1},{0.000035,-1,2,0,0,1},{-0.000033,0
,3,-2,0,0},{-0.000030,0,1,1,0,0},{-0.000029,0,0,-2,2,0},{-
0.000029,1,2,0,0,1},{0.000026,-2,0,2,0,2},{-0.000023,0,1,-2,
2,0},{0.000019,-1,-1,4,0,1}};static int qbar_fish=(sizeof
qbar_cat/sizeof(struct Q60));q1=(qfoo-15019.5)/36525.0;Q26=
DD2R*dmod(qrms+(QfbI+(Qcia+Q25*q1)*q1)*q1,360.0);QNASA=DD2R*
(QfbI+(2.0*Qcia+3.0*Q25*q1)*q1);q29=DD2R*dmod(QERR+(Q27+(Q28
+qgoogle*q1)*q1)*q1,360.0);QYahoO=DD2R*(Q27+(2.0*Q28+3.0*
qgoogle*q1)*q1);QHINT=DD2R*dmod(Q30+(qtrick+(q31+Q32*q1)*q1)
*q1,360.0);Q33=DD2R*(qtrick+(2.0*q31+3.0*Q32*q1)*q1);q37=
DD2R*dmod(Q34+(QBLAcK+(Q35+q36*q1)*q1)*q1,360.0);q38=DD2R*(
QBLAcK+(2.0*Q35+3.0*q36*q1)*q1);q40=DD2R*dmod(q39+(qred+(
QgreEN+QYELLOW*q1)*q1)*q1,360.0);QBLUE=DD2R*(qred+(2.0*
QgreEN+3.0*QYELLOW*q1)*q1);qclinton=DD2R*dmod(QMAGENTA+(
QCyaN+(Q41+QWHITE*q1)*q1)*q1,360.0);q42=DD2R*(QCyaN+(2.0*Q41
+3.0*QWHITE*q1)*q1);qfoobar=sin(qclinton);Q3=cos(qclinton);
q4=q42*Q3;q2=DD2R*(q48+Q49*q1);qfOBAz=sin(q2);qfoobaz=DD2R*
Q49*cos(q2);q2=DD2R*(qSEx+(q54+qbar_foo*q1)*q1);QQUUX=q53*
sin(q2);Q5=DD2R*q53*(q54+2.0*qbar_foo*q1)*cos(q2);Q26+=DD2R*
(Q47*qfOBAz+QQUUX+qbar_bar*qfoobar);QNASA+=DD2R*(Q47*qfoobaz
+Q5+qbar_bar*q4);q29+=DD2R*Q50*qfOBAz;QYahoO+=DD2R*Q50*
qfoobaz;QHINT+=DD2R*(q51*qfOBAz+QQUUX+qbar_baz*qfoobar);Q33
+=DD2R*(q51*qfoobaz+Q5+qbar_baz*q4);q37+=DD2R*(q52*qfOBAz+
QQUUX+q55*qfoobar);q38+=DD2R*(q52*qfoobaz+Q5+q55*q4);QFRED=
qclinton+DD2R*(Q57+Q58*q1);qdog=q42+DD2R*Q58;qcat=sin(QFRED)
;QFISH=cos(QFRED);q40+=DD2R*(QQUUX+QbAR_FObAR*qfoobar+q56*
qcat);QBLUE+=DD2R*(Q5+QbAR_FObAR*q4+q56*qdog*QFISH);Q44=1.0+
(qbUsh+q43*q1)*q1;Q45=qbUsh+2.0*q43*q1;q46=Q44*Q44;qJFK=2.0*
Q44*Q45;QgASp=0.0;Q6=0.0;for(q24=qbar_quux-1;q24>=0;q24--){
q7=Q65[q24].Qbar_fOBaZ;q8=(double)Q65[q24].q61;QBAD=(double)
Q65[q24].Q62;qBuG=(double)Q65[q24].qbar_foobaz;qsilly=(
double)Q65[q24].q63;QVI=Q65[q24].q64;if(QVI==0){QBUGGY=1.0;
QMUM=0.0;}else if(QVI==1){QBUGGY=Q44;QMUM=Q45;}else{QBUGGY=
q46;QMUM=qJFK;}q2=q8*q29+QBAD*QHINT+qBuG*q37+qsilly*q40;qDAd
=q8*QYahoO+QBAD*Q33+qBuG*q38+qsilly*QBLUE;q9=sin(q2);QgASp+=
q7*q9*QBUGGY;Q6+=q7*(cos(q2)*qDAd*QBUGGY+q9*QMUM);}Q10=Q26+
DD2R*QgASp;Q11=(QNASA+DD2R*Q6)/qbaz;QgASp=0.0;Q6=0.0;for(q24
=qBar_doG-1;q24>=0;q24--){q7=QBaR_fRed[q24].Qbar_fOBaZ;q8=(
double)QBaR_fRed[q24].q61;QBAD=(double)QBaR_fRed[q24].Q62;
qBuG=(double)QBaR_fRed[q24].qbar_foobaz;qsilly=(double)
QBaR_fRed[q24].q63;QVI=QBaR_fRed[q24].q64;if(QVI==0){QBUGGY=
1.0;QMUM=0.0;}else if(QVI==1){QBUGGY=Q44;QMUM=Q45;}else{
QBUGGY=q46;QMUM=qJFK;}q2=q8*q29+QBAD*QHINT+qBuG*q37+qsilly*
q40;qDAd=q8*QYahoO+QBAD*Q33+qBuG*q38+qsilly*QBLUE;q9=sin(q2)
;QgASp+=q7*q9*QBUGGY;Q6+=q7*(cos(q2)*qDAd*QBUGGY+q9*QMUM);}
Q14=1.0-Q59*Q3-qbaR_FoOBaR*QFISH;qdisk=Q59*q42*qfoobar+
qbaR_FoOBaR*qdog*qcat;q12=DD2R*QgASp*Q14;Q13=DD2R*(Q6*Q14+
QgASp*qdisk)/qbaz;QgASp=0.0;Q6=0.0;for(q24=qbar_fish-1;q24>=
0;q24--){q7=qbar_cat[q24].Qbar_fOBaZ;q8=(double)qbar_cat[q24
].q61;QBAD=(double)qbar_cat[q24].Q62;qBuG=(double)qbar_cat[
q24].qbar_foobaz;qsilly=(double)qbar_cat[q24].q63;QVI=
qbar_cat[q24].q64;if(QVI==0){QBUGGY=1.0;QMUM=0.0;}else if(
QVI==1){QBUGGY=Q44;QMUM=Q45;}else{QBUGGY=q46;QMUM=qJFK;}q2=
q8*q29+QBAD*QHINT+qBuG*q37+qsilly*q40;qDAd=q8*QYahoO+QBAD*
Q33+qBuG*q38+qsilly*QBLUE;q9=cos(q2);QgASp+=q7*q9*QBUGGY;Q6
+=q7*(-sin(q2)*qDAd*QBUGGY+q9*QMUM);}p=DD2R*QgASp;Q15=DD2R*
Q6/qbaz;q16=sin(p);q17=Q0/q16;QEMPTY=-q17*Q15*cos(p)/q16;
QHELLO=sin(Q10);QBYE=cos(Q10);QMAGIC=sin(q12);q19=cos(q12);
q20=q17*q19;qobSCUrE=q17*Q13;QSPEED=qobSCUrE*QMAGIC-q19*
QEMPTY;q18=q20*QBYE;QFULL=q20*QHELLO;qfast=q17*QMAGIC;qsmall
=-QFULL*Q11-QSPEED*QBYE;QBIG=q18*Q11-QSPEED*QHELLO;QOK=
qobSCUrE*q19+QMAGIC*QEMPTY;q1=(qfoo-51544.5)/36525.0;qIndex=
2000.0+q1*100.0;Q21=DS2R*(0.035+0.00085*(qIndex-qfobar));
qbill=DAS2R*(84381.448+(-46.8150+(-0.00059+0.001813*q1)*q1)*
q1);q22=sin(qbill);q23=cos(qbill);qjoe=Q21*q22;qemacs=Q21*
q23;qbar[0]=q18-qemacs*QFULL+qjoe*qfast;qbar[1]=Q21*q18+
QFULL*q23-qfast*q22;qbar[2]=QFULL*q22+qfast*q23;qbar[3]=
qsmall-qemacs*QBIG+qjoe*QOK;qbar[4]=Q21*qsmall+QBIG*q23-QOK*
q22;qbar[5]=QBIG*q22+QOK*q23;}

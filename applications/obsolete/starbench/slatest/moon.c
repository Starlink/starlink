/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmoon(int qfoo,int qbar,float qbaz,float Q0[6])
#define qfobar 0.01745329252f
#define q1 9.652743551e-12f
#define q2 4.2635212653763e-5f
{int qfoobar,Q3;float q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,
qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,
q9,p,Q10,Q11,q12,Q13,Q14,qdisk,Q15,q16,q17,QEMPTY,q18,QFULL,
qfast,qsmall,QBIG,QOK,QHELLO,QBYE,QMAGIC,q19;static float
q20=270.434164f;static float qobSCUrE=4812.678831f;static
float QSPEED=4680.0f;static float qIndex=132.678831f;static
float Q21=358.475833f;static float qbill=359.990498f;static
float q22=359.990498f;static float q23=296.104608f;static
float qjoe=4771.988491f;static float qemacs=4680.0f;static
float q24=91.988491f;static float QVI=350.737486f;static
float qrms=4452.671142f;static float QfbI=4320.0f;static
float Qcia=132.671142f;static float Q25=11.250889f;static
float Q26=4832.020251f;static float QNASA=4680.0f;static
float QERR=152.020251f;struct Q27{float Q28;int qgoogle;int
q29;int QYahoO;int Q30;};static struct Q27 qtrick[]={{
6.288750f,0,1,0,0},{1.274018f,0,-1,2,0},{0.658309f,0,0,2,0},
{0.213616f,0,2,0,0},{-0.185596f,1,0,0,0},{-0.114336f,0,0,0,2
},{0.058793f,0,-2,2,0},{0.057212f,-1,-1,2,0},{0.053320f,0,1,
2,0},{0.045874f,-1,0,2,0},{0.041024f,-1,1,0,0},{-0.034718f,0
,0,1,0},{-0.030465f,1,1,0,0},{0.015326f,0,0,2,-2},{-
0.012528f,0,1,0,2},{-0.010980f,0,-1,0,2},{0.010674f,0,-1,4,0
},{0.010034f,0,3,0,0},{0.008548f,0,-2,4,0},{-0.007910f,1,-1,
2,0},{-0.006783f,1,0,2,0},{0.005162f,0,1,-1,0},{0.005000f,1,
0,1,0},{0.004049f,-1,1,2,0},{0.003996f,0,2,2,0},{0.003862f,0
,0,4,0},{0.003665f,0,-3,2,0},{0.002695f,-1,2,0,0},{0.002602f
,0,1,-2,-2},{0.002396f,-1,-2,2,0},{-0.002349f,0,1,1,0},{
0.002249f,-2,0,2,0},{-0.002125f,1,2,0,0},{-0.002079f,2,0,0,0
},{0.002059f,-2,-1,2,0},{-0.001773f,0,1,2,-2},{-0.001595f,0,
0,2,2},{0.001220f,-1,-1,4,0},{-0.001110f,0,2,0,2}};static
int q31=(sizeof qtrick/sizeof(struct Q27));static struct Q27
 Q32[]={{5.128189f,0,0,0,1},{0.280606f,0,1,0,1},{0.277693f,0
,1,0,-1},{0.173238f,0,0,2,-1},{0.055413f,0,-1,2,1},{
0.046272f,0,-1,2,-1},{0.032573f,0,0,2,1},{0.017198f,0,2,0,1}
,{0.009267f,0,1,2,-1},{0.008823f,0,2,0,-1},{0.008247f,-1,0,2
,-1},{0.004323f,0,-2,2,-1},{0.004200f,0,1,2,1},{0.003372f,-1
,0,-2,1},{0.002472f,-1,-1,2,1},{0.002222f,-1,0,2,1},{
0.002072f,-1,-1,2,-1},{0.001877f,-1,1,0,1},{0.001828f,0,-1,4
,-1},{-0.001803f,1,0,0,1},{-0.001750f,0,0,0,3},{0.001570f,-1
,1,0,-1},{-0.001487f,0,0,1,1},{-0.001481f,1,1,0,1},{
0.001417f,-1,-1,0,1},{0.001350f,-1,0,0,1},{0.001330f,0,0,-1,
1},{0.001106f,0,3,0,1},{0.001020f,0,0,4,-1}};static int
QHINT=(sizeof Q32/sizeof(struct Q27));static struct Q27 Q33[
]={{0.950724f,0,0,0,0},{0.051818f,0,1,0,0},{0.009531f,0,-1,2
,0},{0.007843f,0,0,2,0},{0.002824f,0,2,0,0}};static int Q34=
(sizeof Q33/sizeof(struct Q27));q4=(float)(qfoo-1900);
qfoobar=qfoo>=4?qfoo%4:3-(-qfoo-1)%4;qfOBAz=((float)(4*(qbar
-1/(qfoobar+1))-qfoobar-2)+(4.0f*qbaz))/1461.0f;qfoobaz=q4+
qfOBAz;QQUUX=qfobar*(float)dmod((double)(q20+QSPEED*qfOBAz+
qIndex*qfoobaz),360.0);Q5=qfobar*(float)dmod((double)(Q21+
q22*qfoobaz),360.0);QFRED=qfobar*(float)dmod((double)(q23+
qemacs*qfOBAz+q24*qfoobaz),360.0);qdog=qfobar*(float)dmod((
double)(QVI+QfbI*qfOBAz+Qcia*qfoobaz),360.0);qcat=qfobar*(
float)dmod((double)(Q25+QNASA*qfOBAz+QERR*qfoobaz),360.0);
QFISH=0.0f;QgASp=0.0f;for(Q3=q31-1;Q3>=0;Q3--){qBuG=qtrick[
Q3].Q28;Q6=(float)qtrick[Q3].qgoogle;q7=(float)qtrick[Q3].
q29;q8=(float)qtrick[Q3].QYahoO;QBAD=(float)qtrick[Q3].Q30;
qsilly=Q6*Q5+q7*QFRED+q8*qdog+QBAD*qcat;QFISH+=qBuG*((float)
sin((double)qsilly));QgASp+=qBuG*((float)cos((double)qsilly)
)*(Q6*qbill+q7*qjoe+q8*qrms+QBAD*Q26);}QBUGGY=QQUUX+qfobar*
QFISH;QMUM=q1*(qobSCUrE/qfobar+QgASp);QFISH=0.0f;QgASp=0.0f;
for(Q3=QHINT-1;Q3>=0;Q3--){qBuG=Q32[Q3].Q28;Q6=(float)Q32[Q3
].qgoogle;q7=(float)Q32[Q3].q29;q8=(float)Q32[Q3].QYahoO;
QBAD=(float)Q32[Q3].Q30;qsilly=Q6*Q5+q7*QFRED+q8*qdog+QBAD*
qcat;QFISH+=qBuG*((float)sin((double)qsilly));QgASp+=qBuG*((
float)cos((double)qsilly))*(Q6*qbill+q7*qjoe+q8*qrms+QBAD*
Q26);}qDAd=qfobar*QFISH;q9=q1*QgASp;QFISH=0.0f;QgASp=0.0f;
for(Q3=Q34-1;Q3>=0;Q3--){qBuG=Q33[Q3].Q28;Q6=(float)Q33[Q3].
qgoogle;q7=(float)Q33[Q3].q29;q8=(float)Q33[Q3].QYahoO;QBAD=
(float)Q33[Q3].Q30;qsilly=Q6*Q5+q7*QFRED+q8*qdog+QBAD*qcat;
QFISH+=qBuG*((float)cos((double)qsilly));QgASp+=qBuG*(-(
float)sin((double)qsilly))*(Q6*qbill+q7*qjoe+q8*qrms+QBAD*
Q26);}p=qfobar*QFISH;Q10=q1*QgASp;Q11=(float)sin((double)p);
q12=q2/Q11;Q13=-q12*Q10*(float)(cos((double)p))/Q11;q18=(
float)sin((double)QBUGGY);QFULL=(float)cos((double)QBUGGY);
qfast=(float)sin((double)qDAd);qsmall=(float)cos((double)
qDAd);QBIG=q12*qsmall;QOK=q12*q9;QHELLO=QOK*qfast-qsmall*Q13
;Q14=QBIG*QFULL;qdisk=QBIG*q18;Q15=q12*qfast;q16=-qdisk*QMUM
-QHELLO*QFULL;q17=Q14*QMUM-QHELLO*q18;QEMPTY=QOK*qsmall+
qfast*Q13;QBYE=qfobar*(23.45229f-0.00013f*qfoobaz);QMAGIC=(
float)sin((double)QBYE);q19=(float)cos((double)QBYE);Q0[0]=
Q14;Q0[1]=qdisk*q19-Q15*QMAGIC;Q0[2]=qdisk*QMAGIC+Q15*q19;Q0
[3]=q16;Q0[4]=q17*q19-QEMPTY*QMAGIC;Q0[5]=q17*QMAGIC+QEMPTY*
q19;}

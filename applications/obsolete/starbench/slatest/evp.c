/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmevp(double qfoo,double qbar,double qbaz[3],double Q0
[3],double qfobar[3],double q1[3]){int q2,qfoobar,Q3,q4;
double qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,
Q6,q7,q8,phi,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,
q12,Q13,Q14,qdisk,Q15,q16,q17,QEMPTY;double q18,QFULL,qfast=
0.0,qsmall,QBIG,QOK,QHELLO,QBYE,QMAGIC,q19,q20,qobSCUrE,
QSPEED,qIndex,Q21,qbill,q22,q23,qjoe,qemacs,q24,QVI,qrms,
QfbI,Qcia,Q25,Q26,QNASA,QERR,Q27,Q28,qgoogle,q29,QYahoO,Q30,
qtrick,q31;double Q32[4],QHINT[7],Q33[17],Q34[4],QBLAcK[4];
double Q35[3][3],q36,q37[3];static double q38=1.990987e-7;
static double q39=1.990969e-7;static double qred=3.122140e-5
;static double QgreEN=2.661699e-6;static double QYELLOW=
2.399485e-7;static double q40=1949.9997904423;static double
QBLUE[4]={8.326827e-11,1.843484e-11,1.988712e-12,
1.881276e-12};static double QMAGENTA=0.99999696;static
double QCyaN[4]={4.960906e-3,2.727436e-3,8.392311e-4,
1.556861e-3};static double Q41=8.978749e-2;static double
QWHITE[3][8]={{1.7400353,6.2565836,4.7199666,1.9636505e-1,
4.1547339,4.6524223,4.2620486,1.4740694},{6.2833195099091e+2
,6.2830194572674e+2,8.3997091449254e+3,8.4334662911720e+3,
5.2993466764997e+1,2.1354275911213e+1,7.5025342197656,
3.8377331909193},{5.2796e-6,-2.6180e-6,-1.9780e-5,-5.6044e-5
,5.8845e-6,5.6797e-6,5.5317e-6,5.6093e-6}};static double
qclinton[3]={4.093198e-1,-2.271110e-4,-2.860401e-8};static
double q42[3][17]={{1.675104e-2,2.220221e-1,1.589963,
2.994089,8.155457e-1,1.735614,1.968564,1.282417,2.280820,
4.833473e-2,5.589232e-2,4.634443e-2,8.997041e-3,2.284178e-2,
4.350267e-2,1.348204e-2,3.106570e-2},{-4.179579e-5,
2.809917e-2,3.418075e-2,2.590824e-2,2.486352e-2,1.763719e-2,
1.524020e-2,8.703393e-3,1.918010e-2,1.641773e-4,-3.455092e-4
,-2.658234e-5,6.329728e-6,-9.941590e-5,-6.839749e-5,
1.091504e-5,-1.665665e-4},{-1.260516e-7,1.852532e-5,
1.430200e-5,4.155840e-6,6.836840e-6,6.370440e-6,-2.517152e-6
,2.289292e-5,4.484520e-6,-4.654200e-7,-7.388560e-7,
7.757000e-8,-1.939256e-9,6.787400e-8,-2.714956e-7,
6.903760e-7,-1.590188e-7}};static double qbUsh[2][15]={{
5.0974222,3.9584962,1.6338070,2.5487111,4.9255514,1.3363463,
1.6072053,1.3629480,5.5657014,5.0708205,3.9318944,4.8989497,
1.3097446,3.5147141,3.5413158},{-7.8604195454652e+2,-
5.7533848094674e+2,-1.1506769618935e+3,-3.9302097727326e+2,-
5.8849265665348e+2,-5.5076098609303e+2,-5.2237501616674e+2,-
1.1790629318198e+3,-1.0977134971135e+3,-1.5774000881978e+2,
5.2963464780000e+1,3.9809289073258e+1,7.7540959633708e+1,
7.9618578146517e+1,-5.4868336758022e+2}};static double q43[5
][15]={{-2.279594e-5,-3.494537e-5,6.593466e-7,1.140767e-5,
9.516893e-6,7.310990e-6,-2.603449e-6,-3.228859e-6,
3.442177e-7,8.702406e-6,-1.488378e-6,-8.043059e-6,
3.699128e-6,2.550120e-6,-6.351059e-7},{1.407414e-5,
2.860401e-7,1.322572e-5,-2.049792e-5,-2.748894e-6,-
1.924710e-6,7.359472e-6,1.308997e-7,2.671323e-6,-8.421214e-6
,-1.251789e-5,-2.991300e-6,-3.316126e-6,-1.241123e-6,
2.341650e-6},{8.273188e-6,1.289448e-7,9.258695e-6,-
4.747930e-6,-1.319381e-6,-8.772849e-7,3.168357e-6,
1.013137e-7,1.832858e-6,-1.372341e-6,5.226868e-7,1.473654e-7
,2.901257e-7,9.901116e-8,1.061492e-6},{1.340565e-5,
1.627237e-5,-4.674248e-7,-2.638763e-6,-4.549908e-6,-
3.334143e-6,1.119056e-6,2.403899e-6,-2.394688e-7,-
1.455234e-6,-2.049301e-7,-3.154542e-7,3.407826e-7,
2.210482e-7,2.878231e-7},{-2.490817e-7,-1.823138e-7,-
3.646275e-7,-1.245408e-7,-1.864821e-7,-1.745256e-7,-
1.655307e-7,-3.736225e-7,-3.478444e-7,-4.998479e-8,0.0,0.0,
0.0,0.0,0.0}};static double Q44=-7.757020e-8;static double
Q45[3][4]={{1.289600e-6,3.102810e-5,9.124190e-6,9.793240e-7}
,{5.550147e-1,4.035027,9.990265e-1,5.508259},{2.076942,
3.525565e-1,2.622706,1.559103e+1}};static double q46[2][3]={
{5.167983,5.491315,5.959853},{8.3286911095275e+3,-
7.2140632838100e+3,1.5542754389685e+4}};static double qJFK[4
][3]={{1.097594e-1,-2.223581e-2,1.148966e-2},{2.896773e-7,
5.083103e-8,5.658888e-8},{5.450474e-2,1.002548e-2,
8.249439e-3},{1.438491e-7,-2.291823e-8,4.063015e-8}};q2=(
qbar<=0.0)?0:1;q18=(qfoo-15019.5)/36525.0;for(q4=0;q4<8;q4++
){QFULL=dmod(QWHITE[0][q4]+q18*(QWHITE[1][q4]+q18*QWHITE[2][
q4]),D2PI);if(q4==0){qfast=QFULL;}else{QHINT[q4-1]=QFULL;}}
qsmall=dmod(qclinton[0]+q18*(qclinton[1]+q18*qclinton[2]),
D2PI);for(q4=0;q4<17;q4++){Q33[q4]=dmod(q42[0][q4]+q18*(q42[
1][q4]+q18*q42[2][q4]),D2PI);}for(q4=0;q4<4;q4++){qfOBAz=
dmod(Q45[1][q4]+q18*Q45[2][q4],D2PI);Q32[q4]=sin(qfOBAz);}
qfoobaz=Q45[0][0]*Q32[0]+Q45[0][1]*Q32[1]+(Q45[0][2]+q18*Q44
)*Q32[2]+Q45[0][3]*Q32[3];QQUUX=0.0;Q5=0.0;QFRED=0.0;for(q4=
0;q4<15;q4++){qfOBAz=dmod(qbUsh[0][q4]+q18*qbUsh[1][q4],D2PI
);qdog=cos(qfOBAz);qcat=sin(qfOBAz);qfoobaz+=q43[0][q4]*qdog
+q43[1][q4]*qcat;Q5+=q43[2][q4]*qdog+q43[3][q4]*qcat;if(q4<
10){QQUUX+=(q43[1][q4]*qdog-q43[0][q4]*qcat)*q43[4][q4];
QFRED+=(q43[3][q4]*qdog-q43[2][q4]*qcat)*q43[4][q4];}}QFISH=
Q33[0];QgASp=QFISH+QFISH;Q6=QFISH*QFISH;QBIG=1.0-Q6;q7=QHINT
[0];q8=q7+q7;phi=QgASp*((1.0-Q6/8.0)*sin(q7)+5.0*QFISH*sin(
q8)/8.0+13.0*Q6*sin(q7+q8)/24.0);QBAD=QHINT[0]+phi;qBuG=sin(
QBAD);qsilly=cos(QBAD);QOK=QBIG/(1.0+QFISH*qsilly);QBUGGY=
QgASp*q39*((1.0+Q6*1.5)*qsilly+QFISH*(1.25-qBuG*qBuG/2.0));
QMUM=q39*QFISH*qBuG/sqrt(QBIG);QHELLO=1.0+Q5;QBYE=QHELLO*(
QMUM+QOK*QFRED);QMAGIC=QHELLO*QOK*(q38+QBUGGY+QQUUX);q19=
dmod(qfast+phi+qfoobaz,D2PI);q20=sin(q19);qobSCUrE=cos(q19);
QSPEED=QBYE*qobSCUrE-QMAGIC*q20;qIndex=QBYE*q20+QMAGIC*
qobSCUrE;qfoobaz=0.0;QQUUX=0.0;qDAd=0.0;q9=0.0;for(q4=0;q4<3
;q4++){qfOBAz=dmod(q46[0][q4]+q18*q46[1][q4],D2PI);qcat=sin(
qfOBAz);qdog=cos(qfOBAz);qfoobaz+=qJFK[0][q4]*qcat;QQUUX+=
qJFK[1][q4]*qdog;qDAd+=qJFK[2][q4]*qdog;q9+=-qJFK[3][q4]*
qcat;}Q10=QHINT[1]+qfoobaz;Q11=sin(Q10);q12=cos(Q10);Q13=
qred/(1.0+qDAd);qfOBAz=Q13*(QgreEN+QQUUX);Q14=Q13*q9;QSPEED
+=qfOBAz*Q11+Q14*q12;qIndex+=-qfOBAz*q12+Q14*Q11;Q21=-Q13*
QYELLOW*cos(QHINT[2]);qbill=QSPEED*QMAGENTA;q22=qIndex*
QMAGENTA;q23=Q21*QMAGENTA;for(q4=0;q4<4;q4++){qdisk=QHINT[q4
+3];Q15=Q33[q4+1];q16=Q33[q4+9];Q10=dmod(qdisk+2.0*q16*sin(
qdisk-Q15),D2PI);Q34[q4]=sin(Q10);QBLAcK[q4]=cos(Q10);qbill
+=QBLUE[q4]*(Q34[q4]+q16*sin(Q15));q22+=-QBLUE[q4]*(QBLAcK[
q4]+q16*cos(Q15));q23+=-QBLUE[q4]*Q33[q4+13]*cos(qdisk-Q33[
q4+5]);}qjoe=cos(qsmall);qemacs=sin(qsmall);q24=qjoe*qIndex-
qemacs*Q21;QVI=qemacs*qIndex+qjoe*Q21;qrms=qjoe*q22-qemacs*
q23;QfbI=qemacs*q22+qjoe*q23;Qcia=QOK*QHELLO;q17=Q41*sin(
QHINT[2]);qfOBAz=Q13*cos(q17);Q25=Qcia*qobSCUrE-qfOBAz*q12;
Q26=Qcia*q20-qfOBAz*Q11;QNASA=-Q13*sin(q17);QERR=Q25*
QMAGENTA;Q27=Q26*QMAGENTA;Q28=QNASA*QMAGENTA;for(q4=0;q4<4;
q4++){QEMPTY=Q33[q4+13]*sin(QHINT[q4+3]-Q33[q4+5]);qfOBAz=
QCyaN[q4]*(1.0-Q33[q4+9]*cos(QHINT[q4+3]-Q33[q4+1]));Q14=
qfOBAz*cos(QEMPTY);QERR-=Q14*QBLAcK[q4];Q27-=Q14*Q34[q4];Q28
-=qfOBAz*sin(QEMPTY);}qgoogle=qjoe*Q26-qemacs*QNASA;q29=
qemacs*Q26+qjoe*QNASA;QYahoO=qjoe*Q27-qemacs*Q28;Q30=qemacs*
Q27+qjoe*Q28;qtrick=sbmepj(qfoo);q31=DS2R*(0.035+(0.00085*(
qtrick-q40)));qfobar[0]=QSPEED-q31*q24;qfobar[1]=q24+q31*
QSPEED;qfobar[2]=QVI;qbaz[0]=qbill-q31*qrms;qbaz[1]=qrms+q31
*qbill;qbaz[2]=QfbI;q1[0]=Q25-q31*qgoogle;q1[1]=qgoogle+q31*
Q25;q1[2]=q29;Q0[0]=QERR-q31*QYahoO;Q0[1]=QYahoO+q31*QERR;Q0
[2]=Q30;if(q2!=0){sbmprec(qtrick,qbar,Q35);for(Q3=0;Q3<3;Q3
++){q36=0.0;for(qfoobar=0;qfoobar<3;qfoobar++){q36+=Q35[Q3][
qfoobar]*qfobar[qfoobar];}q37[Q3]=q36;}for(Q3=0;Q3<3;Q3++){
qfobar[Q3]=q37[Q3];}for(Q3=0;Q3<3;Q3++){q36=0.0;for(qfoobar=
0;qfoobar<3;qfoobar++){q36+=Q35[Q3][qfoobar]*qbaz[qfoobar];}
q37[Q3]=q36;}for(Q3=0;Q3<3;Q3++){qbaz[Q3]=q37[Q3];}for(Q3=0;
Q3<3;Q3++){q36=0.0;for(qfoobar=0;qfoobar<3;qfoobar++){q36+=
Q35[Q3][qfoobar]*q1[qfoobar];}q37[Q3]=q36;}for(Q3=0;Q3<3;Q3
++){q1[Q3]=q37[Q3];}for(Q3=0;Q3<3;Q3++){q36=0.0;for(qfoobar=
0;qfoobar<3;qfoobar++){q36+=Q35[Q3][qfoobar]*Q0[qfoobar];}
q37[Q3]=q36;}for(Q3=0;Q3<3;Q3++){Q0[Q3]=q37[Q3];}}}

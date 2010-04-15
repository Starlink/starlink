/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpertue(double qfoo,double qbar[],int*qbaz)
#define Q0 1.0
#define qfobar 0.0001
#define q1 1e-4
#define q2 0.01
#define qfoobar 10.0
#define Q3 5.0
#define q4 50.0
#define qfOBAz 1e-6
#define qfoobaz 100.0
#define QQUUX 0.01720209895
#define Q5 (QQUUX*QQUUX)
{double QFRED;double qdog[13];double qcat;double QFISH;
double QgASp;double Q6;int q7;double q8,QBAD[3],qBuG[3],
qsilly[3];double QBUGGY,QMUM;double qDAd;double q9=0.0;int
Q10;double Q11[8][13];double q12=0.0;double Q13[8][6];double
 Q14[3],qdisk[3],Q15[3],q16[3];double q17[6],QEMPTY[3];
double q18[3][3];double QFULL[8],qfast[8],qsmall,QBIG;double
 QOK[6],QHELLO[6];double QBYE,QMAGIC;double q19[3],q20[3];
double qobSCUrE[3],QSPEED;double qIndex[3],Q21;int qbill,q22
,q23,qjoe;double qemacs,q24,QVI,qrms,QfbI,Qcia;static double
 Q25[]={6023600.0,408523.5,328900.5,3098710.0,1047.355,
3498.5,22869.0,19314.0,332946.038,27068709.0};*qbaz=0;QFRED=
qfoo;for(qbill=0;qbill<13;qbill++){qdog[qbill]=qbar[qbill];}
qcat=qdog[2];QFISH=QFRED-qcat;QgASp=QFISH;if(fabs(QFISH)>
36525.0)*qbaz=101;Q6=dsign(1.0,QFISH);q8=0.0;for(qbill=0;
qbill<3;qbill++){QBAD[qbill]=0.0;qBuG[qbill]=0.0;qsilly[
qbill]=0.0;}q7=TRUE;while(Q6*QgASp>0.0){if(q7){QBUGGY=q2;}
else{qemacs=0.0;for(qbill=0;qbill<3;qbill++){q24=q20[qbill];
qemacs+=q24*q24;}q24=sqrt(qemacs);if(q24!=0.0){QBUGGY=q1/q24
;if(QBUGGY>qfoobar){QBUGGY=qfoobar;}else if(QBUGGY<q2){
QBUGGY=q2;}}else{QBUGGY=qfoobar;}}QBUGGY*=Q6;QgASp=QFISH-q8;
if(fabs(QBUGGY)>fabs(QgASp))QBUGGY=QgASp;QMUM=QBUGGY/2.0;
qDAd=qcat+q8+QMUM;if(q7||(fabs(qDAd-q9)-q4)>=qfOBAz){q9=qDAd
+Q6*q4;for(Q10=1;Q10<=8;Q10++){q23=Q10-1;sbmplanet(q9,Q10,
QHELLO,&q22);if(q22==1){*qbaz=102;}else if(q22){*qbaz=-1;
return;}sbmpv2ue(QHELLO,q9,0.0,Q11[q23],&q22);if(q22){*qbaz=
-1;return;}}}if(q7||(fabs(qDAd-q12)-Q3)>=qfOBAz){q12=qDAd+Q6
*Q3;for(Q10=1;Q10<=8;Q10++){q23=Q10-1;sbmue2pv(q12,Q11[q23],
Q13[q23],&q22);if(q22){*qbaz=-1;return;}for(q22=3;q22<6;q22
++){Q13[q23][q22]*=86400.0;}qemacs=0.0;for(qbill=0;qbill<3;
qbill++){q24=Q13[q23][qbill];qemacs+=q24*q24;}QFULL[q23]=
qemacs*3.0;qfast[q23]=Q5/(2.0*qemacs*sqrt(qemacs));}}q7=
FALSE;sbmue2pv(qDAd,qdog,QOK,&q22);if(q22){*qbaz=-1;return;}
qemacs=0.0;for(qbill=0;qbill<3;qbill++){q24=QOK[qbill]+QBAD[
qbill]+(qBuG[qbill]+qsilly[qbill]*QMUM/2.0)*QMUM;QHELLO[
qbill]=q24;qemacs+=q24*q24;}QMAGIC=qemacs*sqrt(qemacs);
qemacs=0.0;for(qbill=0;qbill<3;qbill++){q24=QOK[qbill];
qemacs+=q24*q24;}QBYE=qemacs*sqrt(qemacs);for(qbill=0;qbill<
3;qbill++){q19[qbill]=QOK[qbill]/QBYE-QHELLO[qbill]/QMAGIC;
q20[qbill]=0.0;}qjoe=FALSE;QVI=qDAd-q12;qrms=QVI*QVI;for(Q10
=1;Q10<=10;Q10++){q23=Q10-1;if(Q10<=8){qemacs=0.0;for(q22=3;
q22<6;q22++){q24=Q13[q23][q22]*QVI;qemacs+=q24*q24;}qsmall=
1.0+qemacs/QFULL[q23];QBIG=1.0-qfast[q23]*qrms;for(qbill=0;
qbill<3;qbill++){qobSCUrE[qbill]=QBIG*(Q13[q23][qbill]+
qsmall*Q13[q23][qbill+3]*QVI);}}else if(qjoe){if(Q10==9){
sbmepv(qDAd,q16,Q15,qdisk,Q14);for(qbill=0;qbill<3;qbill++){
qobSCUrE[qbill]=q16[qbill];}}else{sbmprec(sbmepj(qDAd),
2000.0,q18);sbmdmoon(qDAd,q17);sbmdmxv(q18,q17,QEMPTY);for(
qbill=0;qbill<3;qbill++){qobSCUrE[qbill]=QEMPTY[qbill]+q16[
qbill];}}}if(Q10<=8||qjoe){qemacs=0.0;for(qbill=0;qbill<3;
qbill++){q24=qobSCUrE[qbill];qemacs+=q24*q24;}QfbI=sqrt(
qemacs);QSPEED=qemacs*QfbI;qemacs=0.0;for(qbill=0;qbill<3;
qbill++){q24=qobSCUrE[qbill]-QHELLO[qbill];qIndex[qbill]=q24
;qemacs+=q24*q24;}QfbI=sqrt(qemacs);if(Q10==3&&QfbI<Q0)qjoe=
TRUE;if(!(qjoe&&(Q10==3))){if(QfbI<qfobar){*qbaz=Q10;}else{
Q21=qemacs*QfbI;q24=Q25[q23];for(qbill=0;qbill<3;qbill++){
q20[qbill]+=(qIndex[qbill]/Q21-qobSCUrE[qbill]/QSPEED)/q24;}
}}}}q8=q8+QBUGGY;for(qbill=0;qbill<3;qbill++){q24=(q19[qbill
]+q20[qbill])*Q5;Qcia=q24*QBUGGY;QBAD[qbill]+=(qBuG[qbill]+
Qcia/2.0)*QBUGGY;qBuG[qbill]+=Qcia;qsilly[qbill]=q24;}QgASp=
QFISH-q8;if(fabs(q8)>=qfoobaz||(Q6*QgASp)<=0.0){qcat+=q8;q8=
0.0;sbmue2pv(qcat,qdog,QOK,&q22);if(q22){*qbaz=-1;return;}
for(qbill=0;qbill<3;qbill++){q22=qbill+3;QHELLO[qbill]=QOK[
qbill]+QBAD[qbill];QHELLO[q22]=QOK[q22]+qBuG[qbill]/86400.0;
QBAD[qbill]=0.0;qBuG[qbill]=0.0;qsilly[qbill]=q20[qbill]*Q5;
}sbmpv2ue(QHELLO,qcat,0.0,qdog,&q22);if(q22){*qbaz=-1;return
;}QFISH=QFRED-qcat;QgASp=QFISH;}}for(qbill=0;qbill<13;qbill
++){qbar[qbill]=qdog[qbill];}}

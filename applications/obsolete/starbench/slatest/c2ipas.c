/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmc2ipas(double qfoo,double qbar[2][3],double qbaz,
double Q0,CIpars*qfobar,double*q1)
#define q2 (149.597870691e6)
#define qfoobar (q2/86400.0)
#define Q3 (499.004782/86400.0)
#define q4 (2.0*9.87063e-9)
{int qfOBAz;double qfoobaz[3],QQUUX[3],Q5[3],QFRED[3],qdog,
qcat,QFISH,QgASp[3],Q6,q7,phi,q8,QBAD,qBuG,qsilly,QBUGGY[3][
3],QMUM,qDAd,q9;qfobar->pmt=sbmepj(qfoo)-2000.0;sbmepv(qfoo,
qfoobaz,QQUUX,Q5,QFRED);for(qfOBAz=0;qfOBAz<3;qfOBAz++){qdog
=qbar[0][qfOBAz]/q2;qcat=qbar[1][qfOBAz]/qfoobar;Q5[qfOBAz]
+=qdog;QFRED[qfOBAz]+=qcat;qfoobaz[qfOBAz]+=qdog;}for(qfOBAz
=0;qfOBAz<3;qfOBAz++){qfobar->eb[qfOBAz]=Q5[qfOBAz];}sbmdvn(
qfoobaz,qfobar->ehn,&QFISH);qfobar->gr2e=q4/QFISH;for(qfOBAz
=0;qfOBAz<3;qfOBAz++){qfobar->abv[qfOBAz]=QFRED[qfOBAz]*Q3;}
sbmdvn(qfobar->abv,QgASp,&Q6);qfobar->ab1=sqrt(1.0-Q6*Q6);
sbmpfw(qfoo,&q7,&phi,&q8,&QBAD);sbmnu(qfoo,&qBuG,&qsilly);q8
+=qBuG;QBAD+=qsilly;sbmfw2m(q7,phi,q8,QBAD,QBUGGY);QMUM=
QBUGGY[2][0];qDAd=QBUGGY[2][1];q9=sbms(qfoo,QMUM,qDAd);*q1=
sbmeors(QBUGGY,q9);sbmg2ixys(QMUM+qbaz,qDAd+Q0,q9,qfobar->
bpn);}

/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmapqk(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double q2[21],double*qfoobar,double*
Q3)
#define q4 0.21094502
{int qfOBAz;double pmt,gr2e,ab1,eb[3],ehn[3],abv[3],qfoobaz[
3],QQUUX,Q5,QFRED[3],p[3],qdog[3],qcat,QFISH,QgASp[3],Q6,q7[
3],q8[3];pmt=q2[0];gr2e=q2[7];ab1=q2[11];for(qfOBAz=0;qfOBAz
<3;qfOBAz++){eb[qfOBAz]=q2[qfOBAz+1];ehn[qfOBAz]=q2[qfOBAz+4
];abv[qfOBAz]=q2[qfOBAz+8];}sbmdcs2c(qfoo,qbar,qfoobaz);
QQUUX=qfobar*DAS2R;Q5=q4*q1*QQUUX;QFRED[0]=(-qbaz*qfoobaz[1]
)-(Q0*cos(qfoo)*sin(qbar))+(Q5*qfoobaz[0]);QFRED[1]=(qbaz*
qfoobaz[0])-(Q0*sin(qfoo)*sin(qbar))+(Q5*qfoobaz[1]);QFRED[2
]=(Q0*cos(qbar))+(Q5*qfoobaz[2]);for(qfOBAz=0;qfOBAz<3;
qfOBAz++){p[qfOBAz]=qfoobaz[qfOBAz]+(pmt*QFRED[qfOBAz])-(
QQUUX*eb[qfOBAz]);}sbmdvn(p,qdog,&Q5);qcat=sbmdvdv(qdog,ehn)
;QFISH=1.0+qcat;Q5=gr2e/gmax(QFISH,1.0e-5);for(qfOBAz=0;
qfOBAz<3;qfOBAz++){QgASp[qfOBAz]=qdog[qfOBAz]+(Q5*(ehn[
qfOBAz]-qcat*qdog[qfOBAz]));}Q6=sbmdvdv(QgASp,abv);Q5=1.0+Q6
/(ab1+1.0);for(qfOBAz=0;qfOBAz<3;qfOBAz++){q7[qfOBAz]=ab1*
QgASp[qfOBAz]+Q5*abv[qfOBAz];}sbmdmxv((double(*)[3])&q2[12],
q7,q8);sbmdcc2s(q8,qfoobar,Q3);*qfoobar=sbmdranrm(*qfoobar);
}

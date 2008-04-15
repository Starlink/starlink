/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmi2opa(double qfoo,double qbar,double qbaz,double phi
,double hm,double Q0,double qfobar,double tk,double q1,
double rh,double wl,double tlr,IOpars*q2)
#define qfoobar 173.14463331
#define Q3 1.00273781191135448
{double q4,qfOBAz,qfoobaz,QQUUX;q2->along=qbaz+sbmsp(qfoo)+
qbar*Q3*DS2R;q2->phi=phi;q2->hm=hm;q4=sin(qbaz);qfOBAz=cos(
qbaz);q2->xpl=Q0*qfOBAz-qfobar*q4;q2->ypl=Q0*q4+qfobar*
qfOBAz;q2->sphi=sin(phi);q2->cphi=cos(phi);sbmgeoc(phi,hm,&
qfoobaz,&QQUUX);q2->diurab=D2PI*qfoobaz*Q3/qfoobar;q2->p=q1;
q2->tk=tk;q2->rh=rh;q2->tlr=tlr;q2->wl=wl;sbmrefco(q2->hm,q2
->tk,q2->p,q2->rh,q2->wl,q2->phi,q2->tlr,1e-10,&q2->refa,&q2
->refb);sbmi2opat(qfoo,q2);}

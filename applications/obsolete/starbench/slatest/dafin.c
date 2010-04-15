/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdafin(char*qfoo,int*qbar,double*qbaz,int*Q0){int
qfobar,q1,q2,qfoobar;double Q3,q4,qfOBAz;q1=0;qfOBAz=0.0;q4=
0.0;Q3=0.0;sbmdfltin(qfoo,qbar,&qfOBAz,&qfobar);if(qfobar>1)
{q1=-1;}else{sbmdfltin(qfoo,qbar,&q4,&q2);if(q2<0||q2>1){q1=
-2;}else{sbmdfltin(qfoo,qbar,&Q3,&qfoobar);if(qfoobar<0||
qfoobar>1){q1=-3;}else if(qfobar>0){if(q2==0){q1=-2;}else if
(qfoobar==0){q1=-3;}else{q1=1;}}else if(q2!=0&&qfoobar==0){
q1=-3;}else if(q2==0&&dint(qfOBAz)!=qfOBAz){q1=-1;}else if((
qfoobar==0&&dint(q4)!=q4)||q4>=60.0){q1=-2;}else if(Q3>=60.0
){q1=-3;}}}if(q1<=0){*qbaz=((fabs(qfOBAz)*60.0+q4)*60.0+Q3)*
DAS2R;if(qfobar<0){*qbaz=-(*qbaz);}}*Q0=q1;}

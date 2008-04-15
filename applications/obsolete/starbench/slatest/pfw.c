/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpfw(double qfoo,double*qbar,double*qbaz,double*Q0,
double*qfobar)
#define q1 36525.0
#define q2 51544.5
{double qfoobar;qfoobar=(qfoo-q2)/q1;*qbar=(-0.052928+(
10.556378+(0.4932044+(-0.00031238+(-0.000002788+(
0.0000000260)*qfoobar)*qfoobar)*qfoobar)*qfoobar)*qfoobar)*
DAS2R;*qbaz=(84381.412819+(-46.811016+(0.0511268+(0.00053289
+(-0.000000440+(-0.0000000176)*qfoobar)*qfoobar)*qfoobar)*
qfoobar)*qfoobar)*DAS2R;*Q0=(-0.041775+(5038.481484+(
1.5584175+(-0.00018522+(-0.000026452+(-0.0000000148)*qfoobar
)*qfoobar)*qfoobar)*qfoobar)*qfoobar)*DAS2R;*qfobar=(
84381.406+(-46.836769+(-0.0001831+(0.00200340+(-0.000000576+
(-0.0000000434)*qfoobar)*qfoobar)*qfoobar)*qfoobar)*qfoobar)
*DAS2R;}

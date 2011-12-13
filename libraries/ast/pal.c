/*
**  Author:
**    Patrick Wallace  (ptw@tpsoft.demon.co.uk)
**
**  License:
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
**    USA.
**
**  History:
**    14-JUN-2005 (DSB):
**       Corrected palSlaDsepv to fix bug which caused precisely antipodal
**       vectors to return zero instead of pi.
**    16-AUG-2005 (DSB):
**       Added palSlaDh2e and palSlaDe2h by translating the corresponding
**       fortran routines into C (by hand).
**    6-OCT-2006 (DSB):
**       Added palSlaGmsta by translating the corresponding fortran routines
**       into C (by hand).
**    31-AUG-2007 (DSB):
**       Modify palSlaDe2h and palSlaDh2e to apply correction for diurnal
**       aberration.
**    18-JUN-2009 (DSB):
**       Added palSlaPvobs by translating the corresponding fortran routine
**       into C (by hand).
**    5-APR-2011 (DSB):
**       Change palSlaDrange to better handle cases where the supplied
**       value is very close to +/- PI.
*/

#include "pal.h"
#include "slamac.h"

void palSlaAddet(double Q0,double FOo,double q1,double*bAR,
double*q2){double Q3[3];double baz[3];int Q4;palSlaEtrms(q1,Q3)
;palSlaDcs2c(Q0,FOo,baz);for(Q4=0;Q4<3;Q4++){baz[Q4]+=Q3[Q4];}
palSlaDcc2s(baz,bAR,q2);*bAR=palSlaDranrm(*bAR);}

void palSlaAmpqk(double fOo,double bar,double baz[21],double*Q0
,double*FobaR){double Q1;double q2;double foobar[3];double
q3[3];double q4[3],FobAZ[3],q5[3],FOOBAZ[3];double q6,QUUX,
FRED,Q7,q8,Q9;int DOG,CAT;Q1=baz[7];q2=baz[11];for(DOG=0;DOG
<3;DOG++){foobar[DOG]=baz[DOG+4];q3[DOG]=baz[DOG+8];}
palSlaDcs2c(fOo,bar,FOOBAZ);palSlaDimxv((double(*)[3])&baz[12],
FOOBAZ,q5);q6=q2+1.0;for(DOG=0;DOG<3;DOG++){FobAZ[DOG]=q5[
DOG];}for(CAT=0;CAT<2;CAT++){QUUX=palSlaDvdv(FobAZ,q3);FRED=1.0
+QUUX;Q7=1.0+QUUX/q6;for(DOG=0;DOG<3;DOG++){FobAZ[DOG]=(FRED
*q5[DOG]-Q7*q3[DOG])/q2;}palSlaDvn(FobAZ,FOOBAZ,&Q7);for(DOG=0;
DOG<3;DOG++){FobAZ[DOG]=FOOBAZ[DOG];}}for(DOG=0;DOG<3;DOG++)
{q4[DOG]=FobAZ[DOG];}for(CAT=0;CAT<5;CAT++){q8=palSlaDvdv(q4,
foobar);Q9=1.0+q8;Q7=Q9-Q1*q8;for(DOG=0;DOG<3;DOG++){q4[DOG]
=(Q9*FobAZ[DOG]-Q1*foobar[DOG])/Q7;}palSlaDvn(q4,q5,&Q7);for(
DOG=0;DOG<3;DOG++){q4[DOG]=q5[DOG];}}palSlaDcc2s(q4,Q0,FobaR);*
Q0=palSlaDranrm(*Q0);}

void palSlaAoppa(double Q0,double q1,double q2,double Q3,double
 Q4,double Q5,double Q6,double foo,double BAR,double Q7,
double BAZ,double FoBar,double Q8[14])
#define q9 173.14463331
#define Q10 1.00273790935
{double q11,Q12,foobar,FOBAZ,q13,FOOBAZ,quUX,q14,q15,fReD,
DOG;q11=cos(Q3);Q12=cos(q2)*q11;foobar=sin(q2)*q11;FOBAZ=sin
(Q3);q13=Q12-Q5*FOBAZ;FOOBAZ=foobar+Q6*FOBAZ;quUX=Q5*Q12-Q6*
foobar+FOBAZ;q14=(q13!=0.0||FOOBAZ!=0.0)?atan2(FOOBAZ,q13):
0.0;q15=atan2(quUX,sqrt(q13*q13+FOOBAZ*FOOBAZ));Q8[0]=q15;Q8
[1]=sin(q15);Q8[2]=cos(q15);palSlaGeoc(q15,Q4,&fReD,&DOG);Q8[3]
=D2PI*fReD*Q10/q9;Q8[4]=Q4;Q8[5]=foo;Q8[6]=BAR;Q8[7]=Q7;Q8[8
]=BAZ;Q8[9]=FoBar;palSlaRefco(Q4,foo,BAR,Q7,BAZ,q15,FoBar,1e-10
,&Q8[10],&Q8[11]);Q8[12]=q14+palSlaEqeqx(Q0)+q1*Q10*DS2R;
palSlaAoppat(Q0,Q8);}
#undef q9
#undef Q10

void palSlaAoppat(double Q0,double FOO[14]){FOO[13]=palSlaGmst(Q0)
+FOO[12];}

void palSlaCaldj(int FOO,int Q0,int BAr,double*Q1,int*baz){int
q2;if((FOO>=0)&&(FOO<=49))q2=FOO+2000;else if((FOO>=50)&&(
FOO<=99))q2=FOO+1900;else q2=FOO;palSlaCldj(q2,Q0,BAr,Q1,baz);}

void palSlaCldj(int Q0,int foo,int Q1,double*q2,int*Q3){long Q4
,q5;static int bar[12]={31,28,31,30,31,30,31,31,30,31,30,31}
;if(Q0<-4699){*Q3=1;return;}if((foo<1)||(foo>12)){*Q3=2;
return;}bar[1]=(((Q0%4)==0)&&(((Q0%100)!=0)||((Q0%400)==0)))
?29:28;*Q3=(Q1<1||Q1>bar[foo-1])?3:0;Q4=(long)Q0;q5=(long)
foo;*q2=(double)((1461L*(Q4-(12L-q5)/10L+4712L))/4L+(306L*((
q5+9L)%12L)+5L)/10L-(3L*((Q4-(12L-q5)/10L+4900L)/100L))/4L+(
long)Q1-2399904L);}

void palSlaCs2c(float q0,float FOO,float q1[3]){float BAR;BAR=(
float)cos(FOO);q1[0]=(float)cos(q0)*BAR;q1[1]=(float)sin(q0)
*BAR;q1[2]=(float)sin(FOO);}

void palSlaDaf2r(int Q0,int FOO,double Q1,double*bar,int*baz){*
baz=0;if((Q1<0.0)||(Q1>=60.0)){*baz=3;return;}if((FOO<0)||(
FOO>59)){*baz=2;return;}if((Q0<0)||(Q0>359)){*baz=1;return;}
*bar=DAS2R*(60.0*(60.0*(double)Q0+(double)FOO)+Q1);}

void palSlaDav2m(double q0[3],double q1[3][3]){double FoO,bar,
Q2,baz,FOBAR,FOOBAR,FobAZ;FoO=q0[0];bar=q0[1];Q2=q0[2];baz=
sqrt(FoO*FoO+bar*bar+Q2*Q2);FOBAR=sin(baz);FOOBAR=cos(baz);
FobAZ=1.0-FOOBAR;if(baz!=0.0){FoO=FoO/baz;bar=bar/baz;Q2=Q2/
baz;}q1[0][0]=FoO*FoO*FobAZ+FOOBAR;q1[0][1]=FoO*bar*FobAZ+Q2
*FOBAR;q1[0][2]=FoO*Q2*FobAZ-bar*FOBAR;q1[1][0]=FoO*bar*
FobAZ-Q2*FOBAR;q1[1][1]=bar*bar*FobAZ+FOOBAR;q1[1][2]=bar*Q2
*FobAZ+FoO*FOBAR;q1[2][0]=FoO*Q2*FobAZ+bar*FOBAR;q1[2][1]=
bar*Q2*FobAZ-FoO*FOBAR;q1[2][2]=Q2*Q2*FobAZ+FOOBAR;}

double palSlaDbear(double q0,double FOo,double q1,double BAR){
double BAZ,q2,q3;BAZ=q1-q0;q3=sin(BAZ)*cos(BAR);q2=sin(BAR)*
cos(FOo)-cos(BAR)*sin(FOo)*cos(BAZ);return(q2!=0.0||q3!=0.0)
?atan2(q3,q2):0.0;}

void palSlaDc62s(double FoO[6],double*bar,double*Q0,double*q1,
double*q2,double*baz,double*q3){double q4,FoBAR,foobar,foBaz
,q5,FOOBAZ,Q6,q7,quux,q8;q4=FoO[0];FoBAR=FoO[1];foobar=FoO[2
];foBaz=FoO[3];q5=FoO[4];FOOBAZ=FoO[5];Q6=q4*q4+FoBAR*FoBAR;
if((quux=Q6+foobar*foobar)==0.0){q4=foBaz;FoBAR=q5;foobar=
FOOBAZ;Q6=q4*q4+FoBAR*FoBAR;quux=Q6+foobar*foobar;}q7=sqrt(
Q6);q8=q4*foBaz+FoBAR*q5;if(Q6!=0.0){*bar=atan2(FoBAR,q4);*
Q0=atan2(foobar,q7);*q2=(q4*q5-FoBAR*foBaz)/Q6;*baz=(FOOBAZ*
Q6-foobar*q8)/(quux*q7);}else{*bar=0.0;*Q0=(foobar!=0.0)?
atan2(foobar,q7):0.0;*q2=0.0;*baz=0.0;}*q3=((*q1=sqrt(quux))
!=0.0)?(q8+foobar*FOOBAZ)/(*q1):0.0;}

void palSlaDcc2s(double Q0[3],double*Q1,double*FoO){double bar,
Q2,BAZ,fobar;bar=Q0[0];Q2=Q0[1];BAZ=Q0[2];fobar=sqrt(bar*bar
+Q2*Q2);*Q1=(fobar!=0.0)?atan2(Q2,bar):0.0;*FoO=(BAZ!=0.0)?
atan2(BAZ,fobar):0.0;}

void palSlaDcs2c(double FOO,double baR,double baz[3]){double Q0
;Q0=cos(baR);baz[0]=cos(FOO)*Q0;baz[1]=sin(FOO)*Q0;baz[2]=
sin(baR);}

void palSlaDd2tf(int foo,double q0,char*Q1,int BAR[4])
#define q2 86400.0
{double baz,Q3,FOBAR,q4,q5,Q6,Q7,Q8;*Q1=(char)((q0<0.0)?'-':
'+');baz=pow(10.0,(double)gmax(foo,0));baz=dint(baz);Q3=baz*
60.0;FOBAR=Q3*60.0;q4=baz*q2*fabs(q0);q4=dnint(q4);q5=q4/
FOBAR;q5=dint(q5);q4=q4-q5*FOBAR;Q6=q4/Q3;Q6=dint(Q6);q4=q4-
Q6*Q3;Q7=q4/baz;Q7=dint(Q7);Q8=q4-Q7*baz;BAR[0]=(int)q5;BAR[
1]=(int)Q6;BAR[2]=(int)Q7;BAR[3]=(int)Q8;}
#undef q2

#include <string.h>
void palSlaDeuler(char*Q0,double FoO,double BAR,double Q1,
double baz[3][3]){int fobar,fOObaR,Q2,FoBaz,q3;double q4[3][
3],Q5[3][3],q6,q7,Q8,foobaz,Q9[3][3];char q10;for(fobar=0;
fobar<3;fobar++){for(fOObaR=0;fOObaR<3;fOObaR++){q4[fOObaR][
fobar]=(fOObaR==fobar)?1.0:0.0;}}Q2=strlen(Q0);for(FoBaz=0;
FoBaz<3;FoBaz++){if(FoBaz<=Q2){for(fobar=0;fobar<3;fobar++){
for(fOObaR=0;fOObaR<3;fOObaR++){Q5[fOObaR][fobar]=(fOObaR==
fobar)?1.0:0.0;}}switch(FoBaz){case 0:q6=FoO;break;case 1:q6
=BAR;break;default:q6=Q1;break;}q7=sin(q6);Q8=cos(q6);q10=Q0
[FoBaz];if((q10=='X')||(q10=='x')||(q10=='1')){Q5[1][1]=Q8;
Q5[1][2]=q7;Q5[2][1]=-q7;Q5[2][2]=Q8;}else if((q10=='Y')||(
q10=='y')||(q10=='2')){Q5[0][0]=Q8;Q5[0][2]=-q7;Q5[2][0]=q7;
Q5[2][2]=Q8;}else if((q10=='Z')||(q10=='z')||(q10=='3')){Q5[
0][0]=Q8;Q5[0][1]=q7;Q5[1][0]=-q7;Q5[1][1]=Q8;}else{Q2=0;}
for(fOObaR=0;fOObaR<3;fOObaR++){for(fobar=0;fobar<3;fobar++)
{foobaz=0.0;for(q3=0;q3<3;q3++){foobaz+=Q5[fOObaR][q3]*q4[q3
][fobar];}Q9[fOObaR][fobar]=foobaz;}}for(fobar=0;fobar<3;
fobar++){for(fOObaR=0;fOObaR<3;fOObaR++){q4[fOObaR][fobar]=
Q9[fOObaR][fobar];}}}}for(fobar=0;fobar<3;fobar++){for(
fOObaR=0;fOObaR<3;fOObaR++){baz[fOObaR][fobar]=q4[fOObaR][
fobar];}}}

void palSlaDimxv(double FOO[3][3],double Q0[3],double BAR[3]){
int Q1,baz;double q2,FOBAR[3];for(baz=0;baz<3;baz++){q2=0.0;
for(Q1=0;Q1<3;Q1++){q2+=FOO[Q1][baz]*Q0[Q1];}FOBAR[baz]=q2;}
for(baz=0;baz<3;baz++){BAR[baz]=FOBAR[baz];}}

void palSlaDjcal(int Q0,double Q1,int FOO[4],int*Q2){double q3,
Q4,Q5,Q6;long bar,baz,q7;if((Q1<=-2395520.0)||(Q1>=1.0e9)){*
Q2=-1;return;}else{q3=pow(10.0,(double)gmax(Q0,0));q3=dnint(
q3);Q4=Q1*q3;Q4=dnint(Q4);Q5=dmod(Q4,q3);if(Q5<0.0)Q5+=q3;Q6
=(Q4-Q5)/q3;bar=(long)dnint(Q6)+2400001L;baz=4L*(bar+((2L*((
4L*bar-17918L)/146097L)*3L)/4L+1L)/2L-37L);q7=10L*(((baz-
237L)%1461L)/4L)+5L;FOO[0]=(int)((baz/1461L)-4712L);FOO[1]=(
int)(((q7/306L+2L)%12L)+1L);FOO[2]=(int)((q7%306L)/10L+1L);
FOO[3]=(int)dnint(Q5);*Q2=0;}}

void palSlaDjcl(double q0,int*foo,int*q1,int*bar,double*BAZ,int
*Q2){double q3,Q4;long FOBAR,q5,Q6;if((q0<=-2395520.0)||(q0
>=1e9)){*Q2=-1;return;}else{*Q2=0;q3=dmod(q0,1.0);if(q3<0.0)
q3+=1.0;Q4=q0-q3;Q4=dnint(Q4);FOBAR=(long)dnint(Q4)+2400001;
q5=4L*(FOBAR+((6L*((4L*FOBAR-17918L)/146097L))/4L+1L)/2L-37L
);Q6=10L*(((q5-237L)%1461L)/4L)+5L;*foo=(int)(q5/1461L-4712L
);*q1=(int)(((Q6/306L+2L)%12L)+1L);*bar=(int)((Q6%306L)/10L+
1L);*BAZ=q3;*Q2=0;}}

void palSlaDmat(int FoO,double*bar,double*q0,double*Q1,int*q2,
int*Q3)
#define baz 1e-20
{int Q4,fobar,Q5,FoOBAr,q6;double q7,fobaz,Q8;double*FOOBAZ,
*QuUX,*q9;*q2=0;*Q1=1.0;for(Q4=0,FOOBAZ=bar;Q4<FoO;Q4++,
FOOBAZ+=FoO){q7=fabs(FOOBAZ[Q4]);fobar=Q4;q9=FOOBAZ;if(Q4!=
FoO){for(Q5=Q4+1,QuUX=FOOBAZ+FoO;Q5<FoO;Q5++,QuUX+=FoO){
fobaz=fabs(QuUX[Q4]);if(fobaz>q7){q7=fobaz;fobar=Q5;q9=QuUX;
}}}if(q7<baz){*q2=-1;}else{if(fobar!=Q4){for(FoOBAr=0;FoOBAr
<FoO;FoOBAr++){fobaz=FOOBAZ[FoOBAr];FOOBAZ[FoOBAr]=q9[FoOBAr
];q9[FoOBAr]=fobaz;}fobaz=q0[Q4];q0[Q4]=q0[fobar];q0[fobar]=
fobaz;*Q1=-*Q1;}Q3[Q4]=fobar;*Q1*=FOOBAZ[Q4];if(fabs(*Q1)<
baz){*q2=-1;}else{FOOBAZ[Q4]=1.0/FOOBAZ[Q4];for(FoOBAr=0;
FoOBAr<FoO;FoOBAr++){if(FoOBAr!=Q4){FOOBAZ[FoOBAr]*=FOOBAZ[
Q4];}}Q8=q0[Q4]*FOOBAZ[Q4];q0[Q4]=Q8;for(Q5=0,QuUX=bar;Q5<
FoO;Q5++,QuUX+=FoO){if(Q5!=Q4){for(FoOBAr=0;FoOBAr<FoO;
FoOBAr++){if(FoOBAr!=Q4){QuUX[FoOBAr]-=QuUX[Q4]*FOOBAZ[
FoOBAr];}}q0[Q5]-=QuUX[Q4]*Q8;}}for(Q5=0,QuUX=bar;Q5<FoO;Q5
++,QuUX+=FoO){if(Q5!=Q4){QuUX[Q4]*=-FOOBAZ[Q4];}}}}}if(*q2!=
0){*Q1=0.0;}else{for(Q4=FoO;Q4-->0;){q6=Q3[Q4];if(Q4!=q6){
for(Q5=0,QuUX=bar;Q5<FoO;Q5++,QuUX+=FoO){fobaz=QuUX[Q4];QuUX
[Q4]=QuUX[q6];QuUX[q6]=fobaz;}}}}}
#undef baz

void palSlaDmxm(double q0[3][3],double q1[3][3],double foo[3][3
]){int bar,q2,q3;double q4,Q5[3][3];for(bar=0;bar<3;bar++){
for(q2=0;q2<3;q2++){q4=0.0;for(q3=0;q3<3;q3++){q4+=q0[bar][
q3]*q1[q3][q2];}Q5[bar][q2]=q4;}}for(q2=0;q2<3;q2++){for(bar
=0;bar<3;bar++){foo[bar][q2]=Q5[bar][q2];}}}

void palSlaDmxv(double FOO[3][3],double baR[3],double BAZ[3]){
int Q0,FOBAR;double q1,Q2[3];for(FOBAR=0;FOBAR<3;FOBAR++){q1
=0.0;for(Q0=0;Q0<3;Q0++){q1+=FOO[FOBAR][Q0]*baR[Q0];}Q2[
FOBAR]=q1;}for(FOBAR=0;FOBAR<3;FOBAR++){BAZ[FOBAR]=Q2[FOBAR]
;}}

double palSlaDrange(double fOo){
   while( fOo < -DPI ) fOo += D2PI;
   while( fOo > DPI ) fOo -= D2PI;
   return fOo;
}

double palSlaDranrm(double FoO){double BAR;BAR=dmod(FoO,D2PI);
return(BAR>=0.0)?BAR:BAR+D2PI;}

double palSlaDsep(double FOO,double BAR,double baz,double q0){
double fobar[3],fOobaR[3];palSlaDcs2c(FOO,BAR,fobar);palSlaDcs2c(
baz,q0,fOobaR);return palSlaDsepv(fobar,fOobaR);}

double palSlaDsepv(double foo[3],double Q0[3]){double BAR[3],q1
[3],BAZ,fObaR;palSlaDvxv(foo,Q0,BAR);palSlaDvn(BAR,q1,&BAZ);fObaR=
palSlaDvdv(foo,Q0);return (BAZ!=0.0||fObaR!=0.0)?atan2(BAZ,fObaR):0.0;}

void palSlaDtf2d(int q0,int foo,double q1,double*bar,int*Q2)
#define q3 86400.0
{*Q2=0;if((q1<0.0)||(q1>=60.0)){*Q2=3;return;}if((foo<0)||(
foo>59)){*Q2=2;return;}if((q0<0)||(q0>23)){*Q2=1;return;}*
bar=(60.0*(60.0*(double)q0+(double)foo)+q1)/q3;}
#undef q3

void palSlaDtf2r(int fOo,int BAR,double Baz,double*fobar,int*
fOOBAr){double q0;palSlaDtf2d(fOo,BAR,Baz,&q0,fOOBAr);*fobar=
D2PI*q0;}

double palSlaDvdv(double q0[3],double q1[3]){return q0[0]*q1[0]
+q0[1]*q1[1]+q0[2]*q1[2];}

void palSlaDvn(double fOo[3],double baR[3],double*BAZ){int
FOBAR;double fOOBAr,q0;fOOBAr=0.0;for(FOBAR=0;FOBAR<3;FOBAR
++){q0=fOo[FOBAR];fOOBAr+=q0*q0;}fOOBAr=sqrt(fOOBAr);*BAZ=
fOOBAr;fOOBAr=(fOOBAr>0.0)?fOOBAr:1.0;for(FOBAR=0;FOBAR<3;
FOBAR++){baR[FOBAR]=fOo[FOBAR]/fOOBAr;}}

void palSlaDvxv(double FOO[3],double Q0[3],double q1[3]){double
 q2[3];int BAR;q2[0]=FOO[1]*Q0[2]-FOO[2]*Q0[1];q2[1]=FOO[2]*
Q0[0]-FOO[0]*Q0[2];q2[2]=FOO[0]*Q0[1]-FOO[1]*Q0[0];for(BAR=0
;BAR<3;BAR++){q1[BAR]=q2[BAR];}}

void palSlaEcmat(double foo,double Q0[3][3]){double bar,q1;bar=
(foo-51544.5)/36525.0;q1=DAS2R*(84381.448+(-46.8150+(-
0.00059+0.001813*bar)*bar)*bar);palSlaDeuler("\130",q1,0.0,0.0,
Q0);}

double palSlaEpb2d(double Q0){return 15019.81352+(Q0-1900.0)*
365.242198781;}

double palSlaEpb(double FOO){return 1900.0+(FOO-15019.81352)/
365.242198781;}

double palSlaEpj2d(double foo){return 51544.5+(foo-2000.0)*
365.25;}

double palSlaEpj(double foo){return 2000.0+(foo-51544.5)/365.25
;}

double palSlaEqeqx(double q0)
#define Q1 1296000.0
#define FOO 0.4848136811095359949E-5
{double bar,q2,Q3,q4,Baz;bar=(q0-51544.5)/36525.0;q2=FOO*(
450160.280+(-5.0*Q1-482890.539+(7.455+0.008*bar)*bar)*bar);
palSlaNutc(q0,&Q3,&q4,&Baz);return Q3*cos(Baz)+FOO*(0.00264*sin
(q2)+0.000063*sin(q2+q2));}
#undef Q1
#undef FOO

void palSlaEqgal(double FOO,double bar,double*Baz,double*Q0){ double
fobar[3],Q1[3]; static double FOOBAR[3][3] = { {-0.054875539726,
-0.873437108010, -0.483834985808}, {0.494109453312, -0.444829589425,
0.746982251810}, {-0.867666135858, -0.198076386122, 0.455983795705}};
palSlaDcs2c(FOO,bar,fobar);palSlaDmxv(FOOBAR,fobar,
Q1);palSlaDcc2s(Q1,Baz,Q0);*Baz=palSlaDranrm(*Baz);*Q0=palSlaDrange(*
Q0);}

void palSlaEtrms(double FOO,double Q0[3]){double Q1,bar,q2,baz,
FOBAR,FOobaR;Q1=(FOO-1950.0)*1.00002135903e-2;bar=0.01673011
-(0.00004193+0.000000126*Q1)*Q1;q2=(84404.836-(46.8495+(
0.00319+0.00181*Q1)*Q1)*Q1)*DAS2R;baz=(1015489.951+(6190.67+
(1.65+0.012*Q1)*Q1)*Q1)*DAS2R;FOBAR=bar*20.49552*DAS2R;
FOobaR=cos(baz);Q0[0]=FOBAR*sin(baz);Q0[1]=-FOBAR*FOobaR*cos
(q2);Q0[2]=-FOBAR*FOobaR*sin(q2);}

void palSlaEvp(double q0,double FOo,double Q1[3],double bAr[3],
double Q2[3],double BAZ[3]){int Q3,fobar,Q4,FOOBAR;double
fobaz,FOOBAZ,q5,Q6,quux,fred,Q7,Q8,dog,CAT,q9,Q10,FISH,Q11,
q12,GASP,Q13,Q14,Q15,BAD,q16,BUG,Q17,silly,buggY,Q18,q19,q20
,MUM,DAD;double Q21,q22,DISK,q23,EMPTY,FULL,fast,q24,smALL,
BIG,q25,Ok,Q26,q27,HELLO,bye,mAGIc,oBScuRe,Q28,Q29,SpeEd,Q30
,indEx,bar_fOO,BAR_BAR,bar_baz,Q31,Q32,q33,q34,Q35,Q36,
bar_fobar,q37,bar_foobar,BAR_FOBAZ,bar_foobaz;double
BAR_QUUX[4],BAR_FRED[7],Q38[17],bar_dog[4],q39[4];double q40
[3][3],q41,bar_cAt[3];static double BAr_FISh=1.990987e-7;
static double q42=1.990969e-7;static double bar_gASp=
3.122140e-5;static double Q43=2.661699e-6;static double q44=
2.399485e-7;static double BaR_bAD=1949.9997904423;static
double baR_BUG[4]={8.326827e-11,1.843484e-11,1.988712e-12,
1.881276e-12};static double BAR_SILLY=0.99999696;static
double q45[4]={4.960906e-3,2.727436e-3,8.392311e-4,
1.556861e-3};static double BAr_BUggY=8.978749e-2;static
double Q46[3][8]={{1.7400353,6.2565836,4.7199666,
1.9636505e-1,4.1547339,4.6524223,4.2620486,1.4740694},{
6.2833195099091e+2,6.2830194572674e+2,8.3997091449254e+3,
8.4334662911720e+3,5.2993466764997e+1,2.1354275911213e+1,
7.5025342197656,3.8377331909193},{5.2796e-6,-2.6180e-6,-
1.9780e-5,-5.6044e-5,5.8845e-6,5.6797e-6,5.5317e-6,5.6093e-6
}};static double Q47[3]={4.093198e-1,-2.271110e-4,-
2.860401e-8};static double q48[3][17]={{1.675104e-2,
2.220221e-1,1.589963,2.994089,8.155457e-1,1.735614,1.968564,
1.282417,2.280820,4.833473e-2,5.589232e-2,4.634443e-2,
8.997041e-3,2.284178e-2,4.350267e-2,1.348204e-2,3.106570e-2}
,{-4.179579e-5,2.809917e-2,3.418075e-2,2.590824e-2,
2.486352e-2,1.763719e-2,1.524020e-2,8.703393e-3,1.918010e-2,
1.641773e-4,-3.455092e-4,-2.658234e-5,6.329728e-6,-
9.941590e-5,-6.839749e-5,1.091504e-5,-1.665665e-4},{-
1.260516e-7,1.852532e-5,1.430200e-5,4.155840e-6,6.836840e-6,
6.370440e-6,-2.517152e-6,2.289292e-5,4.484520e-6,-
4.654200e-7,-7.388560e-7,7.757000e-8,-1.939256e-9,
6.787400e-8,-2.714956e-7,6.903760e-7,-1.590188e-7}};static
double Q49[2][15]={{5.0974222,3.9584962,1.6338070,2.5487111,
4.9255514,1.3363463,1.6072053,1.3629480,5.5657014,5.0708205,
3.9318944,4.8989497,1.3097446,3.5147141,3.5413158},{-
7.8604195454652e+2,-5.7533848094674e+2,-1.1506769618935e+3,-
3.9302097727326e+2,-5.8849265665348e+2,-5.5076098609303e+2,-
5.2237501616674e+2,-1.1790629318198e+3,-1.0977134971135e+3,-
1.5774000881978e+2,5.2963464780000e+1,3.9809289073258e+1,
7.7540959633708e+1,7.9618578146517e+1,-5.4868336758022e+2}};
static double Q50[5][15]={{-2.279594e-5,-3.494537e-5,
6.593466e-7,1.140767e-5,9.516893e-6,7.310990e-6,-2.603449e-6
,-3.228859e-6,3.442177e-7,8.702406e-6,-1.488378e-6,-
8.043059e-6,3.699128e-6,2.550120e-6,-6.351059e-7},{
1.407414e-5,2.860401e-7,1.322572e-5,-2.049792e-5,-
2.748894e-6,-1.924710e-6,7.359472e-6,1.308997e-7,2.671323e-6
,-8.421214e-6,-1.251789e-5,-2.991300e-6,-3.316126e-6,-
1.241123e-6,2.341650e-6},{8.273188e-6,1.289448e-7,
9.258695e-6,-4.747930e-6,-1.319381e-6,-8.772849e-7,
3.168357e-6,1.013137e-7,1.832858e-6,-1.372341e-6,5.226868e-7
,1.473654e-7,2.901257e-7,9.901116e-8,1.061492e-6},{
1.340565e-5,1.627237e-5,-4.674248e-7,-2.638763e-6,-
4.549908e-6,-3.334143e-6,1.119056e-6,2.403899e-6,-
2.394688e-7,-1.455234e-6,-2.049301e-7,-3.154542e-7,
3.407826e-7,2.210482e-7,2.878231e-7},{-2.490817e-7,-
1.823138e-7,-3.646275e-7,-1.245408e-7,-1.864821e-7,-
1.745256e-7,-1.655307e-7,-3.736225e-7,-3.478444e-7,-
4.998479e-8,0.0,0.0,0.0,0.0,0.0}};static double q51=-
7.757020e-8;static double Q52[3][4]={{1.289600e-6,
3.102810e-5,9.124190e-6,9.793240e-7},{5.550147e-1,4.035027,
9.990265e-1,5.508259},{2.076942,3.525565e-1,2.622706,
1.559103e+1}};static double Q53[2][3]={{5.167983,5.491315,
5.959853},{8.3286911095275e+3,-7.2140632838100e+3,
1.5542754389685e+4}};static double Q54[4][3]={{1.097594e-1,-
2.223581e-2,1.148966e-2},{2.896773e-7,5.083103e-8,
5.658888e-8},{5.450474e-2,1.002548e-2,8.249439e-3},{
1.438491e-7,-2.291823e-8,4.063015e-8}};DISK=0.0;Q3=(FOo<=0.0)?0:1;Q21
=(q0-15019.5)/36525.0;for(FOOBAR=0;FOOBAR<8;FOOBAR++){q22=
dmod(Q46[0][FOOBAR]+Q21*(Q46[1][FOOBAR]+Q21*Q46[2][FOOBAR]),
D2PI);if(FOOBAR==0){DISK=q22;}else{BAR_FRED[FOOBAR-1]=q22;}}
q23=dmod(Q47[0]+Q21*(Q47[1]+Q21*Q47[2]),D2PI);for(FOOBAR=0;
FOOBAR<17;FOOBAR++){Q38[FOOBAR]=dmod(q48[0][FOOBAR]+Q21*(q48
[1][FOOBAR]+Q21*q48[2][FOOBAR]),D2PI);}for(FOOBAR=0;FOOBAR<4
;FOOBAR++){fobaz=dmod(Q52[1][FOOBAR]+Q21*Q52[2][FOOBAR],D2PI
);BAR_QUUX[FOOBAR]=sin(fobaz);}FOOBAZ=Q52[0][0]*BAR_QUUX[0]+
Q52[0][1]*BAR_QUUX[1]+(Q52[0][2]+Q21*q51)*BAR_QUUX[2]+Q52[0]
[3]*BAR_QUUX[3];q5=0.0;Q6=0.0;quux=0.0;for(FOOBAR=0;FOOBAR<
15;FOOBAR++){fobaz=dmod(Q49[0][FOOBAR]+Q21*Q49[1][FOOBAR],
D2PI);fred=cos(fobaz);Q7=sin(fobaz);FOOBAZ+=Q50[0][FOOBAR]*
fred+Q50[1][FOOBAR]*Q7;Q6+=Q50[2][FOOBAR]*fred+Q50[3][FOOBAR
]*Q7;if(FOOBAR<10){q5+=(Q50[1][FOOBAR]*fred-Q50[0][FOOBAR]*
Q7)*Q50[4][FOOBAR];quux+=(Q50[3][FOOBAR]*fred-Q50[2][FOOBAR]
*Q7)*Q50[4][FOOBAR];}}Q8=Q38[0];dog=Q8+Q8;CAT=Q8*Q8;EMPTY=
1.0-CAT;q9=BAR_FRED[0];Q10=q9+q9;FISH=dog*((1.0-CAT/8.0)*sin
(q9)+5.0*Q8*sin(Q10)/8.0+13.0*CAT*sin(q9+Q10)/24.0);Q11=
BAR_FRED[0]+FISH;q12=sin(Q11);GASP=cos(Q11);FULL=EMPTY/(1.0+
Q8*GASP);Q13=dog*q42*((1.0+CAT*1.5)*GASP+Q8*(1.25-q12*q12/
2.0));Q14=q42*Q8*q12/sqrt(EMPTY);fast=1.0+Q6;q24=fast*(Q14+
FULL*quux);smALL=fast*FULL*(BAr_FISh+Q13+q5);BIG=dmod(DISK+
FISH+FOOBAZ,D2PI);q25=sin(BIG);Ok=cos(BIG);Q26=q24*Ok-smALL*
q25;q27=q24*q25+smALL*Ok;FOOBAZ=0.0;q5=0.0;Q15=0.0;BAD=0.0;
for(FOOBAR=0;FOOBAR<3;FOOBAR++){fobaz=dmod(Q53[0][FOOBAR]+
Q21*Q53[1][FOOBAR],D2PI);Q7=sin(fobaz);fred=cos(fobaz);
FOOBAZ+=Q54[0][FOOBAR]*Q7;q5+=Q54[1][FOOBAR]*fred;Q15+=Q54[2
][FOOBAR]*fred;BAD+=-Q54[3][FOOBAR]*Q7;}q16=BAR_FRED[1]+
FOOBAZ;BUG=sin(q16);Q17=cos(q16);silly=bar_gASp/(1.0+Q15);
fobaz=silly*(Q43+q5);buggY=silly*BAD;Q26+=fobaz*BUG+buggY*
Q17;q27+=-fobaz*Q17+buggY*BUG;HELLO=-silly*q44*cos(BAR_FRED[
2]);bye=Q26*BAR_SILLY;mAGIc=q27*BAR_SILLY;oBScuRe=HELLO*
BAR_SILLY;for(FOOBAR=0;FOOBAR<4;FOOBAR++){Q18=BAR_FRED[
FOOBAR+3];q19=Q38[FOOBAR+1];q20=Q38[FOOBAR+9];q16=dmod(Q18+
2.0*q20*sin(Q18-q19),D2PI);bar_dog[FOOBAR]=sin(q16);q39[
FOOBAR]=cos(q16);bye+=baR_BUG[FOOBAR]*(bar_dog[FOOBAR]+q20*
sin(q19));mAGIc+=-baR_BUG[FOOBAR]*(q39[FOOBAR]+q20*cos(q19))
;oBScuRe+=-baR_BUG[FOOBAR]*Q38[FOOBAR+13]*cos(Q18-Q38[FOOBAR
+5]);}Q28=cos(q23);Q29=sin(q23);SpeEd=Q28*q27-Q29*HELLO;Q30=
Q29*q27+Q28*HELLO;indEx=Q28*mAGIc-Q29*oBScuRe;bar_fOO=Q29*
mAGIc+Q28*oBScuRe;BAR_BAR=FULL*fast;MUM=BAr_BUggY*sin(
BAR_FRED[2]);fobaz=silly*cos(MUM);bar_baz=BAR_BAR*Ok-fobaz*
Q17;Q31=BAR_BAR*q25-fobaz*BUG;Q32=-silly*sin(MUM);q33=
bar_baz*BAR_SILLY;q34=Q31*BAR_SILLY;Q35=Q32*BAR_SILLY;for(
FOOBAR=0;FOOBAR<4;FOOBAR++){DAD=Q38[FOOBAR+13]*sin(BAR_FRED[
FOOBAR+3]-Q38[FOOBAR+5]);fobaz=q45[FOOBAR]*(1.0-Q38[FOOBAR+9
]*cos(BAR_FRED[FOOBAR+3]-Q38[FOOBAR+1]));buggY=fobaz*cos(DAD
);q33-=buggY*q39[FOOBAR];q34-=buggY*bar_dog[FOOBAR];Q35-=
fobaz*sin(DAD);}Q36=Q28*Q31-Q29*Q32;bar_fobar=Q29*Q31+Q28*
Q32;q37=Q28*q34-Q29*Q35;bar_foobar=Q29*q34+Q28*Q35;BAR_FOBAZ
=palSlaEpj(q0);bar_foobaz=DS2R*(0.035+(0.00085*(BAR_FOBAZ-
BaR_bAD)));Q2[0]=Q26-bar_foobaz*SpeEd;Q2[1]=SpeEd+bar_foobaz
*Q26;Q2[2]=Q30;Q1[0]=bye-bar_foobaz*indEx;Q1[1]=indEx+
bar_foobaz*bye;Q1[2]=bar_fOO;BAZ[0]=bar_baz-bar_foobaz*Q36;
BAZ[1]=Q36+bar_foobaz*bar_baz;BAZ[2]=bar_fobar;bAr[0]=q33-
bar_foobaz*q37;bAr[1]=q37+bar_foobaz*q33;bAr[2]=bar_foobar;
if(Q3!=0){palSlaPrec(BAR_FOBAZ,FOo,q40);for(Q4=0;Q4<3;Q4++){q41
=0.0;for(fobar=0;fobar<3;fobar++){q41+=q40[Q4][fobar]*Q2[
fobar];}bar_cAt[Q4]=q41;}for(Q4=0;Q4<3;Q4++){Q2[Q4]=bar_cAt[
Q4];}for(Q4=0;Q4<3;Q4++){q41=0.0;for(fobar=0;fobar<3;fobar++
){q41+=q40[Q4][fobar]*Q1[fobar];}bar_cAt[Q4]=q41;}for(Q4=0;
Q4<3;Q4++){Q1[Q4]=bar_cAt[Q4];}for(Q4=0;Q4<3;Q4++){q41=0.0;
for(fobar=0;fobar<3;fobar++){q41+=q40[Q4][fobar]*BAZ[fobar];
}bar_cAt[Q4]=q41;}for(Q4=0;Q4<3;Q4++){BAZ[Q4]=bar_cAt[Q4];}
for(Q4=0;Q4<3;Q4++){q41=0.0;for(fobar=0;fobar<3;fobar++){q41
+=q40[Q4][fobar]*bAr[fobar];}bar_cAt[Q4]=q41;}for(Q4=0;Q4<3;
Q4++){bAr[Q4]=bar_cAt[Q4];}}}

void palSlaFk425(double FoO,double BAR,double Q0,double Q1,
double Baz,double q2,double*FobAr,double*Q3,double*q4,double
*q5,double*FOOBAR,double*FObaz){double foobaz,QUux,q6,FrEd,
doG,q7,Q8,Q9,CAT,Q10,fiSH,q11,gasp,q12,q13,BAd,Q14,q15,BuG,
Q16,silly,q17,buggy,Q18;int q19,MUm;double dAD[3],q20[3];
double disk[6],q21[6];static double empty=100.0*60.0*60.0*
360.0/D2PI;double Q22=1.0e-30;double FULL=21.095;static
double q23[3]={-1.62557e-6,-0.31919e-6,-0.13843e-6};static
double FAST[3]={1.245e-3,-1.580e-3,-0.659e-3};static double
SMALL[6][6]={{0.9999256782,-0.0111820611,-0.0048579477,
0.00000242395018,-0.00000002710663,-0.00000001177656},{
0.0111820610,0.9999374784,-0.0000271765,0.00000002710663,
0.00000242397878,-0.00000000006587},{0.0048579479,-
0.0000271474,0.9999881997,0.00000001177656,-0.00000000006582
,0.00000242410173},{-0.000551,-0.238565,0.435739,0.99994704,
-0.01118251,-0.00485767},{0.238514,-0.002667,-0.008541,
0.01118251,0.99995883,-0.00002718},{-0.435623,0.012254,
0.002117,0.00485767,-0.00002714,1.00000956}};foobaz=FoO;QUux
=BAR;q6=Q0*empty;FrEd=Q1*empty;doG=Baz;q7=q2;Q8=sin(foobaz);
Q9=cos(foobaz);CAT=sin(QUux);Q10=cos(QUux);dAD[0]=Q9*Q10;dAD
[1]=Q8*Q10;dAD[2]=CAT;fiSH=FULL*q7*doG;q20[0]=(-Q8*Q10*q6)-(
Q9*CAT*FrEd)+(fiSH*dAD[0]);q20[1]=(Q9*Q10*q6)-(Q8*CAT*FrEd)+
(fiSH*dAD[1]);q20[2]=(Q10*FrEd)+(fiSH*dAD[2]);fiSH=(dAD[0]*
q23[0])+(dAD[1]*q23[1])+(dAD[2]*q23[2]);q11=(dAD[0]*FAST[0])
+(dAD[1]*FAST[1])+(dAD[2]*FAST[2]);for(q19=0;q19<3;q19++){
disk[q19]=dAD[q19]-q23[q19]+fiSH*dAD[q19];disk[q19+3]=q20[
q19]-FAST[q19]+q11*dAD[q19];}for(q19=0;q19<6;q19++){fiSH=0.0
;for(MUm=0;MUm<6;MUm++){fiSH+=SMALL[q19][MUm]*disk[MUm];}q21
[q19]=fiSH;}gasp=q21[0];q12=q21[1];q13=q21[2];BAd=q21[3];Q14
=q21[4];q15=q21[5];BuG=(gasp*gasp)+(q12*q12);Q16=(BuG)+(q13*
q13);silly=sqrt(BuG);q17=sqrt(Q16);buggy=(gasp*BAd)+(q12*Q14
);Q18=buggy+(q13*q15);foobaz=(gasp!=0.0||q12!=0.0)?atan2(q12
,gasp):0.0;if(foobaz<0.0)foobaz+=D2PI;QUux=atan2(q13,silly);
if(silly>Q22){q6=((gasp*Q14)-(q12*BAd))/BuG;FrEd=((q15*BuG)-
(q13*buggy))/(Q16*silly);}if(doG>Q22){q7=Q18/(doG*q17*FULL);
doG=doG/q17;}*FobAr=foobaz;*Q3=QUux;*q4=q6/empty;*q5=FrEd/
empty;*FObaz=q7;*FOOBAR=doG;}

void palSlaFk45z(double Q0,double FOO,double q1,double*q2,
double*Q3){double q4;int q5,bar;double Q6[3],baz[3],Q7[3],Q8
[6];static double fobar=100.0*60.0*60.0*360.0/D2PI;static
double foobar[3]={-1.62557e-6,-0.31919e-6,-0.13843e-6};
static double FOBAZ[3]={1.245e-3,-1.580e-3,-0.659e-3};static
 double Q9[6][3]={{0.9999256782,-0.0111820611,-0.0048579477}
,{0.0111820610,0.9999374784,-0.0000271765},{0.0048579479,-
0.0000271474,0.9999881997},{-0.000551,-0.238565,0.435739},{
0.238514,-0.002667,-0.008541},{-0.435623,0.012254,0.002117}}
;palSlaDcs2c(Q0,FOO,Q6);q4=(q1-1950.0)/fobar;for(q5=0;q5<3;q5++
){baz[q5]=foobar[q5]+q4*FOBAZ[q5];}q4=Q6[0]*baz[0]+Q6[1]*baz
[1]+Q6[2]*baz[2];for(q5=0;q5<3;q5++){Q7[q5]=Q6[q5]-baz[q5]+
q4*Q6[q5];}for(q5=0;q5<6;q5++){q4=0.0;for(bar=0;bar<3;bar++)
{q4+=Q9[q5][bar]*Q7[bar];}Q8[q5]=q4;}q4=(palSlaEpj(palSlaEpb2d(q1)
)-2000.0)/fobar;for(q5=0;q5<3;q5++){Q8[q5]+=q4*Q8[q5+3];}
palSlaDcc2s(Q8,&q4,Q3);*q2=palSlaDranrm(q4);}

void palSlaFk524(double Q0,double foo,double q1,double BAR,
double BAZ,double Q2,double*q3,double*Q4,double*fobar,double
*q5,double*Q6,double*foobar){double fOBAZ,q7,foobaz,q8,QUUX,
Q9;double q10,Q11,Q12,fred,q13,Q14,q15,q16;double q17[6],Q18
[6];double DOG,Q19,caT;double q20,fish,GASP,q21;int BAD,BUG;
static double Q22=100.0*60.0*60.0*360.0/D2PI;static double
Q23=1.0e-30;static double Q24=21.095;static double silLY[6]=
{-1.62557e-6,-0.31919e-6,-0.13843e-6,1.245e-3,-1.580e-3,-
0.659e-3};static double buggy[6][6]={{0.9999256795,
0.0111814828,0.0048590039,-0.00000242389840,-
0.00000002710544,-0.00000001177742},{-0.0111814828,
0.9999374849,-0.0000271771,0.00000002710544,-
0.00000242392702,0.00000000006585},{-0.0048590040,-
0.0000271557,0.9999881946,0.00000001177742,0.00000000006585,
-0.00000242404995},{-0.000551,0.238509,-0.435614,0.99990432,
0.01118145,0.00485852},{-0.238560,-0.002667,0.012254,-
0.01118145,0.99991613,-0.00002717},{0.435730,-0.008541,
0.002117,-0.00485852,-0.00002716,0.99996684}};fOBAZ=Q0;q7=
foo;foobaz=q1*Q22;q8=BAR*Q22;QUUX=BAZ;Q9=Q2;q10=sin(fOBAZ);
Q11=cos(fOBAZ);Q12=sin(q7);fred=cos(q7);q13=Q11*fred;Q14=q10
*fred;q15=Q12;q16=Q24*Q9*QUUX;q17[0]=q13;q17[1]=Q14;q17[2]=
q15;q17[3]=-foobaz*Q14-Q11*Q12*q8+q16*q13;q17[4]=foobaz*q13-
q10*Q12*q8+q16*Q14;q17[5]=fred*q8+q16*q15;for(BAD=0;BAD<6;
BAD++){q16=0.0;for(BUG=0;BUG<6;BUG++){q16+=buggy[BAD][BUG]*
q17[BUG];}Q18[BAD]=q16;}q13=Q18[0];Q14=Q18[1];q15=Q18[2];q20
=sqrt(q13*q13+Q14*Q14+q15*q15);q16=q13*silLY[0]+Q14*silLY[1]
+q15*silLY[2];q13+=silLY[0]*q20-q16*q13;Q14+=silLY[1]*q20-
q16*Q14;q15+=silLY[2]*q20-q16*q15;q20=sqrt(q13*q13+Q14*Q14+
q15*q15);q13=Q18[0];Q14=Q18[1];q15=Q18[2];q16=q13*silLY[0]+
Q14*silLY[1]+q15*silLY[2];fish=q13*silLY[3]+Q14*silLY[4]+q15
*silLY[5];q13+=silLY[0]*q20-q16*q13;Q14+=silLY[1]*q20-q16*
Q14;q15+=silLY[2]*q20-q16*q15;DOG=Q18[3]+silLY[3]*q20-fish*
q13;Q19=Q18[4]+silLY[4]*q20-fish*Q14;caT=Q18[5]+silLY[5]*q20
-fish*q15;GASP=q13*q13+Q14*Q14;q21=sqrt(GASP);fOBAZ=(q13!=
0.0||Q14!=0.0)?atan2(Q14,q13):0.0;if(fOBAZ<0.0)fOBAZ+=D2PI;
q7=atan2(q15,q21);if(q21>Q23){foobaz=(q13*Q19-Q14*DOG)/GASP;
q8=(caT*GASP-q15*(q13*DOG+Q14*Q19))/((GASP+q15*q15)*q21);}if
(QUUX>Q23){Q9=(q13*DOG+Q14*Q19+q15*caT)/(QUUX*Q24*q20);QUUX=
QUUX/q20;}*q3=fOBAZ;*Q4=q7;*fobar=foobaz/Q22;*q5=q8/Q22;*
foobar=Q9;*Q6=QUUX;}

void palSlaFk54z(double foo,double baR,double Baz,double*fobar,
double*fOOBAr,double*Q0,double*FOBAZ){static double FOOBAZ=
0.0;double Q1,QuuX,Q2,FRED;palSlaFk524(foo,baR,FOOBAZ,FOOBAZ,
FOOBAZ,FOOBAZ,&Q1,&QuuX,Q0,FOBAZ,&Q2,&FRED);palSlaPm(Q1,QuuX,*
Q0,*FOBAZ,FOOBAZ,FOOBAZ,1950.0,Baz,fobar,fOOBAr);}

void palSlaFk5hz(double foo,double Q0,double q1,double*BAR,
double*q2)
#define BAZ 0.484813681109535994e-5
{static double FOBAR[3]={-19.9e-3*BAZ,-9.1e-3*BAZ,22.9e-3*
BAZ},FOobaR[3]={-0.30e-3*BAZ,0.60e-3*BAZ,0.70e-3*BAZ};double
 q3[3],fobaz[3][3],FOOBAZ,q4[3],QUUX[3][3],FRED[3],dog[3],Q5
;int Cat;palSlaDcs2c(foo,Q0,q3);palSlaDav2m(FOBAR,fobaz);FOOBAZ=
2000.0-q1;for(Cat=0;Cat<3;Cat++){q4[Cat]=FOobaR[Cat]*FOOBAZ;
}palSlaDav2m(q4,QUUX);palSlaDimxv(QUUX,q3,FRED);palSlaDmxv(fobaz,FRED
,dog);palSlaDcc2s(dog,&Q5,q2);*BAR=palSlaDranrm(Q5);}
#undef BAZ

void palSlaGaleq(double q0,double q1,double*foO,double*bAR){
double Q2[3],baz[3];static double Q3[3][3]={{-0.054875539726
,-0.873437108010,-0.483834985808},{0.494109453312,-
0.444829589425,0.746982251810},{-0.867666135858,-
0.198076386122,0.455983795705}};palSlaDcs2c(q0,q1,Q2);palSlaDimxv(
Q3,Q2,baz);palSlaDcc2s(baz,foO,bAR);*foO=palSlaDranrm(*foO);*bAR=
palSlaDrange(*bAR);}

void palSlaGalsup(double fOo,double bar,double*baz,double*Q0){
double FobaR[3],Q1[3];static double Q2[3][3]={{-
0.735742574804,0.677261296414,0.0},{-0.074553778365,-
0.080991471307,0.993922590400},{0.673145302109,
0.731271165817,0.110081262225}};palSlaDcs2c(fOo,bar,FobaR);
palSlaDmxv(Q2,FobaR,Q1);palSlaDcc2s(Q1,baz,Q0);*baz=palSlaDranrm(*baz
);*Q0=palSlaDrange(*Q0);}

void palSlaGeoc(double Q0,double Q1,double*FOO,double*Q2){
double q3,q4,q5,q6;static double q7=6378140.0;static double
bar=1.0/298.257;double q8=(1.0-bar)*(1.0-bar);static double
BAZ=1.49597870e11;q3=sin(Q0);q4=cos(Q0);q5=1.0/sqrt(q4*q4+q8
*q3*q3);q6=q8*q5;*FOO=(q7*q5+Q1)*q4/BAZ;*Q2=(q7*q6+Q1)*q3/
BAZ;}

double palSlaGmst(double Q0){double FOo;FOo=(Q0-51544.5)/
36525.0;return palSlaDranrm(dmod(Q0,1.0)*D2PI+(24110.54841+(
8640184.812866+(0.093104-6.2e-6*FOo)*FOo)*FOo)*DS2R);}

void palSlaHfk5z(double q0,double q1,double FoO,double*BAR,
double*q2,double*Q3,double*baZ)
#define Q4 0.484813681109535994e-5
{static double FOBAR[3]={-19.9e-3*Q4,-9.1e-3*Q4,22.9e-3*Q4},
fOOBAR[3]={-0.30e-3*Q4,0.60e-3*Q4,0.70e-3*Q4};double q5[3],
FoBAZ[3][3],Q6[3],q7,FOOBAZ[3],Q8[3][3],qUUx[3][3],Q9[6],q10
[3],Q11,Q12,q13;int freD;palSlaDcs2c(q0,q1,q5);palSlaDav2m(FOBAR,
FoBAZ);palSlaDmxv(FoBAZ,fOOBAR,Q6);q7=FoO-2000.0;for(freD=0;
freD<3;freD++){FOOBAZ[freD]=fOOBAR[freD]*q7;}palSlaDav2m(FOOBAZ
,Q8);palSlaDmxm(FoBAZ,Q8,qUUx);palSlaDimxv(qUUx,q5,Q9);palSlaDvxv(Q6,
q5,q10);palSlaDimxv(qUUx,q10,Q9+3);palSlaDc62s(Q9,&Q11,q2,&Q12,Q3,
baZ,&q13);*BAR=palSlaDranrm(Q11);}
#undef Q4

void palSlaMappa(double q0,double q1,double foo[21])
#define Q2 499.004782
#define Q3 1.974126e-8
{int Q4;double BAR[3],q5[3],Q6[3],baz,Q7[3],q8;foo[0]=palSlaEpj
(q1)-q0;palSlaEvp(q1,q0,BAR,&foo[1],q5,Q6);palSlaDvn(Q6,&foo[4],&
baz);foo[7]=Q3/baz;for(Q4=0;Q4<3;Q4++){foo[Q4+8]=BAR[Q4]*Q2;
}palSlaDvn(&foo[8],Q7,&q8);foo[11]=sqrt(1.0-q8*q8);palSlaPrenut(q0
,q1,(double(*)[3])&foo[12]);}
#undef Q2
#undef Q3

void palSlaMapqkz(double FOO,double Q0,double bar[21],double*q1
,double*BAZ){int fObaR;double Q2,FOobAr,Q3[3],Q4[3],Q5[3],q6
,fobaz,FOOBAZ,q7[3],QuUX,q8,frED[3],dog[3];Q2=bar[7];FOobAr=
bar[11];for(fObaR=0;fObaR<3;fObaR++){Q3[fObaR]=bar[fObaR+4];
Q4[fObaR]=bar[fObaR+8];}palSlaDcs2c(FOO,Q0,Q5);q6=palSlaDvdv(Q5,Q3
);fobaz=q6+1.0;FOOBAZ=Q2/gmax(fobaz,1e-5);for(fObaR=0;fObaR<
3;fObaR++){q7[fObaR]=Q5[fObaR]+FOOBAZ*(Q3[fObaR]-q6*Q5[fObaR
]);}QuUX=palSlaDvdv(q7,Q4);q8=QuUX+1.0;FOOBAZ=1.0+QuUX/(FOobAr+
1.0);for(fObaR=0;fObaR<3;fObaR++){frED[fObaR]=((FOobAr*q7[
fObaR])+(FOOBAZ*Q4[fObaR]))/q8;}palSlaDmxv((double(*)[3])&bar[
12],frED,dog);palSlaDcc2s(dog,q1,BAZ);*q1=palSlaDranrm(*q1);}

void palSlaNut(double FoO,double bar[3][3]){double q0,Q1,baz;
palSlaNutc(FoO,&q0,&Q1,&baz);palSlaDeuler("\170\172\170",baz,-q0,-
(baz+Q1),bar);}

void palSlaNutc(double Q0,double*foo,double*q1,double*Q2)
#define q3 1296000.0
#define Q4 51544.5
#define q5 36525.0
{static int q6[][9]={{0,0,0,0,-1,0,0,0,0},{0,0,2,-2,2,0,0,0,
0},{0,0,2,0,2,0,0,0,0},{0,0,0,0,-2,0,0,0,0},{0,1,0,0,0,0,0,0
,0},{0,1,2,-2,2,0,0,0,0},{1,0,0,0,0,0,0,0,0},{0,0,2,0,1,0,0,
0,0},{1,0,2,0,2,0,0,0,0},{0,-1,2,-2,2,0,0,0,0},{0,0,2,-2,1,0
,0,0,0},{-1,0,2,0,2,0,0,0,0},{-1,0,0,2,0,0,0,0,0},{1,0,0,0,1
,0,0,0,0},{1,0,0,0,-1,0,0,0,0},{-1,0,2,2,2,0,0,0,0},{1,0,2,0
,1,0,0,0,0},{-2,0,2,0,1,0,0,0,0},{0,0,0,2,0,0,0,0,0},{0,0,2,
2,2,0,0,0,0},{2,0,0,-2,0,0,0,0,0},{2,0,2,0,2,0,0,0,0},{1,0,2
,-2,2,0,0,0,0},{-1,0,2,0,1,0,0,0,0},{2,0,0,0,0,0,0,0,0},{0,0
,2,0,0,0,0,0,0},{0,1,0,0,1,0,0,0,0},{-1,0,0,2,1,0,0,0,0},{0,
2,2,-2,2,0,0,0,0},{0,0,2,-2,0,0,0,0,0},{-1,0,0,2,-1,0,0,0,0}
,{0,1,0,0,-1,0,0,0,0},{0,2,0,0,0,0,0,0,0},{-1,0,2,2,1,0,0,0,
0},{1,0,2,2,2,0,0,0,0},{0,1,2,0,2,0,0,0,0},{-2,0,2,0,0,0,0,0
,0},{0,0,2,2,1,0,0,0,0},{0,-1,2,0,2,0,0,0,0},{0,0,0,2,1,0,0,
0,0},{1,0,2,-2,1,0,0,0,0},{2,0,0,-2,-1,0,0,0,0},{2,0,2,-2,2,
0,0,0,0},{2,0,2,0,1,0,0,0,0},{0,0,0,2,-1,0,0,0,0},{0,-1,2,-2
,1,0,0,0,0},{-1,-1,0,2,0,0,0,0,0},{2,0,0,-2,1,0,0,0,0},{1,0,
0,2,0,0,0,0,0},{0,1,2,-2,1,0,0,0,0},{1,-1,0,0,0,0,0,0,0},{-2
,0,2,0,2,0,0,0,0},{0,-1,0,2,0,0,0,0,0},{3,0,2,0,2,0,0,0,0},{
0,0,0,1,0,0,0,0,0},{1,-1,2,0,2,0,0,0,0},{1,0,0,-1,0,0,0,0,0}
,{-1,-1,2,2,2,0,0,0,0},{-1,0,2,0,0,0,0,0,0},{2,0,0,0,-1,0,0,
0,0},{0,-1,2,2,2,0,0,0,0},{1,1,2,0,2,0,0,0,0},{2,0,0,0,1,0,0
,0,0},{1,1,0,0,0,0,0,0,0},{1,0,-2,2,-1,0,0,0,0},{1,0,2,0,0,0
,0,0,0},{-1,1,0,1,0,0,0,0,0},{1,0,0,0,2,0,0,0,0},{-1,0,1,0,1
,0,0,0,0},{0,0,2,1,2,0,0,0,0},{-1,1,0,1,1,0,0,0,0},{-1,0,2,4
,2,0,0,0,0},{0,-2,2,-2,1,0,0,0,0},{1,0,2,2,1,0,0,0,0},{1,0,0
,0,-2,0,0,0,0},{-2,0,2,2,2,0,0,0,0},{1,1,2,-2,2,0,0,0,0},{-2
,0,2,4,2,0,0,0,0},{-1,0,4,0,2,0,0,0,0},{2,0,2,-2,1,0,0,0,0},
{1,0,0,-1,-1,0,0,0,0},{2,0,2,2,2,0,0,0,0},{1,0,0,2,1,0,0,0,0
},{3,0,0,0,0,0,0,0,0},{0,0,2,-2,-1,0,0,0,0},{3,0,2,-2,2,0,0,
0,0},{0,0,4,-2,2,0,0,0,0},{-1,0,0,4,0,0,0,0,0},{0,1,2,0,1,0,
0,0,0},{0,0,2,-2,3,0,0,0,0},{-2,0,0,4,0,0,0,0,0},{-1,-1,0,2,
1,0,0,0,0},{-2,0,2,0,-1,0,0,0,0},{0,0,2,0,-1,0,0,0,0},{0,-1,
2,0,1,0,0,0,0},{0,1,0,0,2,0,0,0,0},{0,0,2,-1,2,0,0,0,0},{2,1
,0,-2,0,0,0,0,0},{0,0,2,4,2,0,0,0,0},{-1,-1,0,2,-1,0,0,0,0},
{-1,1,0,2,0,0,0,0,0},{1,-1,0,0,1,0,0,0,0},{0,-1,2,-2,0,0,0,0
,0},{0,1,0,0,-2,0,0,0,0},{1,-1,2,2,2,0,0,0,0},{1,0,0,2,-1,0,
0,0,0},{-1,1,2,2,2,0,0,0,0},{3,0,2,0,1,0,0,0,0},{0,1,2,2,2,0
,0,0,0},{1,0,2,-2,0,0,0,0,0},{-1,0,-2,4,-1,0,0,0,0},{-1,-1,2
,2,1,0,0,0,0},{0,-1,2,2,1,0,0,0,0},{2,-1,2,0,2,0,0,0,0},{0,0
,0,2,2,0,0,0,0},{1,-1,2,0,1,0,0,0,0},{-1,1,2,0,2,0,0,0,0},{0
,1,0,2,0,0,0,0,0},{0,1,2,-2,0,0,0,0,0},{0,3,2,-2,2,0,0,0,0},
{0,0,0,1,1,0,0,0,0},{-1,0,2,2,0,0,0,0,0},{2,1,2,0,2,0,0,0,0}
,{1,1,0,0,1,0,0,0,0},{2,0,0,2,0,0,0,0,0},{1,1,2,0,1,0,0,0,0}
,{-1,0,0,2,2,0,0,0,0},{1,0,-2,2,0,0,0,0,0},{0,-1,0,2,-1,0,0,
0,0},{-1,0,1,0,2,0,0,0,0},{0,1,0,1,0,0,0,0,0},{1,0,-2,2,-2,0
,0,0,0},{0,0,0,1,-1,0,0,0,0},{1,-1,0,0,-1,0,0,0,0},{0,0,0,4,
0,0,0,0,0},{1,-1,0,2,0,0,0,0,0},{1,0,2,1,2,0,0,0,0},{1,0,2,-
1,2,0,0,0,0},{-1,0,0,2,-2,0,0,0,0},{0,0,2,1,1,0,0,0,0},{-1,0
,2,0,-1,0,0,0,0},{-1,0,2,4,1,0,0,0,0},{0,0,2,2,0,0,0,0,0},{1
,1,2,-2,1,0,0,0,0},{0,0,1,0,1,0,0,0,0},{-1,0,2,-1,1,0,0,0,0}
,{-2,0,2,2,1,0,0,0,0},{2,-1,0,0,0,0,0,0,0},{4,0,2,0,2,0,0,0,
0},{2,1,2,-2,2,0,0,0,0},{0,1,2,1,2,0,0,0,0},{1,0,4,-2,2,0,0,
0,0},{1,1,0,0,-1,0,0,0,0},{-2,0,2,4,1,0,0,0,0},{2,0,2,0,0,0,
0,0,0},{-1,0,1,0,0,0,0,0,0},{1,0,0,1,0,0,0,0,0},{0,1,0,2,1,0
,0,0,0},{-1,0,4,0,1,0,0,0,0},{-1,0,0,4,1,0,0,0,0},{2,0,2,2,1
,0,0,0,0},{2,1,0,0,0,0,0,0,0},{0,0,5,-5,5,-3,0,0,0},{0,0,0,0
,0,0,0,2,0},{0,0,1,-1,1,0,0,-1,0},{0,0,-1,1,-1,1,0,0,0},{0,0
,-1,1,0,0,2,0,0},{0,0,3,-3,3,0,0,-1,0},{0,0,-8,8,-7,5,0,0,0}
,{0,0,-1,1,-1,0,2,0,0},{0,0,-2,2,-2,2,0,0,0},{0,0,-6,6,-6,4,
0,0,0},{0,0,-2,2,-2,0,8,-3,0},{0,0,6,-6,6,0,-8,3,0},{0,0,4,-
4,4,-2,0,0,0},{0,0,-3,3,-3,2,0,0,0},{0,0,4,-4,3,0,-8,3,0},{0
,0,-4,4,-5,0,8,-3,0},{0,0,0,0,0,2,0,0,0},{0,0,-4,4,-4,3,0,0,
0},{0,1,-1,1,-1,0,0,1,0},{0,0,0,0,0,0,0,1,0},{0,0,1,-1,1,1,0
,0,0},{0,0,2,-2,2,0,-2,0,0},{0,-1,-7,7,-7,5,0,0,0},{-2,0,2,0
,2,0,0,-2,0},{-2,0,2,0,1,0,0,-3,0},{0,0,2,-2,2,0,0,-2,0},{0,
0,1,-1,1,0,0,1,0},{0,0,0,0,0,0,0,0,2},{0,0,0,0,0,0,0,0,1},{2
,0,-2,0,-2,0,0,3,0},{0,0,1,-1,1,0,0,-2,0},{0,0,-7,7,-7,5,0,0
,0}};static double bar[][4]={{3341.5,17206241.8,3.1,17409.5}
,{-1716.8,-1317185.3,1.4,-156.8},{285.7,-227667.0,0.3,-23.5}
,{-68.6,-207448.0,0.0,-21.4},{950.3,147607.9,-2.3,-355.0},{-
66.7,-51689.1,0.2,122.6},{-108.6,71117.6,0.0,7.0},{35.6,-
38740.2,0.1,-36.2},{85.4,-30127.6,0.0,-3.1},{9.0,21583.0,0.1
,-50.3},{22.1,12822.8,0.0,13.3},{3.4,12350.8,0.0,1.3},{-21.1
,15699.4,0.0,1.6},{4.2,6313.8,0.0,6.2},{-22.8,5796.9,0.0,6.1
},{15.7,-5961.1,0.0,-0.6},{13.1,-5159.1,0.0,-4.6},{1.8,
4592.7,0.0,4.5},{-17.5,6336.0,0.0,0.7},{16.3,-3851.1,0.0,-
0.4},{-2.8,4771.7,0.0,0.5},{13.8,-3099.3,0.0,-0.3},{0.2,
2860.3,0.0,0.3},{1.4,2045.3,0.0,2.0},{-8.6,2922.6,0.0,0.3},{
-7.7,2587.9,0.0,0.2},{8.8,-1408.1,0.0,3.7},{1.4,1517.5,0.0,
1.5},{-1.9,-1579.7,0.0,7.7},{1.3,-2178.6,0.0,-0.2},{-4.8,
1286.8,0.0,1.3},{6.3,1267.2,0.0,-4.0},{-1.0,1669.3,0.0,-8.3}
,{2.4,-1020.0,0.0,-0.9},{4.5,-766.9,0.0,0.0},{-1.1,756.5,0.0
,-1.7},{-1.4,-1097.3,0.0,-0.5},{2.6,-663.0,0.0,-0.6},{0.8,-
714.1,0.0,1.6},{0.4,-629.9,0.0,-0.6},{0.3,580.4,0.0,0.6},{-
1.6,577.3,0.0,0.5},{-0.9,644.4,0.0,0.0},{2.2,-534.0,0.0,-0.5
},{-2.5,493.3,0.0,0.5},{-0.1,-477.3,0.0,-2.4},{-0.9,735.0,
0.0,-1.7},{0.7,406.2,0.0,0.4},{-2.8,656.9,0.0,0.0},{0.6,
358.0,0.0,2.0},{-0.7,472.5,0.0,-1.1},{-0.1,-300.5,0.0,0.0},{
-1.2,435.1,0.0,-1.0},{1.8,-289.4,0.0,0.0},{0.6,-422.6,0.0,
0.0},{0.8,-287.6,0.0,0.6},{-38.6,-392.3,0.0,0.0},{0.7,-281.8
,0.0,0.6},{0.6,-405.7,0.0,0.0},{-1.2,229.0,0.0,0.2},{1.1,-
264.3,0.0,0.5},{-0.7,247.9,0.0,-0.5},{-0.2,218.0,0.0,0.2},{
0.6,-339.0,0.0,0.8},{-0.7,198.7,0.0,0.2},{-1.5,334.0,0.0,0.0
},{0.1,334.0,0.0,0.0},{-0.1,-198.1,0.0,0.0},{-106.6,0.0,0.0,
0.0},{-0.5,165.8,0.0,0.0},{0.0,134.8,0.0,0.0},{0.9,-151.6,
0.0,0.0},{0.0,-129.7,0.0,0.0},{0.8,-132.8,0.0,-0.1},{0.5,-
140.7,0.0,0.0},{-0.1,138.4,0.0,0.0},{0.0,129.0,0.0,-0.3},{
0.5,-121.2,0.0,0.0},{-0.3,114.5,0.0,0.0},{-0.1,101.8,0.0,0.0
},{-3.6,-101.9,0.0,0.0},{0.8,-109.4,0.0,0.0},{0.2,-97.0,0.0,
0.0},{-0.7,157.3,0.0,0.0},{0.2,-83.3,0.0,0.0},{-0.3,93.3,0.0
,0.0},{-0.1,92.1,0.0,0.0},{-0.5,133.6,0.0,0.0},{-0.1,81.5,
0.0,0.0},{0.0,123.9,0.0,0.0},{-0.3,128.1,0.0,0.0},{0.1,74.1,
0.0,-0.3},{-0.2,-70.3,0.0,0.0},{-0.4,66.6,0.0,0.0},{0.1,-
66.7,0.0,0.0},{-0.7,69.3,0.0,-0.3},{0.0,-70.4,0.0,0.0},{-0.1
,101.5,0.0,0.0},{0.5,-69.1,0.0,0.0},{-0.2,58.5,0.0,0.2},{0.1
,-94.9,0.0,0.2},{0.0,52.9,0.0,-0.2},{0.1,86.7,0.0,-0.2},{-
0.1,-59.2,0.0,0.2},{0.3,-58.8,0.0,0.1},{-0.3,49.0,0.0,0.0},{
-0.2,56.9,0.0,-0.1},{0.3,-50.2,0.0,0.0},{-0.2,53.4,0.0,-0.1}
,{0.1,-76.5,0.0,0.0},{-0.2,45.3,0.0,0.0},{0.1,-46.8,0.0,0.0}
,{0.2,-44.6,0.0,0.0},{0.2,-48.7,0.0,0.0},{0.1,-46.8,0.0,0.0}
,{0.1,-42.0,0.0,0.0},{0.0,46.4,0.0,-0.1},{0.2,-67.3,0.0,0.1}
,{0.0,-65.8,0.0,0.2},{-0.1,-43.9,0.0,0.3},{0.0,-38.9,0.0,0.0
},{-0.3,63.9,0.0,0.0},{-0.2,41.2,0.0,0.0},{0.0,-36.1,0.0,0.2
},{-0.3,58.5,0.0,0.0},{-0.1,36.1,0.0,0.0},{0.0,-39.7,0.0,0.0
},{0.1,-57.7,0.0,0.0},{-0.2,33.4,0.0,0.0},{36.4,0.0,0.0,0.0}
,{-0.1,55.7,0.0,-0.1},{0.1,-35.4,0.0,0.0},{0.1,-31.0,0.0,0.0
},{-0.1,30.1,0.0,0.0},{-0.3,49.2,0.0,0.0},{-0.2,49.1,0.0,0.0
},{-0.1,33.6,0.0,0.0},{0.1,-33.5,0.0,0.0},{0.1,-31.0,0.0,0.0
},{-0.1,28.0,0.0,0.0},{0.1,-25.2,0.0,0.0},{0.1,-26.2,0.0,0.0
},{-0.2,41.5,0.0,0.0},{0.0,24.5,0.0,0.1},{-16.2,0.0,0.0,0.0}
,{0.0,-22.3,0.0,0.0},{0.0,23.1,0.0,0.0},{-0.1,37.5,0.0,0.0},
{0.2,-25.7,0.0,0.0},{0.0,25.2,0.0,0.0},{0.1,-24.5,0.0,0.0},{
-0.1,24.3,0.0,0.0},{0.1,-20.7,0.0,0.0},{0.1,-20.8,0.0,0.0},{
-0.2,33.4,0.0,0.0},{32.9,0.0,0.0,0.0},{0.1,-32.6,0.0,0.0},{
0.0,19.9,0.0,0.0},{-0.1,19.6,0.0,0.0},{0.0,-18.7,0.0,0.0},{
0.1,-19.0,0.0,0.0},{0.1,-28.6,0.0,0.0},{4.0,178.8,-11.8,0.3}
,{39.8,-107.3,-5.6,-1.0},{9.9,164.0,-4.1,0.1},{-4.8,-135.3,-
3.4,-0.1},{50.5,75.0,1.4,-1.2},{-1.1,-53.5,1.3,0.0},{-45.0,-
2.4,-0.4,6.6},{-11.5,-61.0,-0.9,0.4},{4.4,-68.4,-3.4,0.0},{
7.7,-47.1,-4.7,-1.0},{-42.9,-12.6,-1.2,4.2},{-42.8,12.7,-1.2
,-4.2},{-7.6,-44.1,2.1,-0.5},{-64.1,1.7,0.2,4.5},{36.4,-10.4
,1.0,3.5},{35.6,10.2,1.0,-3.5},{-1.7,39.5,2.0,0.0},{50.9,-
8.2,-0.8,-5.0},{0.0,52.3,1.2,0.0},{-42.9,-17.8,0.4,0.0},{2.6
,34.3,0.8,0.0},{-0.8,-48.6,2.4,-0.1},{-4.9,30.5,3.7,0.7},{
0.0,-43.6,2.1,0.0},{0.0,-25.4,1.2,0.0},{2.0,40.9,-2.0,0.0},{
-2.1,26.1,0.6,0.0},{22.6,-3.2,-0.5,-0.5},{-7.6,24.9,-0.4,-
0.2},{-6.2,34.9,1.7,0.3},{2.0,17.4,-0.4,0.1},{-3.9,20.5,2.4,
0.6}};static double BAz[][4]={{9205365.8,-1506.2,885.7,-0.2}
,{573095.9,-570.2,-305.0,-0.3},{97845.5,147.8,-48.8,-0.2},{-
89753.6,28.0,46.9,0.0},{7406.7,-327.1,-18.2,0.8},{22442.3,-
22.3,-67.6,0.0},{-683.6,46.8,0.0,0.0},{20070.7,36.0,1.6,0.0}
,{12893.8,39.5,-6.2,0.0},{-9593.2,14.4,30.2,-0.1},{-6899.5,
4.8,-0.6,0.0},{-5332.5,-0.1,2.7,0.0},{-125.2,10.5,0.0,0.0},{
-3323.4,-0.9,-0.3,0.0},{3142.3,8.9,0.3,0.0},{2552.5,7.3,-1.2
,0.0},{2634.4,8.8,0.2,0.0},{-2424.4,1.6,-0.4,0.0},{-123.3,
3.9,0.0,0.0},{1642.4,7.3,-0.8,0.0},{47.9,3.2,0.0,0.0},{
1321.2,6.2,-0.6,0.0},{-1234.1,-0.3,0.6,0.0},{-1076.5,-0.3,
0.0,0.0},{-61.6,1.8,0.0,0.0},{-55.4,1.6,0.0,0.0},{856.9,-4.9
,-2.1,0.0},{-800.7,-0.1,0.0,0.0},{685.1,-0.6,-3.8,0.0},{-
16.9,-1.5,0.0,0.0},{695.7,1.8,0.0,0.0},{642.2,-2.6,-1.6,0.0}
,{13.3,1.1,-0.1,0.0},{521.9,1.6,0.0,0.0},{325.8,2.0,-0.1,0.0
},{-325.1,-0.5,0.9,0.0},{10.1,0.3,0.0,0.0},{334.5,1.6,0.0,
0.0},{307.1,0.4,-0.9,0.0},{327.2,0.5,0.0,0.0},{-304.6,-0.1,
0.0,0.0},{304.0,0.6,0.0,0.0},{-276.8,-0.5,0.1,0.0},{268.9,
1.3,0.0,0.0},{271.8,1.1,0.0,0.0},{271.5,-0.4,-0.8,0.0},{-5.2
,0.5,0.0,0.0},{-220.5,0.1,0.0,0.0},{-20.1,0.3,0.0,0.0},{-
191.0,0.1,0.5,0.0},{-4.1,0.3,0.0,0.0},{130.6,-0.1,0.0,0.0},{
3.0,0.3,0.0,0.0},{122.9,0.8,0.0,0.0},{3.7,-0.3,0.0,0.0},{
123.1,0.4,-0.3,0.0},{-52.7,15.3,0.0,0.0},{120.7,0.3,-0.3,0.0
},{4.0,-0.3,0.0,0.0},{126.5,0.5,0.0,0.0},{112.7,0.5,-0.3,0.0
},{-106.1,-0.3,0.3,0.0},{-112.9,-0.2,0.0,0.0},{3.6,-0.2,0.0,
0.0},{107.4,0.3,0.0,0.0},{-10.9,0.2,0.0,0.0},{-0.9,0.0,0.0,
0.0},{85.4,0.0,0.0,0.0},{0.0,-88.8,0.0,0.0},{-71.0,-0.2,0.0,
0.0},{-70.3,0.0,0.0,0.0},{64.5,0.4,0.0,0.0},{69.8,0.0,0.0,
0.0},{66.1,0.4,0.0,0.0},{-61.0,-0.2,0.0,0.0},{-59.5,-0.1,0.0
,0.0},{-55.6,0.0,0.2,0.0},{51.7,0.2,0.0,0.0},{-49.0,-0.1,0.0
,0.0},{-52.7,-0.1,0.0,0.0},{-49.6,1.4,0.0,0.0},{46.3,0.4,0.0
,0.0},{49.6,0.1,0.0,0.0},{-5.1,0.1,0.0,0.0},{-44.0,-0.1,0.0,
0.0},{-39.9,-0.1,0.0,0.0},{-39.5,-0.1,0.0,0.0},{-3.9,0.1,0.0
,0.0},{-42.1,-0.1,0.0,0.0},{-17.2,0.1,0.0,0.0},{-2.3,0.1,0.0
,0.0},{-39.2,0.0,0.0,0.0},{-38.4,0.1,0.0,0.0},{36.8,0.2,0.0,
0.0},{34.6,0.1,0.0,0.0},{-32.7,0.3,0.0,0.0},{30.4,0.0,0.0,
0.0},{0.4,0.1,0.0,0.0},{29.3,0.2,0.0,0.0},{31.6,0.1,0.0,0.0}
,{0.8,-0.1,0.0,0.0},{-27.9,0.0,0.0,0.0},{2.9,0.0,0.0,0.0},{-
25.3,0.0,0.0,0.0},{25.0,0.1,0.0,0.0},{27.5,0.1,0.0,0.0},{-
24.4,-0.1,0.0,0.0},{24.9,0.2,0.0,0.0},{-22.8,-0.1,0.0,0.0},{
0.9,-0.1,0.0,0.0},{24.4,0.1,0.0,0.0},{23.9,0.1,0.0,0.0},{
22.5,0.1,0.0,0.0},{20.8,0.1,0.0,0.0},{20.1,0.0,0.0,0.0},{
21.5,0.1,0.0,0.0},{-20.0,0.0,0.0,0.0},{1.4,0.0,0.0,0.0},{-
0.2,-0.1,0.0,0.0},{19.0,0.0,-0.1,0.0},{20.5,0.0,0.0,0.0},{-
2.0,0.0,0.0,0.0},{-17.6,-0.1,0.0,0.0},{19.0,0.0,0.0,0.0},{-
2.4,0.0,0.0,0.0},{-18.4,-0.1,0.0,0.0},{17.1,0.0,0.0,0.0},{
0.4,0.0,0.0,0.0},{18.4,0.1,0.0,0.0},{0.0,17.4,0.0,0.0},{-0.6
,0.0,0.0,0.0},{-15.4,0.0,0.0,0.0},{-16.8,-0.1,0.0,0.0},{16.3
,0.0,0.0,0.0},{-2.0,0.0,0.0,0.0},{-1.5,0.0,0.0,0.0},{-14.3,-
0.1,0.0,0.0},{14.4,0.0,0.0,0.0},{-13.4,0.0,0.0,0.0},{-14.3,-
0.1,0.0,0.0},{-13.7,0.0,0.0,0.0},{13.1,0.1,0.0,0.0},{-1.7,
0.0,0.0,0.0},{-12.8,0.0,0.0,0.0},{0.0,-14.4,0.0,0.0},{12.4,
0.0,0.0,0.0},{-12.0,0.0,0.0,0.0},{-0.8,0.0,0.0,0.0},{10.9,
0.1,0.0,0.0},{-10.8,0.0,0.0,0.0},{10.5,0.0,0.0,0.0},{-10.4,
0.0,0.0,0.0},{-11.2,0.0,0.0,0.0},{10.5,0.1,0.0,0.0},{-1.4,
0.0,0.0,0.0},{0.0,0.1,0.0,0.0},{0.7,0.0,0.0,0.0},{-10.3,0.0,
0.0,0.0},{-10.0,0.0,0.0,0.0},{9.6,0.0,0.0,0.0},{9.4,0.1,0.0,
0.0},{0.6,0.0,0.0,0.0},{-87.7,4.4,-0.4,-6.3},{46.3,22.4,0.5,
-2.4},{15.6,-3.4,0.1,0.4},{5.2,5.8,0.2,-0.1},{-30.1,26.9,0.7
,0.0},{23.2,-0.5,0.0,0.6},{1.0,23.2,3.4,0.0},{-12.2,-4.3,0.0
,0.0},{-2.1,-3.7,-0.2,0.1},{-18.6,-3.8,-0.4,1.8},{5.5,-18.7,
-1.8,-0.5},{-5.5,-18.7,1.8,-0.5},{18.4,-3.6,0.3,0.9},{-0.6,
1.3,0.0,0.0},{-5.6,-19.5,1.9,0.0},{5.5,-19.1,-1.9,0.0},{-
17.3,-0.8,0.0,0.9},{-3.2,-8.3,-0.8,0.3},{-0.1,0.0,0.0,0.0},{
-5.4,7.8,-0.3,0.0},{-14.8,1.4,0.0,0.3},{-3.8,0.4,0.0,-0.2},{
12.6,3.2,0.5,-1.5},{0.1,0.0,0.0,0.0},{-13.6,2.4,-0.1,0.0},{
0.9,1.2,0.0,0.0},{-11.9,-0.5,0.0,0.3},{0.4,12.0,0.3,-0.2},{
8.3,6.1,-0.1,0.1},{0.0,0.0,0.0,0.0},{0.4,-10.8,0.3,0.0},{9.6
,2.2,0.3,-1.2}};static int FObaR=sizeof(q6)/sizeof(int)/9;
int foObAR;double Q7,Q8,q9,FOBAZ,q10,q11,Q12,q13,q14,Q15,
foobaz,quux,Q16,Q17,q18;Q7=(Q0-Q4)/q5;Q8=134.96340251*DD2R+
fmod(Q7*(1717915923.2178+Q7*(31.8792+Q7*(0.051635+Q7*(-
0.00024470)))),q3)*DAS2R;q9=357.52910918*DD2R+fmod(Q7*(
129596581.0481+Q7*(-0.5532+Q7*(0.000136+Q7*(-0.00001149)))),
q3)*DAS2R;FOBAZ=93.27209062*DD2R+fmod(Q7*(1739527262.8478+Q7
*(-12.7512+Q7*(-0.001037+Q7*(0.00000417)))),q3)*DAS2R;q10=
297.85019547*DD2R+fmod(Q7*(1602961601.2090+Q7*(-6.3706+Q7*(
0.006539+Q7*(-0.00003169)))),q3)*DAS2R;q11=125.04455501*DD2R
+fmod(Q7*(-6962890.5431+Q7*(7.4722+Q7*(0.007702+Q7*(-
0.00005939)))),q3)*DAS2R;Q12=181.97980085*DD2R+fmod(
210664136.433548*Q7,q3)*DAS2R;q13=355.43299958*DD2R+fmod(
68905077.493988*Q7,q3)*DAS2R;q14=34.35151874*DD2R+fmod(
10925660.377991*Q7,q3)*DAS2R;Q15=50.07744430*DD2R+fmod(
4399609.855732*Q7,q3)*DAS2R;Q17=-153.1*sin(q9)-1.9*sin(2.0*
q9);q18=0.0;for(foObAR=FObaR-1;foObAR>=0;foObAR--){foobaz=((
double)q6[foObAR][0])*Q8+((double)q6[foObAR][1])*q9+((double
)q6[foObAR][2])*FOBAZ+((double)q6[foObAR][3])*q10+((double)
q6[foObAR][4])*q11+((double)q6[foObAR][5])*Q12+((double)q6[
foObAR][6])*q13+((double)q6[foObAR][7])*q14+((double)q6[
foObAR][8])*Q15;quux=cos(foobaz);Q16=sin(foobaz);Q17+=(bar[
foObAR][0]+bar[foObAR][2]*Q7)*quux+(bar[foObAR][1]+bar[
foObAR][3]*Q7)*Q16;q18+=(BAz[foObAR][0]+BAz[foObAR][2]*Q7)*
quux+(BAz[foObAR][1]+BAz[foObAR][3]*Q7)*Q16;}*foo=(Q17*1e-6-
0.042888-0.29856*Q7)*DAS2R;*q1=(q18*1e-6-0.005171-0.02408*Q7
)*DAS2R;*Q2=(84381.412+(-46.80927+(-0.000152+(0.0019989+(-
0.00000051+(-0.000000025)*Q7)*Q7)*Q7)*Q7)*Q7)*DAS2R;}
#undef q3
#undef Q4
#undef q5

void palSlaPm(double Q0,double foo,double Q1,double BAR,double
BAZ,double Q2,double Q3,double q4,double*fobar,double*q5){
static double q6=(365.25*86400.0/149597870.0)*DAS2R;int q7;
double FOOBAR,FOBAZ[3],foobaz,QUuX[3];palSlaDcs2c(Q0,foo,QUuX);
FOOBAR=q6*Q2*BAZ;FOBAZ[0]=-Q1*QUuX[1]-BAR*cos(Q0)*sin(foo)+
FOOBAR*QUuX[0];FOBAZ[1]=Q1*QUuX[0]-BAR*sin(Q0)*sin(foo)+
FOOBAR*QUuX[1];FOBAZ[2]=BAR*cos(foo)+FOOBAR*QUuX[2];foobaz=
q4-Q3;for(q7=0;q7<3;q7++)QUuX[q7]=QUuX[q7]+(foobaz*FOBAZ[q7]
);palSlaDcc2s(QUuX,fobar,q5);*fobar=palSlaDranrm(*fobar);}

void palSlaPrebn(double foo,double q0,double q1[3][3]){double
BAr,q2,Baz,Q3,fobar,FOOBAR,q4;BAr=(foo-1850.0)/100.0;q2=(q0-
foo)/100.0;Baz=q2*DAS2R;Q3=2303.5548+(1.39720+0.000059*BAr)*
BAr;fobar=(Q3+(0.30242-0.000269*BAr+0.017996*q2)*q2)*Baz;
FOOBAR=(Q3+(1.09478+0.000387*BAr+0.018324*q2)*q2)*Baz;q4=(
2005.1125+(-0.85294-0.000365*BAr)*BAr+(-0.42647-0.000365*BAr
-0.041802*q2)*q2)*Baz;palSlaDeuler("\132\131\132",-fobar,q4,-
FOOBAR,q1);}

void palSlaPrec(double q0,double q1,double q2[3][3]){double Q3,
Q4,Q5,FOO,BAR,Q6,BAZ;Q3=(q0-2000.0)/100.0;Q4=(q1-q0)/100.0;
Q5=Q4*DAS2R;FOO=2306.2181+((1.39656-(0.000139*Q3))*Q3);BAR=(
FOO+((0.30188-0.000344*Q3)+0.017998*Q4)*Q4)*Q5;Q6=(FOO+((
1.09468+0.000066*Q3)+0.018203*Q4)*Q4)*Q5;BAZ=((2004.3109+(-
0.85330-0.000217*Q3)*Q3)+((-0.42665-0.000217*Q3)-0.041833*Q4
)*Q4)*Q5;palSlaDeuler("\132\131\132",-BAR,BAZ,-Q6,q2);}

void palSlaPrenut(double FOO,double Q0,double q1[3][3]){double
BAr[3][3],q2[3][3];palSlaPrec(FOO,palSlaEpj(Q0),BAr);palSlaNut(Q0,q2)
;palSlaDmxm(q2,BAr,q1);}

void palSlaRefco(double foo,double Q0,double q1,double BAr,
double q2,double baz,double q3,double Q4,double*FOBAR,double
*q5){double FOOBAR,Q6;static double Q7=0.7853981633974483;
static double q8=1.325817663668033;palSlaRefro(Q7,foo,Q0,q1,BAr
,q2,baz,q3,Q4,&FOOBAR);palSlaRefro(q8,foo,Q0,q1,BAr,q2,baz,q3,
Q4,&Q6);*FOBAR=(64.0*FOOBAR-Q6)/60.0;*q5=(Q6-4.0*FOOBAR)/
60.0;}

static void q0(double,double,double,double,double,double,
double,double,double,double,double,double,double*,double*,
double*);static void FOO(double,double,double,double,double,
double*,double*);void palSlaRefro(double q1,double BAR,double
Q2,double Q3,double q4,double q5,double q6,double q7,double
baz,double*FOBAR)
#define foobar 16384
{static double fobaz=1.623156204;static double Q8=8314.32;
static double FOObAz=28.9644;static double q9=18.0152;static
 double quux=6378120.0;static double q10=18.36;static double
 q11=11000.0;static double FREd=80000.0;double dog;double
Q12;double q13;double q14;double Q15;double Q16,caT,q17,FISH
,GASP,bad;double Q18;double q19;double Q20;double bug;int
SILLY,Q21,Q22,buggy,mum,dad;double q23,q24,q25,DISK,EMPty,
q26,FuLl,q27,FAST,SMALL,big,OK,HeLLo,bYE,q28,MaGIC,oBscuRe,
speed,iNdEX,bAR_FOO,Bar_BAr,Q29,Q30,q31,q32,bar_baz,Q33,q34,
bar_fOBAR,Q35,Q36,BAR_FOOBAR,Q37,q38,bAr_FOBaZ,q39,Q40,q41,
baR_fOoBaz,q42,BaR_qUux,Q43,BAR_FRED,BaR_DoG,Q44,BAr_CAt,
bar_fish,Q45,q46,Q47,Q48;
#define q49(Q50,q51) ((q51)/(Q50+q51));
Q48=0.0;
q23=palSlaDrange(q1);q24=fabs(q23);q24=gmin(q24,fobaz);q25=gmax
(BAR,-1000.0);q25=gmin(q25,FREd);Q12=gmax(Q2,100.0);Q12=gmin
(Q12,500.0);DISK=gmax(Q3,0.0);DISK=gmin(DISK,10000.0);EMPty=
gmax(q4,0.0);EMPty=gmin(EMPty,1.0);q26=gmax(q5,0.1);q13=fabs
(q7);q13=gmax(q13,0.001);q13=gmin(q13,0.01);q28=fabs(baz);
q28=gmax(q28,1e-12);FuLl=gmin(q28,0.1)/2.0;dad=(q26<=100.0);
q27=q26*q26;FAST=9.784*(1.0-0.0026*cos(2.0*q6)-2.8e-7*q25);
SMALL=(dad)?((287.604+1.6288/q27+0.0136/(q27*q27))*273.15/
1013.25)*1e-6:77.6890e-6;bug=FAST*FOObAz/Q8;big=bug/q13;q14=
big-2.0;Q15=q10-2.0;OK=Q12-273.15;HeLLo=pow(10.0,(0.7859+
0.03477*OK)/(1.0+0.00412*OK))*(1.0+DISK*(4.5e-6+6e-10*OK*OK)
);bYE=(DISK>0.0)?EMPty*HeLLo/(1.0-(1.0-EMPty)*HeLLo/DISK):
0.0;q28=bYE*(1.0-q9/FOObAz)*big/(q10-big);Q16=SMALL*(DISK+
q28)/Q12;caT=(SMALL*q28+(dad?11.2684e-6:6.3938e-6)*bYE)/Q12;
q17=(big-1.0)*q13*Q16/Q12;FISH=(q10-1.0)*q13*caT/Q12;GASP=
dad?0.0:375463e-6*bYE/Q12;bad=GASP*Q15*q13/(Q12*Q12);dog=
quux+q25;q0(dog,Q12,q13,q14,Q15,Q16,caT,q17,FISH,GASP,bad,
dog,&MaGIC,&oBscuRe,&speed);iNdEX=oBscuRe*dog*sin(q24);
bAR_FOO=q49(oBscuRe,speed);Q18=quux+gmax(q11,q25);q0(dog,Q12
,q13,q14,Q15,Q16,caT,q17,FISH,GASP,bad,Q18,&q19,&Q20,&
Bar_BAr);Q29=asin(iNdEX/(Q18*Q20));Q30=q49(Q20,Bar_BAr);FOO(
Q18,q19,Q20,bug,Q18,&q31,&q32);bar_baz=asin(iNdEX/(Q18*q31))
;Q33=q49(q31,q32);q34=quux+FREd;FOO(Q18,q19,Q20,bug,q34,&
bar_fOBAR,&Q35);Q36=asin(iNdEX/(q34*bar_fOBAR));BAR_FOOBAR=
q49(bar_fOBAR,Q35);for(Q21=1;Q21<=2;Q21++){Q37=1.0;SILLY=8;
if(Q21==1){q38=q24;bAr_FOBaZ=Q29-q38;q39=bAR_FOO;Q40=Q30;}
else{q38=bar_baz;bAr_FOBaZ=Q36-q38;q39=Q33;Q40=BAR_FOOBAR;}
q41=0.0;baR_fOoBaz=0.0;Q22=1;for(;;){q42=bAr_FOBaZ/(double)
SILLY;BaR_qUux=(Q21==1)?dog:Q18;for(buggy=1;buggy<SILLY;
buggy+=Q22){Q43=sin(q38+q42*(double)buggy);if(Q43>1e-20){q28
=iNdEX/Q43;BAR_FRED=BaR_qUux;mum=0;do{if(Q21==1){q0(dog,Q12,
q13,q14,Q15,Q16,caT,q17,FISH,GASP,bad,BAR_FRED,&Q44,&BAr_CAt
,&bar_fish);}else{FOO(Q18,q19,Q20,bug,BAR_FRED,&BAr_CAt,&
bar_fish);}BaR_DoG=(BAR_FRED*BAr_CAt-q28)/(BAr_CAt+bar_fish)
;BAR_FRED-=BaR_DoG;}while(fabs(BaR_DoG)>1.0&&mum++<=4);
BaR_qUux=BAR_FRED;}if(Q21==1){q0(dog,Q12,q13,q14,Q15,Q16,caT
,q17,FISH,GASP,bad,BaR_qUux,&Q45,&BAr_CAt,&bar_fish);}else{
FOO(Q18,q19,Q20,bug,BaR_qUux,&BAr_CAt,&bar_fish);}q46=q49(
BAr_CAt,bar_fish);if(Q22==1&&buggy%2==0){baR_fOoBaz+=q46;}
else{q41+=q46;}}Q47=q42*(q39+4.0*q41+2.0*baR_fOoBaz+Q40)/3.0
;if(Q21==1)Q48=Q47;if(fabs(Q47-Q37)<=FuLl||SILLY>=foobar)
break;Q37=Q47;SILLY+=SILLY;baR_fOoBaz+=q41;q41=0.0;Q22=2;}}*
FOBAR=Q48+Q47;if(q23<0.0)*FOBAR=-(*FOBAR);}static void q0(
double dog,double Q12,double q13,double q14,double Q15,
double Q16,double caT,double q17,double FISH,double GASP,
double bad,double BaR_qUux,double*Q45,double*BAr_CAt,double*
bar_fish){double q28,Q52,Q53,BAR_GASp;q28=Q12-q13*(BaR_qUux-
dog);q28=gmin(q28,320.0);q28=gmax(q28,100.0);Q52=q28/Q12;Q53
=pow(Q52,q14);BAR_GASp=pow(Q52,Q15);*Q45=q28;*BAr_CAt=1.0+(
Q16*Q53-(caT-GASP/q28)*BAR_GASp)*Q52;*bar_fish=BaR_qUux*(-
q17*Q53+(FISH-bad/Q52)*BAR_GASp);}static void FOO(double Q18
,double q19,double Q20,double bug,double BaR_qUux,double*
BAr_CAt,double*bar_fish){double BAR_BAD,q28;BAR_BAD=bug/q19;
q28=(Q20-1.0)*exp(-BAR_BAD*(BaR_qUux-Q18));*BAr_CAt=1.0+q28;
*bar_fish=-BaR_qUux*BAR_BAD*q28;}
#undef foobar
#undef q49

float palSlaRverot(float Q0,float q1,float foo,float q2)
#define q3 0.4655
{return(float)(q3*cos((double)Q0)*sin((double)(q2-q1))*cos((
double)foo));}
#undef q3

float palSlaRvgalc(float Q0,float FOo){static float q1[3]={-
108.70408f,97.86251f,-164.33610f};float BAR[3];palSlaCs2c(Q0,
FOo,BAR);return palSlaVdv(q1,BAR);}

float palSlaRvlg(float FOO,float BAR){static float baz[3]={-
148.23284f,133.44888f,-224.09467f};float Q0[3];palSlaCs2c(FOO,
BAR,Q0);return palSlaVdv(baz,Q0);}

float palSlaRvlsrd(float foo,float Q0){static float BAr[3]={
0.63823f,14.58542f,-7.80116f};float q1[3];palSlaCs2c(foo,Q0,q1)
;return palSlaVdv(BAr,q1);}

float palSlaRvlsrk(float q0,float Q1){static float foo[3]={-
0.29000f,17.31726f,-10.00141f};float q2[3];palSlaCs2c(q0,Q1,q2)
;return palSlaVdv(foo,q2);}

void palSlaSubet(double Q0,double FOO,double q1,double*BAR,
double*q2){double Q3[3],Q4[3],Baz;int q5;palSlaEtrms(q1,Q3);
palSlaDcs2c(Q0,FOO,Q4);Baz=1.0+palSlaDvdv(Q4,Q3);for(q5=0;q5<3;q5
++){Q4[q5]=Baz*Q4[q5]-Q3[q5];}palSlaDcc2s(Q4,BAR,q2);*BAR=
palSlaDranrm(*BAR);}

void palSlaSupgal(double foo,double baR,double*BAZ,double*q0){
double Q1[3],FOBAR[3];static double Q2[3][3]={{-
0.735742574804,0.677261296414,0.0},{-0.074553778365,-
0.080991471307,0.993922590400},{0.673145302109,
0.731271165817,0.110081262225}};palSlaDcs2c(foo,baR,Q1);
palSlaDimxv(Q2,Q1,FOBAR);palSlaDcc2s(FOBAR,BAZ,q0);*BAZ=palSlaDranrm(
*BAZ);*q0=palSlaDrange(*q0);}

float palSlaVdv(float FOO[3],float BAR[3]){return FOO[0]*BAR[0]
+FOO[1]*BAR[1]+FOO[2]*BAR[2];}


/* Not quite like slaDh2e since it converts from topocentric (az,el) to
   apparent (ha,dec). This includes a correction for diurnal aberration.
   The magnitude of the diurnal aberration vector should be supplied in
   parameter "diurab". The extra code is taken from the Fortran routine
   SLA_OAPQK. */

void palSlaDh2e(double az,double el,double phi,double diurab,double *ha,double *dec){
 double sa,ca,se,ce,sp,cp,x,y,z,r,xmhda,ymhda,zmhda,f;

 sa=sin(az);
 ca=cos(az);
 se=sin(el);
 ce=cos(el);
 sp=sin(phi);
 cp=cos(phi);

/* Cartesian (az,el) to Cartesian (ha,dec) - note, +ha, not -ha. */
 xmhda=-ca*ce*sp+se*cp;
 ymhda=-sa*ce;
 zmhda=ca*ce*cp+se*sp;

/* Correct this vector for diurnal aberration. Since the above
  expressions produce +ha rather than -ha, we do not negate "diurab"
  before using it. Compare this to SLA_AOPQK. */
 f = (1-diurab*ymhda);
 x = f*xmhda;
 y = f*(ymhda+diurab);
 z = f*zmhda;

/* Cartesian (ha,dec) to spherical (ha,dec). */
 r=sqrt(x*x+y*y);
 if (r==0.0) {
    *ha=0.0;
 } else {
    *ha=atan2(y,x);
 }
 *dec=atan2(z,r);
}


/* Not quite like slaDe2h since it converts from apparent (ha,dec) to
   topocentric (az,el). This includes a correction for diurnal
   aberration. The magnitude of the diurnal aberration vector should be
   supplied in parameter "diurab". The extra code is taken from the
   Fortran routine SLA_AOPQK. */

void palSlaDe2h( double ha, double dec, double phi, double diurab,
                 double *az, double *el){
 double sh,ch,sd,cd,sp,cp,x,y,z,r,a,xhd,yhd,zhd,xhdt,yhdt,zhdt,f;

 sh=sin(ha);
 ch=cos(ha);
 sd=sin(dec);
 cd=cos(dec);
 sp=sin(phi);
 cp=cos(phi);

/* Components of cartesian (-ha,dec) vector. */
 xhd = ch*cd;
 yhd = -sh*cd;
 zhd = sd;

/* Modify the above vector to apply diurnal aberration. */
 f = (1.0-diurab*yhd);
 xhdt = f*xhd;
 yhdt = f*(yhd+diurab);
 zhdt = f*zhd;

/* Convert to cartesian (az,el). */
 x=-xhdt*sp+zhdt*cp;
 y=yhdt;
 z=xhdt*cp+zhdt*sp;

/* Convert to spherical (az,el). */
 r=sqrt(x*x+y*y);
 if( r == 0.0 ) {
    a=0.0;
 } else {
    a=atan2(y,x);
 }

 if(a<0.0) a=a+D2PI;

 *az=a;
 *el=atan2(z,r);
}



double palSlaGmsta( double Dt, double uQ){ static double
s2r=7.272205216643039903848712E-5; double r,d1,d2,t; if(Dt<uQ) { d1=Dt;
d2=uQ; } else { d1=uQ; d2=Dt; } t=(d1+(d2-51544.5))/36525.0;
r=palSlaDranrm(s2r*(24110.54841+ (8640184.812866+ (0.093104
-6.2E-6*t)*t)*t +86400.0*(fmod(d1,1.0)+fmod(d2,1.0)))); return r; }

void palSlaPvobs(double p,double h,double stl,double pv[6]){double
r,z,s,c,v;double sr=7.292115855306589E-5;palSlaGeoc(p,h,&r,&z);
s=sin(stl);c=cos(stl);v=sr*r;pv[0]=r*c;pv[1]=r*s;pv[2]=z;
pv[3]=-v*s;pv[4]=v*c;pv[5]=0.0;}




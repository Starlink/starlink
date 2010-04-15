#include <stdio.h>
#include <string.h>
#include <time.h>
#include "slatest/sbmlib.h"
#include "slatest/sbmmac.h"
void vcs ( char *s, char *sok, char *func, char *test, int *status );
void viv ( int ival, int ivalok, char *func, char *test, int *status );
void vlv ( long ival, long ivalok, char *func, char *test, int *status );
void vvd ( double val, double valok, double dval,
char *func, char *test, int *status );
void err ( char *func, char *test, int *status );
void t_addet ( int *status );
void t_afin ( int *status );
void t_airmas ( int *status );
void t_altaz ( int *status );
void t_amp ( int *status );
void t_aop ( int *status );
void t_bear ( int *status );
void t_caf2r ( int *status );
void t_caldj ( int *status );
void t_calyd ( int *status );
void t_cc2s ( int *status );
void t_cc62s ( int *status );
void t_cd2tf ( int *status );
void t_cldj ( int *status );
void t_cr2af ( int *status );
void t_cr2tf ( int *status );
void t_cs2c6 ( int *status );
void t_ctf2d ( int *status );
void t_ctf2r ( int *status );
void t_dat ( int *status );
void t_dbjin ( int *status );
void t_djcal ( int *status );
void t_dmat ( int *status );
void t_dsep ( int *status );
void t_e2h ( int *status );
void t_earth ( int *status );
void t_ecleq ( int *status );
void t_ecmat ( int *status );
void t_ecor ( int *status );
void t_eg50 ( int *status );
void t_epb ( int *status );
void t_epb2d ( int *status );
void t_epco ( int *status );
void t_epj ( int *status );
void t_epj2d ( int *status );
void t_eqecl ( int *status );
void t_eqeqx ( int *status );
void t_eqgal ( int *status );
void t_etrms ( int *status );
void t_evp ( int *status );
void t_fitxy ( int *status );
void t_fk425 ( int *status );
void t_fk45z ( int *status );
void t_fk524 ( int *status );
void t_fk54z ( int *status );
void t_flotin ( int *status );
void t_galeq ( int *status );
void t_galsup ( int *status );
void t_ge50 ( int *status );
void t_gmst ( int *status );
void t_intin ( int *status );
void t_kbj ( int *status );
void t_map ( int *status );
void t_moon ( int *status );
void t_nut ( int *status );
void t_obs ( int *status );
void t_pa ( int *status );
void t_pcd ( int *status );
void t_pda2h ( int *status );
void t_pdq2h ( int *status );
void t_planet ( int *status );
void t_pm ( int *status );
void t_polmo ( int *status );
void t_prebn ( int *status );
void t_prec ( int *status );
void t_preces ( int *status );
void t_prenut ( int *status );
void t_pvobs ( int *status );
void t_range ( int *status );
void t_ranorm ( int *status );
void t_rcc ( int *status );
void t_ref ( int *status );
void t_rv ( int *status );
void t_smat ( int *status );
void t_supgal ( int *status );
void t_svd ( int *status );
void t_tp ( int *status );
void t_tpv ( int *status );
void t_vecmat ( int *status );
void t_zd ( int *status );
int main ()
/*
**  - - - - - - - -
**   s b m t e s t
**  - - - - - - - -
**
**  Exercise slalib as part of a system benchmark.
**
**  Each slalib function is tested to some useful but in most cases
**  not exhaustive level.  Successful completion is signalled by an
**  absence of output messages and a returned status of 1.  Failure
**  of a given function or group of functions results in error
**  messages and a returned status of 0.
**
**  Any messages go to stdio.
**
**  P.T.Wallace   Starlink   6 October 1994
**
**
** Modifications (tmg) :
**   Modified for inclusion as a C benchmark in the Starlink benchmarking
**   package. The tests are executed repetitively with some tests removed
**   from the loop so as to distribute the time the processor spends in
**   each function more evenly. Even so, the execution time is still
**   dominated by libm maths functions. See Below for details:

                                seconds
##############################| 9.9800    __ieee754_sqrt
         #####################| 7.1000    __ieee754_rem_pio2
            ##################| 6.1400    __kernel_cos
               ###############| 4.8700    slaGresid
                ##############| 4.7000    __kernel_sin
                    ##########| 3.4900    slaDeuler
                    ##########| 3.3800    __ieee754_log
                       #######| 2.4400    __sin
                        ######| 2.1600    slaRcc
                        ######| 2.0800    slaRandom
                        ######| 2.0100    slaNutc
                          ####| 1.4800    _time
                          ####| 1.2700    __ieee754_fmod
                          ####| 1.2500    slaEvp
                           ###| 1.1300    t_gresid
                           ###| 1.0800    cos
                           ###| 1.0300    .mul
                           ###| 0.8700    slaSvd
                           ###| 0.8500    vvd
                            ##| 0.7700    atan
                            ##| 0.7300    __fmod
                            ##| 0.6600    slaMoon
                            ##| 0.5900    rand
                            ##| 0.5900    __sqrt
                            ##| 0.5900    log
                            ##| 0.5300    __ieee754_exp
                             #| 0.4800    __ieee754_pow
                             #| 0.4500    idchf
                             #| 0.4400    __ieee754_atan2
                             #| 0.4400    fabs
                             #| 0.3800    slaDmat
                             #| 0.2900    slaDfltin
                             #| 0.2300    atms
                             #| 0.2200    __exp
                             #| 0.2100    slaRefro
                             #| 0.2000    t_vecmat
                             #| 0.1800    __atan2
                             #| 0.1700    slaDmxv
                              | 0.1600    rms
                              | 0.1600    slaPrec
                              | 0.1500    slaDmxm
                              | 0.1500    slaFitxy
                              | 0.1500    slaWait
                              | 0.1400    slaObs
                              | 0.1200    strlen
                              | 0.1100    viv
                              | 0.1100    pow
                              | 0.1100    idchi
                              | 0.1100    slaDcs2c
                              | 0.1100    slaFk524
                              | 0.1100    t_map
                              | 0.1000    atmt
                              | 0.1000    t_fitxy
                              | 0.0900    slaMapqk
                              | 0.0900    slaDimxv
                              | 0.0900    slaSvdcov
                              | 0.0800    slaDcc2s
                              | 0.0700    t_svd
                              | 0.0700    slaAltaz
                              | 0.0700    slaDcmpf
                              | 0.0700    slaDranrm
                              | 0.0700    slaDvn
                              | 0.0700    slaEarth

*/
{
int status;
int iloop;
status = 1;
for ( iloop = 0; iloop <= 500; iloop++ ) {
t_addet ( &status );
t_afin ( &status );
t_airmas ( &status );
t_altaz ( &status );
t_amp ( &status );
t_bear ( &status );
t_caf2r ( &status );
t_caldj ( &status );
t_calyd ( &status );
t_cc2s ( &status );
t_cc62s ( &status );
t_cd2tf ( &status );
t_cldj ( &status );
t_cr2af ( &status );
t_cr2tf ( &status );
t_cs2c6 ( &status );
t_ctf2d ( &status );
t_ctf2r ( &status );
t_dat ( &status );
t_dbjin ( &status );
t_djcal ( &status );
t_dmat ( &status );
t_dsep ( &status );
t_e2h ( &status );
t_earth ( &status );
t_ecleq ( &status );
t_ecmat ( &status );
t_ecor ( &status );
t_eg50 ( &status );
t_epb ( &status );
t_epb2d ( &status );
t_epco ( &status );
t_epj ( &status );
t_epj2d ( &status );
t_eqecl ( &status );
t_eqeqx ( &status );
t_eqgal ( &status );
t_etrms ( &status );
t_evp ( &status );
t_fitxy ( &status );
t_fk425 ( &status );
t_fk45z ( &status );
t_fk524 ( &status );
t_fk54z ( &status );
t_flotin ( &status );
t_galeq ( &status );
t_galsup ( &status );
t_ge50 ( &status );
t_gmst ( &status );
t_intin ( &status );
t_kbj ( &status );
t_map ( &status );
t_moon ( &status );
t_nut ( &status );
t_obs ( &status );
t_pa ( &status );
t_pcd ( &status );
t_pda2h ( &status );
t_pdq2h ( &status );
t_planet ( &status );
t_pm ( &status );
t_polmo ( &status );
t_prebn ( &status );
t_prec ( &status );
t_preces ( &status );
t_prenut ( &status );
t_pvobs ( &status );
t_range ( &status );
t_ranorm ( &status );
t_rcc ( &status );
t_rv ( &status );
t_smat ( &status );
t_supgal ( &status );
t_svd ( &status );
t_tp ( &status );
t_tpv ( &status );
t_vecmat ( &status );
t_zd ( &status );
}

/* The following tests are outside the loop, since they seem to dominate  */
/* the CPU usage. t_aop does a lot of refractive index corrections and    */
/* t_wait does a CPU bound loop for 5 seconds to implement a 'wait'.      */

   t_aop ( &status );
   t_ref ( &status );


if ( status != 1 ) {
status = 0;
printf ( "\n! slalib validation failed !\n" );
}
return status;
}
void vcs ( char *s, char *sok, char *func, char *test, int *status )
{
    if ( strcmp ( s, sok ) != 0 ) {
        printf ( "%s test %s fails, %s != %s \n", func, test,  s, sok );
        *status = 0;
    }
}

void viv ( int ival, int ivalok, char *func, char *test, int *status )
{
    if ( ival != ivalok ) {
        printf ( "%s test %s fails, %d != %d \n", func, test, ival, ivalok );
        *status = 0;
    }
}

void vlv ( long ival, long ivalok, char *func, char *test, int *status )
{
    if ( ival != ivalok ) {
        printf ( "%s test %s fails, %ld != %ld \n", func, test, ival, ivalok );
        *status = 0;
    }
}

void vvd ( double val, double valok, double dval,
           char *func, char *test, int *status )
{
    if ( fabs ( val - valok ) > 1.0E9 * dval ) {
        printf ( "%s test %s fails, %.14g != %.14g \n", func, test, val, valok );
        *status = 0;
    }
}

void t_addet ( int *status )
{
double rm = 2.0;
double dm = -1.0;
double eq = 1975.0;
double r1, d1, r2, d2;
sbmaddet ( rm, dm, eq, &r1, &d1 );
vvd ( r1 - rm, 2.983864874295250e-6, 1e-12, "slaAddet", "r", status );
vvd ( d1 - dm, 2.379650804185118e-7, 1e-12, "slaAddet", "d", status );
sbmsubet ( r1, d1, eq, &r2, &d2 );
vvd ( r2 - rm, 0.0, 1e-12, "slaSubet", "r", status );
vvd ( d2 - dm, 0.0, 1e-12, "slaSubet", "d", status );
}
void t_afin ( int *status )
{
int i, j;
float f;
double d;
char *s = "12 34 56.7 |";
i = 1;
sbmafin(s, &i, &f, &j);
viv ( i, 12, "slaAfin", "i", status );
vvd ( (double) f , 0.2196045986911432, 1e-6, "slaAfin", "a", status );
viv ( j, 0, "slaAfin", "j", status );
i = 1;
sbmdafin(s, &i, &d, &j);
viv ( i, 12, "slaDafin", "i", status );
vvd ( d, 0.2196045986911432, 1e-12, "slaDafin", "a", status );
viv ( j, 0, "slaDafin", "j", status );
}
void t_airmas ( int *status )
{
vvd ( sbmairmas ( 1.2354 ), 3.015698990074724, 1e-12,
"slaAirmas", "", status );
}
void t_altaz ( int *status )
{
double az, azd, azdd, el, eld, eldd, pa, pad, padd;
sbmaltaz ( 0.7, -0.7, -0.65,
&az, &azd, &azdd, &el, &eld, &eldd, &pa, &pad, &padd );
vvd ( az, 4.400560746660174, 1e-12, "slaAltaz", "az", status );
vvd ( azd, -0.2015438937145421, 1e-13, "slaAltaz", "azd", status );
vvd ( azdd, -0.4381266949668748, 1e-13, "slaAltaz", "azdd", status );
vvd ( el, 1.026646506651396, 1e-12, "slaAltaz", "el", status );
vvd ( eld, -0.7576920683826450, 1e-13, "slaAltaz", "eld", status );
vvd ( eldd, 0.04922465406857453, 1e-14, "slaAltaz", "eldd", status );
vvd ( pa, 1.707639969653937, 1e-12, "slaAltaz", "pa", status );
vvd ( pad, 0.4717832355365627, 1e-13, "slaAltaz", "pad", status );
vvd ( padd, -0.2957914128185515, 1e-13, "slaAltaz", "padd", status );
}
void t_amp ( int *status )
{
double rm, dm;
sbmamp ( 2.345, -1.234, 50100.0, 1990.0, &rm, &dm );
vvd ( rm, 2.344472214378815, 1e-12, "slaAmp", "r", status );
vvd ( dm, -1.233573077897767, 1e-12, "slaAmp", "d", status );
}
void t_aop ( int *status )
{
int i;
double rap, dap, date, dut, elongm, phim, hm, xp, yp, tdk, pmb,
rh, wl, tlr, aob, zob, hob, dob, rob, aoprms[14];
dap = -0.1234;
date = 51000.1;
dut = 25.0;
elongm = 2.1;
phim = 0.5;
hm = 3000.0;
xp = -0.5e-6;
yp = 1e-6;
tdk = 280.0;
pmb = 550.0;
rh = 0.6;
tlr = 0.006;
for ( i = 1; i <= 3; i++ ) {
if ( i == 1 ) {
rap = 2.7;
wl = 0.45;
} else if ( i == 2 ) {
rap = 2.345;
} else {
wl = 1e6;
}
sbmaop ( rap, dap, date, dut, elongm, phim, hm, xp, yp, tdk,
pmb, rh, wl, tlr, &aob, &zob, &hob, &dob, &rob );
if ( i == 1 ) {
vvd ( aob, 1.812817892476956, 1e-10, "slaAop", "lo aob", status );
vvd ( zob, 1.393860677500245, 1e-10, "slaAop", "lo zob", status );
vvd ( hob, -1.297807835147504, 1e-10, "slaAop", "lo hob", status );
vvd ( dob, -0.1229670782142078, 1e-10, "slaAop", "lo dob", status );
vvd ( rob, 2.699270317765674, 1e-10, "slaAop", "lo rob", status );
} else if ( i == 2 ) {
vvd ( aob, 2.019928163768254, 1e-10, "slaAop", "aob/o", status );
vvd ( zob, 1.101316023691672, 1e-10, "slaAop", "zob/o", status );
vvd ( hob, -0.9432921619874285, 1e-10, "slaAop", "hob/o", status );
vvd ( dob, -0.1232144782991837, 1e-10, "slaAop", "dob/o", status );
vvd ( rob, 2.344754644605597, 1e-10, "slaAop", "rob/o", status );
} else {
vvd ( aob, 2.019928163768254, 1e-10, "slaAop", "aob/r", status );
vvd ( zob, 1.101268433234379, 1e-10, "slaAop", "zob/r", status );
vvd ( hob, -0.9432539625716996, 1e-10, "slaAop", "hob/r", status );
vvd ( dob, -0.1231857086539891, 1e-10, "slaAop", "dob/r", status );
vvd ( rob, 2.344716445189869, 1e-10, "slaAop", "rob/r", status );
}
}
date = 48000.3;
wl = 0.45;
sbmaoppa ( date, dut, elongm, phim, hm, xp, yp, tdk,
pmb, rh, wl, tlr, aoprms );
vvd ( aoprms[0], 0.4999993892136306, 1e-13, "slaAoppa", "0", status );
vvd ( aoprms[1], 0.4794250025886467, 1e-13, "slaAoppa", "1", status );
vvd ( aoprms[2], 0.8775828547167932, 1e-13, "slaAoppa", "2", status );
vvd ( aoprms[3], 1.363180872136126e-6, 1e-13, "slaAoppa", "3", status );
vvd ( aoprms[4], 3000.0, 1e-10, "slaAoppa", "4", status );
vvd ( aoprms[5], 280.0, 1e-11, "slaAoppa", "5", status );
vvd ( aoprms[6], 550.0, 1e-11, "slaAoppa", "6", status );
vvd ( aoprms[7], 0.6, 1e-13, "slaAoppa", "7", status );
vvd ( aoprms[8], 0.45, 1e-13, "slaAoppa", "8", status );
vvd ( aoprms[9], 0.006, 1e-15, "slaAoppa", "9", status );
vvd ( aoprms[10], 0.0001562740687424627, 1e-13, "slaAoppa", "10", status );
vvd ( aoprms[11], -1.792226374164214e-7, 1e-13, "slaAoppa", "11", status );
vvd ( aoprms[12], 2.101874242809689, 1e-13, "slaAoppa", "12", status );
vvd ( aoprms[13], 7.601916813393611, 1e-8, "slaAoppa", "13", status );
sbmoap ( "r", 1.6, -1.01, date, dut, elongm, phim, hm, xp, yp, tdk,
pmb, rh, wl, tlr, &rap, &dap );
vvd ( rap, 1.601197520673055, 1e-10, "slaOap", "Rr", status );
vvd ( dap, -1.012528463483862, 1e-10, "slaOap", "Rd", status );
sbmoap ( "h", -1.234, 2.34, date, dut, elongm, phim, hm, xp, yp, tdk,
pmb, rh, wl, tlr, &rap, &dap );
vvd ( rap, 5.693087749203874, 1e-10, "slaOap", "Hr", status );
vvd ( dap, 0.8010281394195191, 1e-10, "slaOap", "Hd", status );
sbmoap ( "a", 6.1, 1.1, date, dut, elongm, phim, hm, xp, yp, tdk,
pmb, rh, wl, tlr, &rap, &dap );
vvd ( rap, 5.894305259460625, 1e-10, "slaOap", "Ar", status );
vvd ( dap, 1.406150710678982, 1e-10, "slaOap", "Ad", status );
sbmoapqk ( "r", 2.1, -0.345, aoprms, &rap, &dap );
vvd ( rap, 2.100239618105427, 1e-10, "slaOapqk", "Rr", status );
vvd ( dap, -0.3452428595613835, 1e-10, "slaOapqk", "Rd", status );
sbmoapqk ( "h", -0.01, 1.03, aoprms, &rap, &dap );
vvd ( rap, 1.328731944843008, 1e-10, "slaOapqk", "Hr", status );
vvd ( dap, 1.030091534979027, 1e-10, "slaOapqk", "Hd", status );
sbmoapqk ( "a", 4.321, 0.987, aoprms, &rap, &dap );
vvd ( rap, 0.4375507302147783, 1e-10, "slaOapqk", "Ar", status );
vvd ( dap, -0.01520897927762213, 1e-10, "slaOapqk", "Ad", status );
sbmaoppat ( date + DS2R, aoprms );
vvd ( aoprms[13], 7.602374990557347, 1e-8, "slaAoppat", "", status );
}
void t_bear ( int *status )
{
vvd ( (double) sbmbear ( 1.234f, -0.123f, 2.345f, 0.789f ),
0.7045970341781791, 1e-6, "slaBear", "", status );
vvd ( sbmdbear ( 1.234, -0.123, 2.345, 0.789 ),
0.7045970341781791, 1e-12, "slaDbear", "", status );
}
void t_caf2r ( int *status )
{
float r;
double dr;
int j;
sbmcaf2r ( 76, 54, 32.1f, &r, &j );
vvd ( (double) r, 1.342313819975276, 1e-6, "slaCaf2r", "r", status );
viv ( j, 0, "slaCaf2r", "j", status );
sbmdaf2r ( 76, 54, 32.1, &dr, &j );
vvd ( dr, 1.342313819975276, 1e-12, "slaDaf2r", "r", status );
viv ( j, 0, "slaCaf2r", "j", status );
}
void t_caldj ( int *status )
{
double djm;
int j;
sbmcaldj ( 1999, 12, 31, &djm, &j );
vvd ( djm, 51543.0, 0.0, "slaCaldj", "", status );
}
void t_calyd ( int *status )
{
int ny, nd, j;
sbmcalyd ( 46, 4, 30, &ny, &nd, &j );
viv ( ny, 2046, "slaCalyd", "y", status );
viv ( nd, 120, "slaCalyd", "d", status );
viv ( j, 0, "slaCalyd", "j", status );
sbmclyd ( -5000, 1, 1, &ny, &nd, & j);
viv ( j, 1, "slaClyd", "illegal year", status );
sbmclyd ( 1900, 0, 1, &ny, &nd, & j);
viv ( j, 2, "slaClyd", "illegal month", status );
sbmclyd ( 1900, 2, 29, &ny, &nd, & j);
viv ( ny, 1900, "slaClyd", "illegal day (y)", status );
viv ( nd, 61, "slaClyd", "illegal day (d)", status );
viv ( j, 3, "slaClyd", "illegal day (j)", status );
sbmclyd ( 2000, 2, 29, &ny, &nd, & j);
viv ( ny, 2000, "slaClyd", "y", status );
viv ( nd, 60, "slaClyd", "d", status );
viv ( j, 0, "slaClyd", "j", status );
}
void t_cc2s ( int *status )
{
float v[3] = { 100.0f, -50.0f, 25.0f }, a, b;
double dv[3] = { 100.0, -50.0, 25.0 }, da, db;
sbmcc2s ( v, &a, &b );
vvd ( (double) a, -0.4636476090008061, 1e-6, "slaCc2s", "a", status );
vvd ( (double) b, 0.2199879773954594, 1e-6, "slaCc2s", "b", status );
sbmdcc2s ( dv, &da, &db );
vvd ( da, -0.4636476090008061, 1e-12, "slaDcc2s", "a", status );
vvd ( db, 0.2199879773954594, 1e-12, "slaDcc2s", "b", status );
}
void t_cc62s ( int *status )
{
float v[6] = { 100.0f, -50.0f, 25.0f, -0.1f, 0.2f, 0.7f },
a, b, r, ad, bd, rd;
double dv[6] = { 100.0, -50.0, 25.0, -0.1, 0.2, 0.7 },
da, db, dr, dad, dbd, drd;
sbmcc62s ( v, &a, &b, &r, &ad, &bd, &rd );
vvd ( (double) a, -0.4636476090008061, 1e-6, "slaCc62s", "a", status );
vvd ( (double) b, 0.2199879773954594, 1e-6, "slaCc62s", "b", status );
vvd ( (double) r, 114.564392373896, 1e-3, "slaCc62s", "r", status );
vvd ( (double) ad, 0.001200000000000000, 1e-9, "slaCc62s", "ad", status );
vvd ( (double) bd, 0.006303582107999407, 1e-8, "slaCc62s", "bd", status );
vvd ( (double) rd, -0.02182178902359925, 1e-7, "slaCc62s", "rd", status );
sbmdc62s ( dv, &da, &db, &dr, &dad, &dbd, &drd );
vvd ( da, -0.4636476090008061, 1e-6, "slaDc62s", "a", status );
vvd ( db, 0.2199879773954594, 1e-6, "slaDc62s", "b", status );
vvd ( dr, 114.564392373896, 1e-9, "slaDc62s", "r", status );
vvd ( dad, 0.001200000000000000, 1e-15, "slaDc62s", "ad", status );
vvd ( dbd, 0.006303582107999407, 1e-14, "slaDc62s", "bd", status );
vvd ( drd, -0.02182178902359925, 1e-13, "slaDc62s", "rd", status );
}
void t_cd2tf ( int *status )
{
char s;
int ihmsf[4];
sbmcd2tf ( 4, -0.987654321f, &s, ihmsf );
viv ( (int) s, (int) '-', "slaCd2tf", "s", status );
viv ( ihmsf[0], 23, "slaCd2tf", "[0]", status );
viv ( ihmsf[1], 42, "slaCd2tf", "[1]", status );
viv ( ihmsf[2], 13, "slaCd2tf", "[2]", status );
vvd ( (double) ihmsf[3], 3333.0, 1000.0, "slaCd2tf", "[3]", status );
sbmdd2tf ( 4, -0.987654321, &s, ihmsf );
viv ( (int) s, (int) '-', "slaDd2tf", "s", status );
viv ( ihmsf[0], 23, "slaDd2tf", "[0]", status );
viv ( ihmsf[1], 42, "slaDd2tf", "[1]", status );
viv ( ihmsf[2], 13, "slaDd2tf", "[2]", status );
viv ( ihmsf[3], 3333, "slaDd2tf", "[3]", status );
}
void t_cldj ( int *status )
{
double d;
int j;
sbmcldj ( 1899, 12, 31, &d, &j );
vvd ( d, 15019.0, 0.0, "slaCldj", "d", status );
viv ( j, 0, "slaCldj", "j", status );
}
void t_cr2af ( int *status )
{
char s;
int idmsf[4];
sbmcr2af ( 4, 2.345f, &s, idmsf );
viv ( (int) s, (int) '+', "slaCr2af", "s", status );
viv ( idmsf[0], 134, "slaCr2af", "[0]", status );
viv ( idmsf[1], 21, "slaCr2af", "[1]", status );
viv ( idmsf[2], 30, "slaCr2af", "[2]", status );
vvd ( (double) idmsf[3], 9706.0, 1000.0, "slaCr2af", "[3]", status );
sbmdr2af ( 4, 2.345, &s, idmsf );
viv ( (int) s, (int) '+', "slaDr2af", "s", status );
viv ( idmsf[0], 134, "slaDr2af", "[0]", status );
viv ( idmsf[1], 21, "slaDr2af", "[1]", status );
viv ( idmsf[2], 30, "slaDr2af", "[2]", status );
viv ( idmsf[3], 9706, "slaDr2af", "[3]", status );
}
void t_cr2tf ( int *status )
{
char s;
int ihmsf[4];
sbmcr2tf ( 4, -3.01234f, &s, ihmsf );
viv ( (int) s, (int) '-', "slaCr2tf", "s", status );
viv ( ihmsf[0], 11, "slaCr2tf", "[0]", status );
viv ( ihmsf[1], 30, "slaCr2tf", "[1]", status );
viv ( ihmsf[2], 22, "slaCr2tf", "[2]", status );
vvd ( (double) ihmsf[3], 6484.0, 1000.0, "slaCr2tf", "[3]", status );
sbmdr2tf ( 4, -3.01234, &s, ihmsf );
viv ( (int) s, (int) '-', "slaDr2tf", "s", status );
viv ( ihmsf[0], 11, "slaDr2tf", "[0]", status );
viv ( ihmsf[1], 30, "slaDr2tf", "[1]", status );
viv ( ihmsf[2], 22, "slaDr2tf", "[2]", status );
viv ( ihmsf[3], 6484, "slaDr2tf", "[3]", status );
}
void t_cs2c6 ( int *status )
{
float v[6];
double dv[6];
sbmcs2c6( -3.21f, 0.123f, 0.456f, -7.8e-6f, 9.01e-6f, -1.23e-5f, v );
vvd ( (double) v[0], -0.4514964673880165,
1e-6, "slaCs2c6", "x", status );
vvd ( (double) v[1],  0.03093394277342585,
1e-6, "slaCs2c6", "y", status );
vvd ( (double) v[2],  0.05594668105108779,
1e-6, "slaCs2c6", "z", status );
vvd ( (double) v[3],  1.292270850663260e-5,
1e-6, "slaCs2c6", "xd", status );
vvd ( (double) v[4],  2.652814182060692e-6,
1e-6, "slaCs2c6", "yd", status );
vvd ( (double) v[5],  2.568431853930293e-6,
1e-6, "slaCs2c6", "zd", status );
sbmds2c6( -3.21, 0.123, 0.456, -7.8e-6, 9.01e-6, -1.23e-5, dv );
vvd ( dv[0], -0.4514964673880165, 1e-12, "slaDs2c6", "x", status );
vvd ( dv[1],  0.03093394277342585, 1e-12, "slaDs2c6", "y", status );
vvd ( dv[2],  0.05594668105108779, 1e-12, "slaDs2c6", "z", status );
vvd ( dv[3],  1.292270850663260e-5, 1e-12, "slaDs2c6", "xd", status );
vvd ( dv[4],  2.652814182060692e-6, 1e-12, "slaDs2c6", "yd", status );
vvd ( dv[5],  2.568431853930293e-6, 1e-12, "slaDs2c6", "zd", status );
}
void t_ctf2d ( int *status )
{
float d;
double dd;
int j;
sbmctf2d (23, 56, 59.1f, &d, &j);
vvd ( (double) d, 0.99790625, 1e-6, "slaCtf2d", "d", status );
viv ( j, 0, "slaCtf2d", "j", status );
sbmdtf2d (23, 56, 59.1, &dd, &j);
vvd ( dd, 0.99790625, 1e-12, "slaDtf2d", "d", status );
viv ( j, 0, "slaDtf2d", "j", status );
}
void t_ctf2r ( int *status )
{
float r;
double dr;
int j;
sbmctf2r (23, 56, 59.1f, &r, &j);
vvd ( (double) r, 6.270029887942679, 1e-6, "slaCtf2r", "r", status );
viv ( j, 0, "slaCtf2r", "j", status );
sbmdtf2r (23, 56, 59.1, &dr, &j);
vvd ( dr, 6.270029887942679, 1e-12, "slaDtf2r", "r", status );
viv ( j, 0, "slaDtf2r", "j", status );
}
void t_dat ( int *status )
{
vvd ( sbmdat ( 43900.0 ), 18.0, 0.0, "slaDat", "", status );
vvd ( sbmdtt ( 41317.0 ), 42.184, 0.0, "slaDtt", "", status );
vvd ( sbmdt ( 500.0 ), 4686.7, 1e-10, "slaDt", "500", status );
vvd ( sbmdt ( 1400.0 ), 408.0, 1e-11, "slaDt", "1400", status );
vvd ( sbmdt ( 1950.0 ), 27.99145626, 1e-12, "slaDt", "1950", status );
}
void t_dbjin ( int *status )
{
char s[] = "  B1950, , J 2000, b1975 JE     ";
double d;
int i, ja, jb;
i = 1;
d = 0.0;
sbmdbjin ( s, &i, &d, &ja, &jb );
viv ( i, 9, "slaDbjin", "i1", status );
vvd ( d, 1950.0, 0.0, "slaDbjin", "d1", status );
viv ( ja, 0, "slaDbjin", "ja1", status );
viv ( jb, 1, "slaDbjin", "jb1", status );
sbmdbjin ( s, &i, &d, &ja, &jb );
viv ( i, 11, "slaDbjin", "i2", status );
vvd ( d, 1950.0, 0.0, "slaDbjin", "d2", status );
viv ( ja, 1, "slaDbjin", "ja2", status );
viv ( jb, 0, "slaDbjin", "jb2", status );
sbmdbjin ( s, &i, &d, &ja, &jb );
viv ( i, 19, "slaDbjin", "i3", status );
vvd ( d, 2000.0, 0.0, "slaDbjin", "d3", status );
viv ( ja, 0, "slaDbjin", "ja3", status );
viv ( jb, 2, "slaDbjin", "jb3", status );
sbmdbjin ( s, &i, &d, &ja, &jb );
viv ( i, 26, "slaDbjin", "i4", status );
vvd ( d, 1975.0, 0.0, "slaDbjin", "d4", status );
viv ( ja, 0, "slaDbjin", "ja4", status );
viv ( jb, 1, "slaDbjin", "jb4", status );
sbmdbjin ( s, &i, &d, &ja, &jb );
viv ( i, 26, "slaDbjin", "i5", status );
vvd ( d, 1975.0, 0.0, "slaDbjin", "d5", status );
viv ( ja, 1, "slaDbjin", "ja5", status );
viv ( jb, 0, "slaDbjin", "jb5", status );
}
void t_djcal ( int *status )
{
double djm = 50123.9999;
int iydmf[4], j, iy, im, id;
double f;
sbmdjcal ( 4, djm, iydmf, &j );
viv ( iydmf[0], 1996, "slaDjcal", "y", status );
viv ( iydmf[1], 2, "slaDjcal", "m", status );
viv ( iydmf[2], 10, "slaDjcal", "d", status );
viv ( iydmf[3], 9999, "slaDjcal", "f", status );
viv ( j, 0, "slaDjcal", "j", status );
sbmdjcl ( djm, &iy, &im, &id, &f, &j );
viv ( iy, 1996, "slaDjcl", "y", status );
viv ( im, 2, "slaDjcl", "m", status );
viv ( id, 10, "slaDjcl", "d", status );
vvd ( f, 0.9999, 1e-7, "slaDjcl", "f", status );
viv ( j, 0, "slaDjcl", "j", status );
}
void t_dmat ( int *status )
{
double da[3][3] = {
{ 2.22,     1.6578,     1.380522     },
{ 1.6578,   1.380522,   1.22548578   },
{ 1.380522, 1.22548578, 1.1356276122 }
};
double dv[3] = {  2.28625, 1.7128825, 1.429432225 };
double dd;
int j, iw[3];
sbmdmat ( 3, (double *) da, dv, &dd, &j, iw );
vvd ( da[0][0], 18.02550629769198,
1e-10, "slaDmat", "a[0][0]", status );
vvd ( da[0][1], -52.16386644917481,
1e-10, "slaDmat", "a[0][1]", status );
vvd ( da[0][2], 34.37875949717994,
1e-10, "slaDmat", "a[0][2]", status );
vvd ( da[1][0], -52.16386644917477,
1e-10, "slaDmat", "a[1][0]", status );
vvd ( da[1][1], 168.1778099099869,
1e-10, "slaDmat", "a[1][1]", status );
vvd ( da[1][2], -118.0722869694278,
1e-10, "slaDmat", "a[1][2]", status );
vvd ( da[2][0], 34.37875949717988,
1e-10, "slaDmat", "a[2][0]", status );
vvd ( da[2][1], -118.07228696942770,
1e-10, "slaDmat", "a[2][1]", status );
vvd ( da[2][2], 86.50307003740468,
1e-10, "slaDmat", "a[2][2]", status );
vvd ( dv[0], 1.002346480763383,
1e-12, "slaDmat", "v[0]", status );
vvd ( dv[1], 0.0328559401697292,
1e-12, "slaDmat", "v[1]", status );
vvd ( dv[2], 0.004760688414898454,
1e-12, "slaDmat", "v[2]", status );
vvd ( dd, 0.003658344147359863,
1e-12, "slaDmat", "d", status );
viv ( j, 0, "slaDmat", "j", status );
}
void t_dsep ( int *status )
{
vvd ( sbmdsep ( 3.0, -1.0, -0.5, 0.3 ), 2.392112332415784, 1e-13,
"slaDsep", "", status );
vvd ( (double) sbmsep ( 3.0f, -1.0f, -0.5f, 0.3f), 2.392112332415784, 1e-6,
"slaSep", "", status );
}
void t_e2h ( int *status )
{
double dh, dd, dp, da, de;
float h, d, p, a, e;
dh = -0.3;
dd = -1.1;
dp = -0.7;
h = (float) dh;
d = (float) dd;
p = (float) dp;
sbmde2h ( dh, dd, dp, &da, &de );
vvd ( da, 2.820087515852369, 1e-12, "slaDe2h", "az", status );
vvd ( de, 1.132711866443304, 1e-12, "slaDe2h", "el", status );
sbme2h ( h, d, p, &a, &e );
vvd ( (double) a, 2.820087515852369, 1e-6, "slaE2h", "az", status );
vvd ( (double) e, 1.132711866443304, 1e-6, "slaE2h", "el", status );
sbmdh2e ( da, de, dp, &dh, &dd );
vvd ( dh, -0.3, 1e-12, "slaDh2e", "HA", status );
vvd ( dd, -1.1, 1e-12, "slaDh2e", "dec", status );
sbmh2e ( a, e, p, &h, &d );
vvd ( (double) h, -0.3, 1e-6, "slaH2e", "HA", status );
vvd ( (double) d, -1.1, 1e-6, "slaH2e", "dec", status );
}
void t_earth ( int *status )
{
float pv[6];
sbmearth ( 1978, 174, 0.87f, pv );
vvd ( (double) pv[0], 3.590843e-2, 1e-6, "slaEarth", "pv[0]", status );
vvd ( (double) pv[1], -9.319286e-1, 1e-6, "slaEarth", "pv[1]", status );
vvd ( (double) pv[2], -4.041040e-1, 1e-6, "slaEarth", "pv[2]", status );
vvd ( (double) pv[3], 1.956930e-7, 1e-13, "slaEarth", "pv[3]", status );
vvd ( (double) pv[4], 5.743768e-9, 1e-13, "slaEarth", "pv[4]", status );
vvd ( (double) pv[5], 2.511983e-9, 1e-13, "slaEarth", "pv[5]", status );
}
void t_ecleq ( int *status )
{
double r, d;
sbmecleq ( 1.234, -0.123, 43210.0, &r, &d );
vvd ( r, 1.229910118208851, 1e-12, "slaEcleq", "RA", status );
vvd ( d, 0.2638461400411088, 1e-12, "slaEcleq", "Dec", status );
}
void t_ecmat ( int *status )
{
double rm[3][3];
sbmecmat ( 41234.0, rm );
vvd ( rm[0][0], 1.0, 1e-12, "slaEcmat", "[0][0]", status );
vvd ( rm[0][1], 0.0, 1e-12, "slaEcmat", "[0][1]", status );
vvd ( rm[0][2], 0.0, 1e-12, "slaEcmat", "[0][2]", status );
vvd ( rm[1][0], 0.0, 1e-12, "slaEcmat", "[1][0]", status );
vvd ( rm[1][1], 0.917456575085716, 1e-12,
"slaEcmat", "[1][1]", status );
vvd ( rm[1][2], 0.397835937079581, 1e-12,
"slaEcmat", "[1][2]", status );
vvd ( rm[2][0], 0.0, 1e-12, "slaEcmat", "[2][0]", status );
vvd ( rm[2][1], -0.397835937079581, 1e-12,
"slaEcmat", "[2][1]", status );
vvd ( rm[2][2], 0.917456575085716, 1e-12,
"slaEcmat", "[2][2]", status );
}
void t_ecor ( int *status )
{
float rv, tl;
sbmecor ( 2.345f, -0.567f, 1995, 306, 0.037f, &rv, &tl );
vvd ( (double) rv, -19.182460, 1e-4, "slaEcor", "rv", status );
vvd ( (double) tl, -120.366300, 1e-3, "slaEcor", "tl", status );
}
void t_eg50 ( int *status )
{
double dl, db;
sbmeg50 ( 3.012, 1.234, &dl, &db );
vvd ( dl, 2.305557953813397, 1e-12, "slaEg50", "l", status );
vvd ( db, 0.7903600886585871, 1e-12, "slaEg50", "b", status );
}
void t_epb ( int *status )
{
vvd ( sbmepb ( 45123.0 ), 1982.419793168669, 1e-8,
"slaEpb", "", status );
}
void t_epb2d ( int *status )
{
vvd ( sbmepb2d ( 1975.5 ), 42595.5995279655, 1e-7,
"slaEpb2d", "", status );
}
void t_epco ( int *status )
{
vvd ( sbmepco ( 'b', 'J', 2000.0 ), 2000.001277513665, 1e-7,
"slaEpco", "bJ", status );
vvd ( sbmepco ( 'J', 'b', 1950.0 ), 1949.999790442300, 1e-7,
"slaEpco", "Jb", status );
vvd ( sbmepco ( 'j', 'J', 2000.0 ), 2000.0, 1e-7,
"slaEpco", "jJ", status );
}
void t_epj ( int *status )
{
vvd ( sbmepj ( 42999.0 ), 1976.603696098563, 1e-7,
"slaEpj", "", status );
}
void t_epj2d ( int *status )
{
vvd ( sbmepj2d ( 2010.077 ), 55225.124250, 1e-6,
"slaEpj2d", "", status );
}
void t_eqecl ( int *status )
{
double dl, db;
sbmeqecl ( 0.789, -0.123, 46555.0, &dl, &db );
vvd ( dl, 0.7036566430349022, 1e-12, "slaEqecl", "l", status );
vvd ( db, -0.4036047164116848, 1e-12, "slaEqecl", "b", status );
}
void t_eqeqx ( int *status )
{
vvd ( sbmeqeqx ( 41234.0 ), 5.376047445838220e-5, 1e-17,
"slaEqeqx", "", status );
}
void t_eqgal ( int *status )
{
double dl, db;
sbmeqgal ( 5.67, -1.23, &dl, &db );
vvd ( dl, 5.612270780904526, 1e-12, "slaEqgal", "dl", status );
vvd ( db, -0.6800521449061520, 1e-12, "slaEqgal", "db", status );
}
void t_etrms ( int *status )
{
double ev[3];
sbmetrms ( 1976.9, ev );
vvd ( ev[0], -1.621617102537041e-6, 1e-18, "slaEtrms", "x", status );
vvd ( ev[1], -3.310070088507914e-7, 1e-18, "slaEtrms", "y", status );
vvd ( ev[2], -1.435296627515719e-7, 1e-18, "slaEtrms", "z", status );
}
void t_evp ( int *status )
{
double dvb[3], dpb[3], dvh[3], dph[3];
sbmevp ( 50100.0, 1990.0, dvb, dpb, dvh, dph );
vvd ( dvb[0], -1.807210068602723e-7, 1e-16, "slaEvp", "dvb[x]", status );
vvd ( dvb[1], -8.385891022440320e-8, 1e-16, "slaEvp", "dvb[y]", status );
vvd ( dvb[2], -3.635846882638055e-8, 1e-16, "slaEvp", "dvb[z]", status );
vvd ( dpb[0], -0.4515615297360333, 1e-8, "slaEvp", "dpb[x]", status );
vvd ( dpb[1],  0.8103788166239596, 1e-8, "slaEvp", "dpb[y]", status );
vvd ( dpb[2],  0.3514505204144827, 1e-8, "slaEvp", "dpb[z]", status );
vvd ( dvh[0], -1.806354061155555e-7, 1e-16, "slaEvp", "dvh[x]", status );
vvd ( dvh[1], -8.383798678086174e-8, 1e-16, "slaEvp", "dvh[y]", status );
vvd ( dvh[2], -3.635185843644782e-8, 1e-16, "slaEvp", "dvh[z]", status );
vvd ( dph[0], -0.4478571659918565, 1e-8, "slaEvp", "dph[x]", status );
vvd ( dph[1],  0.8036439916076232, 1e-8, "slaEvp", "dph[y]", status );
vvd ( dph[2],  0.3484298459102053, 1e-8, "slaEvp", "dph[z]", status );
}
void t_fitxy ( int *status )
{
#define NPTS (8)
double xye[NPTS][2] = {
{ -23.4, -12.1 },
{  32.0, -15.3 },
{  10.9,  23.7 },
{  -3.0,  16.1 },
{  45.0,  32.5 },
{   8.6, -17.0 },
{  15.3,  10.0 },
{ 121.7,  -3.8 }
};
double xym[NPTS][2] = {
{ -23.41,  12.12 },
{  32.03,  15.34 },
{  10.93, -23.72 },
{  -3.01, -16.10 },
{  44.90, -32.46 },
{   8.55,  17.02 },
{  15.31, -10.07 },
{ 120.92,   3.81 }
};
double coeffs[6], xyp[NPTS][2], xrms, yrms, rrms, bkwds[6],
x2, y2, xz, yz, xs, ys, perp, orient;
int j;
sbmfitxy ( 4, NPTS, xye, xym, coeffs, &j );
vvd ( coeffs[0], -7.938263381515947e-3,
1e-12, "slaFitxy", "4/0", status );
vvd ( coeffs[1], 1.004640925187200,
1e-12, "slaFitxy", "4/1", status );
vvd ( coeffs[2], 3.976948048238268e-4,
1e-12, "slaFitxy", "4/2", status );
vvd ( coeffs[3], -2.501031681585021e-2,
1e-12, "slaFitxy", "4/3", status );
vvd ( coeffs[4], 3.976948048238268e-4,
1e-12, "slaFitxy", "4/4", status );
vvd ( coeffs[5], -1.004640925187200,
1e-12, "slaFitxy", "4/5", status );
viv ( j, 0, "slaFitxy", "4/j", status );
sbmfitxy ( 6, NPTS, xye, xym, coeffs, &j );
vvd ( coeffs[0], -2.617232551841476e-2,
1e-12, "slaFitxy", "6/0", status );
vvd ( coeffs[1], 1.005634905041421,
1e-12, "slaFitxy", "6/1", status );
vvd ( coeffs[2], 2.133045023329208e-3,
1e-12, "slaFitxy", "6/2", status );
vvd ( coeffs[3], 3.846993364431164e-3,
1e-12, "slaFitxy", "6/3", status );
vvd ( coeffs[4], 1.301671386431460e-4,
1e-12, "slaFitxy", "6/4", status );
vvd ( coeffs[5], -0.9994827065693964,
1e-12, "slaFitxy", "6/5", status );
viv ( j, 0, "slaFitxy", "6/j", status );
sbmpxy ( NPTS, xye, xym, coeffs, xyp, &xrms, &yrms, &rrms );
vvd ( xyp[0][0], -23.542232946855340, 1e-12, "slaPxy", "x0", status );
vvd ( xyp[0][1], -12.112930622972290, 1e-12, "slaPxy", "y0", status );
vvd ( xyp[1][0], 32.217034593616180, 1e-12, "slaPxy", "x1", status );
vvd ( xyp[1][1], -15.324048471959370, 1e-12, "slaPxy", "y1", status );
vvd ( xyp[2][0], 10.914821358630950, 1e-12, "slaPxy", "x2", status );
vvd ( xyp[2][1], 23.712999520015880, 1e-12, "slaPxy", "y2", status );
vvd ( xyp[3][0], -3.087475414568693, 1e-12, "slaPxy", "x3", status );
vvd ( xyp[3][1], 16.095126766044400, 1e-12, "slaPxy", "y3", status );
vvd ( xyp[4][0], 45.057596269384130, 1e-12, "slaPxy", "x4", status );
vvd ( xyp[4][1], 32.452900153132120, 1e-12, "slaPxy", "y4", status );
vvd ( xyp[5][0], 8.608310538882801, 1e-12, "slaPxy", "x5", status );
vvd ( xyp[5][1], -17.006235743411300, 1e-12, "slaPxy", "y5", status );
vvd ( xyp[6][0], 15.348618307280820, 1e-12, "slaPxy", "x6", status );
vvd ( xyp[6][1], 10.070630707410880, 1e-12, "slaPxy", "y6", status );
vvd ( xyp[7][0], 121.583327293629100, 1e-12, "slaPxy", "x7", status );
vvd ( xyp[7][1], -3.788442308260240, 1e-12, "slaPxy", "y7", status );
vvd ( xrms ,0.1087247110488075, 1e-13, "slaPxy", "xrms", status );
vvd ( yrms, 0.03224481175794666, 1e-13, "slaPxy", "yrms", status );
vvd ( rrms, 0.1134054261398109, 1e-13, "slaPxy", "rrms", status );
sbminvf ( coeffs, bkwds, &j );
vvd ( bkwds[0], 0.02601750208015891, 1e-12, "slaInvf", "0", status);
vvd ( bkwds[1], 0.9943963945040283, 1e-12, "slaInvf", "1", status);
vvd ( bkwds[2], 0.002122190075497872, 1e-12, "slaInvf", "2", status);
vvd ( bkwds[3], 0.003852372795370861, 1e-12, "slaInvf", "3", status);
vvd ( bkwds[4], 0.0001295047252932767, 1e-12, "slaInvf", "4", status);
vvd ( bkwds[5], -1.000517284779212, 1e-12, "slaInvf", "5", status);
viv ( j, 0, "slaInvf", "j", status );
sbmxy2xy ( 44.5, 32.5, coeffs, &x2, &y2 );
vvd ( x2, 44.793904912083030, 1e-11, "slaXy2xy", "x", status);
vvd ( y2, -32.473548532471330, 1e-11, "slaXy2xy", "y", status);
sbmdcmpf ( coeffs, &xz, &yz, &xs, &ys, &perp, &orient );
vvd ( xz, -0.02603383018243591, 1e-12, "slaDcmpf", "xz", status);
vvd ( yz, -0.003845593915717770, 1e-12, "slaDcmpf", "yz", status);
vvd ( xs, -1.005634913465693, 1e-12, "slaDcmpf", "xs", status);
vvd ( ys, 0.9994849826847614, 1e-12, "slaDcmpf", "ys", status);
vvd ( perp, -0.002004707996156263, 1e-12, "slaDcmpf", "p", status);
vvd ( orient, 3.140460861823333, 1e-12, "slaDcmpf", "o", status);
}
void t_fk425 ( int *status )
{
double r2000, d2000, dr2000, dd2000, p2000, v2000;
sbmfk425 ( 1.234, -0.123, -1e-5, 2e-6, 0.5, 20.0,
&r2000, &d2000, &dr2000, &dd2000, &p2000, &v2000 );
vvd ( r2000, 1.244117554618727, 1e-12, "slaFk425", "r", status );
vvd ( d2000, -0.1213164254458709, 1e-12, "slaFk425", "d", status );
vvd ( dr2000, -9.964265838268711e-6, 1e-17, "slaFk425", "dr", status );
vvd ( dd2000, 2.038065265773541e-6, 1e-17, "slaFk425", "dd", status );
vvd ( p2000, 0.4997443812415410, 1e-12, "slaFk425", "p", status );
vvd ( v2000, 20.010460915421010, 1e-11, "slaFk425", "v", status );
}
void t_fk45z ( int *status )
{
double r2000, d2000;
sbmfk45z ( 1.234, -0.123, 1984.0, &r2000, &d2000 );
vvd ( r2000, 1.244616510731691, 1e-12, "slaFk45z", "r", status );
vvd ( d2000, -0.1214185839586555, 1e-12, "slaFk45z", "d", status );
}
void t_fk524 ( int *status )
{
double r1950, d1950, dr1950, dd1950, p1950, v1950;
sbmfk524 ( 4.567, -1.23, -3e-5, 8e-6, 0.29, -35.0,
&r1950, &d1950, &dr1950, &dd1950, &p1950, &v1950 );
vvd ( r1950, 4.543778603272084, 1e-12, "slaFk524", "r", status );
vvd ( d1950, -1.229642790187574, 1e-12, "slaFk524", "d", status );
vvd ( dr1950, -2.957873121769244e-5, 1e-17, "slaFk524", "dr", status );
vvd ( dd1950, 8.117725309659079e-6, 1e-17, "slaFk524", "dd", status );
vvd ( p1950, 0.2898494999992917, 1e-12, "slaFk524", "p", status );
vvd ( v1950, -35.026862824252680, 1e-11, "slaFk524", "v", status );
}
void t_fk54z ( int *status )
{
double r1950, d1950, dr1950, dd1950;
sbmfk54z ( 0.001, -1.55, 1900.0, &r1950, &d1950, &dr1950, &dd1950 );
vvd ( r1950, 6.271585543439484, 1e-12, "slaFk54z", "r", status );
vvd ( d1950, -1.554861715330319, 1e-12, "slaFk54z", "d", status );
vvd ( dr1950, -4.175410876044272e-8, 1e-20, "slaFk54z", "dr", status );
vvd ( dd1950, 2.118595098308522e-8, 1e-20, "slaFk54z", "dd", status );
}
void t_flotin ( int *status )
{
char s[] = "  12.345, , -0 1e3-4 2000  E     ";
float fv;
double dv;
int i, j;
i = 1;
fv = 0.0f;
sbmflotin ( s, &i, &fv, &j );
viv ( i, 10, "slaFlotin", "i1", status );
vvd ( (double) fv, 12.345, 1e-6, "slaFlotin", "v1", status );
viv ( j, 0, "slaFlotin", "j1", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 12, "slaFlotin", "i2", status );
vvd ( (double) fv, 12.345, 1e-6, "slaFlotin", "v2", status );
viv ( j, 1, "slaFlotin", "j2", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 16, "slaFlotin", "i3", status );
vvd ( (double) fv, 0.0, 0.0, "slaFlotin", "v3", status );
viv ( j, -1, "slaFlotin", "j3", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 19, "slaFlotin", "i4", status );
vvd ( (double) fv, 1000.0, 0.0, "slaFlotin", "v4", status );
viv ( j, 0, "slaFlotin", "j4", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 22, "slaFlotin", "i5", status );
vvd ( (double) fv, -4.0, 0.0, "slaFlotin", "v5", status );
viv ( j, -1, "slaFlotin", "j5", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 28, "slaFlotin", "i6", status );
vvd ( (double) fv, 2000.0, 0.0, "slaFlotin", "v6", status );
viv ( j, 0, "slaFlotin", "j6", status );
sbmflotin ( s, &i, &fv, &j );
viv ( i, 34, "slaFlotin", "i7", status );
vvd ( (double) fv, 2000.0, 0.0, "slaFlotin", "v7", status );
viv ( j, 2, "slaFlotin", "j7", status );
i = 1;
dv = 0.0f;
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 10, "slaDfltin", "i1", status );
vvd ( dv, 12.345, 1e-12, "slaDfltin", "v1", status );
viv ( j, 0, "slaDfltin", "j1", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 12, "slaDfltin", "i2", status );
vvd ( dv, 12.345, 1e-12, "slaDfltin", "v2", status );
viv ( j, 1, "slaDfltin", "j2", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 16, "slaDfltin", "i3", status );
vvd ( dv, 0.0, 0.0, "slaDfltin", "v3", status );
viv ( j, -1, "slaDfltin", "j3", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 19, "slaDfltin", "i4", status );
vvd ( dv, 1000.0, 0.0, "slaDfltin", "v4", status );
viv ( j, 0, "slaDfltin", "j4", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 22, "slaDfltin", "i5", status );
vvd ( dv, -4.0, 0.0, "slaDfltin", "v5", status );
viv ( j, -1, "slaDfltin", "j5", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 28, "slaDfltin", "i6", status );
vvd ( dv, 2000.0, 0.0, "slaDfltin", "v6", status );
viv ( j, 0, "slaDfltin", "j6", status );
sbmdfltin ( s, &i, &dv, &j );
viv ( i, 34, "slaDfltin", "i7", status );
vvd ( dv, 2000.0, 0.0, "slaDfltin", "v7", status );
viv ( j, 2, "slaDfltin", "j7", status );
}
void t_galeq ( int *status )
{
double dr, dd;
sbmgaleq ( 5.67, -1.23, &dr, &dd );
vvd ( dr, 0.04729270418071426, 1e-12, "slaGaleq", "dr", status );
vvd ( dd, -0.7834003666745548, 1e-12, "slaGaleq", "dd", status );
}
void t_galsup ( int *status )
{
double dsl, dsb;
sbmgalsup ( 6.1, -1.4, &dsl, &dsb );
vvd ( dsl, 4.567933268859171, 1e-12, "slaGalsup", "dsl", status );
vvd ( dsb, -0.01862369899731829, 1e-12, "slaGalsup", "dsb", status );
}
void t_ge50 ( int *status )
{
double dr, dd;
sbmge50 ( 6.1, -1.55, &dr, &dd );
vvd ( dr, 0.1966825219934508, 1e-12, "slaGe50", "dr", status );
vvd ( dd, -0.4924752701678960, 1e-12, "slaGe50", "dd", status );
}
void t_gmst ( int *status )
{
vvd ( sbmgmst ( 43999.999 ), 3.907497135495134,
1e-8, "slaGmst", "", status );
vvd ( sbmgmsta ( 43999.0, 0.999 ), 3.907497135495134,
1e-10, "slaGmsta", "", status );
}
void t_intin ( int *status )
{
char s[] = "  -12345, , -0  2000  +     ";
long n;
int i, j;
i = 1;
n = 0;
sbmintin ( s, &i, &n, &j );
viv ( i, 10, "slaIntin", "i1", status );
vlv ( n, -12345L, "slaIntin", "v1", status );
viv ( j, -1, "slaIntin", "j1", status );
sbmintin ( s, &i, &n, &j );
viv ( i, 12, "slaIntin", "i2", status );
vlv ( n, -12345L, "slaIntin", "v2", status );
viv ( j, 1, "slaIntin", "j2", status );
sbmintin ( s, &i, &n, &j );
viv ( i, 17, "slaIntin", "i3", status );
vlv ( n, 0L, "slaIntin", "v3", status );
viv ( j, -1, "slaIntin", "j3", status );
sbmintin ( s, &i, &n, &j );
viv ( i, 23, "slaIntin", "i4", status );
vlv ( n, 2000L, "slaIntin", "v4", status );
viv ( j, 0, "slaIntin", "j4", status );
sbmintin ( s, &i, &n, &j );
viv ( i, 29, "slaIntin", "i5", status );
vlv ( n, 2000L, "slaIntin", "v5", status );
viv ( j, 2, "slaIntin", "j5", status );
}
void t_kbj ( int *status )
{
double e;
char k[] = "?";
int j;
e = 1950.0;
sbmkbj ( -1, e, k, &j );
vcs ( k, " ", "slaKbj", "jb1", status );
viv ( j, 1, "slaKbj", "j1", status );
sbmkbj ( 0, e, k, &j );
vcs ( k, "B", "slaKbj", "jb2", status );
viv ( j, 0, "slaKbj", "j2", status );
sbmkbj ( 1, e, k, &j );
vcs ( k, "B", "slaKbj", "jb3", status );
viv ( j, 0, "slaKbj", "j3", status );
sbmkbj ( 2, e, k, &j );
vcs ( k, "J", "slaKbj", "jb4", status );
viv ( j, 0, "slaKbj", "j4", status );
sbmkbj ( 3, e, k, &j );
vcs ( k, " ", "slaKbj", "jb5", status );
viv ( j, 1, "slaKbj", "j5", status );
e = 2000.0;
sbmkbj ( 0, e, k, &j );
vcs ( k, "J", "slaKbj", "jb6", status );
viv ( j, 0, "slaKbj", "j6", status );
sbmkbj ( 1, e, k, &j );
vcs ( k, "B", "slaKbj", "jb7", status );
viv ( j, 0, "slaKbj", "j7", status );
sbmkbj ( 2, e, k, &j );
vcs ( k, "J", "slaKbj", "jb8", status );
viv ( j, 0, "slaKbj", "j8", status );
}
void t_map ( int *status )
{
double ra, da, amprms[21];
sbmmap ( 6.123, -0.999, 1.23e-5, -0.987e-5,
0.123, 32.1, 1999.0, 43210.9, &ra, &da );
vvd ( ra, 6.117130303515811, 1e-12, "slaMap", "ra", status );
vvd ( da, -1.000880825417138, 1e-12, "slaMap", "da", status );
sbmmappa ( 2020.0, 45012.3, amprms );
vvd ( amprms[0], -37.884188911704310,
1e-11, "slaMappa", "amprms[0]", status );
vvd ( amprms[1],  -0.7888341859486424,
1e-8, "slaMappa", "amprms[1]", status );
vvd ( amprms[2],   0.5405321789059870,
1e-8, "slaMappa", "amprms[2]", status );
vvd ( amprms[3],   0.2340784267119091,
1e-8, "slaMappa", "amprms[3]", status );
vvd ( amprms[4],  -0.8067807553151404,
1e-9, "slaMappa", "amprms[4]", status );
vvd ( amprms[5],   0.5420884771335385,
1e-9, "slaMappa", "amprms[5]", status );
vvd ( amprms[6],   0.2350423277032729,
1e-9, "slaMappa", "amprms[6]", status );
vvd ( amprms[7],   1.999729472165140e-8,
1e-19, "slaMappa", "amprms[7]", status );
vvd ( amprms[8],  -6.035531043781443e-5,
1e-13, "slaMappa", "amprms[8]", status );
vvd ( amprms[9],  -7.381891582517198e-5,
1e-13, "slaMappa", "amprms[9]", status );
vvd ( amprms[10], -3.200897749854970e-5,
1e-13, "slaMappa", "amprms[10]", status );
vvd ( amprms[11],  0.9999999949417148,
1e-12, "slaMappa", "amprms[11]", status );
vvd ( amprms[12],  0.9999566749218135,
1e-12, "slaMappa", "amprms[12]", status );
vvd ( amprms[13],  8.537330993692050e-3,
1e-12, "slaMappa", "amprms[13]", status );
vvd ( amprms[14],  3.709751853511750e-3,
1e-12, "slaMappa", "amprms[14]", status );
vvd ( amprms[15], -8.537384191359673e-3,
1e-12, "slaMappa", "amprms[15]", status );
vvd ( amprms[16],  0.9999635558703782,
1e-12, "slaMappa", "amprms[16]", status );
vvd ( amprms[17], -1.495941733212430e-6,
1e-12, "slaMappa", "amprms[17]", status );
vvd ( amprms[18], -3.709629426184059e-3,
1e-12, "slaMappa", "amprms[18]", status );
vvd ( amprms[19], -3.017569990664859e-5,
1e-12, "slaMappa", "amprms[19]", status );
vvd ( amprms[20],  0.9999931188457986,
1e-12, "slaMappa", "amprms[20]", status );
sbmmapqk ( 1.234, -0.987, -1.2e-5, -0.99, 0.75, -23.4, amprms,
&ra, &da );
vvd ( ra, 1.223337558432569, 1e-12, "slaMapqk", "ra", status );
vvd ( da, 0.5558838553934530, 1e-12, "slaMapqk", "da", status );
sbmmapqkz ( 6.012, 1.234, amprms, &ra, &da );
vvd ( ra, 6.006091123298217, 1e-12, "slaMapqkz", "ra", status );
vvd ( da, 1.230458458768652, 1e-12, "slaMapqkz", "da", status );
}
void t_moon ( int *status )
{
float pv[6];
sbmmoon ( 1999, 365, 0.9f, pv );
vvd ( (double) pv[0], -2.155729505970773e-3, 1e-6,
"slaMoon", "[0]", status );
vvd ( (double) pv[1], -1.538107758633427e-3, 1e-6,
"slaMoon", "[1]", status );
vvd ( (double) pv[2], -4.003940552689305e-4, 1e-6 ,
"slaMoon", "[2]", status );
vvd ( (double) pv[3],  3.629209419071314e-9, 1e-12,
"slaMoon", "[3]", status );
vvd ( (double) pv[4], -4.989667166259157e-9, 1e-12,
"slaMoon", "[4]", status );
vvd ( (double) pv[5], -2.160752457288307e-9, 1e-12,
"slaMoon", "[5]", status );
}
void t_nut ( int *status )
{
double rmatn[3][3], dpsi, deps, eps0;
sbmnut ( 46012.34, rmatn );
vvd ( rmatn[0][0],  9.999999969503286e-1, 1e-12,
"slaNut", "[0][0]", status );
vvd ( rmatn[0][1],  7.165271185308232e-5, 1e-12,
"slaNut", "[0][1]", status );
vvd ( rmatn[0][2],  3.106817873556357e-5, 1e-12,
"slaNut", "[0][2]", status );
vvd ( rmatn[1][0], -7.165197227383028e-5, 1e-12,
"slaNut", "[1][0]", status );
vvd ( rmatn[1][1],  9.999999971496466e-1, 1e-12,
"slaNut", "[1][1]", status );
vvd ( rmatn[1][2], -2.380550008323961e-5, 1e-12,
"slaNut", "[1][2]", status );
vvd ( rmatn[2][0], -3.106988437564627e-5, 1e-12,
"slaNut", "[2][0]", status );
vvd ( rmatn[2][1],  2.380327391436537e-5, 1e-12,
"slaNut", "[2][1]", status );
vvd ( rmatn[2][2],  9.999999992340332e-1, 1e-12,
"slaNut", "[2][2]", status );
sbmnutc ( 50123.4, &dpsi, &deps, &eps0 );
vvd ( dpsi, 3.523550954747942e-5, 1e-17, "slaNutc", "dpsi", status );
vvd ( deps, -4.143371566683342e-5, 1e-17, "slaNutc", "deps", status );
vvd ( eps0, 0.4091016349007751, 1e-12, "slaNutc", "eps0", status );
}
void t_obs ( int *status )
{
int n;
char c[10], name[40];
double w, p, h;
n = 0;
strcpy ( c, "MMT" );
sbmobs ( n, c, name, &w, &p, &h );
vcs ( c, "MMT", "slaObs", "1/c", status );
vcs ( name, "MMT 6.5m, Mt Hopkins", "slaObs", "1/name", status );
vvd ( w, 1.935300099241333, 1e-8, "slaObs", "1/w", status );
vvd ( p, 0.5530735081550342, 1e-10, "slaObs", "1/p", status );
vvd ( h, 2608.0, 1e-10, "slaObs", "1/h", status );
n = 61;
sbmobs ( n, c, name, &w, &p, &h );
vcs ( c, "KECK1", "slaObs", "2/c", status );
vcs ( name, "Keck 10m Telescope #1", "slaObs", "2/name", status );
vvd ( w, 2.713594287769484, 1e-8, "slaObs", "1/w", status );
vvd ( p, 0.3460813373779556, 1e-8, "slaObs", "1/p", status );
vvd ( h, 4160.0, 1e-10, "slaObs", "1/h", status );
n = 100;
sbmobs ( n, c, name, &w, &p, &h );
vcs ( c, "KECK1", "slaObs", "3/c", status );
vcs ( name, "?", "slaObs", "1/name", status );
vvd ( w, 2.713594287769484, 1e-8, "slaObs", "1/w", status );
vvd ( p, 0.3460813373779556, 1e-8, "slaObs", "1/p", status );
vvd ( h, 4160.0, 1e-10, "slaObs", "1/h", status );
n = 0;
strcpy ( c, "JUNK" );
sbmobs ( n, c, name, &w, &p, &h );
vcs ( c, "JUNK", "slaObs", "4/c", status );
vcs ( name, "?", "slaObs", "1/name", status );
vvd ( w, 2.713594287769484, 1e-8, "slaObs", "1/w", status );
vvd ( p, 0.3460813373779556, 1e-8, "slaObs", "1/p", status );
vvd ( h, 4160.0, 1e-10, "slaObs", "1/h", status );
}
void t_pa ( int *status )
{
vvd ( sbmpa ( -1.567, 1.5123, 0.987 ), -1.486288540423851,
1e-12, "slaPa", "", status );
vvd ( sbmpa ( 0.0, 0.789, 0.789 ), 0.0,
0.0, "slaPa", "zenith", status );
}
void t_pcd ( int *status )
{
double disco = 178.585, x, y;
x = 0.0123;
y = -0.00987;
sbmpcd ( disco, &x, &y );
vvd ( x, 0.01284630845735895, 1e-14, "slaPcd", "x", status );
vvd ( y, -0.01030837922553926, 1e-14, "slaPcd", "y", status );
sbmunpcd ( disco, &x, &y );
vvd (x, 0.01230000008075912, 1e-14, "slaUnpcd", "x", status );
vvd ( y, -0.009870000064804270, 1e-14, "slaUnpcd", "y", status );
}
void t_pda2h ( int *status )
{
double h1, h2;
int j1, j2;
sbmpda2h ( -0.51, -1.31, 3.1, &h1, &j1, &h2, &j2 );
vvd ( h1, -0.1161784556585303, 1e-14, "slaPda2h", "h1", status );
viv ( j1, 0, "slaPda2h", "j1", status );
vvd ( h2, -2.984787179226459, 1e-13, "slaPda2h", "h2", status );
viv ( j2, 0, "slaPda2h", "j2", status );
}
void t_pdq2h ( int *status )
{
double h1, h2;
int j1, j2;
sbmpdq2h ( 0.9, 0.2, 0.1, &h1, &j1, &h2, &j2 );
vvd ( h1, 0.1042809894435257, 1e-14, "slaPdq2h", "h1", status );
viv ( j1, 0, "slaPdq2h", "j1", status );
vvd ( h2, 2.997450098818439, 1e-13, "slaPdq2h", "h2", status );
viv ( j2, 0, "slaPdq2h", "j2", status );
}
void t_planet ( int *status )
{
double pv[6], ra, dec, diam;
int j, np;
sbmplanet ( 1e6, 0, pv, &j );
vvd ( pv[0], 0.0, 0.0, "slaPlanet", "[0] 1", status );
vvd ( pv[1], 0.0, 0.0, "slaPlanet", "[1] 1", status );
vvd ( pv[2], 0.0, 0.0, "slaPlanet", "[2] 1", status );
vvd ( pv[3], 0.0, 0.0, "slaPlanet", "[3] 1", status );
vvd ( pv[4], 0.0, 0.0, "slaPlanet", "[4] 1", status );
vvd ( pv[5], 0.0, 0.0, "slaPlanet", "[5] 1", status );
viv ( j, -1, "slaPlanet", "j 1", status );
sbmplanet ( 1e6, 9, pv, &j );
viv ( j, -1, "slaPlanet", "j 2", status );
sbmplanet ( -320000.0, 3, pv, &j );
vvd ( pv[0],  9.308038666830432e-1, 1e-12, "slaPlanet", "[0] 3", status );
vvd ( pv[1],  3.258319040267160e-1, 1e-12, "slaPlanet", "[1] 3", status );
vvd ( pv[2],  1.422794544483677e-1, 1e-12, "slaPlanet", "[2] 3", status );
vvd ( pv[3], -7.441503423919205e-8, 1e-12, "slaPlanet", "[3] 3", status );
vvd ( pv[4],  1.699734557527607e-7, 1e-12, "slaPlanet", "[4] 3", status );
vvd ( pv[5],  7.415505122996871e-8, 1e-12, "slaPlanet", "[5] 3", status );
viv ( j, 1, "slaPlanet", "j 3", status );
sbmplanet ( 43999.9, 1, pv, &j );
vvd ( pv[0],  2.945293959257472e-1, 1e-12, "slaPlanet", "[0] 4", status );
vvd ( pv[1], -2.452204176600992e-1, 1e-12, "slaPlanet", "[1] 4", status );
vvd ( pv[2], -1.615427700571952e-1, 1e-12, "slaPlanet", "[2] 4", status );
vvd ( pv[3],  1.636421147459004e-7, 1e-12, "slaPlanet", "[3] 4", status );
vvd ( pv[4],  2.252949422574926e-7, 1e-12, "slaPlanet", "[4] 4", status );
vvd ( pv[5],  1.033542799062396e-7, 1e-12, "slaPlanet", "[5] 4", status );
viv ( j, 0, "slaPlanet", "j 4", status );
sbmrdplan ( 40999.9, 0, 0.1, -0.9, &ra, &dec, &diam );
vvd ( ra, 5.772270117668358, 1e-12, "slaDplan", "RA 0", status );
vvd ( dec, -2.089208099835293e-1, 1e-12, "slaDplan", "dec 0", status );
vvd ( diam, 9.415338935223200e-3, 1e-15, "slaDplan", "diam 0", status );
sbmrdplan ( 41999.9, 1, 1.1, -0.9, &ra, &dec, &diam );
vvd ( ra, 3.866363225661290, 1e-12, "slaDplan", "RA 1", status );
vvd ( dec, -2.594429951470958e-1, 1e-12, "slaDplan", "dec 1", status );
vvd ( diam, 4.638468996797394e-5, 1e-15, "slaDplan", "diam 1", status );
sbmrdplan ( 42999.9, 2, 2.1, 0.9, &ra, &dec, &diam );
vvd ( ra, 2.695383090678534, 1e-12, "slaDplan", "RA 2", status );
vvd ( dec, 2.124045012323843e-1, 1e-12, "slaDplan", "dec 2", status );
vvd ( diam, 4.892222838694186e-5, 1e-15, "slaDplan", "diam 2", status );
sbmrdplan ( 43999.9, 3, 3.1, 0.9, &ra, &dec, &diam );
vvd ( ra, 2.908326598807061, 1e-12, "slaDplan", "RA 3", status );
vvd ( dec, 8.729787098230739e-2, 1e-12, "slaDplan", "dec 3", status );
vvd ( diam, 8.581305872704111e-3, 1e-15, "slaDplan", "diam 3", status );
sbmrdplan ( 44999.9, 4, -0.1, 1.1, &ra, &dec, &diam );
vvd ( ra, 3.429840766702363, 1e-12, "slaDplan", "RA 4", status );
vvd ( dec, -6.979849897615531e-2, 1e-12, "slaDplan", "dec 4", status );
vvd ( diam, 4.540536678443045e-5, 1e-15, "slaDplan", "diam 4", status );
sbmrdplan ( 45999.9, 5, -1.1, 0.1, &ra, &dec, &diam );
vvd ( ra, 4.864669488348910, 1e-12, "slaDplan", "RA 5", status );
vvd ( dec, -4.077714387746500e-1, 1e-12, "slaDplan", "dec 5", status );
vvd ( diam, 1.727945579027739e-4, 1e-15, "slaDplan", "diam 5", status );
sbmrdplan ( 46999.9, 6, -2.1, -0.1, &ra, &dec, &diam );
vvd ( ra, 4.432929862414968, 1e-12, "slaDplan", "RA 6", status );
vvd ( dec, -3.682820962366541e-1, 1e-12, "slaDplan", "dec 6", status );
vvd ( diam, 8.670829016098461e-5, 1e-15, "slaDplan", "diam 6", status );
sbmrdplan ( 47999.9, 7, -3.1, -1.1, &ra, &dec, &diam );
vvd ( ra, 4.894972507391056, 1e-12, "slaDplan", "RA 7", status );
vvd ( dec, -4.084069086862206e-1, 1e-12, "slaDplan", "dec 7", status );
vvd ( diam, 1.793916783976057e-5, 1e-15, "slaDplan", "diam 7", status );
sbmrdplan ( 48999.9, 8, 0.0, 0.0, &ra, &dec, &diam );
vvd ( ra, 5.066050352567723, 1e-12, "slaDplan", "RA 8", status );
vvd ( dec, -3.744690778439816e-1, 1e-12, "slaDplan", "dec 8", status );
vvd ( diam, 1.062210086082752e-5, 1e-15, "slaDplan", "diam 8", status );
}
void t_pm ( int *status )
{
double r1, d1;
sbmpm ( 5.43, -0.87, -0.33e-5, 0.77e-5, 0.7, 50.3, 1899.0, 1943.0,
&r1, &d1 );
vvd ( r1, 5.429855087793876, 1e-12, "slaPm", "r", status );
vvd ( d1, -0.8696617307805086, 1e-12, "slaPm", "d", status );
}
void t_polmo ( int *status )
{
double elong, phi, daz;
sbmpolmo ( 0.7, -0.5, 1e-6, -2e-6, &elong, &phi, &daz );
vvd ( elong,  0.7000004837322044,   1e-12, "slaPolmo", "elong", status );
vvd ( phi, -0.4999979467222241,   1e-12, "slaPolmo", "phi", status );
vvd ( daz,  1.008982781275728e-6, 1e-12, "slaPolmo", "daz", status );
}
void t_prebn ( int *status )
{
double rmatp[3][3];
sbmprebn ( 1925.0, 1975.0, rmatp );
vvd ( rmatp[0][0],  9.999257613786738e-1, 1e-12,
"slaPrebn", "[0][0]", status );
vvd ( rmatp[0][1], -1.117444640880939e-2, 1e-12,
"slaPrebn", "[0][1]", status );
vvd ( rmatp[0][2], -4.858341150654265e-3, 1e-12,
"slaPrebn", "[0][2]", status );
vvd ( rmatp[1][0],  1.117444639746558e-2, 1e-12,
"slaPrebn", "[1][0]", status );
vvd ( rmatp[1][1],  9.999375635561940e-1, 1e-12,
"slaPrebn", "[1][1]", status );
vvd ( rmatp[1][2], -2.714797892626396e-5, 1e-12,
"slaPrebn", "[1][2]", status );
vvd ( rmatp[2][0],  4.858341176745641e-3, 1e-12,
"slaPrebn", "[2][0]", status );
vvd ( rmatp[2][1], -2.714330927085065e-5, 1e-12,
"slaPrebn", "[2][1]", status );
vvd ( rmatp[2][2],  9.999881978224798e-1, 1e-12,
"slaPrebn", "[2][2]", status );
}
void t_prec ( int *status )
{
double rmatp[3][3];
sbmprec ( 1925.0, 1975.0, rmatp );
vvd ( rmatp[0][0],  9.999257249850045e-1, 1e-12,
"slaPrec", "[0][0]", status );
vvd ( rmatp[0][1], -1.117719859160180e-2, 1e-12,
"slaPrec", "[0][1]", status );
vvd ( rmatp[0][2], -4.859500474027002e-3, 1e-12,
"slaPrec", "[0][2]", status );
vvd ( rmatp[1][0],  1.117719858025860e-2, 1e-12,
"slaPrec", "[1][0]", status );
vvd ( rmatp[1][1],  9.999375327960091e-1, 1e-12,
"slaPrec", "[1][1]", status );
vvd ( rmatp[1][2], -2.716114374174549e-5, 1e-12,
"slaPrec", "[1][2]", status );
vvd ( rmatp[2][0],  4.859500500117173e-3, 1e-12,
"slaPrec", "[2][0]", status );
vvd ( rmatp[2][1], -2.715647545167383e-5, 1e-12,
"slaPrec", "[2][1]", status );
vvd ( rmatp[2][2],  9.999881921889954e-1, 1e-12,
"slaPrec", "[2][2]", status );
sbmprecl ( 1925.0, 1975.0, rmatp );
vvd ( rmatp[0][0],  9.999257331781050e-1, 1e-12,
"slaPrec", "[0][0]", status );
vvd ( rmatp[0][1], -1.117658038434041e-2, 1e-12,
"slaPrec", "[0][1]", status );
vvd ( rmatp[0][2], -4.859236477249598e-3, 1e-12,
"slaPrec", "[0][2]", status );
vvd ( rmatp[1][0],  1.117658037299592e-2, 1e-12,
"slaPrec", "[1][0]", status );
vvd ( rmatp[1][1],  9.999375397061558e-1, 1e-12,
"slaPrec", "[1][1]", status );
vvd ( rmatp[1][2], -2.715816653174189e-5, 1e-12,
"slaPrec", "[1][2]", status );
vvd ( rmatp[2][0],  4.859236503342703e-3, 1e-12,
"slaPrec", "[2][0]", status );
vvd ( rmatp[2][1], -2.715349745834860e-5, 1e-12,
"slaPrec", "[2][1]", status );
vvd ( rmatp[2][2],  9.999881934719490e-1, 1e-12,
"slaPrec", "[2][2]", status );
}
void t_preces ( int *status )
{
double ra, dc;
ra = 6.28;
dc = -1.123;
sbmpreces ( "Fk4", 1925.0, 1950.0, &ra, &dc );
vvd ( ra,  0.002403604864728447, 1e-12, "slaPreces", "r", status );
vvd ( dc, -1.120570643322045, 1e-12, "slaPreces", "d", status );
ra = 0.0123;
dc = 1.0987;
sbmpreces ( "fK5", 2050.0, 1990.0, &ra, &dc );
vvd ( ra, 6.282003602708382, 1e-12, "slaPreces", "r", status );
vvd ( dc, 1.092870326188383, 1e-12, "slaPreces", "d", status );
}
void t_prenut ( int *status )
{
double rmatpn[3][3];
sbmprenut ( 1985.0, 50123.4567, rmatpn );
vvd ( rmatpn[0][0],  9.999962354793337e-1, 1e-12,
"slaPrenut", "[0][0]", status );
vvd ( rmatpn[0][1], -2.516547040904801e-3, 1e-12,
"slaPrenut", "[0][1]", status );
vvd ( rmatpn[0][2], -1.093626148077242e-3, 1e-12,
"slaPrenut", "[0][2]", status );
vvd ( rmatpn[1][0],  2.516592325534284e-3, 1e-12,
"slaPrenut", "[1][0]", status );
vvd ( rmatpn[1][1],  9.999968325751710e-1, 1e-12,
"slaPrenut", "[1][1]", status );
vvd ( rmatpn[1][2],  4.003364341849627e-5, 1e-12,
"slaPrenut", "[1][2]", status );
vvd ( rmatpn[2][0],  1.093521937551745e-3, 1e-12,
"slaPrenut", "[2][0]", status );
vvd ( rmatpn[2][1], -4.278570388226845e-5, 1e-12,
"slaPrenut", "[2][1]", status );
vvd ( rmatpn[2][2],  9.999994011893985e-1, 1e-12,
"slaPrenut", "[2][2]", status );
}
void t_pvobs ( int *status )
{
double pv[6];
sbmpvobs ( 0.5123, 3001.0, -0.567, pv );
vvd ( pv[0], 0.3138647803054939e-4, 1e-16, "slaPvobs", "[0]", status );
vvd ( pv[1],-0.1998515596527082e-4, 1e-16, "slaPvobs", "[1]", status );
vvd ( pv[2], 0.2078572043443275e-4, 1e-16, "slaPvobs", "[2]", status );
vvd ( pv[3], 0.1457340726851264e-8, 1e-20, "slaPvobs", "[3]", status );
vvd ( pv[4], 0.2288738340888011e-8, 1e-20, "slaPvobs", "[4]", status );
vvd ( pv[5], 0.0,                   0.0,   "slaPvobs", "[5]", status );
}
void t_range ( int *status )
{
vvd ( (double) sbmrange ( -4.0f ), 2.283185307179586,
1e-6, "slaRange", "", status );
vvd ( sbmdrange ( -4.0 ), 2.283185307179586,
1e-12, "slaDrange", "", status );
}
void t_ranorm ( int *status )
{
vvd ( (double) sbmranorm ( -0.1f ), 6.183185307179587,
1e-6, "slaRanorm", "1", status );
vvd ( sbmdranrm ( -0.1 ), 6.183185307179587,
1e-12, "slaDranrm", "2", status );
}
void t_rcc ( int *status )
{
vvd ( sbmrcc ( 48939.123, 0.76543, 5.0123, 5525.242, 3190.0 ),
-1.280651825977972e-3, 1e-15, "slaRcc", "", status );
}
void t_ref ( int *status )
{
double ref, refa, refb, refa2, refb2, vu[3], vr[3], zr;
sbmrefro ( 1.4, 3456.7, 280.0, 678.9, 0.55, 0.9,
-0.3, 0.006, 1e-9, &ref );
vvd ( ref, 0.001055131660287618, 1e-12, "slaRefro", "o", status );
sbmrefro ( 1.4, 3456.7, 280.0, 678.9, 1000.0, 0.9,
-0.3, 0.006, 1e-9, &ref );
vvd ( ref, 0.001054073862675877, 1e-12, "slaRefro", "r", status );
sbmrefco ( 2111.1, 275.9, 709.3, 0.9, 0.77,
-1.03, 0.0067, 1e-12, &refa, &refb );
vvd ( refa, 2.007122307680774e-4, 1e-12, "slaRefco", "a", status );
vvd ( refb, -2.222956186826262e-7, 1e-15, "slaRefco", "b", status );
sbmatmdsp ( 275.9, 709.3, 0.9, 0.77,
refa, refb, 0.5, &refa2, &refb2 );
vvd ( refa2, 2.034442151954884e-4, 1e-12, "slaAtmdsp", "a", status );
vvd ( refb2, -2.250847764571001e-7, 1e-15, "slaAtmdsp", "b", status );
sbmdcs2c ( 0.345, 0.456, vu );
sbmrefv ( vu, refa, refb, vr );
vvd ( vr[0], 0.8447487115363892, 1e-12, "slaRefv", "x1", status );
vvd ( vr[1], 0.3035794914846369, 1e-12, "slaRefv", "y1", status );
vvd ( vr[2], 0.4407256592343119, 1e-12, "slaRefv", "z1", status );
sbmdcs2c ( 3.7, 0.03, vu );
sbmrefv ( vu, refa, refb, vr );
vvd ( vr[0], -0.8476187735954409, 1e-12, "slaRefv", "x2", status );
vvd ( vr[1], -0.5295354830463532, 1e-12, "slaRefv", "y2", status );
vvd ( vr[2], 0.03229135845259886, 1e-12, "slaRefv", "z2", status );
sbmrefz ( 0.567, refa, refb, &zr );
vvd ( zr, 0.5668722910257588, 1e-12, "slaRefz", "hi el", status );
sbmrefz ( 1.55, refa, refb, &zr );
vvd ( zr, 1.545697522405808, 1e-12, "slaRefz", "lo el", status );
}
void t_rv ( int *status )
{
vvd ( (double) sbmrverot ( -0.777f, 5.67f, -0.3f, 3.19f ),
-0.1948098355075913, 1e-6, "slaRverot", "", status );
vvd ( (double) sbmrvgalc ( 1.11f, -0.99f ),
158.9630759840254, 1e-3, "slaRvgalc", "", status );
vvd ( (double) sbmrvlg ( 3.97f, 1.09f ),
-197.818762175363, 1e-3, "slaRvlg", "", status );
vvd ( (double) sbmrvlsrd ( 6.01f, 0.1f ),
-4.082811335150567, 1e-5, "slaRvlsrd", "", status );
vvd ( (double) sbmrvlsrk ( 6.01f, 0.1f ),
-5.925180579830265, 1e-5, "slaRvlsrk", "", status );
}
void t_smat ( int *status )
{
float a[3][3] = {
{ 2.22f,     1.6578f,     1.380522f     },
{ 1.6578f,   1.380522f,   1.22548578f   },
{ 1.380522f, 1.22548578f, 1.1356276122f }
};
float v[3] = { 2.28625f, 1.7128825f, 1.429432225f };
float d;
int j, iw[3];
sbmsmat ( 3, (float *) a, v, &d, &j, iw );
vvd ( (double) a[0][0], 18.02550629769198,
1e-2, "slaSmat", "a[0][0]", status );
vvd ( (double) a[0][1], -52.16386644917481,
1e-2, "slaSmat", "a[0][1]", status );
vvd ( (double) a[0][2], 34.37875949717994,
1e-2, "slaSmat", "a[0][2]", status );
vvd ( (double) a[1][0], -52.16386644917477,
1e-2, "slaSmat", "a[1][0]", status );
vvd ( (double) a[1][1], 168.1778099099869,
1e-2, "slaSmat", "a[1][1]", status );
vvd ( (double) a[1][2], -118.0722869694278,
1e-2, "slaSmat", "a[1][2]", status );
vvd ( (double) a[2][0], 34.37875949717988,
1e-2, "slaSmat", "a[2][0]", status );
vvd ( (double) a[2][1], -118.07228696942770,
1e-2, "slaSmat", "a[2][1]", status );
vvd ( (double) a[2][2], 86.50307003740468,
1e-2, "slaSmat", "a[2][2]", status );
vvd ( (double) v[0], 1.002346480763383,
1e-4, "slaSmat", "v[0]", status );
vvd ( (double) v[1], 0.0328559401697292,
1e-4, "slaSmat", "v[1]", status );
vvd ( (double) v[2], 0.004760688414898454,
1e-4, "slaSmat", "v[2]", status );
vvd ( (double) d, 0.003658344147359863,
1e-4, "slaSmat", "d", status );
viv ( j, 0, "slaSmat", "j", status );
}
void t_supgal ( int *status )
{
double dl, db;
sbmsupgal ( 6.1, -1.4, &dl, &db );
vvd ( dl, 3.798775860769474, 1e-12, "slaSupgal", "dl", status );
vvd ( db, -0.1397070490669407, 1e-12, "slaSupgal", "db", status );
}
void t_svd ( int *status )
{
#define MP (10)
#define NP (6)
#define NC (7)
double a[MP][NP], w[NP], v[NP][NP], work[NP], b[MP], x[NP], c[NC][NC];
int m = 5, n = 4;
int i, j;
double val;
for ( i = 0; i < m; i++ ) {
val = (double) ( i + 1 ) / 2.0;
b[i] = 23.0 - 3.0 * val - 11.0 * sin ( val ) + 13.0 * cos ( val );
a[i][0] = 1.0;
a[i][1] = val;
a[i][2] = sin ( val );
a[i][3] = cos ( val );
}
sbmsvd ( m, n, MP, NP, (double *) a, w, (double *) v, work, &j );
if ( a[0][0] > 0.0 ) {
for ( i = 0; i < m; i++ ) {
for ( j = 0; j < n; j++ ) {
a[i][j] = - a[i][j];
v[i][j] = - v[i][j];
}
}
}
vvd ( a[0][0], -0.21532492989299, 1e-12, "slaSvd", "a[0][0]", status );
vvd ( a[0][1],  0.67675050651267, 1e-12, "slaSvd", "a[0][1]", status );
vvd ( a[0][2], -0.37267876361644, 1e-12, "slaSvd", "a[0][2]", status );
vvd ( a[0][3],  0.58330405917160, 1e-12, "slaSvd", "a[0][3]", status );
vvd ( a[1][0], -0.33693420368121, 1e-12, "slaSvd", "a[1][0]", status );
vvd ( a[1][1],  0.48011695963936, 1e-12, "slaSvd", "a[1][1]", status );
vvd ( a[1][2],  0.62656568539705, 1e-12, "slaSvd", "a[1][2]", status );
vvd ( a[1][3], -0.17479918328198, 1e-12, "slaSvd", "a[1][3]", status );
vvd ( a[2][0], -0.44396825906047, 1e-12, "slaSvd", "a[2][0]", status );
vvd ( a[2][1],  0.18255923809825, 1e-12, "slaSvd", "a[2][1]", status );
vvd ( a[2][2],  0.02228154115994, 1e-12, "slaSvd", "a[2][2]", status );
vvd ( a[2][3], -0.51743308030238, 1e-12, "slaSvd", "a[2][3]", status );
vvd ( a[3][0], -0.53172583816951, 1e-12, "slaSvd", "a[3][0]", status );
vvd ( a[3][1], -0.16537863535943, 1e-12, "slaSvd", "a[3][1]", status );
vvd ( a[3][2], -0.61134201569990, 1e-12, "slaSvd", "a[3][2]", status );
vvd ( a[3][3], -0.28871221824912, 1e-12, "slaSvd", "a[3][3]", status );
vvd ( a[4][0], -0.60022523682867, 1e-12, "slaSvd", "a[4][0]", status );
vvd ( a[4][1], -0.50081781972404, 1e-12, "slaSvd", "a[4][1]", status );
vvd ( a[4][2],  0.30706750690326, 1e-12, "slaSvd", "a[4][2]", status );
vvd ( a[4][3],  0.52736124480318, 1e-12, "slaSvd", "a[4][3]", status );
vvd ( w[0], 4.57362714220621, 1e-12, "slaSvd", "w[0]", status );
vvd ( w[1], 1.64056393111226, 1e-12, "slaSvd", "w[1]", status );
vvd ( w[2], 0.03999179717447, 1e-12, "slaSvd", "w[2]", status );
vvd ( w[3], 0.37267332634218, 1e-12, "slaSvd", "w[3]", status );
vvd ( v[0][0], -0.46531525230679, 1e-12, "slaSvd", "v[0][0]", status );
vvd ( v[0][1],  0.41036514115630, 1e-12, "slaSvd", "v[0][1]", status );
vvd ( v[0][2], -0.70279526907678, 1e-12, "slaSvd", "v[0][2]", status );
vvd ( v[0][3],  0.34808185338758, 1e-12, "slaSvd", "v[0][3]", status );
vvd ( v[1][0], -0.80342444002914, 1e-12, "slaSvd", "v[1][0]", status );
vvd ( v[1][1], -0.29896472833787, 1e-12, "slaSvd", "v[1][1]", status );
vvd ( v[1][2],  0.46592932810178, 1e-12, "slaSvd", "v[1][2]", status );
vvd ( v[1][3],  0.21917828721921, 1e-12, "slaSvd", "v[1][3]", status );
vvd ( v[2][0], -0.36564497020801, 1e-12, "slaSvd", "v[2][0]", status );
vvd ( v[2][1],  0.28066812941896, 1e-12, "slaSvd", "v[2][1]", status );
vvd ( v[2][2], -0.03324480702665, 1e-12, "slaSvd", "v[2][2]", status );
vvd ( v[2][3], -0.88680546891402, 1e-12, "slaSvd", "v[2][3]", status );
vvd ( v[3][0],  0.06553350971918, 1e-12, "slaSvd", "v[3][0]", status );
vvd ( v[3][1],  0.81452191085452, 1e-12, "slaSvd", "v[3][1]", status );
vvd ( v[3][2],  0.53654771808636, 1e-12, "slaSvd", "v[3][2]", status );
vvd ( v[3][3],  0.21065602782287, 1e-12, "slaSvd", "v[3][3]", status );
sbmsvdsol ( m, n, MP, NP, b, (double *) a, w, (double *) v, work, x );
vvd ( x[0],  23.0, 1e-12, "slaSvdsol", "x[0]", status );
vvd ( x[1],  -3.0, 1e-12, "slaSvdsol", "x[1]", status );
vvd ( x[2], -11.0, 1e-12, "slaSvdsol", "x[2]", status );
vvd ( x[3],  13.0, 1e-12, "slaSvdsol", "x[3]", status );
sbmsvdcov ( n, NP, NC, w, (double *) v, work, (double *) c );
vvd ( c[0][0],  309.77269378273270, 1e-10,
"slaSvdcov", "c[0][0]", status );
vvd ( c[0][1], -204.22043941662150, 1e-10,
"slaSvdcov", "c[0][1]", status );
vvd ( c[0][2],   12.43704316907477, 1e-10,
"slaSvdcov", "c[0][2]", status );
vvd ( c[0][3], -235.12299986206710, 1e-10,
"slaSvdcov", "c[0][3]", status );
vvd ( c[1][0], -204.22043941662150, 1e-10,
"slaSvdcov", "c[1][0]", status );
vvd ( c[1][1],  136.14695961108110, 1e-10,
"slaSvdcov", "c[1][1]", status );
vvd ( c[1][2],  -11.10167446246327, 1e-10,
"slaSvdcov", "c[1][2]", status );
vvd ( c[1][3],  156.54937371198730, 1e-10,
"slaSvdcov", "c[1][3]", status );
vvd ( c[2][0],   12.43704316907477, 1e-10,
"slaSvdcov", "c[2][0]", status );
vvd ( c[2][1],  -11.10167446246327, 1e-10,
"slaSvdcov", "c[2][1]", status );
vvd ( c[2][2],    6.38909830090602, 1e-10,
"slaSvdcov", "c[2][2]", status );
vvd ( c[2][3],  -12.41424302586736, 1e-10,
"slaSvdcov", "c[2][3]", status );
vvd ( c[3][0], -235.12299986206710, 1e-10,
"slaSvdcov", "c[3][0]", status );
vvd ( c[3][1],  156.54937371198730, 1e-10,
"slaSvdcov", "c[3][1]", status );
vvd ( c[3][2],  -12.41424302586736, 1e-10,
"slaSvdcov", "c[3][2]", status );
vvd ( c[3][3],  180.56719842359560, 1e-10,
"slaSvdcov", "c[3][3]", status );
}
void t_tp ( int *status )
{
float r0, d0, r1, d1, x, y, r2, d2, r01, d01, r02, d02;
double dr0, dd0, dr1, dd1, dx, dy, dr2, dd2, dr01, dd01, dr02, dd02;
int j;
r0 = 3.1f;
d0 = -0.9f;
r1 = r0 + 0.2f;
d1 = d0 - 0.1f;
sbms2tp ( r1, d1, r0, d0, &x, &y, &j );
vvd ( (double) x, 0.1086112301590404, 1e-6, "slaS2tp", "x", status );
vvd ( (double) y, -0.1095506200711452, 1e-6, "slaS2tp", "y", status );
viv ( j, 0, "slaS2tp", "j", status );
sbmtp2s ( x, y, r0, d0, &r2, &d2 );
vvd ( (double) ( r2 - r1 ), 0.0, 1e-6, "slaTp2s", "r", status );
vvd ( (double) ( d2 - d1 ), 0.0, 1e-6, "slaTp2s", "d", status );
sbmtps2c ( x, y, r2, d2, &r01, &d01, &r02, &d02, &j );
vvd ( (double) r01,  3.1, 1e-6, "slaTps2c", "r1", status );
vvd ( (double) d01, -0.9, 1e-6, "slaTps2c", "d1", status );
vvd ( (double) r02, 0.3584073464102072, 1e-6, "slaTps2c", "r2", status );
vvd ( (double) d02, -2.023361658234722, 1e-6, "slaTps2c", "d2", status );
viv ( j, 1, "slaTps2c", "n", status );
dr0 = 3.1;
dd0 = -0.9;
dr1 = dr0 + 0.2;
dd1 = dd0 - 0.1;
sbmds2tp ( dr1, dd1, dr0, dd0, &dx, &dy, &j );
vvd ( dx, 0.1086112301590404, 1e-12, "slaDs2tp", "x", status );
vvd ( dy, -0.1095506200711452, 1e-12, "slaDs2tp", "y", status );
viv ( j, 0, "slaDs2tp", "j", status );
sbmdtp2s ( dx, dy, dr0, dd0, &dr2, &dd2 );
vvd ( r2 - r1, 0.0, 1e-12, "slaDtp2s", "r", status );
vvd ( d2 - d1, 0.0, 1e-12, "slaDtp2s", "d", status );
sbmdtps2c ( dx, dy, dr2, dd2, &dr01, &dd01, &dr02, &dd02, &j );
vvd ( dr01,  3.1, 1e-12, "slaDtps2c", "r1", status );
vvd ( dd01, -0.9, 1e-12, "slaDtps2c", "d1", status );
vvd ( dr02, 0.3584073464102072, 1e-12, "slaDtps2c", "r2", status );
vvd ( dd02, -2.023361658234722, 1e-12, "slaDtps2c", "d2", status );
viv ( j, 1, "slaDtps2c", "n", status );
}
void t_tpv ( int *status )
{
float rxi, reta, rv[3], rv0[3], rtxi, rteta, rtv[3], rtv01[3], rtv02[3];
double xi, eta, x, y, z, v[3], v0[3], txi, teta, tv[3], tv01[3], tv02[3];
int j;
xi = -0.1;
eta = 0.055;
rxi = (float) xi;
reta = (float) eta;
x = -0.7;
y = -0.13;
z = sqrt ( 1.0 - x * x - y * y );
rv[0] = (float) x;
rv[1] = (float) y;
rv[2] = (float) z;
v[0]=x;
v[1]=y;
v[2]=z;
x = -0.72;
y = -0.16;
z = sqrt ( 1.0 - x * x - y * y );
rv0[0] = (float) x;
rv0[1] = (float) y;
rv0[2] = (float) z;
v0[0] = x;
v0[1] = y;
v0[2] = z;
sbmtp2v ( rxi, reta, rv0, rtv );
vvd ( (double) rtv[0], -0.7008874, 1e-6, "slaTp2v", "v[0]", status );
vvd ( (double) rtv[1], -0.05397407, 1e-6, "slaTp2v", "v[1]", status );
vvd ( (double) rtv[2], 0.7112268, 1e-6, "slaTp2v", "v[2]", status );
sbmdtp2v ( xi, eta, v0, tv );
vvd ( tv[0], -0.7008874281280771, 1e-13, "slaDtp2v", "v[0]", status );
vvd ( tv[1], -0.05397406827952735, 1e-13, "slaDtp2v", "v[1]", status );
vvd ( tv[2], 0.7112268365615617, 1e-13, "slaDtp2v", "v[2]", status );
sbmv2tp ( rv, rv0, &rtxi, &rteta, &j);
vvd ( (double) rtxi, -0.02497228, 1e-6, "slaV2tp", "xi", status );
vvd ( (double) rteta, 0.03748143, 1e-6, "slaV2tp", "eta", status );
viv ( j, 0, "slaV2tp", "j", status );
sbmdv2tp ( v, v0, &txi, &teta, &j );
vvd ( txi, -0.02497229197023852, 1e-13, "slaDv2tp", "xi", status );
vvd ( teta, 0.03748140764224765, 1e-13, "slaDv2tp", "eta", status );
viv ( j, 0, "slaDv2tp", "j", status );
sbmtpv2c ( rxi, reta, rv, rtv01, rtv02, &j );
vvd ( (double) rtv01[0], -0.7074573732537283, 1e-6,
"slaTpv2c", "v01[0]", status );
vvd ( (double) rtv01[1], -0.2372965765309941, 1e-6,
"slaTpv2c", "v01[1]", status );
vvd ( (double) rtv01[2], 0.6657284730245545, 1e-6,
"slaTpv2c", "v01[2]", status );
vvd ( (double) rtv02[0], -0.6680480104758149, 1e-6,
"slaTpv2c", "v02[0]", status );
vvd ( (double) rtv02[1], -0.02915588494045333, 1e-6,
"slaTpv2c", "v02[1]", status );
vvd ( (double) rtv02[2], 0.7435467638774610, 1e-6,
"slaTpv2c", "v02[2]", status );
viv ( j, 1, "slaTpv2c", "n", status );
sbmdtpv2c ( xi, eta, v, tv01, tv02, &j );
vvd ( tv01[0], -0.7074573732537283, 1e-13, "slaDtpv2c", "v01[0]", status );
vvd ( tv01[1], -0.2372965765309941, 1e-13, "slaDtpv2c", "v01[1]", status );
vvd ( tv01[2], 0.6657284730245545, 1e-13, "slaDtpv2c", "v01[2]", status );
vvd ( tv02[0], -0.6680480104758149, 1e-13, "slaDtpv2c", "v02[0]", status );
vvd ( tv02[1], -0.02915588494045333, 1e-13, "slaDtpv2c", "v02[1]", status );
vvd ( tv02[2], 0.7435467638774610, 1e-13, "slaDtpv2c", "v02[2]", status );
viv ( j, 1, "slaDtpv2c", "n", status );
}
void t_vecmat ( int *status )
{
int i;
float av[3], rm1[3][3], rm2[3][3], rm[3][3], v1[3], v2[3],
v3[3], v4[3], v5[3], vm, v6[3], v7[3];
double dav[3], drm1[3][3], drm2[3][3], drm[3][3], dv1[3], dv2[3],
dv3[3], dv4[3], dv5[3], dvm, dv6[3], dv7[3];
av[0] = -0.123f;
av[1] = 0.0987f;
av[2] = 0.0654f;
sbmav2m ( av, rm1 );
vvd ( (double) rm1[0][0], 0.9930075842721269,
1e-6, "slaAv2m", "00", status );
vvd ( (double) rm1[0][1], 0.05902743090199868,
1e-6, "slaAv2m", "01", status );
vvd ( (double) rm1[0][2], -0.1022335560329612,
1e-6, "slaAv2m", "02", status );
vvd ( (double) rm1[1][0], -0.07113807138648245,
1e-6, "slaAv2m", "10", status );
vvd ( (double) rm1[1][1], 0.9903204657727545,
1e-6, "slaAv2m", "11", status );
vvd ( (double) rm1[1][2], -0.1191836812279541,
1e-6, "slaAv2m", "12", status );
vvd ( (double) rm1[2][0], 0.09420887631983825,
1e-6, "slaAv2m", "20", status );
vvd ( (double) rm1[2][1], 0.1256229973879967,
1e-6, "slaAv2m", "21", status );
vvd ( (double) rm1[2][2], 0.9875948309655174,
1e-6, "slaAv2m", "22", status );
sbmeuler ( "YZY", 2.345f, -0.333f, 2.222f, rm2 );
vvd ( (double) rm2[0][0], -0.1681574770810878,
1e-6, "slaEuler", "00", status );
vvd ( (double) rm2[0][1], 0.1981362273264315,
1e-6, "slaEuler", "01", status );
vvd ( (double) rm2[0][2], 0.9656423242187410,
1e-6, "slaEuler", "02", status );
vvd ( (double) rm2[1][0], -0.2285369373983370,
1e-6, "slaEuler", "10", status );
vvd ( (double) rm2[1][1], 0.9450659587140423,
1e-6, "slaEuler", "11", status );
vvd ( (double) rm2[1][2], -0.2337117924378156,
1e-6, "slaEuler", "12", status );
vvd ( (double) rm2[2][0], -0.9589024617479674,
1e-6, "slaEuler", "20", status );
vvd ( (double) rm2[2][1], -0.2599853247796050,
1e-6, "slaEuler", "21", status );
vvd ( (double) rm2[2][2], -0.1136384607117296,
1e-6, "slaEuler", "22", status );
sbmmxm ( rm2, rm1, rm );
vvd ( (double) rm[0][0], -0.09010460088585805,
1e-6, "slaMxm", "00", status );
vvd ( (double) rm[0][1], 0.3075993402463796,
1e-6, "slaMxm", "01", status );
vvd ( (double) rm[0][2], 0.9472400998581048,
1e-6, "slaMxm", "02", status );
vvd ( (double) rm[1][0], -0.3161868071070688,
1e-6, "slaMxm", "10", status );
vvd ( (double) rm[1][1], 0.8930686362478707,
1e-6, "slaMxm", "11", status );
vvd ( (double) rm[1][2],-0.3200848543149236,
1e-6, "slaMxm", "12", status );
vvd ( (double) rm[2][0],-0.9444083141897035,
1e-6, "slaMxm", "20", status );
vvd ( (double) rm[2][1],-0.3283459407855694,
1e-6, "slaMxm", "21", status );
vvd ( (double) rm[2][2], 0.01678926022795169,
1e-6, "slaMxm", "22", status );
sbmcs2c ( 3.0123f, -0.999f, v1 );
vvd ( (double) v1[0], -0.5366267667260525,
1e-6, "slaCs2c", "x", status );
vvd ( (double) v1[1], 0.06977111097651444,
1e-6, "slaCs2c", "y", status );
vvd ( (double) v1[2], -0.8409302618566215,
1e-6, "slaCs2c", "z", status );
sbmmxv ( rm1, v1, v2 );
sbmmxv ( rm2, v2, v3 );
vvd ( (double) v3[0], -0.7267487768696160,
1e-6, "slaMxv", "x", status );
vvd ( (double) v3[1], 0.5011537352639822,
1e-6, "slaMxv", "y", status );
vvd ( (double) v3[2], 0.4697671220397141,
1e-6, "slaMxv", "z", status );
sbmimxv ( rm, v3, v4 );
vvd ( (double) v4[0], -0.5366267667260526,
1e-6, "slaImxv", "x", status );
vvd ( (double) v4[1], 0.06977111097651445,
1e-6, "slaImxv", "y", status );
vvd ( (double) v4[2], -0.8409302618566215,
1e-6, "slaImxv", "z", status );
sbmm2av ( rm, v5 );
vvd ( (double) v5[0], 0.006889040510209034,
1e-6, "slaM2av", "x", status );
vvd ( (double) v5[1], -1.577473205461961,
1e-6, "slaM2av", "y", status );
vvd ( (double) v5[2], 0.5201843672856759,
1e-6, "slaM2av", "z", status );
for ( i = 0; i < 3; i++ ) {
v5[i] *= 1000.0f;
}
sbmvn ( v5, v6, &vm );
vvd ( (double) v6[0], 0.004147420704640065,
1e-6, "slaVn", "x", status );
vvd ( (double) v6[1], -0.9496888606842218,
1e-6, "slaVn", "y", status );
vvd ( (double) v6[2], 0.3131674740355448,
1e-6, "slaVn", "z", status );
vvd ( (double) vm, 1661.042127339937,
1e-3, "slaVn", "m", status );
vvd ( (double) sbmvdv ( v6, v1 ), -0.3318384698006295,
1e-6, "slaVn", "", status );
sbmvxv (v6, v1, v7 );
vvd ( (double) v7[0], 0.7767720597123304,
1e-6, "slaVxv", "x", status );
vvd ( (double) v7[1], -0.1645663574562769,
1e-6, "slaVxv", "y", status );
vvd ( (double) v7[2], -0.5093390925544726,
1e-6, "slaVxv", "z", status );
dav[0] = -0.123;
dav[1] = 0.0987;
dav[2] = 0.0654;
sbmdav2m ( dav, drm1 );
vvd ( drm1[0][0], 0.9930075842721269, 1e-12, "slaDav2m", "00", status );
vvd ( drm1[0][1], 0.05902743090199868, 1e-12, "slaDav2m", "01", status );
vvd ( drm1[0][2], -0.1022335560329612, 1e-12, "slaDav2m", "02", status );
vvd ( drm1[1][0], -0.07113807138648245, 1e-12, "slaDav2m", "10", status );
vvd ( drm1[1][1], 0.9903204657727545, 1e-12, "slaDav2m", "11", status );
vvd ( drm1[1][2], -0.1191836812279541, 1e-12, "slaDav2m", "12", status );
vvd ( drm1[2][0], 0.09420887631983825, 1e-12, "slaDav2m", "20", status );
vvd ( drm1[2][1], 0.1256229973879967, 1e-12, "slaDav2m", "21", status );
vvd ( drm1[2][2], 0.9875948309655174, 1e-12, "slaDav2m", "22", status );
sbmdeuler ( "YZY", 2.345, -0.333, 2.222, drm2 );
vvd ( drm2[0][0], -0.1681574770810878, 1e-12, "slaDeuler", "00", status );
vvd ( drm2[0][1], 0.1981362273264315, 1e-12, "slaDeuler", "01", status );
vvd ( drm2[0][2], 0.9656423242187410, 1e-12, "slaDeuler", "02", status );
vvd ( drm2[1][0], -0.2285369373983370, 1e-12, "slaDeuler", "10", status );
vvd ( drm2[1][1], 0.9450659587140423, 1e-12, "slaDeuler", "11", status );
vvd ( drm2[1][2], -0.2337117924378156, 1e-12, "slaDeuler", "12", status );
vvd ( drm2[2][0], -0.9589024617479674, 1e-12, "slaDeuler", "20", status );
vvd ( drm2[2][1], -0.2599853247796050, 1e-12, "slaDeuler", "21", status );
vvd ( drm2[2][2], -0.1136384607117296, 1e-12, "slaDeuler", "22", status );
sbmdmxm ( drm2, drm1, drm );
vvd ( drm[0][0], -0.09010460088585805 , 1e-12, "slaDmxm", "00", status );
vvd ( drm[0][1], 0.3075993402463796, 1e-12, "slaDmxm", "01", status );
vvd ( drm[0][2], 0.9472400998581048, 1e-12, "slaDmxm", "02", status );
vvd ( drm[1][0], -0.3161868071070688, 1e-12, "slaDmxm", "10", status );
vvd ( drm[1][1], 0.8930686362478707, 1e-12, "slaDmxm", "11", status );
vvd ( drm[1][2], -0.3200848543149236, 1e-12, "slaDmxm", "12", status );
vvd ( drm[2][0], -0.9444083141897035, 1e-12, "slaDmxm", "20", status );
vvd ( drm[2][1], -0.3283459407855694, 1e-12, "slaDmxm", "21", status );
vvd ( drm[2][2], 0.01678926022795169, 1e-12, "slaDmxm", "22", status );
sbmdcs2c ( 3.0123, -0.999, dv1 );
vvd ( dv1[0], -0.5366267667260525, 1e-12, "slaDcs2c", "x", status );
vvd ( dv1[1], 0.06977111097651444, 1e-12, "slaDcs2c", "y", status );
vvd ( dv1[2], -0.8409302618566215, 1e-12, "slaDcs2c", "z", status );
sbmdmxv ( drm1, dv1, dv2 );
sbmdmxv ( drm2, dv2, dv3 );
vvd ( dv3[0], -0.7267487768696160, 1e-12, "slaDmxv", "x", status );
vvd ( dv3[1], 0.5011537352639822, 1e-12, "slaDmxv", "y", status );
vvd ( dv3[2], 0.4697671220397141, 1e-12, "slaDmxv", "z", status );
sbmdimxv ( drm, dv3, dv4 );
vvd ( dv4[0], -0.5366267667260526 , 1e-12, "slaDimxv", "x", status );
vvd ( dv4[1], 0.06977111097651445, 1e-12, "slaDimxv", "y", status );
vvd ( dv4[2], -0.8409302618566215 , 1e-12, "slaDimxv", "z", status );
sbmdm2av ( drm, dv5 );
vvd ( dv5[0], 0.006889040510209034, 1e-12, "slaDm2av", "x", status );
vvd ( dv5[1], -1.577473205461961, 1e-12, "slaDm2av", "y", status );
vvd ( dv5[2], 0.5201843672856759, 1e-12, "slaDm2av", "z", status );
for ( i = 0; i < 3; i++ ) {
dv5[i] *= 1000.0;
}
sbmdvn ( dv5, dv6, &dvm );
vvd ( dv6[0], 0.004147420704640065, 1e-12, "slaDvn", "x", status );
vvd ( dv6[1], -0.9496888606842218, 1e-12, "slaDvn", "y", status );
vvd ( dv6[2], 0.3131674740355448, 1e-12, "slaDvn", "z", status );
vvd ( dvm, 1661.042127339937, 1e-9, "slaDvn", "m", status );
vvd ( sbmdvdv ( dv6, dv1 ), -0.3318384698006295,
1e-12, "slaDvn", "", status );
sbmdvxv (dv6, dv1, dv7 );
vvd ( dv7[0], 0.7767720597123304, 1e-12, "slaDvxv", "x", status );
vvd ( dv7[1], -0.1645663574562769, 1e-12, "slaDvxv", "y", status );
vvd ( dv7[2], -0.5093390925544726, 1e-12, "slaDvxv", "z", status );
}
void t_zd ( int *status )
{
vvd ( sbmzd ( -1.023, -0.876, -0.432 ),
0.8963914139430839, 1e-12, "slaZd", "", status );
}

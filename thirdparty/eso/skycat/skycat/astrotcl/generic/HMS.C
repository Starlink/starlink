/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id: HMS.C,v 1.1.1.1 2006/01/12 16:43:53 abrighto Exp $
 *
 * HMS.C - method definitions for class HMS
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * Peter W. Draper 27 Jan 03  Added extra_precision flags for milli-arcsec
 *                 08 Aug 08  Use libwcs dec2str and ra2str functions in the
 *                            print methods. Previous version wasn't careful
 *                            enough at the boundary conditions (given the
 *                            fraction part of the arcseconds value, which
 *                            can round up).
 */
static const char* const rcsId="@(#) $Id: HMS.C,v 1.1.1.1 2006/01/12 16:43:53 abrighto Exp $";

#include <cstdio>
#include <cstring>
#include <cmath>
#include "error.h"
#include "HMS.h"
#include "fitshead.h"

//  Default precision.
int HMS::extra_precision = 0;

/*
 * constructor - from H:M:S.sss, calculate double value
 * (note: hours is taken as double incase of "-00:mm:ss"
 */
HMS::HMS(double hours, int min, double sec)
    : hours_(int(hours)), min_(min), sec_(sec), show_sign_(0)
{
    val_ = (sec/60.0 + min)/60.0;

    double z = -0.0;  // check also for neg zero
    if (hours < 0.0 || memcmp(&z, &hours, sizeof(double)) == 0) {
	val_ = hours_ - val_;
	hours_ = -hours_;
	sign_ = '-';
    }
    else {
	val_ = hours_ + val_;
	sign_ = '+';
    }
}


/*
 * constructor - from decimal value, calculate H:M:S.sss
 */
HMS::HMS(double val)
    : val_(val), show_sign_(0)
{
    double dd, md, v = val, z = -0.0; // check also for neg zero
    if (v < 0.0 || memcmp(&z, &v, sizeof(double)) == 0) {
	sign_ = '-';
	v = -v;
    }
    else {
	sign_ = '+';
    }

    if ( extra_precision ) {
       dd = v + 0.000000000001;
    }
    else {
       dd = v + 0.0000000001; 
    } 
    hours_ = (int)dd;
    md = (dd - hours_) * 60.;
    min_ = (int)md;
    sec_ = (md - min_) * 60.;
}


/*
 * constructor - from string value, in format H:M:S.sss, hh, d.ddd, or 
 * H M S...  
 * If hflag is 1 and the value is not in H:M:S and is not an
 * integer (has a decimal point) convert to hours by dividing by 15.
 * If dflag is specified, it is set to 1 if the value was divided by 15.
 */
HMS::HMS(const char* s, int hflag, int* dflag)
  : show_sign_(0)
{
     if (!s) {
         val_ = sec_ = 0.;
         hours_ = min_ = 0;
         return;
     }

    double hours = 0;
    int min = 0;
    double sec = 0.0;
    double val = 0.0;
    int n = sscanf(s, "%lf%*[: ]%d%*[: ]%lf", &hours, &min, &sec);
    if (n >= 2) {
	// note: on HP, scanf on "-0.0" returns "0.0", on sun, "-0.0"
	if (hours == 0.0 && strchr(s, '-'))
	    hours = -0.0;
	*this = HMS(hours, min, sec);
    }
    else if (n == 1) {
	if (sscanf(s, "%lf", &val) == 1) {
	    if (hflag && strchr(s, '.')) {
		*this = HMS(val/15.);
		if (dflag)
		    *dflag = 1;
	    }
	    else
		*this = HMS(val);
	}
	else {
	    *this = HMS(hours, 0, 0);
	}
    }
    else {
	val_ = HMS_NULL;	// error
    }
}


/*
 * print in the given buffer in H:M:S format
 */
void HMS::print(char* buf) const
{
    if ( extra_precision ) {
        print_extra_precise_( buf );
    }
    else {
        print_normal_precise_( buf );
    }
}

//  Show 2 digits prec for dec, 3 for ra
void HMS::print_normal_precise_( char *buf ) const
{
    char lbuf[32];

    if ( show_sign_ ) {
        dec2str( lbuf, 32, val_, 2 );
    }
    else {
        ra2str( lbuf, 32, val_ * 15.0, 3 );
    }
    strncpy( buf, lbuf, 32 );
}

//  Show 4 digits prec for dec, 5 for ra
void HMS::print_extra_precise_( char *buf ) const
{
    char lbuf[32];

    if ( show_sign_ ) {
        dec2str( lbuf, 32, val_, 4 );
    }
    else {
        ra2str( lbuf, 32, val_ * 15.0, 5 );
    }
    strncpy( buf, lbuf, 32 );
}


/*
 * write this object to the given stream in the format
 * H:M:S.sss
 */
ostream& operator<<(ostream& os, const HMS& hms)
{
    char buf[80];
    hms.print(buf);
    os << buf;
    return os;
}


/*
 * read an HMS object from the given stream in the format
 * H:M:S.sss or H M S
 */
istream& operator>>(istream& is, HMS& hms)
{
    char c;
    double hours = 0;
    int min = 0;
    double sec = 0.0;
    is >> hours >> c >> min >> c >> sec;
    hms = HMS(hours, min, sec);
    return is;
}

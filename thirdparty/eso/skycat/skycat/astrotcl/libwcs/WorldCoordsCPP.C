/*
 * E.S.O. - VLT project/ESO Archive 
 * $Id$
 *
 * WorldCoords.C - method definitions for class WorldCoords
 * 
 * See the man page for a complete description.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */
static const char* const rcsId="@(#) $Id$";


#include <cstdio>
#include <cstring>
#include <cctype>
#include <cmath>
#include "error.h"
#include "wcs.h"
#include "WorldCoords.hxx"

// prototypes C  routines used (the header files are not ANSI C or C++ compat...)
extern "C" {
    // used to convert equinox
    int prej_q (double q0[2],	// IN: ra+dec at equinox eq0 in degrees 
		double q1[2], 	// OUT: precessed to equinox eq1	
		double eq0, 	// IN: Initial equinox (Julian Years)	
		double eq1);	// IN: Final equinox (Julian Years) 	
}


/*
 * check range of ra,dec values and return 0 if OK
 */
int WorldCoords::checkRange()
{
    double ra = ra_.val(), dec = dec_.val();

    if (ra < -0.001 || ra >= 25.0)
	return error("RA value out of range (0..24 hours)");

    if (dec < -90. || dec > 90.) 
	return error("DEC value out of range (-90..+90 deg)");
    
    return 0;
}


/** Set the equinox from the string and return 0 if okay (defaults to 2000). */
static int getEquinox(const char* equinoxStr, double& equinox) {
    if (!equinoxStr || strcmp(equinoxStr, "J2000") == 0) {
	equinox = 2000.;
	return 0;
    }
    if (strcmp(equinoxStr, "B1950") == 0) {
	equinox = 1950.;
	return 0;
    }
    if (*equinoxStr == 'J' || *equinoxStr == 'B')
	equinoxStr++;

    if (sscanf(equinoxStr, "%lf", &equinox) == 1) {
	return 0;
    }
    return 1;
}


/*
 * convert from one equinox to another.
 */
int WorldCoords::convertEquinox(double from_equinox, double to_equinox)
{
    if (from_equinox == to_equinox)
	return 0;

    double q0[2], q1[2];
    q0[0] = ra_.val() * 15;	// hours to degrees
    q0[1] = dec_.val();
    if (prej_q(q0, q1, from_equinox, to_equinox)) { // was successful
	ra_ = HMS(q1[0]/15);	// degrees to hours
	dec_ = HMS(q1[1]);
	dec_.show_sign(1);
	return 0;		// OK
    }
    // error return
    char buf[126];
    sprintf(buf, "could not convert equinox from %g to %g\n", 
	    from_equinox, to_equinox);
    return error(buf);
}


/*
 * Convert from one equinox (or named coordinate system) to another.
 * The first 2 parameters may be numbers, such as "2000" or "1950", or
 * coordinate system names, like "J2000", "FK5", "GALACTIC", "ECLIPTIC", ...
 * Epoch is the besselian epoch in years.
 * If dflag is 1, the ra value was converted to hours by dividing by 15
 */
int WorldCoords::convertEquinox(const char* fromEquinoxStr, const char* toEquinoxStr, double epoch, int dflag)
{
    // check for numerical equinox
    double from_equinox = 0.;
    double to_equinox = 0.;
    if (getEquinox(fromEquinoxStr, from_equinox) == 0 && getEquinox(toEquinoxStr, to_equinox) == 0)
	return convertEquinox(from_equinox, to_equinox);
    
    // convert from one system to another
    int sys1 = wcscsys((char*)fromEquinoxStr);
    if (sys1 == -1)
	return error("bad equinox value: ", fromEquinoxStr);
    int sys2 = wcscsys((char*)toEquinoxStr);
    if (sys2 == -1)
	return error("bad equinox value: ", toEquinoxStr);
    double dtheta = ra_.val();
    if (dflag)
	dtheta *= 15;  // hours to degrees
    double dphi = dec_.val();
    wcscon(sys1, sys2, from_equinox, to_equinox, &dtheta, &dphi, epoch);
    if (sys2 == WCS_J2000 || sys2 == WCS_B1950)
	dtheta /= 15;  // degrees to hours
    ra_ = HMS(dtheta);	
    dec_ = HMS(dphi);
    dec_.show_sign(1);
    return 0;
}


/*
 * constructor: note that the ra arg is H:M:S, while dec is D:M:S
 */
WorldCoords::WorldCoords(const HMS& ra, const HMS& dec, double equinox)
    : ra_(ra), dec_(dec) 
{
    dec_.show_sign(1);
    status_ = checkRange() || convertEquinox(equinox);
}


/*
 * constructor: note that the ra arg is H:M:S, while dec is D:M:S.
 * EquinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
WorldCoords::WorldCoords(const HMS& ra, const HMS& dec, const char* equinoxStr)
    : ra_(ra), dec_(dec) 
{
    dec_.show_sign(1);
    status_ = convertEquinox(equinoxStr);
}


/*
 * constructor: note that the ra and dec args are both in degrees
 */
WorldCoords::WorldCoords(double ra, double dec, double equinox)
    : ra_(ra/15), dec_(dec) 
{
    dec_.show_sign(1);
    status_ =  checkRange() || convertEquinox(equinox);
}


/*
 * constructor: note that the ra and dec args are both in degrees.
 * equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
WorldCoords::WorldCoords(double ra, double dec, const char* equinoxStr)
    : ra_(ra/15), dec_(dec) 
{
    dec_.show_sign(1);
    status_ = convertEquinox(equinoxStr);
}


/*
 * constructor: note that r... is H:M:S, while d... is D:M:S
 */
WorldCoords::WorldCoords(double rh, int rm, double rs, double dd, int dm, double ds,
		double equinox)
    : ra_(rh, rm, rs), dec_(dd, dm, ds) 
{
    dec_.show_sign(1);
    status_ = checkRange() || convertEquinox(equinox);
}
   
/*
 * constructor: note that r... is H:M:S, while d... is D:M:S.
 * equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
WorldCoords::WorldCoords(double rh, int rm, double rs, double dd, int dm, double ds,
		const char* equinoxStr)
    : ra_(rh, rm, rs), dec_(dd, dm, ds) 
{
    dec_.show_sign(1);
    status_ = convertEquinox(equinoxStr);
}
   

/*
 * constructor - parse a free format string assumed to contain RA and DEC
 *
 * Allowed formats of input string:
 *
 * 	hh mm ss.s +/-dd mm ss.s 
 * or
 * 	d.ddd +/-d.dddd
 *
 * or
 * 	hh +/-d.dddd
 *
 * If hflag is 1 and the ra value is not in H:M:S and is not an integer,
 * convert to hours by dividing by 15.
 */
WorldCoords::WorldCoords(const char* ra_str, const char* dec_str, double equinox, int hflag)
    : status_(0), ra_(ra_str, hflag), dec_(dec_str)
{
    if (ra_.isNull()) {
        status_ = 1;
	return;
    }
    if (dec_.isNull()) {
	status_ = 1;
	return;
    }
	
    dec_.show_sign(1);
    status_ = checkRange() || convertEquinox(equinox);
}

/*
 * constructor - parse coordinates in string format as above, except that
 * equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
WorldCoords::WorldCoords(const char* ra_str, const char* dec_str, const char* equinoxStr, int hflag)
    : status_(0), dec_(dec_str)
{
    int dflag = 0;  // set to 1 if ra was divided by 15
    ra_ = HMS(ra_str, hflag, &dflag);

    if (ra_.isNull()) {
        status_ = 1;
	return;
    }
    if (dec_.isNull()) {
	status_ = 1;
	return;
    }
	
    dec_.show_sign(1);

    double equinox = 2000.;
    if (getEquinox(equinoxStr, equinox) == 0) {
	status_ = checkRange() || convertEquinox(equinox);
    }
    else {
	// hack - need to know if the RA value is in hours or deg
	status_ = convertEquinox(equinoxStr, "J2000", 0., dflag);
    }
}


/*
 * Print the coordinates in the given buffers:
 * If hmsFlag is non-zero, in H:M:S [+-]D:M:S format, otherwise in decimal
 * degrees.
 */
void WorldCoords::print(char* ra_buf, char* dec_buf, double equinox, int hmsFlag) 
{
    if (equinox == 2000.0) {
	if (hmsFlag) {
	    ra_.print(ra_buf); 
	    dec_.print(dec_buf);
	} else {
	    sprintf(ra_buf, "%.17g", ra_deg());
	    sprintf(dec_buf, "%.17g", dec_deg());
	}
    }
    else {
	// make tmp copy and convert equinox before printing
	WorldCoords tmp = *this;
	tmp.convertEquinox(2000.0, equinox);
	if (hmsFlag) {
	    tmp.ra_.print(ra_buf); 
	    tmp.dec_.print(dec_buf);
	}
	else {
	    sprintf(ra_buf, "%.17g", tmp.ra_deg());
	    sprintf(dec_buf, "%.17g", tmp.dec_deg());
	}
    }
}


/*
 * Print the coordinates in the given buffers:
 * If hmsFlag is non-zero, in H:M:S [+-]D:M:S format, otherwise in decimal
 * degrees.
 * Here equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 * The printed coordinates are converted to the given equinox (or system).
 */
void WorldCoords::print(char* ra_buf, char* dec_buf, const char* equinoxStr, int hmsFlag) 
{
    double equinox = 2000.;
    if (getEquinox(equinoxStr, equinox) == 0) {
	print(ra_buf, dec_buf, equinox, hmsFlag);
    }
    else {
	// make tmp copy and convert equinox before printing
	WorldCoords tmp = *this;
	tmp.convertEquinox("J2000", equinoxStr);
	if (hmsFlag) {
	    tmp.ra_.print(ra_buf); 
	    tmp.dec_.print(dec_buf);
	}
	else {
	    sprintf(ra_buf, "%.17g", tmp.ra_deg());
	    sprintf(dec_buf, "%.17g", tmp.dec_deg());
	}
    }
}


/*
 * Print the coordinates to the given stream in the given equinox.
 */
void WorldCoords::print(ostream& os, double equinox)
{
    if (equinox == 2000.0) {
	os << *this;
    }
    else {
	// make tmp copy and convert equinox before printing
	WorldCoords tmp = *this;
	tmp.convertEquinox(2000.0, equinox);
	os << tmp;
    }
}
    

/*
 * Print the coordinates to the given stream in the given equinox (or system).
 * Here equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
void WorldCoords::print(ostream& os, const char* equinoxStr)
{
    double equinox = 2000.;
    if (getEquinox(equinoxStr, equinox) == 0) {
	print(os, equinox);
    }
    else {
	// make tmp copy and convert equinox before printing
	WorldCoords tmp = *this;
	tmp.convertEquinox("J2000", equinoxStr);
	os << tmp;
    }
}

    
/*
 * get ra and dec in degrees in the given equinox
 */
void WorldCoords::get(double& ra, double& dec, double equinox)
{
    if (equinox == 2000.0) {
	ra =  ra_deg();
	dec = dec_deg();
    }
    else {
	// make tmp copy and convert equinox 
	WorldCoords tmp = *this;
	tmp.convertEquinox(2000.0, equinox);
	ra = tmp.ra_deg();
	dec = tmp.dec_deg();
    }
}

    
/*
 * Get ra and dec in degrees in the given equinox.
 * Here equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC".
 */
void WorldCoords::get(double& ra, double& dec, const char* equinoxStr)
{
    double equinox = 2000.;
    if (getEquinox(equinoxStr, equinox) == 0) {
	get(ra, dec, equinox);
    }
    else {
	// make tmp copy and convert equinox 
	WorldCoords tmp = *this;
	tmp.convertEquinox("J2000", equinoxStr);
	ra = tmp.ra_.val();
	dec = tmp.dec_.val();
    }
}

   
/*
 * output operator: format: h:m:s[+-]d:m:s (J2000) or "" for null coords
 */
ostream& operator<<(ostream& os, const WorldCoords& pos)
{
    if (pos.isNull())
	os << "\"\"";
    else
	os << pos.ra_ << " " << pos.dec_;
    return os;
}

#if 0
/*
 * input operator: format:  h:m:s[+-]d:m:s (J2000)
 */
istream& operator>>(istream& is, WorldCoords& pos)
{
    is >> pos.ra_ >> pos.dec_;	// XXX need to input equinox ?
    pos.dec_.show_sign(1);
    return is;
}
#endif


/*
 * util: dispos computes distance and position angle solving a spherical 
 * triangle (no approximations)
 * INPUT        :coords in decimal degrees
 * OUTPUT       :dist in arcmin, returns phi in degrees (East of North)
 * AUTHOR       :a.p.martinez
 */
static double dispos(double dra0,	 // IN: center RA
		     double decd0,       // IN: center DEC
		     double dra,	 // IN: point RA
		     double decd,	 // IN: point DEC
		     double& dist)	 // OUT: distance in arcmin
{
    double alf,alf0,del,del0,phi;
    double sd,sd0,cd,cd0,cosda,cosd,sind,sinpa,cospa;
    const double radian=180./M_PI;

	// coo transformed in radiants 
    alf = dra / radian ;
    alf0= dra0 / radian ;
    del = decd / radian;
    del0= decd0 / radian;

    sd0=sin(del0);
    sd =sin(del);
    cd0=cos(del0);
    cd =cos(del);
    cosda=cos(alf-alf0);
    cosd=sd0*sd+cd0*cd*cosda;
    dist=acos(cosd);
    phi=0.0;
    if(dist > 0.0000004)
    {
	sind=sin(dist);
	cospa=(sd*cd0 - cd*sd0*cosda)/sind;
	if(cospa>1.0)cospa=1.0;
	sinpa=cd*sin(alf-alf0)/sind;
	phi=acos(cospa)*radian;
	if(sinpa < 0.0)phi = 360.0-phi;
    }
    dist *=radian;
    dist *=60.0;
    if(decd0 == 90.) phi = 180.0;
    if(decd0 == -90.) phi = 0.0;
    return(phi);
}

/*
 * return the distance between this position and the given one in arcmin
 */
double WorldCoords::dist(WorldCoords& pos) const
{
    double dist;
    dispos(ra_deg(), dec_deg(), pos.ra_deg(), pos.dec_deg(), dist);
    return dist;
}


/*
 * return the distance between this position and the given one in arcmin
 * and also set the position angle
 */
double WorldCoords::dist(WorldCoords& pos, double& pa) const
{
    double dist;
    pa = dispos(ra_deg(), dec_deg(), pos.ra_deg(), pos.dec_deg(), dist);
    return dist;
}


/* 
 * static member to get the distance between 2 points in arcmin
 */
double WorldCoords::dist(double ra0, double dec0, double ra1, double dec1)
{
    double dist;
    dispos(ra0, dec0, ra1, dec1, dist);
    return dist;
}


// taken from starbase: search.c, used below
#define X__PI	3.14159265358979323846
#define X_2PI	( 2 * X__PI )
#define X_R2D	(X_2PI / 360.0)
#define X_R2H	(X_2PI /  24.0)
#define X_H2D	(360.0 /  24.0)

#define r2h(r)	( (r) / X_R2H )
#define h2r(d)	( (d) * X_R2H )
#define r2d(r)	( (r) / X_R2D )
#define d2r(d)	( (d) * X_R2D )
#define h2d(r)	( (r) * X_H2D )
#define d2h(d)	( (d) / X_H2D )

/*
 * Given a radius in arcmin, set pos1 and pos2 to the 2 endpoints that form a box
 * with center at this position.
 */
int WorldCoords::box(double radius, WorldCoords& pos1, WorldCoords& pos2) const
{
    // get units in degrees
    double ra = ra_.val(), dec = dec_.val();
    radius /= 60.0;

    // get width of square
    double width = sqrt(2.0*radius*radius);
    double r1, r2, d1, d2;
    double cosdec;

    d1 = dec - width / 2.0;
    if ( d1 <= -90.0 ) {
	d1 = -90.0;
	d2 = dec + width / 2.0;
	r1 =  0.0;
	r2 = 24.0;
    } else {
	d2 = dec + width / 2.0;
	if ( d2 >=  90.0 ) {
	    d1 = dec - width / 2.0;
	    d2 = 90.0;
	    r1  = 0.0;
	    r2  = 24.0;
	} else {
	    if ( dec > 0.0 )
		cosdec = fabs(cos(d2r(d1)));
	    else	
		cosdec = fabs(cos(d2r(d2)));

	    r1 = ra - d2h(width) / 2 / cosdec;
	    r2 = ra + d2h(width) / 2 / cosdec;

	    if ( r1 <  0.0 ) 
		r1 += 24;
	    if ( r2 > 24.0 )
		r2 -= 24;
	}
    }
    
    pos1 = WorldCoords(h2d(r1), d1);
    pos2 = WorldCoords(h2d(r2), d2);

    return 0;
}


/*
 * Given the endpoints of a box (pos1, pos2), set width, height and radius in 
 * arcmin, and return the position at the center of the box
 */
WorldCoords WorldCoords::center(const WorldCoords& pos1, const WorldCoords& pos2, 
	       double& radius, double& width, double& height) 
{
    WorldCoords result;
    if (pos1.status() || pos2.status()) {
	error("invalid WCS position argument");
	return result;
    }

    // get center pos
    double ra1 = pos1.ra_deg(), dec1 = pos1.dec_deg();
    double ra2 = pos2.ra_deg(), dec2 = pos2.dec_deg();
    double ra = (ra1 + ra2)/2.0;
    double dec = (dec1 + dec2)/2.0;
    result = WorldCoords(ra, dec); 
    
    // get width and height of box
    width = (wcsdist(ra1, dec1, ra2, dec1) * 60.);
    height = (wcsdist(ra1, dec1, ra1, dec2) * 60.);

    // radius is half the distance from pos1 to pos2 
    radius = (wcsdist(ra1, dec1, ra2, dec2) * 60.)/2.;
    
    return result;
}




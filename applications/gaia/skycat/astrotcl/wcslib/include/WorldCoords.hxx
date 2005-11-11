// -*-c++-*-
#ifndef _WorldCoords_h_
#define _WorldCoords_h_

/*
 * E.S.O. - VLT project 
 * $Id$
 *
 * WorldCoords.h - class representing world coordinates 
 *                 (right-ascension, declination, stored as J2000 internally)
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  27 Sep 95  Created
 */

#include <cstdio>
#include "HMS.h"

const double WCS_NULL = HMS_NULL;	// null value for double coordinate value


/*
 * Class WorldCoords
 */
class WorldCoords {
protected:
    HMS ra_, dec_;		// right-ascension, declination
    int status_;		// status for errors in constructor

    // convert from one equinox to another
    int convertEquinox(double from_equinox, double to_equinox = 2000.0);

    // convert from one equinox (or named coordinate system) to another (string version).
    // equinoxStr may be a numerical equinox value, or a coordinate system name, such as
    // "J2000", "FK5", "GALACTIC", "ECLIPTIC", etc. (upper or lower case, abbrieviations okay).
    // If dflag is 1, the ra value was converted to hours by dividing by 15
    int convertEquinox(const char* from_equinox, const char* to_equinox = "J2000", double epoch = 0., int dflag = 1);

    // check range of ra,dec values
    int checkRange();
    
public:

    // constructor - initialize null coords
    WorldCoords() : status_(0) {}

    // constructor: note that ra is H:M:S, while dec is D:M:S
    WorldCoords(const HMS& ra, const HMS& dec, double equinox = 2000.0);

    // constructor: note that ra is H:M:S, while dec is D:M:S
    WorldCoords(const HMS& ra, const HMS& dec, const char* equinoxStr);

    // constructor: note that ra and dec are both in degrees
    WorldCoords(double ra, double dec, double equinox = 2000.0);

    // constructor: note that ra and dec are both in degrees
    WorldCoords(double ra, double dec, const char* equinoxStr);

    // constructor: note that r... is H:M:S, while d... is D:M:S
    WorldCoords(double rh, int rm, double rs, double dd, int dm, double ds,
		double equinox = 2000.0);
   
    // constructor: note that r... is H:M:S, while d... is D:M:S
    WorldCoords(double rh, int rm, double rs, double dd, int dm, double ds,
		const char* equinoxStr);
   
    // constructor - parse RA and DEC in string format
    WorldCoords(const char* ra, const char* dec, double equinox = 2000.0, int hflag = 0);

    // constructor - parse coordinates in string format
    // Note: equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC"
    WorldCoords(const char* ra, const char* dec, const char* equinoxStr, int hflag = 0);

    // return true if the coords are null
    int isNull() const {return ra_.isNull() ||  dec_.isNull();}

    // set to the null value
    void setNull() {ra_.setNull(); dec_.setNull();}
   
#if 0
    // input operator: format: H:M:S[+-]D:M:S, J2000
    friend istream& operator>>(istream&, WorldCoords& pos);
#endif

    // output operator: format: H:M:S[+-]D:M:S, J2000
    friend ostream& operator<<(ostream&, const WorldCoords& pos);

    // print coords to the given buffer with given equinox in the given format
    void print(char* ra_buf, char* dec_buf, double equinox = 2000.0, int hmsFlag=1);
    
    // print coords to the given buffer with given equinox in the given format
    // Note: equinoxStr may be a number or the system name, such as "GALACTIC" or "ECLIPTIC"
    void print(char* ra_buf, char* dec_buf, const char* equinoxStr, int hmsFlag=1);
    
    // print coords to the given stream with the given equinox in H:M:S format
    void print(ostream& os, double equinox = 2000.0);

    // print coords to the given stream with the given equinox (or system name) in H:M:S format
    void print(ostream& os, const char* equinoxStr);

    // get ra and dec in degrees in the given equinox
    void get(double& ra, double& dec, double equinox);
    
    // get ra and dec in degrees in the given equinox
    void get(double& ra, double& dec, const char* equinoxStr);
    
    // check for equality
    int operator==(const WorldCoords& pos) const {
	return ra_ == pos.ra_ && dec_ == pos.dec_;
    }
    int operator!=(const WorldCoords& pos) const {
	return ra_ != pos.ra_ || dec_ != pos.dec_;
    }

    // return the difference between 2 world coord points
    friend WorldCoords operator-(const WorldCoords& a, const WorldCoords& b) {
	return WorldCoords(a.ra_ - b.ra_, a.dec_ - b.dec_);
    }

    // short cuts
    
    // return ra and dec in degrees
    double ra_deg() const {return ra_.val()*15;}
    double dec_deg() const {return dec_.val();}

    // get distance in arcmin between points
    double dist(WorldCoords& pos) const;

    // get distance in arcmin and position angle between points
    double dist(WorldCoords& pos, double& pa) const;

    // static member to get the distance between 2 points (in deg) in arcmin
    static double dist(double ra0, double dec0, double ra1, double dec1);

    // Given a radius in arcmin, set pos1 and pos2 to the 2 endpoints that form a box
    // with center at this position.
    int box(double radius, WorldCoords& pos1, WorldCoords& pos2) const;

    // Given the endpoints of a box (pos1, pos2), set width, height and radius in 
    // arcmin, and return the center position of the box.
    static WorldCoords center(const WorldCoords& pos1, const WorldCoords& pos2, 
			      double& radius, double& width, double& height);

    // member access
    const HMS& ra() const {return ra_;}
    const HMS& dec() const {return dec_;}
    int status() const {return status_;}
};



#endif /* _WorldCoords_h_ */

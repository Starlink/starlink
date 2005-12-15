// -*-c++-*-
#ifndef _HMS_h_
#define _HMS_h_

/*
 * E.S.O. - VLT project 
 * $Id: HMS.h,v 1.5 2005/02/02 01:43:04 brighton Exp $
 *
 * HMS.h - class representing a value of the form "hours:min:sec"
 *
 * See the man page for a complete description.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  27 Sep 95  Created
 * pbiereic        17/02/03   Added 'using namespace std'. Removed ::std specs.
 */

#include <iostream>
#include <cmath>
using namespace std;

const double HMS_NULL = HUGE_VAL;	// null value for a double

/*
 * Class HMS
 */
class HMS {
protected:
    int hours_;			// base 60 values H:M:S.sss
    int min_;
    double sec_;
    double val_;		// value converted to decimal
    
    short show_sign_;		// flag, if true, include sign (+/-) when printing
    char sign_;			// '+' or '-'
  
public:
    // constructors
    HMS() : hours_(0), min_(0), sec_(0.0), val_(HMS_NULL), show_sign_(0) {}
    HMS(double hours, int min, double sec);
    HMS(double val);
    HMS(const char* s, int hflag = 0, int* dflag = 0);
    
    // return true if this object has the null value
    int isNull() const {return val_ == HMS_NULL;}
    
    // set to the null value
    void setNull() {val_ = HMS_NULL;}
   
    // member access
    int hours() const {return hours_;}
    int min() const {return min_;}
    double sec() const {return sec_;}
    double val() const {return val_;}
    char sign() const {return sign_;}

    // set to true to cause << to print with leading sign even when positive
    void show_sign(int b) {show_sign_ = b;}

    // ... (add I/O and arithmetic operators here) ...
    
    // output operator
    friend ostream& operator<<(ostream&, const HMS&);
    friend istream& operator>>(istream&, HMS&);

    // print in the given buffer in H:M:S format
    void print(char* buf) const;

    int operator<(const HMS& hms) const {
	return val_ < hms.val_;
    }
    int operator<=(const HMS& hms) const {
	return val_ <= hms.val_;
    }
    int operator>(const HMS& hms) const {
	return val_ > hms.val_;
    }
    int operator>=(const HMS& hms) const {
	return val_ >= hms.val_;
    }
    int operator==(const HMS& hms) const {
	return fabs(val_ - hms.val_) <= 0.0000000001;
    }
    int operator!=(const HMS& hms) const {
	return fabs(val_ - hms.val_) >= 0.0000000001;
    }

    // return the difference between 2 HMS values
    friend double operator-(const HMS& a, const HMS& b) {
	return (a.val_ - b.val_);
    }

    // set how many decimal places to show for arc seconds, this is 2
    // by default, setting this to 1 shows 3 (milli arcsec).
    static int extra_precision;

    protected:
    void print_normal_precise_( char *buf ) const;
    void print_extra_precise_( char *buf ) const;

};



#endif /* _HMS_h_ */

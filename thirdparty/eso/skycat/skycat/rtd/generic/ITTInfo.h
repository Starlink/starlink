// -*-c++-*-
#ifndef _ITTInfo_h_
#define _ITTInfo_h_
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: ITTInfo.h,v 1.4 1998/09/28 21:54:26 abrighto Exp $" 
 *
 * ITTInfo.h - class definitions for reading in ITT 
 *             (intensity transfer table) files
 * 
 * See the man page for a complete description.
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */

#include <sys/types.h>
#include <iostream.h>

enum {MAX_ITT=256};            /* work with 8bit color */


// one of these is used to hold ITT info for each ITT
// file read
class ITTInfo {
private:
    char* name_;		// filename
    double* value_;		// array of ITT values
    ITTInfo* next_;		// pointer to next ITT

    // copy constructor (not defined)
    ITTInfo(const ITTInfo&);

public:
    // constructor
    ITTInfo(char* name, double* value);

    // destructor
    ~ITTInfo();

    // create and return ITT from a file description
    static ITTInfo* get(char* filename);

    // write a list of ITT files
    static void list(ostream&);

    // member access
    const char* name() const {return name_;}
    ITTInfo* next() {return next_;}

    // Copy the rgb color values from src to dest and interpolate based 
    // on the ITT table and the count of available colors
    void interpolate(XColor* src, XColor* dest, int colorCount);

    // Copy the rgb color values from src to dest as above,
    // and also scale the ITT values by the given amount
    void scale(int amount, XColor* src, XColor* dest, int colorCount); 
};

#endif /* _ITTInfo_h_ */

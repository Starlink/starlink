// -*-c++-*-
#ifndef _LookupTable_h_
#define _LookupTable_h_
/*
 * E.S.O. - VLT project / ESO Archive
 *
 * "@(#) $Id: LookupTable.h,v 1.4 1998/07/23 23:37:55 abrighto Exp $" 
 *
 * LookupTable.h - declarations for class LookupTable, a class for managing
 *                 an image color lookup table used to convert image pixel
 *                 values to XImage byte values.
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  09 Aug 96  Created
 * Peter W. Draper 03 Mar 98  Converted to use unsigned long values
 *                            (need this to support visuals & depths)
 */

#include <sys/types.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

typedef unsigned char byte;


/* 
 * This class is used internally for reference counting and subclassing.
 * The public interface is through the LookupTable class.
 */
class LookupTableRep {
friend class LookupTable;
protected:
    unsigned long* lookup_;	// lookup table
    int size_;			// size of lookup table
    int refcnt_;		// reference count
    int status_;		// status after constructor

    // local util methods
    void fillLookup(int pixval, int imageval, int isSigned);
    int setLookup(int& imageval, int imagelim, int pixval);

public:
     // constructor (derived classes call this)
    LookupTableRep(int size);

    // destructor
    virtual ~LookupTableRep();

    // normally you use either a lookup table for shorts or for bytes
    enum {SHORT_SIZE = 65536, BYTE_SIZE = 256};

    // color scaling methods
    void linearScale(int lcut, int hcut, int isSigned, 
		     int ncolors, unsigned long* colors);

    void logScale(int lcut, int hcut, int isSigned, 
		  int ncolors, unsigned long* colors, double expo);

    void sqrtScale(int lcut, int hcut, int isSigned, 
		   int ncolors, unsigned long* colors, double expo);

    void histeqScale(int lcut, int hcut, int isSigned, 
		     int ncolors, unsigned long* colors,
		     int* histogram, int area);

    // reset to given color
    void reset(int color) {
	if (lookup_) memset(lookup_, color, sizeof(unsigned long) * size_);
    }
    
    // set the color value for a specific pixel value (blank pixel, for example)
    void setPixelColor(int pixval, unsigned long color);
};


/*
 * This class defines the public interface. It uses reference counting
 * with the above class to make it easier to implement different views of
 * the same image data.
 */
class LookupTable {
private:
    LookupTableRep* rep_;		// internal representation for reference counting

public:
    // constructor 
    LookupTable(int size);

    // copy constructor
    LookupTable(const LookupTable&);

    // constructor, from a pointer to a subclass of LookupTableRep
    LookupTable(LookupTableRep* rep) : rep_(rep) {}

    // destructor
    ~LookupTable();

    // assignment
    LookupTable& operator=(const LookupTable&);

    // color scaling methods
    void linearScale(int lcut, int hcut, int isSigned, int ncolors, 
		     unsigned long* colors) {
	if (rep_) 
	    rep_->linearScale(lcut, hcut, isSigned, ncolors, colors);
    }

    void logScale(int lcut, int hcut, int isSigned, int ncolors, 
		  unsigned long* colors, double expo = 10.0) {
	if (rep_) 
	    rep_->logScale(lcut, hcut, isSigned, ncolors, colors, expo);
    }

    void sqrtScale(int lcut, int hcut, int isSigned, int ncolors, 
		   unsigned long* colors, double expo = 10.0) {
	if (rep_) 
	    rep_->sqrtScale(lcut, hcut, isSigned, ncolors, colors, expo);
    }

    void histeqScale(int lcut, int hcut, int isSigned, 
		     int ncolors, unsigned long* colors,
		     int* histogram, int area) {
	if (rep_) 
	    rep_->histeqScale(lcut, hcut, isSigned, ncolors, colors, histogram, area);
    }

    // reset to given color
    void reset(int color) {
	if (rep_) rep_->reset(color);
    }
    
    // set the color value for a specific pixel value (blank pixel, for example)
    void setPixelColor(int pixval, unsigned long color) {
	if (rep_) rep_->setPixelColor(pixval, color);
    }

    // look up and return a value (this should be a fast, inline operation)
    // no check, for speed, not needed if using default lookup size
    unsigned long operator[](ushort i) {
#ifdef DEBUG
	assert(rep_ && i<rep_->size_);
#endif       
	return rep_->lookup_[i];
    }

    // note: if status is non-zero, the other methods are undefined
    int status() const {return rep_ ? rep_->status_ : 1;}
    int size() const {return rep_->size_;}

};

#endif /* _LookupTable_h_ */


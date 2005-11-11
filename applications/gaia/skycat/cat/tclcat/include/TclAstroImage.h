// -*-c++-*-
#ifndef _TclAstroImage_h_
#define _TclAstroImage_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: TclAstroImage.h,v 1.2 2003/01/20 15:52:21 brighton Exp $
 *
 * TclAstroImage.h - Tcl interface to the AstroImage C++ class for 
 * 		     accessing images from catalogs
 *
 * ------------------------------------------------------------------
 * NOTE: This class is obsolete, please use the TclAstroCat class
 *       instead.
 * ------------------------------------------------------------------
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <cstdio>
#include "TclCommand.h"
#include "AstroImage.h"


/*
 * This class declares the methods used to implement the Tcl astroimage
 * command for retrieving images from catalogs.
 */
class TclAstroImage : public TclCommand {
protected:
    AstroImage* im_;		// pointer to current open catalog
    WorldOrImageCoords pos_;	// saved position from last request
    double equinox_;		// saved equinox from last request
    FILE* feedback_;		// file ptr for feedback during xfer, if set
   
    // call a member function by name
    virtual int call(const char* name, int len, int argc, char* argv[]);

public:
    // constructor
    TclAstroImage(Tcl_Interp*, const char* cmdname, const char* instname);
    ~TclAstroImage();
   
    // entry point from Tcl
    static int astroImageCmd(ClientData, Tcl_Interp* interp, int argc, char* argv[]);

    // -- tcl subcommands --
    virtual int authorizeCmd(int argc, char* argv[]);
    virtual int openCmd(int argc, char* argv[]);
    virtual int closeCmd(int argc, char* argv[]);
    virtual int getimageCmd(int argc, char* argv[]);
    virtual int centerposCmd(int argc, char* argv[]);
    virtual int infoCmd(int argc, char* argv[]);
    virtual int copyrightCmd(int argc, char* argv[]);
    virtual int helpCmd(int argc, char* argv[]);
    virtual int feedbackCmd(int argc, char* argv[]);
    virtual int longnameCmd(int argc, char* argv[]);
    virtual int shortnameCmd(int argc, char* argv[]);
    virtual int iswcsCmd(int argc, char* argv[]);
    virtual int ispixCmd(int argc, char* argv[]);

};

#endif /* _TclAstroImage_h_ */


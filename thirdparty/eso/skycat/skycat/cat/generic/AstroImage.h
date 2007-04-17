// -*-c++-*-
#ifndef _AstroImage_h_
#define _AstroImage_h_

/*
 * E.S.O. - VLT project/ESO Archive
 * $Id: AstroImage.h,v 1.1.1.1 2006/01/12 16:36:41 abrighto Exp $
 *
 * AstroImage.h - base class definitions for classes that retrieve an image
 *                from a catalog based on object name, position, width and height.
 *
 * ------------------------------------------------------------------
 * NOTE: This class is obsolete, please use the AstroCatalog class
 * instead.
 * ------------------------------------------------------------------
 *
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  30 Sep 95  Created
 */


#include <cstdio>
#include "HTTP.h"
#include "WorldOrImageCoords.h"
#include "CatalogInfo.h"


/*
 * Class AstroImage
 *
 * This class is used to retrieve images from a remote image server based
 * on a name or position and a width and height in arc minutes.  The main
 * entry point is the "open" method, which returns a pointer to a class
 * object for the image server.
 */
class AstroImage {
protected:
    HTTP http_;			// http server handle
    char* tmpfile_;		// temp file to hold fits image
    int status_;		// status after constructor

    CatalogInfoEntry* entry_;   // ptr to the entry for this image svr

    // constructor - create catalog class instance
    // note: public interface uses AstroImage::open to hide subclass info
    AstroImage(CatalogInfoEntry*);

public:
    // copy constructor 
    AstroImage(const AstroImage&);

    // destructor - close catalog and free any resources
    virtual ~AstroImage();

    // open the named catalog and return a pointer to an AstroImage
    // object allocated for it or NULL if errors occur
    static AstroImage* open(const char* name);

    // pass a request to the catalog and return the name of a FITS file
    // containing the resulting image, or NULL if not found
    int getImage(const WorldOrImageCoords& pos, double width, double height);
    int getImage(const char* url);

    // return a pointer to the first config entry
    // (for link list traversal)
    static CatalogInfoEntry* firstCatalog() {
	return CatalogInfo::first();
    }
    
    // set the file ptr to use for http feedback during image transfers
    void feedback(FILE* f) {http_.feedback(f);}

    // member access:

    // return status (after constructor) for error checking
    int status() {return status_;}

    // return the handle for the HTTP object used to do the GET
    // (can be used to determine header values, or check if a 
    //  username and password are needed)
    HTTP& http() {return http_;}

    // set/get the temp file to use for getting images via http
    void tmpfile(const char* name);
    const char* tmpfile() {return tmpfile_;}

    // return the name of this service
    const char* name() {return entry_->longName();}
    const char* longName() {return entry_->longName();}
    const char* shortName() {return entry_->shortName();}

    // return the copyright field
    const char* copyright() {return entry_->copyright();}

    // return the help field
    const char* help() {return entry_->help();}

    // return true if the image server uses world coordinates
    int isWcs() {return entry_->isWcs();}

    // return true if the image server uses image pixel coordinates
    int isPix() {return entry_->isPix();}
};



#endif /* _AstroImage_h_ */

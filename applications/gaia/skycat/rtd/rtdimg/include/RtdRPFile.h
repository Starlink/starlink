#ifndef RTDRPFILE_H
#define RTDRPFILE_H

/*
 * E.S.O. - VLT project / ESO Archive
 *
 * RtdRPFile.h - class definitions for class RtdRPFile, RtdFITSCube,
 *	and RtdFITSComp.
 * 
 * See the man page for a complete description.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * D.Hopkinson	   17/04/97  Created
 * A.Brighton	   08/12/97  fixed lots of C++ errors found by SunPro CC
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/shm.h>
#include <sys/stat.h>

#include "TclCommand.h"
#include "tcl.h"
#include "tk.h"
#include "rtdImageEvent.h"
#include "rtdSem.h"
#include "Mem.h"

#define RTD_SHMBUFFS	5	// Number of shared memory buffers to create.
#define FITSBLOCK       2880

/*
 * This is an abstract base class from which the main classes for the file
 * type are derived. This simply sets up virtual chains for the functions
 * that are to be accessed from the recorder/playback tools.
 */
class RtdRPFile {

protected:
    FILE *fPtr;			// File pointer to file being read/written
    int imageCounter_;		// Count of image from start of record/playback.
    int imageCounter__;		// same for update_count()
    int xPixels_;		// Pixel width of each image.
    int yPixels_;		// Pixel height of each image.
    int bytesPerPixel_;		// Bytes per pixel.
    int dataType_;		// BYTE, SHORT, FLOAT, etc
    int startIndex_;		// Index of first image in file (maybe non-zero)
    int imageIndex_;		// Actual index of image in array.
    int hasTimeInfo_;		// File has included timestamp information.
    double *timeStamps_;	// Array of image event timestamps.
    double fileSize_;		// Current size of file.
    double maxFileSize_;	// Maximum allowed size of file.
    int numFileImages_;		// Number of image segments allowed in file.
    int numFileImages__;	// same for update_count()
    int fileFull_;		// Flag: true if file is at max size
    int status_;		// Object instance status.
    int shmSize_;		// Size of shared memory
    Tcl_Interp* interp_;        // tcl interpreter 
    char* instname_;            // name of tcl command created for this object
    char* fileName_;             // filename

    RtdRPFile();				// Null Constructor.
    RtdRPFile(Tcl_Interp* interp, char* instname,
              char *fileName, char *accFlag, double maxFileSize);               // Constructor.

    virtual int open(char *errMsg) {return 0;}	// Initialise props of existing file.

    // Check that the subimage information is consistent with the image.
    void checkSubImage(rtdIMAGE_INFO *, int&, int&, int&, int&);

public:
    virtual ~RtdRPFile();		// Destructor.

    // cleanup
    void cleanup();

    void update_count();

    // Get the next segment of image data.

    virtual int getNextImage(rtdShm *) {return 0;}

    // Get the previous segment of image data.
    virtual int getPrevImage(rtdShm *) {return 0;}

    // Add an image segment onto file end.
    virtual int addImage(rtdIMAGE_INFO *, int, int, int, int, int) {return 0;}

    // Goto a particular image index in the file.
    virtual void gotoImageIndex(int index) {fprintf(stderr, "Don't call\n");}

    // Goto a particular image count in the file.
    void gotoImageCount(int count);

    // round off file size
    static void padFile(FILE* f, int size);

    // Given an existing file, make and initialise the required file object.
    static RtdRPFile *makeFileObject(Tcl_Interp* interp, char* instname, char *fileName, char *errMsg);

    // Get the time increment between this and the next image.
    double getTimeIncrement(int direction);

    // Allocate shared memory and semaphores for the image
    int getShm(int numShm, rtdShm *shmInfo);

    // Private member access
    const double fileSize() {return fileSize_;}
    const int imageCounter() {return imageCounter_;}
    const int imageIndex() {return imageIndex_;}
    const int status() {return status_;}
    Tcl_Interp* interp() {return interp_;}
    char* instname() {return instname_;}
    const int hasTimeInfo() {return hasTimeInfo_;}
    const int bytesPerPixel() {return bytesPerPixel_;}
    const int xPixels() {return xPixels_;}
    const int yPixels() {return yPixels_;}
    const int dataType() {return dataType_;}
    const int numFileImages() {return numFileImages_;}
    const double maxFileSize() {return maxFileSize_;}
    const int fileFull() {return fileFull_;}
};

/*
 * This class defines the above methods for the case where the file is in
 * the form of a FITS cube.
 */
class RtdFITSCube : public RtdRPFile {

protected:
    // Size of the FITS header
    int FITSHeaderSize_;

    // Write the header at the top of the FITS cube.
    int writeFITSHeader(const rtdIMAGE_INFO *, int, int, int);

    int open(char *);

public:
    int getNextImage(rtdShm *);
    int getPrevImage(rtdShm *);
    int addImage(rtdIMAGE_INFO *, int, int, int, int, int);
    void gotoImageIndex(int index);

    // Constructor - call base class constructor.
    RtdFITSCube(Tcl_Interp* interp, char* instname, char *fileName, char *accFlag, double maxFileSize) :
	RtdRPFile(interp, instname, fileName, accFlag, maxFileSize),
	   FITSHeaderSize_(0) {}

    // NULL constructor.
    RtdFITSCube() : RtdRPFile() {}

    // Destructor.
    ~RtdFITSCube();
};

/*
 * This class defines the above methods for the case where the file is in
 * the form of a stack of compressed FITS files.
 *
 * NB This is not yet implemented - this class template shows how to implement
 * a record file type by defining the methods below.
 */
class RtdFITSComp : public RtdRPFile {

protected:

    int open(char*) {return 0;}

public:
    int getNextImage(rtdShm *){return 0;}
    int getPrevImage(rtdShm *){return 0;}
    int addImage(rtdIMAGE_INFO *, int, int, int, int, int){return 0;}
    void gotoImageIndex(int index) {}

    // Constructor - call base class contructor.
    RtdFITSComp(Tcl_Interp* interp, char* instname, char *fileName, char *accFlag, double maxFileSize) :
	RtdRPFile(interp, instname, fileName, accFlag, maxFileSize) {}

    // NULL constructor.
    RtdFITSComp() : RtdRPFile() {}

    // Destructor - call base class destructor.
    ~RtdFITSComp() {}
};

#endif

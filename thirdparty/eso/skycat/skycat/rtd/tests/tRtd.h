#ifndef tRtd_h
#define tRtd_h
/*
 * E.S.O. - VLT project 
 * "@(#) $Id: tRtd.h,v 1.1.1.1 2006/01/12 16:38:02 abrighto Exp $" 
 *
 * tRtd.h - definitions for tRtd
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * pbiereic        05/02/03  Created
 */

#define MAX_NX 1500   	// max #of pixels in x
#define MAX_NY 1500  	// max #of pixels in x

// default FITS file (for option -I).
#define DFILE		"../images/ngc1275.fits"

/* struct for command line options */
typedef struct opts {
    int  rapid_x;	// rapid frame start x
    int  rapid_y;	// rapid frame start y
    int  main_width;	// image width
    int  main_height;	// main image height
    int  rapid_width;	// rapid image width
    int  rapid_height;	// rapid image height
    int  rapid_id;	// rapid frame id
    int  verbose;	// verbose flag
    char *rtd_camera;	// name of camera
    int  delay;		// delay between updates
    int  numShm;	// number of shm buffers to use
    int  lock;		// Flag: use semaphore locking
    char *fitsFile;	// FITS file to use for image updates
    int  starx;		// FITS image star center x
    int  stary;		// FITS image star center y
    int  starbbox;	// FITS image star box length (and width)
    int  starjitter;	// FITS image star max. jitter
    int  shmEndian;	// native byte order, big or little Endian data
    int  useFits;	// use Fits file for generating the image
    int  dataType;	// image data type
} opts;

void errexit(const char* msg1, const char* msg2 = "");
void cleanup(int i=0);
void errprint(const char* buf);
void usage(opts *opt);
void parseInput(int argc, char** argv, opts *opt);

#endif /* tRtd_h */







/*  Avoid inclusion into files more than once. */
#ifndef _StarRtdForeignCmds_
#define _StarRtdForeignCmds_

/*
//+
// Name:
//    StarRtdForeignCmds

// Purpose:
//    Include file that defines the structure that describes any
//    "foreign" commands that require direct access to the displayed
//    image.

// Notes:
//    This file is designed to be included in C and C++ code.

//  Copyright:
//    Copyright (C) 1998 Central Laboratory of the Research Councils

// Authors:
//    P.W. Draper (PWD)

// History:
//    13-JUN-1996 (PWD):
//       Original version.

//-
*/

#include "tcl.h"

/*
// Enumerate the known data types (these are as used by ImageData and
// correspond roughly to FITS BITPIX field). This is a copy of the
// main enumeration in ImageIO, and is for including in C files.
*/
#ifndef __cplusplus
enum ImageDataType {
  BYTE_IMAGE   =  8,  /*  8 bit images  */
  X_IMAGE      = -8,  /* special, already color scaled, X image data */
  SHORT_IMAGE  =  16, /* 16 bit signed */
  USHORT_IMAGE = -16, /* 16 bit unsigned */
  LONG_IMAGE   =  32, /* 32 bit integer */
  FLOAT_IMAGE  = -32  /* 32 bit floating point */
};
#endif

/*
// Define the foreign command image information structure.
*/
struct StarImageInfo {
  int NDFid;                 /*  NDF identifier if available, 0 if not */
  void *imageData;           /*  Pointer to the displayed image data */
  enum ImageDataType type;   /*  Data type of displayed image */
  int nx;                    /*  X size of displayed image */
  int ny;                    /*  Y size of displayed image */
  Tcl_Interp *interp;        /*  The TCL interpreter, can be used to
                              *  pass back results */
};

/*
// Prototype any foreign commands. Note these all have the same
// arguments -- a pointer to the information structure and the
// addition arguments passed as an unprocessed string. They return a
// a status int and possibly an error string. The error string is only
// accessed if status is false, in which case the string will be freed
// after being used.
*/

#ifdef __cplusplus
extern "C" {
#endif
  int patchCmd( struct StarImageInfo *infoPtr, char *args, char **errStr );
  int writesliceCmd( struct StarImageInfo *infoPtr, char *args, char **errStr ); 
  int centroidCmd( struct StarImageInfo *infoPtr, char *args, char **errStr ); 


//  Define structure used to access foreign commands and enter the
//  names that they will be known by and the address of the function.
static struct StarRtdForeignCmds {

  // Command name (as passed to RTD foreign method)
  const char *name;       

  // Ptr to foreign function
  int (*fptr)( StarImageInfo *infoPtr, char *args, char **errStr );   

} foreigncmds_[] = {
  { "patch",      &patchCmd },
  { "writeslice", &writesliceCmd },
  { "centroid", &centroidCmd }
};
#ifdef __cplusplus
}
#endif
#endif /* StarRtdForeignCmds */

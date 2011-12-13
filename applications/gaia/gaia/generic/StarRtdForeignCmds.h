/*  Avoid inclusion into files more than once. */
#ifndef _StarRtdForeignCmds_
#define _StarRtdForeignCmds_

/*
 *+
 *  Name:
 *     StarRtdForeignCmds

 *  Purpose:
 *     Include file that defines the structure that describes any
 *     "foreign" commands that require direct access to the displayed
 *     image.

 *  Notes:
 *     This file is designed to be included in C and C++ code.

 *  Copyright:
 *     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     P.W. Draper (PWD)

 *  History:
 *     13-JUN-1996 (PWD):
 *        Original version.

 *-
 */

#include "tcl.h"

/*
 *  Enumerate the known data types (these are as used by ImageData and
 *  correspond roughly to FITS BITPIX field). This is a copy of the
 *  main enumeration in ImageIO, and is for including in C files.
 */
#ifndef __cplusplus
enum ImageDataType {
   BYTE_IMAGE  =   8,   /* 8 bit unsigned images  */
   X_IMAGE      = -8,   /* 8 bit signed, really a special type as
                           Skycat assumes this is already color
                           scaled, X image data */
   SHORT_IMAGE  =  16,  /* 16 bit signed */
   USHORT_IMAGE = -16,  /* 16 bit unsigned */
   LONG_IMAGE   =  32,  /* 32 bit integer */
   FLOAT_IMAGE  = -32,  /* 32 bit floating point */
   DOUBLE_IMAGE  = -64  /* 64 bit floating point */
};
#endif

/*
 *  Define the foreign command image information structure.
 */
struct StarImageInfo {
   int NDFid;                 /*  NDF identifier if available, 0 if not */
   void *imageData;           /*  Pointer to the displayed image data */
   int swap;                  /*  Whether data pointed needs byte swapping */
   enum ImageDataType type;   /*  Data type of displayed image */
   int nx;                    /*  X size of displayed image */
   int ny;                    /*  Y size of displayed image */
   Tcl_Interp *interp;        /*  The TCL interpreter, can be used to
                               *  pass back results */
};

/*
 *  Prototype any foreign commands. Note these all have the same
 *  arguments -- a pointer to the information structure and the
 *  addition arguments passed as an unprocessed string. They return a a
 *  status int and possibly an error string. The error string is only
 *  accessed if status is false, in which case the string will be freed
 *  after being used.
 */

#ifdef __cplusplus
extern "C" {
#endif

int patchCmd( struct StarImageInfo *infoPtr, char *args, char **errStr );
int writesliceCmd( struct StarImageInfo *infoPtr, char *args, char **errStr );
int centroidCmd( struct StarImageInfo *infoPtr, char *args, char **errStr );

/*
 *  Define structure used to access foreign commands and enter the
 *  names that they will be known by and the address of the
 *  function.
 */
#ifdef __cplusplus
static struct StarRtdForeignCmds {

   /*  Command name (as passed to RTD foreign method) */
   const char *name;

   /*  Ptr to foreign function */
   int (*fptr)( StarImageInfo *infoPtr, char *args, char **errStr );

} foreigncmds_[] = {
   { "patch",      &patchCmd },
   { "writeslice", &writesliceCmd },
   { "centroid",   &centroidCmd }
};
#endif
#ifdef __cplusplus
}
#endif

#endif /* StarRtdForeignCmds */

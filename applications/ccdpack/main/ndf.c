/*
*+
*  Name:
*     ndf

*  Purpose:
*     Provides new commands "ndf" and "ndfset" for use with Tcl.

*  Language:
*     ANSI C.

*  Type of Module:
*     C extension to Tcl8.

*  Description:
*     This file provides an object to use within Tcl programs for
*     manipulating NDFs.  There are two types of objects implemented
*     here, an ndf and and ndfset.  Many of the same commands apply
*     to both types of objects (e.g. you can execute "$ob name"
*     to find the name of the ndf or ndfset object), but there is
*     no straightforward inheritance relationship between the two.
*     An ndfset object has-an array of ndf objects as well as some
*     other characteristics.
*
*     Creating an ndf or ndfset object will open the named NDF(s);
*     deleting the object will annul them.
*
*     The commands can provide to calling Tcl scripts the NDF
*     dimensions, name and so on.  Other services are also provided
*     to calling C code (e.g. other extensions to Tcl), such as
*     mapping the NDF and generating an array suitable for feeding
*     to PGPLOT for plotting on a display device.
*
*     The following prologue items document the behaviour of both ndf and
*     ndfset objects.

*  Usage:
*     set ndfob [ndf ndfname]
*     set ndfsetob [ndfset setname ndfname ?ndfname ...?]

*  Constructors:
*     ndf name
*        Creates an NDF object to manipulate the NDF which has the filename
*        "name".  Returns the name of a new object command which can
*        be used to manipulate the ndf object.
*
*     ndfset setname ndfname ?ndfname ...?
*        Creates a new ndfset object with the given identifying setname
*        (for presentation to the user via the ndfset name object command)
*        and containing the named NDFs.  If an empty string is supplied
*        for the setname then the name of the first NDF in the list will
*        be used.  Returns the name of a new object command which can be
*        used to manipulate the new ndfset object.

*  Object Commands:
*     ndfob-or-ndfsetob addframe baseframe coeffs ?newdomain?
*        This adds a new frame to the WCS component of the NDF; note it
*        does not write to the NDF on disk, it only affects the
*        properties of the NDF object available to Tcl.
*
*        The baseframe argument (specified either as a numerical
*        frame index, as a domain name, or as one of the special
*        strings "BASE" or "CURRENT") gives the frame from which
*        the new one is to be attached.  The coeffs argument is a
*        six-element list giving the coefficients of the linear
*        mapping connecting it to baseframe.  The optional newdomain
*        argument gives a domain name for the new frame.
*
*     ndfob-or-ndfsetob bbox frame
*        Returns a list giving the extent of a bounding box large enough
*        to hold the image when resampled from Base (Grid) coordinates into
*        the indicated frame and plotted.  The returned value is a list
*        of the form {{lo hi} {lo hi} .. } with one pair for each of the
*        dimensions.
*
*        The frame may be specified either as a numerical frame index,
*        as a domain name, or as one of the special strings "BASE" or
*        "CURRENT".
*
*     ndfob bounds
*        Returns a list giving the bounds of the NDF.  There are ndim
*        elements in the list, where ndim is the number of dimensions
*        of the NDF.  Each element is a two element list giving the
*        lower and upper pixel index bound.
*
*     ndfob-or-ndfsetob centroid x y frame zoom
*        This gives the centroided position of the object near the
*        point which has coordinates (x,y) in the coordinates of
*        frame.  Zoom gives the zoom factor at which the object was
*        identified by the user, on the basis that if the display
*        of the image is all squashed up small the user is unlikely
*        to have selected positions to pixel accuracy.
*
*        The frame may be specified either as a numerical frame index,
*        as a domain name, or as one of the special strings "BASE"
*        or "CURRENT".
*
*     ndfob-or-ndfsetob destroy
*        Releases resources associated with the object.  For an
*        ndf object it will annul the associated NDF; for an
*        ndfset object it will annul all the NDFs associated with
*        the members of the ndfset.
*
*     ndfob-or-ndfsetob display ?-resamp? device loperc hiperc frame ?plotstyle?
*        Plots the image on the named PGPLOT device, with colour cutoffs
*        determined by the loperc and hiperc arguments, and axes drawn
*        according to the frame argument.  Loperc and hiperc give the
*        percentile values to use for cutoffs.  The plotstyle string
*        may contain any plotting options desired, in astSet format.
*
*        If the '-resamp' option is given, then the image will be
*        resampled into the indicated frame before plotting; otherwise
*        the frame argument is just used for plotting axes.
*
*        Applied to an ndf object without the -resamp option, the image
*        is plotted in its Base frame.  Applied to an ndfset object
*        without -resamp, it is resampled into its CCD_SET frame.
*        It is an error to attempt to display an ndfset object without
*        -resamp unless all its member ndf objects have CCD_SET frames.
*
*        The frame may be specified either as a numerical frame index,
*        as a domain name, or as one of the special strings "BASE"
*        or "CURRENT".
*
*     ndfob fitshead ?key? ...
*        This method allows access to any FITS headers contained in the
*        .MORE.FITS extension of the NDF.  If any optional key arguments
*        are supplied, then it will return an ordered list containing
*        the values of the FITS headers with the named keywords.  Absent
*        headers are represented by empty strings.  If no key arguments
*        are supplied, it will return a list of all the headers; each
*        element is the whole header card, except that trailing spaces
*        and empty comments are stripped.
*
*     ndfob-or-ndfsetob frameatt attname ?frame?
*        Returns frame attribute values from the associated WCS frameset.
*        If the optional frame argument is specified, then the value
*        of the attribute is returned for the specified frame.  If not,
*        then a list is returned containing one element with the value
*        for each of the frames in the frameset.  The attname parameter
*        may be any attribute possessed by a frame; a useful attname
*        is "Domain".
*
*        If the special value "index" is supplied as the attname argument,
*        then the numeric value(s) of the frame(s) specified by the
*        frame argument is returned instead of the value of a real
*        attribute of the frame.
*
*        The optional frame may be specified either as a numerical frame
*        index, as a domain name, or as one of the special strings
*        "BASE" or "CURRENT".
*
*     ndfob-or-ndfsetob mapped ?setmap?
*        This method controls mapping of the image's DATA component(s).
*        With no optional setmap argument it returns a boolean value
*        indicating whether the DATA array is currently mapped.
*        If the setmap argument is supplied, it will force the DATA
*        array to be mapped or unmapped according to the supplied
*        value (it may take any of the values recognised by Tcl_GetBoolean).
*        Except as modified explicitly by such calls, the DATA array
*        will be mapped when it is needed, and unmapped only when
*        the ndf object is destroyed.  Thus it is never necessary to
*        call this method; if it is unlikely that the DATA array will
*        be needed again however, it may be called to force its unmapping.
*
*     ndfob-or-ndfsetob name
*        Returns an identifying string.  For an ndf object this is the
*        name initially used to open the NDF.  For an ndfset object
*        it is probably the Set Name attribute common to the NDFs.
*
*     ndfsetob ndfdo index|all cmd ?args? ...
*        This executes a command on an ndf member of an ndfset object
*        as if it were being addressed as an ndf object.  The index
*        (starting at zero for the first one) indicates which ndf
*        object the command is to be performed on, and the cmd and
*        args argument give the command and its arguments.  If the
*        literal 'all' is given for the index argument, the command
*        will be performed on all of the member NDFs and the
*
*     ndfsetob nndf
*        Returns the number of member NDFs in the ndfset.
*
*     ndfob percentile perc ?perc? ...
*        This calculates the value(s) of the data in the image which
*        correspond(s) to the given percentile value(s).  It returns a
*        list containing as many elements as the number of perc values
*        specified.  It is more efficient to calculate a multiple
*        percentile values using a single call to this method than
*        multiple calls.  It caches previously requested values, so
*        that requesting a percentile which has been requested at any
*        previous time in the lifetime of this object is fast.
*
*     ndfob-or-ndfsetob pixelsize frame
*        Returns the approximate linear size of an NDF (grid) pixel in
*        units of the current frame.  This only properly makes
*        sense if certain assumptions about the form of the transformation
*        between the two frames hold true, but if it approximates to
*        a combination of zooms, rotations and translations the result
*        should well defined enough to be useful.
*
*        The frame may be specified either as a numerical frame index,
*        as a domain name, or as one of the special strings "BASE" or
*        "CURRENT".
*
*     ndfob polygon frame
*        Returns a list of points ({x1 y1} {x2 y2} ...) which map out the
*        outline in the given frame of the grid of the NDF.  Currently
*        this method only returns a list of four points, one for the
*        transformed position of each corner; therefore if the mapping
*        is quite nonlinear (and has bendy sides) it may not contain
*        the pixels of the grid very accurately.
*
*        This method only works on the first two dimensions of the NDF,
*        regardless of the NDF's dimensionality.
*
*     ndfob validndf
*        Returns true if the NDF identifier is valid.  This method
*        can be used and caught to see whether a command represents
*        an object belonging to the ndf class.
*
*     ndfsetob validndfset
*        Returns true if the NDF identifiers of all the member NDFs
*        are valid. This method can be used and caught to see whether
*        a command represents an object belonging to the ndfset class.
*
*     ndfob-or-ndfsetob wcstran ?-format? from to pos ?pos ...?
*        Transforms a set of points between coordinate frames in
*        the WCS frameset.
*           - options  -- If '-format' is given, the output will be a
*                         text strings generated by AST_FORMAT, otherwise
*                         it will be numeric values.
*           - from     -- The frame from which to convert
*           - to       -- The frame to which to convert
*           - pos      -- Coordinates of a point in the 'from' frame
*
*        The frames from and to may be specified either as a numerical
*        frame index, as a domain name, or as one of the special strings
*        "BASE" or "CURRENT".  The return value is a list of points,
*        each element of the list containing the coordinates in the 'to'
*        frame of the corresponding pos argument.

*  Copyright:
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-AUG-2000 (MBT):
*        Initial version.
*     9-MAR-2001 (MBT):
*        Upgraded for use with Sets.
*     18-NOV-2005 (TIMJ):
*        Use official HDS C interface
*     {enter_further_changes_here}

*  Bugs:
*     In several cases, if a command exits with error status, it may
*     have leaked some memory.  On the whole, such leaks should be
*     pretty small.
*     {note_new_bugs_here}

*-
*/


#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <math.h>
#include "sae_par.h"
#include "dat_par.h"
#include "img.h"
#include "ndf.h"
#include "tcl.h"
#include "mers.h"
#include "cnf.h"
#include "tclndf.h"
#include "ccdaux.h"
#include "ast.h"
#include "cpgplot.h"
#include "star/hds.h"

F77_SUBROUTINE(ccd1_fra)(
    CHARACTER(TYPE), INTEGER(EL), POINTER(IPARR), INTEGER(NFRAC), DOUBLE(FRAC),
    LOGICAL(BAD), DOUBLE(CLFRAC), DOUBLE(VALUES), INTEGER(STATUS)
    TRAIL(TYPE) );
F77_SUBROUTINE(ccd1_lnmap)(
    DOUBLE_ARRAY(TR), INTEGER(MAP), INTEGER(STATUS) );
F77_SUBROUTINE(ccd1_ftget)(
    INTEGER(NCARDS), POINTER(IPFITS), INTEGER(SCARD), CHARACTER(NAME),
    CHARACTER(VALUE), INTEGER(ICARD), INTEGER(STATUS)
    TRAIL(NAME) TRAIL(VALUE) );

F77_SUBROUTINE(ccg1_cenb)(
    DOUBLE(XINIT), DOUBLE(YINIT), BYTE(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_cenub)(
    DOUBLE(XINIT), DOUBLE(YINIT), UBYTE(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_cenw)(
    DOUBLE(XINIT), DOUBLE(YINIT), WORD(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_cenuw)(
    DOUBLE(XINIT), DOUBLE(YINIT), UWORD(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_ceni)(
    DOUBLE(XINIT), DOUBLE(YINIT), INTEGER(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_cenr)(
    DOUBLE(XINIT), DOUBLE(YINIT), REAL(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );
F77_SUBROUTINE(ccg1_cend)(
    DOUBLE(XINIT), DOUBLE(YINIT), DOUBLE(IMAGE), INTEGER(NCOL), INTEGER(NLINE),
    INTEGER(ISIZE), LOGICAL(SIGN), DOUBLE(MAXSHF), INTEGER(MAXIT),
    DOUBLE(TOLER), DOUBLE(XACC), DOUBLE(YACC), INTEGER(STATUS) );

/* Unions for use with the Tcl hashes which store calculated percentiles.
   Tcl hash tables use something the size of a pointer for both the key
   and the value of each hash.  Since we want floating point numbers as
   both the key and the value, we put these in the pointer slots.
   Since they sometimes need to look like pointers for passing to hash
   manipulation routines, we therefore need to muck about with unions.
   If a pointer wasn't big enough to store the values (as it wouldn't
   be on Linux if we used double, for instance) then this strategy
   would be no good, and it would have to be recoded using real pointers
   (and the extra memory management which that would imply).  A test
   is made in the Ndf_Init routine that the system we've compiled on
   has types the right size for this sort of thing to be OK.
*/
   typedef union {
      char *hash;               /* Tcl hash's view of the hash key */
      float data;               /* Hash user's view of the hash key */
   } PercHashKey;
   typedef union {
      ClientData hash;          /* Tcl hash's view of the hash value */
      float data;               /* Hash users's view of the hash value */
   } PercHashValue;


/* General purpose static buffers.
 */
   static char buffer[ 4096 ];

/* Machinery for calling ndfdisplay as a background process.
 */
   struct ndfdisplay_args {
      char *device;
      char *settings;
      int *pixbloc;
      float *gbox;
      float *vp;
      double *bbox;
      double psize;
      double zoom;
      AstFrameSet *wcs;
      int *iframes;
      int *pframes;
      int xpix;
      int ypix;
   };


/**********************************************************************/
   int NdfCmd( ClientData clientData, Tcl_Interp *interp, int objc,
               Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      Ndf *ndf;
      static int counter = 0;
      Tcl_Obj *op;

/* Check command arguments.  There should be a single extra argument for
   the constructor, the name of the NDF. */
      if ( objc != 2 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "ndfName" );
         return TCL_ERROR;
      }

/* Create the new NDF data structure. */
      if ( newNdf( interp, objv[ 1 ], &ndf ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Create a new Tcl command corresponding to the newly created object. */
      sprintf( buffer, "ndf%d", counter );
      Tcl_CreateObjCommand( interp, buffer, ObjectNdfCmd, (ClientData) ndf,
                            DeleteNdf );

/* Bump the object count. */
      counter++;

/* Return the name of the new command as the result of this command. */
      op = Tcl_NewStringObj( buffer, -1 );
      Tcl_SetObjResult( interp, op );

/* Return successful operation code. */
      return TCL_OK;
   }



/**********************************************************************/
   void DeleteNdf( ClientData clientData ) {
/**********************************************************************/
      Ndf *ndf = (Ndf *) clientData;
      forgetNdf( ndf );
   }


/**********************************************************************/
   int ObjectNdfCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                     Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      NdfOrNdfset *ndf = (NdfOrNdfset *) clientData;
      Ndf1 *ndf1 = ndf->nmember ? NULL : ndf->content.ndf1;
      AstFrameSet *wcs = ndf->wcs;
      Tcl_Obj *result;
      Tcl_Obj *op;
      char *command = Tcl_GetString( objv[ 1 ] );
      int i;
      int j;

/* We deal with certain commands (those which work the same on ndf or
   ndfset objects) by passing them straight to the ObjectNdfsetCmd()
   routine. */
      if ( ! strcmp( command, "addframe" ) ||
           ! strcmp( command, "bbox" ) ||
           ! strcmp( command, "centroid" ) ||
           ! strcmp( command, "destroy" ) ||
           ! strcmp( command, "display" ) ||
           ! strcmp( command, "frameatt" ) ||
           ! strcmp( command, "mapped" ) ||
           ! strcmp( command, "name" ) ||
           ! strcmp( command, "pixelsize" ) ||
           ! strcmp( command, "wcstran" ) ) {
         return ObjectNdfsetCmd( clientData, interp, objc, objv );
      }

/* Take care of pending Tcl events now, in case what we are about to do
   is time-consuming. */
      tclupdate();

/**********************************************************************/
/* "bounds" command                                                   */
/**********************************************************************/
      if ( ! strcmp( command, "bounds" ) ) {

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Set result. */
         result = Tcl_NewListObj( 0, NULL );
         for ( i = 0; i < ndf1->ndim; i++ ) {
            sprintf( buffer, "%d %d", ndf1->lbnd[ i ], ndf1->ubnd[ i ] );
            op = Tcl_NewStringObj( buffer, -1 );
            Tcl_ListObjAppendElement( interp, result, op );
         }
      }

/**********************************************************************/
/* "fitshead" command                                                 */
/**********************************************************************/
      else if ( ! strcmp( command, "fitshead" ) ) {

/* If we have not attempted to load the FITS extension before, do so now. */
         if ( ! ndf1->fits.loaded ) {
            STARCALL(
               int there;

/* See if a FITS extension exists. */
               ndfXstat( ndf1->identifier, "FITS", &there, status );

/* If it does, then map it. */
               if ( F77_ISTRUE(there) ) {

/* Get an HDS locator. */
                  ndf1->fits.loc = NULL;
                  ndfXloc( ndf1->identifier, "FITS", "READ", &ndf1->fits.loc, status );

/* Map the data and store the pointer in the ndf data structure. */

		  datMapV( ndf1->fits.loc, "_CHAR*80", "READ", (void**)&ndf1->fits.data,
			   &ndf1->fits.ncard, status );
               }

/* There was no FITS extension; record this as effectively a mapped FITS
   block of zero length. */
               else {
                  ndf1->fits.ncard = 0;
		  ndf1->fits.loc = NULL;
               }

/* Record that we have got any FITS headers there are to get. */
               ndf1->fits.loaded = 1;
            )
         }

/* Initialise result object. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );

/* No further arguments: return the whole FITS block as a list of lines. */
         if ( objc == 2 ) {
            char *card;
            char *pc;
            Tcl_Obj *cardobj;
            for ( j = 0; j < ndf1->fits.ncard; j++ ) {
               card = ndf1->fits.data + j * 80;
               pc = card + 79;
               while ( *pc == ' ' && pc > card ) pc--;
               while ( *pc == '/' && pc > card ) pc--;
               while ( *pc == ' ' && pc > card ) pc--;
               cardobj = Tcl_NewStringObj( card, ( pc - card + 1 ) );
               Tcl_ListObjAppendElement( interp, result, cardobj );
            }
         }

/* There are additional arguments: for each one find the (first) matching
   line and append it to the list for return.  Multiple calls of ccd1_ftget_
   are not the most efficient way to do this if there are many items in
   the list, but calls to this routine are unlikely ever to be very
   time-critical. */
         else {
            char *key;
            char *pc;
            Tcl_Obj *valobj;
            result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
            for ( i = 2; i < objc; i++ ) {
               key = Tcl_GetString( objv[ i ] );
               errMark();
               {
                  int status[] = { SAI__OK };
                  int one = 1;
                  int icard;
                  int incards = ndf1->fits.ncard;
                  F77_POINTER_TYPE ipfits;
                  DECLARE_CHARACTER( fkey, 80 );
                  DECLARE_CHARACTER( fvalue, 80 );
                  cnfExprt( key, fkey, 80 );
                  ipfits = cnfFptr( ndf1->fits.data );
                  F77_CALL(ccd1_ftget)( INTEGER_ARG(&incards),
                                        POINTER_ARG(&ipfits),
                                        INTEGER_ARG(&one),
                                        CHARACTER_ARG(fkey),
                                        CHARACTER_ARG(fvalue),
                                        INTEGER_ARG(&icard),
                                        INTEGER_ARG(status)
                                        TRAIL_ARG(fkey) TRAIL_ARG(fvalue) );
                  if ( *status == SAI__OK ) {
                     pc = fvalue + fvalue_length - 1;
                     while ( *pc == ' ' && pc > fvalue ) pc--;
                     valobj = Tcl_NewStringObj( fvalue, ( pc - fvalue + 1 ) );
                  }
                  else {
                     errAnnul( status );
                     valobj = Tcl_NewStringObj( "", 0 );
                  }
               }
               errRlse();
               Tcl_ListObjAppendElement( interp, result, valobj );
            }
         }
      }


/**********************************************************************/
/* "percentile" command                                               */
/**********************************************************************/
      else if ( ! strcmp( command, "percentile" ) ) {
         double *percs;
         double *vals;
         int nperc;

/* Check syntax. */
         nperc = objc - 2;
         if ( nperc < 1 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "perc ?perc? ..." );
            return TCL_ERROR;
         }

/* Allocate arrays in which to store percentiles and values. */
         percs = malloc( nperc * sizeof( double ) );
         vals = malloc( nperc * sizeof( double ) );
         if ( tclmemok( interp, percs ) != TCL_OK ||
              tclmemok( interp, vals ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Store each of the percentile arguments in the percentiles array. */
         for ( i = 0; i < nperc; i++ ) {
            if ( Tcl_GetDoubleFromObj( interp, objv[ i + 2 ], &percs[ i ] )
                 != TCL_OK ) {
               return TCL_ERROR;
            }
         }

/* Get the values. */
         STARCALL(
            getpercentiles( ndf1, nperc, percs, vals, status );
         )

/* Create the result object. */
         result = Tcl_NewListObj( 0, NULL );
         for ( i = 0; i < nperc; i++ ) {
            op = Tcl_NewDoubleObj( vals[ i ] );
            Tcl_ListObjAppendElement( interp, result, op );
         }

/* Tidy up. */
         free( percs );
         free( vals );
      }


/**********************************************************************/
/* "polygon" command                                                  */
/**********************************************************************/
      else if ( ! strcmp( command, "polygon" ) ) {
         int iframe;
         double xb[ 4 ];
         double yb[ 4 ];
         double xf[ 4 ];
         double yf[ 4 ];
         AstMapping *map;

/* Check syntax. */
         if ( objc != 3 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "frame" );
            return TCL_ERROR;
         }

/* Get frame argument. */
         if ( NdfGetIframeFromObj( interp, objv[ 2 ], wcs, &iframe )
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Set positions of the corners in the base frame. */
         xb[ 0 ] = xb[ 3 ] = 0.5;
         xb[ 1 ] = xb[ 2 ] = ndf1->ubnd[ 0 ] - ndf1->lbnd[ 0 ] + 1.5;
         yb[ 0 ] = yb[ 1 ] = 0.5;
         yb[ 2 ] = yb[ 3 ] = ndf1->ubnd[ 1 ] - ndf1->lbnd[ 1 ] + 1.5;

/* Get the positions of the corners in the requested frame. */
         ASTCALL(
            astBegin;
            map = astGetMapping( wcs, AST__BASE, iframe );
            astTran2( map, 4, xb, yb, 1, xf, yf );
            astEnd;
         )

/* Set the result object. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
         for ( i = 0; i < 4; i++ ) {
            Tcl_Obj *ov[ 2 ];
            ov[ 0 ] = Tcl_NewDoubleObj( xf[ i ] );
            ov[ 1 ] = Tcl_NewDoubleObj( yf[ i ] );
            Tcl_ListObjAppendElement( interp, result, Tcl_NewListObj( 2, ov ) );
         }
      }


/**********************************************************************/
/* "validndf" command                                                 */
/**********************************************************************/
      else if ( ! strcmp( command, "validndf" ) ) {
         int valid = 0;

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Check validity of the NDF identifier.  It should always be valid though. */
         STARCALL(
            ndfValid( ndf1->identifier, &valid, status );
         )

/* Set result value. */
         result = Tcl_NewBooleanObj( F77_ISTRUE(valid) );
      }


/**********************************************************************/
/* No known command.                                                  */
/**********************************************************************/
      else {

/* Exit with a helpful message and error status. */
         result = Tcl_NewStringObj( "", 0 );
         Tcl_AppendObjToObj( result, objv[ 0 ] );
         Tcl_AppendStringsToObj( result, " ", (char *) NULL );
         Tcl_AppendObjToObj( result, objv[ 1 ] );
         Tcl_AppendStringsToObj( result, ": ndf object command not known.  ",
                               "Should be one of -",
                               "\n    addframe baseframe coeffs ?newdomain?",
                               "\n    bbox frame",
                               "\n    centroid x y frame zoom",
                               "\n    bounds",
                               "\n    destroy",
                               "\n    display ?-resamp? device loperc hiperc "
                                             "frame ?plotstyle?",
                               "\n    fitshead ?key? ...",
                               "\n    frameatt attname ?frame?",
                               "\n    mapped ?setmap?",
                               "\n    name",
                               "\n    percentile perc ?perc? ...",
                               "\n    pixelsize frame",
                               "\n    polygon frame",
                               "\n    validndf",
                               "\n    wcstran ?-format? from to pos ?pos ...?",
                               (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* If we've got here, we want to exit with the result in result, and
   success status. */
      Tcl_SetObjResult( interp, result );
      return TCL_OK;
   }



/**********************************************************************/
   int NdfsetCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                  Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      Ndfset *ndfset;
      static int counter = 0;
      int i;
      int nleng;
      int nndf;
      Tcl_Obj *op;
      char *ndfsetname;

/* Check command arguments.  There should be one or more extra arguments
   for the constructor, each the name of an NDF. */
      if ( objc < 3 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "setName ndfName ?ndfName ...?" );
         return TCL_ERROR;
      }

/* Get the number of NDFs in the Set. */
      nndf = objc - 2;

/* Allocate the data structure to hold the client data. */
      ndfset = malloc( sizeof( Ndfset ) );
      ndfset->plotarray = malloc( sizeof( Plotarray ) );
      ndfset->content.ndfs = malloc( nndf * sizeof( Ndf * ) );
      if ( tclmemok( interp, ndfset ) != TCL_OK ||
           tclmemok( interp, ndfset->plotarray ) != TCL_OK ||
           tclmemok( interp, ndfset->content.ndfs ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Open each named NDF and store a pointer to the resulting data structure. */
      for ( i = 0; i < nndf; i++ ) {
         if ( newNdf( interp, objv[ i + 2 ],
                      &ndfset->content.ndfs[ i ] ) != TCL_OK ) {
            return TCL_ERROR;
         }
      }

/* Set the name of the ndfset.  If the empty string is supplied, use the
   name of the first NDF. */
      ndfsetname = Tcl_GetStringFromObj( objv[ 1 ], &nleng );
      if ( nleng == 0 ) {
         ndfsetname = ndfset->content.ndfs[ 0 ]->name;
         nleng = strlen( ndfsetname );
      }
      ndfset->name = malloc( nleng + 1 );
      if ( tclmemok( interp, ndfset->name ) != TCL_OK ) {
         return TCL_ERROR;
      }
      strcpy( ndfset->name, ndfsetname );

/* Set the ndfset's WCS frameset, arbitrarily, to that of the first of
   of the NDFs. */
      ndfset->wcs = ndfset->content.ndfs[ 0 ]->wcs;

/* Do some other initialisation. */
      ndfset->nmember = nndf;
      ndfset->plotarray->exists = 0;
      ndfset->plotarray->data = NULL;

/* Create a new Tcl command corresponding to the newly created object. */
      sprintf( buffer, "ndfset%d", counter );
      Tcl_CreateObjCommand( interp, buffer, ObjectNdfsetCmd,
                            (ClientData) ndfset, DeleteNdfset );

/* Bump the object count. */
      counter++;

/* Return the name of the new command as the result of this command. */
      op = Tcl_NewStringObj( buffer, -1 );
      Tcl_SetObjResult( interp, op );

/* Return successful operation code. */
      return TCL_OK;
   }


/**********************************************************************/
   void DeleteNdfset( ClientData clientData ) {
/**********************************************************************/
      Ndfset *ndfset = (Ndfset *) clientData;
      int i;

      free( ndfset->name );
      free( ndfset->plotarray->data );
      free( ndfset->plotarray );
      for ( i = 0; i < ndfset->nmember; i++ ) {
         forgetNdf( ndfset->content.ndfs[ i ] );
      }
      free( ndfset->content.ndfs );
      free( ndfset );
   }


/**********************************************************************/
   int ObjectNdfsetCmd( ClientData clientData, Tcl_Interp *interp, int objc,
                        Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      NdfOrNdfset *ndfset = (NdfOrNdfset *) clientData;
      Ndf **ndfs;
      AstFrameSet *wcs = ndfset->wcs;
      Tcl_Obj *result = NULL;
      char *command = Tcl_GetString( objv[ 1 ] );
      int nndf;

/* Take care of pending Tcl events now, in case what we are about to do
   is time-consuming. */
      tclupdate();

/* Get a list of ndf objects we can work with. */
      if ( ndfset->nmember ) {
         nndf = ndfset->nmember;
         ndfs = ndfset->content.ndfs;
      }
      else {
         nndf = 1;
         ndfs = &ndfset;
      }


/**********************************************************************/
/* "addframe" command                                                 */
/**********************************************************************/
      if ( ! strcmp( command, "addframe" ) ) {
         int i;
         int ibfrm;
         int ncoeff;
         int nfrm;
         double coeffs[ 6 ];
         const char *cbadstr;
         char *dname;
         AstFrame *newframe;
         AstMapping *map;
         Tcl_Obj **ov;

/* Check syntax. */
         if ( objc < 4 || objc > 5 ) {
            Tcl_WrongNumArgs( interp, 2, objv,
                              "baseframe coeffs ?newdomain?" );
            return TCL_ERROR;
         }

/* Interpret the frame argument. */
         if ( NdfGetIframeFromObj( interp, objv[ 2 ], wcs, &ibfrm )
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get linear mapping coefficients. */
         cbadstr = "coeffs should be a list of six numbers";
         if ( Tcl_ListObjGetElements( interp, objv[ 3 ], &ncoeff, &ov )
              != TCL_OK || ncoeff != 6 ) {
            Tcl_SetObjResult( interp, Tcl_NewStringObj( cbadstr, -1 ) );
            return TCL_ERROR;
         }
         for ( i = 0; i < 6; i++ ) {
            if ( Tcl_GetDoubleFromObj( interp, ov[ i ], &coeffs[ i ] )
                 != TCL_OK ) {
               Tcl_SetObjResult( interp, Tcl_NewStringObj( cbadstr, -1 ) );
               return TCL_ERROR;
            }
         }

/* Get the new domain name if one exists. */
         if ( objc == 5 ) {
            dname = Tcl_GetString( objv[ 4 ] );
         }
         else {
            dname = " ";
         }

/* Turn the coefficients into a linear mapping. */
         STARCALL(
            F77_CALL(ccd1_lnmap)( DOUBLE_ARRAY_ARG(coeffs), INTEGER_ARG(&map),
                                  INTEGER_ARG(status) );
         )
         ASTCALL(

/* Construct a new frame; make it a (deep) copy of the selected base frame. */
            newframe = astCopy( astGetFrame( wcs, ibfrm ) );

/* Set its domain. */
            astSetC( newframe, "Domain", dname );

/* Graft it onto the frameset */
            astAddFrame( wcs, ibfrm, map, newframe );

/* Get the index of the new frame (equal to the new total number of frames). */
            nfrm = astGetI( wcs, "Nframe" );
         )

/* Set the result to the index of the newly created frame. */
         result = Tcl_NewIntObj( nfrm );
      }

/**********************************************************************/
/* "bbox" command                                                     */
/**********************************************************************/
      else if ( ! strcmp( command, "bbox" ) ) {
         Tcl_Obj *ov[ 2 ];
         double lbox[ NDF__MXDIM ];
         double ubox[ NDF__MXDIM ];
         int i;
         int *iframes;
         int ndim;

/* Check syntax. */
         if ( objc != 3 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "frame" );
            return TCL_ERROR;
         }

/* Allocate some memory. */
         iframes = malloc( nndf * sizeof( int ) );
         if ( tclmemok( interp, iframes ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Interpret the frame argument. */
         if ( NdfGetIframesFromObj( interp, objv[ 2 ], ndfset, iframes )
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get the bounding box. */
         STARCALL(
            getbbox( ndfset, iframes, lbox, ubox, status );
         )

/* Release memory. */
         free( iframes );

/* Get the number of dimensions. */
         ndim = 0;
         for ( i = 0; i < nndf; i++ ) {
            ndim = max( ndim, ndfs[ i ]->content.ndf1->ndim );
         }

/* Set the result object. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
         for ( i = 0; i < ndim; i++ ) {
            ov[ 0 ] = Tcl_NewDoubleObj( lbox[ i ] );
            ov[ 1 ] = Tcl_NewDoubleObj( ubox[ i ] );
            Tcl_ListObjAppendElement( interp, result, Tcl_NewListObj( 2, ov ) );
         }
      }


/**********************************************************************/
/* "centroid" command                                                 */
/**********************************************************************/
      else if ( ! strcmp( command, "centroid" ) ) {
         int badtype;
         int done;
         int i;
         int *iframes;
         int maxit;
         int ssize;
         int xdim;
         int ydim;
         double cscale;
         double gxacc;
         double gxc;
         double gyacc;
         double gyc;
         double maxshift;
         double toler;
         double xacc;
         double xc;
         double yacc;
         double yc;
         double zoom;
         Ndf1 *ndf1;
         AstMapping *map;
         const F77_LOGICAL_TYPE true = F77_TRUE;

/* Check syntax. */
         if ( objc != 6 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "x y frame zoom" );
            return TCL_ERROR;
         }

/* Allocate some memory. */
         iframes = malloc( nndf * sizeof( int ) );
         if ( tclmemok( interp, iframes ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Interpret the frame argument. */
         if ( NdfGetIframesFromObj( interp, objv[ 4 ], ndfset, iframes )
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get the numerical arguments. */
         if ( Tcl_GetDoubleFromObj( interp, objv[ 2 ], &xc ) != TCL_OK ||
              Tcl_GetDoubleFromObj( interp, objv[ 3 ], &yc ) != TCL_OK ||
              Tcl_GetDoubleFromObj( interp, objv[ 5 ], &zoom) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Set up some parameters for the centroid location. */
         cscale = max( 1.0, 1.0 / zoom );
         ssize = (int) rint( cscale * 9.0 );
         maxshift = cscale * 5.5;
         toler = min( 0.5, cscale * 0.05 );
         maxit = 5;

/* Go through the NDFs in the ndfset looking for one which contains the
   point.  No account is taken of the possibility that the point falls
   within the bounds of more than 1 NDF, but this isn't a very likely
   eventuality for normal data. */
         done = 0;
         for ( i = 0; i < nndf; i++ ) {
            ndf1 = ndfs[ i ]->content.ndf1;

/* Get the dimensions of the NDF. */
            xdim = ndf1->ubnd[ 0 ] - ndf1->lbnd[ 0 ] + 1;
            ydim = ndf1->ubnd[ 1 ] - ndf1->lbnd[ 1 ] + 1;

/* Map the points from its coordinates in the selected frame to GRID
   coordinates. */
            ASTCALL(
               map = astGetMapping( ndfs[ i ]->wcs, iframes[ i ], AST__BASE );
               astTran2( map, 1, &xc, &yc, 1, &gxc, &gyc );
            )

/* See if the point is within this NDF. */
            if ( !done && gxc >= 0.5 && gxc <= 0.5 + (double) xdim
                       && gyc >= 0.5 && gyc <= 0.5 + (double) ydim ) {

/* Ensure that the NDF is mapped. */
               if ( ! ndf1->mapped ) {
                  STARCALL(
                     domapdata( ndf1, status );
                  )
               }

/* Define a macro to call a type-dependent function. */
#define CASE_CENT(Xccdtype,Xcnftype,Xgentype) \
                  case Xccdtype: \
                     F77_CALL(ccg1_cen##Xgentype)( \
                               DOUBLE_ARG(&gxc), DOUBLE_ARG(&gyc), \
                               Xcnftype##_ARG(ndf1->data), \
                               INTEGER_ARG(&xdim), INTEGER_ARG(&ydim), \
                               INTEGER_ARG(&ssize), LOGICAL_ARG(&true), \
                               DOUBLE_ARG(&maxshift), \
                               INTEGER_ARG(&maxit), DOUBLE_ARG(&toler), \
                               DOUBLE_ARG(&gxacc), DOUBLE_ARG(&gyacc), \
                               INTEGER_ARG(status) ); \
                     break;

/* Use the macro we have defined to invoke one of the typed centroiding
   subroutines. */
               STARCALL(
                  errMark();
                  badtype = 0;
                  switch( CCD_TYPE( ndf1->mtype ) ) {
                     CASE_CENT(CCD_TYPE_B,BYTE,b)
                     CASE_CENT(CCD_TYPE_UB,UBYTE,ub)
                     CASE_CENT(CCD_TYPE_W,WORD,w)
                     CASE_CENT(CCD_TYPE_UW,UWORD,uw)
                     CASE_CENT(CCD_TYPE_I,INTEGER,i)
                     CASE_CENT(CCD_TYPE_R,REAL,r)
                     CASE_CENT(CCD_TYPE_D,DOUBLE,d)
                     default:
                        badtype = 1;
                  }
                  if ( ! badtype ) {
                     if ( *status == SAI__OK ) {
                        done = 1;
                     }
                     else {
                        errAnnul( status );
                     }
                  }
                  errRlse();
                  if ( badtype ) {
                     *status = SAI__ERROR;
                     errRep( "OFFSET_BADTYP", "Bad type value", status );
                  }
               )

/* Undefine the macro. */
#undef CASE_CENT

/* The centroid has been found.  Transform the coordinates back to those
   of the selected frame. */
               if ( done ) {
                  ASTCALL(
                     astTran2( map, 1, &gxacc, &gyacc, 0, &xacc, &yacc );
                  )
               }

/* Clear up AST objects. */
               ASTCALL(
                  map = astAnnul( map );
               )
            }
         }

/* Free allocated memory. */
         free( iframes );

/* Construct the result. */
         if ( done ) {
            result = Tcl_NewListObj( 0, NULL );
            Tcl_ListObjAppendElement( interp, result,
                                      Tcl_NewDoubleObj( xacc ) );
            Tcl_ListObjAppendElement( interp, result,
                                      Tcl_NewDoubleObj( yacc ) );
         }
         else {
            result = Tcl_NewStringObj( "Centroiding failed", -1 );
            Tcl_SetObjResult( interp, result );
            return TCL_ERROR;
         }
      }

/**********************************************************************/
/* "destroy" command                                                  */
/**********************************************************************/
      else if ( ! strcmp( command, "destroy" ) ) {

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Initiate destruction.  This call will delete the command from the
   interpreter and invoke our object deletion callback too.  */
         Tcl_DeleteCommand( interp, Tcl_GetString( objv[ 0 ] ) );

/* Set result. */
         result = Tcl_NewStringObj( "", 0 );
      }


/**********************************************************************/
/* "display" command                                                  */
/**********************************************************************/
      else if ( ! strcmp( command, "display" ) ) {
         int const badcolour = 0;
         int i;
         int locolour;
         int hicolour;
         int nflag;
         int resamp;
         int xpix;
         int ypix;
         int *iframes;
         int *pframes;
         int *pixbloc;
         char *device;
         char *settings;
         float aspect;
         float factor;
         float gbox[ 4 ];
         float vp[ 4 ];
         float xplo;
         float xphi;
         float yplo;
         float yphi;
         double bbox[ 4 ];
         double loperc;
         double hiperc;
         double psize;
         double zoom;
         struct ndfdisplay_args args;
         Tcl_Obj argob;
         Tcl_Obj *ov[ 1 ];

/* Process flags. */
         resamp = 0;
         nflag = 0;
         if ( objc > 2 + nflag &&
              ! strcmp( Tcl_GetString( objv[ 2 + nflag ] ), "-resamp" ) ) {
            resamp = 1;
            nflag++;
         }

/* Check syntax. */
         if ( objc < 6 + nflag || objc > 7 + nflag ) {
            Tcl_WrongNumArgs( interp, 2, objv, "?-resamp? "
                              "device loperc hiperc frame ?plotstyle?" );
                            /* 2      3      4      5     6           */
            return TCL_ERROR;
         }

/* Get string arguments. */
         device = Tcl_GetString( objv[ 2 + nflag ] );
         settings = ( objc == 7 + nflag ) ? Tcl_GetString( objv[ 6 + nflag ] )
                                          : "";

/* Get numeric arguments. */
         if ( Tcl_GetDoubleFromObj( interp,
                                    objv[ 3 + nflag ], &loperc ) != TCL_OK ||
              Tcl_GetDoubleFromObj( interp,
                                    objv[ 4 + nflag ], &hiperc ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Allocate some space. */
         iframes = malloc( nndf * sizeof( int ) );
         pframes = malloc( nndf * sizeof( int ) );
         if ( tclmemok( interp, iframes ) != TCL_OK ||
              tclmemok( interp, pframes ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get frame argument. */
         if ( NdfGetIframesFromObj( interp, objv[ 5 + nflag ],
                                    ndfset, iframes ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Set the frame into which the image will be resampled.  If resamp is
   true, then it will be the frame specified in the arguments.  If
   resamp is false, then it will be AST__BASE for a single image or
   the CCD_SET frame for an Ndfset. */
         if ( resamp ) {
            for ( i = 0; i < nndf; i++ ) {
               pframes[ i ] = iframes[ i ];
            }
         }
         else if ( nndf > 1 ) {
            if ( NdfGetIframesFromObj( interp,
                                       Tcl_NewStringObj( "CCD_SET", -1 ),
                                       ndfset, pframes )
                 != TCL_OK ) {
               return TCL_ERROR;
            }
            for ( i = 0; i < nndf; i++ ) {
               if ( pframes[ i ] == AST__NOFRAME ) {
                  result = Tcl_NewStringObj( "CCD_SET alignment frame "
                           "absent in one or more Ndfset members.", -1 );
                  Tcl_SetObjResult( interp, result );
                  return TCL_ERROR;
               }
            }
         }
         else {
            pframes[ 0 ] = AST__BASE;
         }

/* Open the PGPLOT plotting device. */
         if ( cpgopen( device ) <= 0 ) {
            result = Tcl_NewStringObj( "Failed to open plotting device", -1 );
            Tcl_SetObjResult( interp, result );
            return TCL_ERROR;
         }

/* Query the plotting device for highest and lowest available colour indices. */
         cpgqcir( &locolour, &hicolour );

/* Set the plotting coordinate limits. */
         STARCALL(
            psize = getpixelsize( ndfset, pframes[ 0 ], status );
            getbbox( ndfset, pframes, bbox, bbox + 2, status );
         )
         for ( i = 0; i < 4; i++ ) gbox[ i ] = (float) bbox[ i ];

/* Set the viewport to use all of the available surface, within the
   constraint that the correct aspect ratio is retained.  Doing this
   directly by using cpgwnad() can introduce small inaccuracies. */
         cpgsvp( 0.0, 1.0, 0.0, 1.0 );
         cpgqvp( 3, &xplo, &xphi, &yplo, &yphi );
         vp[ 0 ] = 0.0;
         vp[ 1 ] = 1.0;
         vp[ 2 ] = 0.0;
         vp[ 3 ] = 1.0;
         aspect = ( xphi - xplo ) / ( yphi - yplo )
                * ( gbox[ 3 ] - gbox[ 1 ] ) / ( gbox[ 2 ] - gbox[ 0 ] );
         if ( aspect > 1.0 ) {
            aspect = 1.0 / aspect;
            vp[ 0 ] = 0.5 * ( 1.0 - aspect );
            vp[ 1 ] = 0.5 * ( 1.0 + aspect );
         }
         else if ( aspect < 1.0 ) {
            vp[ 2 ] = 0.5 * ( 1.0 - aspect );
            vp[ 3 ] = 0.5 * ( 1.0 + aspect );
         }
         cpgsvp( vp[ 0 ], vp[ 1 ], vp[ 2 ], vp[ 3 ] );
         cpgswin( gbox[ 0 ], gbox[ 2 ], gbox[ 1 ], gbox[ 3 ] );

/* Get the viewport size in pixels. */
         cpgqvp( 3, &xplo, &xphi, &yplo, &yphi );

/* Close PGPLOT down for now. */
         cpgclos();

/* Update pending Tcl events. */
         tclupdate();

/* Set the zoom factor so that PGPLOT can optimise the plotting.  Basically
   this entails making the pixel array the same shape as the plotting
   surface.  However, making it a lot bigger than the plotting surface
   would be a waste of memory. */
         factor = 1.0;
         zoom = 10.0 / psize;
         while ( zoom > 2.1 / psize ) {
            zoom = ( xphi - xplo ) / ( bbox[ 2 ] - bbox[ 0 ] ) / factor++;
         }

/* Prepare the image in a PGPLOT-friendly form. */
         STARCALL(
            pixbloc = getpixbloc( ndfset, pframes, zoom, loperc, hiperc,
                                  locolour, hicolour, badcolour, status );
         )
         xpix = ndfset->plotarray->xdim;
         ypix = ndfset->plotarray->ydim;

/* The rest of the processing can be performed as an independent background
   process, since it may take a significant amount of time and we want
   Tcl events to continue to be serviced. */

/* Set up the arguments block for the rest of the processing. */
         argob.internalRep.otherValuePtr = &args;
         ov[ 0 ] = &argob;
         args.device = device;
         args.settings = settings;
         args.pixbloc = pixbloc;
         args.gbox = gbox;
         args.bbox = bbox;
         args.vp = vp;
         args.psize = psize;
         args.zoom = ndfset->plotarray->zoom;
         args.wcs = wcs;
         args.iframes = iframes;
         args.pframes = pframes;
         args.xpix = xpix;
         args.ypix = ypix;

/* Call the routine to do the plotting. */
/* I was experimenting with doing this in a subprocess so that the Tcl
   event loop was attended to during processing.  This is why ndfdisplay
   is implemented as a separate routine here.  It can be invoked in
   this way using the call:
      if ( tclbgcmd( (ClientData) ndfdisplay, interp, 1, ov ) != TCL_OK )
   However, this doesn't seem to solve enough problems to be worthwhile,
   so it is currently written to execute in the main process. */
         if ( ((Tcl_ObjCmdProc *) ndfdisplay)( (ClientData) NULL,
                                               interp, 1, ov ) != TCL_OK ) {
            printf( "It's an error" );
            return TCL_ERROR;
         }

/* Free some resources. */
         free( iframes );
         free( pframes );

/* Get the result and return. */
         result = Tcl_GetObjResult( interp );
      }


/**********************************************************************/
/* "frameatt" command                                                 */
/**********************************************************************/
      else if ( ! strcmp( command, "frameatt" ) ) {
         int getindex;
         int ifrm;
         char *attname;
         const char (*aval);
         AstFrame *frame;

/* Check syntax. */
         if ( objc < 3 || objc > 4 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "attname ?frame?" );
            return TCL_ERROR;
         }

/* Get string argument. */
         attname = Tcl_GetString( objv[ 2 ] );
         getindex = strcasecmp( attname, "index" ) ? 0 : 1;

/* Get frame argument. */
         ifrm = AST__NOFRAME;
         if ( objc == 4 ) {
            if ( NdfGetIframeFromObj( interp, objv[ 3 ], wcs, &ifrm )
                 != TCL_OK ) {
               return TCL_ERROR;
            }
         }

/* Begin AST context. */
         ASTCALL(
            astBegin;

/* Either get the value for a single frame and place it in the result
   object. */
            if ( ifrm != AST__NOFRAME ) {
               if ( getindex ) {
                  result = Tcl_NewIntObj( ifrm );
               }
               else {
                  frame = astGetFrame( wcs, ifrm );
                  aval = astGetC( frame, attname );
                  result = Tcl_NewStringObj( aval, -1 );
               }
            }

/* Or loop over all the frames and append the value for each one to
   the result object. */
            else {
               Tcl_Obj *ob;
               result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
               for ( ifrm = 1; ifrm <= astGetI( wcs, "Nframe" ); ifrm++ ) {
                  if ( getindex ) {
                     ob = Tcl_NewIntObj( ifrm );
                  }
                  else {
                     frame = astGetFrame( wcs, ifrm );
                     ob = Tcl_NewStringObj( astGetC( frame, attname ), -1 );
                     if ( ! astOK ) {
                        astClearStatus;
                        ob = Tcl_NewDoubleObj( astGetD( frame, attname ) );
                     }
                  }
                  Tcl_ListObjAppendElement( interp, result, ob );
               }
            }

/* End AST context. */
            astEnd;
         )
      }


/**********************************************************************/
/* "mapped" command                                                   */
/**********************************************************************/
      else if ( ! strcmp( command, "mapped" ) ) {
         Ndf1 *ndf1;
         int i;
         int requested;

/* More than three arguments - syntax error. */
         if ( objc > 3 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "?setmap?" );
            return TCL_ERROR;
         }

/* Three arguments - get a Boolean value for the requested state. */
         if ( objc == 3 ) {
            if ( Tcl_GetBooleanFromObj( interp, objv[ 2 ], &requested )
                 != TCL_OK ) {
               result = Tcl_NewStringObj( "ndf mapped: third argument \"", -1 );
               Tcl_AppendObjToObj( result, objv[ 2 ] );
               Tcl_AppendStringsToObj( result, "\" should be Boolean",
                                       (char *) NULL );
               Tcl_SetObjResult( interp, result );
               return TCL_ERROR;
            }
         }

/* Set value of result to the current (intial) value of the attribute. */
         result = Tcl_NewBooleanObj( ndfs[ 0 ]->content.ndf1->mapped );
         Tcl_SetObjResult( interp, result );

/* Now we check for each NDF whether a state different from the current
   state has been requested.  If not, the following code will just fall
   through to the exit. */
         STARCALL(
            for ( i = 0; i < nndf; i++ ) {
               ndf1 = ndfs[ i ]->content.ndf1;

/* A mapped state has been requested, and we are not already mapped. */
               if ( objc == 3 && requested && ! ndf1->mapped ) {
                  domapdata( ndf1, status );
               }

/* An unmapped state has been requested, and we are currently mapped. */
               else if ( objc == 3 && ! requested && ndf1->mapped ) {
                  dounmapdata( ndf1, status );
               }
            }
         )
      }


/**********************************************************************/
/* "name" command                                                     */
/**********************************************************************/
      else if ( ! strcmp( command, "name" ) ) {

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Set result and return successfully. */
         result = Tcl_NewStringObj( ndfset->name, -1 );
      }


/**********************************************************************/
/* "ndfdo" command                                                    */
/**********************************************************************/
      else if ( ! strcmp( command, "ndfdo" ) ) {
         int doall;
         int ndfobjc;
         int imember;
         Tcl_Obj **ndfobjv;

/* Check syntax. */
         ndfobjc = objc - 2;
         if ( ndfobjc < 1 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "index|all command ?args...?" );
            return TCL_ERROR;
         }
         ndfobjv = (Tcl_Obj **CONST) objv + ( objc - ndfobjc );

/* Obtain the member index or the value "all" indicating which member(s)
   the command should be performed on. */
         doall = 0;
         if ( Tcl_GetIntFromObj( interp, objv[ 2 ], &imember ) != TCL_OK ) {
            if ( ! strcmp( Tcl_GetString( objv[ 2 ] ), "all" ) ) {
               Tcl_SetObjResult( interp, Tcl_NewStringObj( "", -1 ) );
               doall = 1;
            }
            else {
               return TCL_ERROR;
            }
         }

/* Either loop over all member ndfs, handing off each request to
   ObjectNdfCmd and appending to a list object. */
         if ( doall ) {
            result = Tcl_NewListObj( 0, NULL );
            for ( imember = 0; imember < nndf; imember++ ) {
               if ( ObjectNdfCmd( (ClientData) ndfset->content.ndfs[ imember ],
                                  interp, ndfobjc, ndfobjv ) != TCL_OK ||
                    Tcl_ListObjAppendElement( interp, result,
                                              Tcl_GetObjResult( interp ) )
                                                             != TCL_OK ) {
                       return TCL_ERROR;
               }
            }
         }

/* Or check that we have been asked about an extant member and hand a
   single request to ObjectNdfCmd. */
         else {
            if ( ( imember >= ndfset->nmember || imember < 0 ) ) {
               sprintf( buffer, "ndfset ndfob: index %d out of range 0..%d",
                        imember, ndfset->nmember - 1 );
               result = Tcl_NewStringObj( buffer, -1 );
               Tcl_SetObjResult( interp, result );
               return TCL_ERROR;
            }
            else {
               if ( ObjectNdfCmd( (ClientData) ndfset->content.ndfs[ imember ],
                                  interp, ndfobjc, ndfobjv ) != TCL_OK ) {
                  return TCL_ERROR;
               }
               result = Tcl_GetObjResult( interp );
            }
         }
      }


/**********************************************************************/
/* "nndf" command                                                     */
/**********************************************************************/
      else if ( ! strcmp( command, "nndf" ) ) {

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Return the number of members. */
         result = Tcl_NewIntObj( nndf );
      }


/**********************************************************************/
/* "pixelsize" command                                                */
/**********************************************************************/
      else if ( ! strcmp( command, "pixelsize" ) ) {
         double size;
         int iframe;

/* Check syntax. */
         if ( objc != 3 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "frame" );
            return TCL_ERROR;
         }

/* Get frame argument. */
         if ( NdfGetIframeFromObj( interp,
                                   objv[ 2 ], wcs, &iframe ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get the pixel unit size. */
         STARCALL(
            size = getpixelsize( ndfset, iframe, status );
         )

/* Set the result object. */
         result = Tcl_NewDoubleObj( size );
      }


/**********************************************************************/
/* "validndfset" command                                              */
/**********************************************************************/
      else if ( ! strcmp( command, "validndfset" ) ) {
         int i;
         int ok;
         int valid;
         Ndf1 *ndf1;

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Check that it is an ndfset not an ndf. */
         if ( ndfset->nmember <= 0 ) {
            valid = 0;
         }
         else {

/* Check that each of the member NDFs has a valid identifier.  They should
   do. */
            valid = 1;
            STARCALL(
               for ( i = 0; i < ndfset->nmember; i++ ) {
                  ndf1 = ndfset->content.ndfs[ i ]->content.ndf1;
                  ndfValid( ndf1->identifier, &ok, status );
                  valid = valid && F77_ISTRUE(ok);
               }
            )
         }

/* Set the result value. */
         result = Tcl_NewBooleanObj( valid );
      }


/**********************************************************************/
/* "wcstran" command                                                  */
/**********************************************************************/
      else if ( ! strcmp( command, "wcstran" ) ) {
         int form;
         int iframe[ 2 ];
         int i;
         int j;
         int nflag;
         int nin;
         int nout;
         int npos;
         const char (*aval);
         double *inpos;
         double *outpos;
         AstMapping *imap = NULL;
         AstFrame *outframe;

/* Process flags. */
         form = 0;
         nflag = 0;
         if ( objc > 2 + nflag &&
              ! strcmp( Tcl_GetString( objv[ 2 + nflag ] ), "-format" ) ) {
            form = 1;
            nflag++;
         }

/* Check syntax. */
         if ( objc + nflag < 4 ) {
            Tcl_WrongNumArgs( interp, 2, objv,
                              "?-format? from to pos ?pos ...?" );
            return TCL_ERROR;
         }

/* Get frame arguments. */
         for ( i = 0; i < 2; i++ ) {
            if ( NdfGetIframeFromObj( interp, objv[ nflag + 2 + i ], wcs,
                                      &iframe[ i ] ) != TCL_OK ) {
               return TCL_ERROR;
            }
         }

/* Get number of positions to transform. */
         npos = objc - nflag - 4;

/* Begin AST context. */
         ASTCALL(
            astBegin;

/* Get the mapping between the two frames. */
            imap = astGetMapping( wcs, iframe[ 0 ], iframe[ 1 ] );
            outframe = astGetFrame( wcs, iframe[ 1 ] );
            nin = astGetI( imap, "Nin" );
            nout = astGetI( imap, "Nout" );
         )

/* Allocate space for coordinate transformations. */
         inpos = malloc( nin * npos * sizeof( double ) );
         outpos = malloc( nout * npos * sizeof( double ) );
         if ( tclmemok( interp, inpos ) != TCL_OK ||
              tclmemok( interp, outpos ) != TCL_OK ) {
            astEnd;
            return TCL_ERROR;
         }

/* Get coordinate lists. */
         for ( i = 0; i < npos; i++ ) {
            Tcl_Obj **ov;
            int oc;
            Tcl_ListObjGetElements( interp, objv[ 4 + nflag + i ], &oc, &ov );
            if ( oc != nin ) {
               result = Tcl_NewStringObj( "Wrong number of coordinates", -1 );
               Tcl_SetObjResult( interp, result );
               free( inpos );
               free( outpos );
               astEnd;
               return TCL_ERROR;
            }
            for ( j = 0; j < nin; j++ ) {
               if ( Tcl_GetDoubleFromObj( interp, ov[ j ],
                                          inpos + j * npos + i ) != TCL_OK ) {
                  free( inpos );
                  free( outpos );
                  astEnd;
                  return TCL_ERROR;
               }
            }
         }

/* Do the mapping. */
         ASTCALL(
            astTranN( imap, npos, nin, npos, (const double *) inpos,
                      1, nout, npos, outpos );
         )

/* Turn the translated coordinates into Tcl Objects for return. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
         for ( i = 0; i < npos; i++ ) {
            Tcl_Obj **ov;
            ov = malloc( nout * sizeof( Tcl_Obj ) );
            if ( tclmemok( interp, ov ) != TCL_OK ) {
               free( inpos );
               free( outpos );
               astEnd;
               return TCL_ERROR;
            }
            for ( j = 0; j < nout; j++ ) {
               if ( form ) {
                  ASTCALL(
                     aval = astFormat( outframe, j + 1,
                                       outpos[ j * npos + i ] );
                  )
                  ov[ j ] = Tcl_NewStringObj( aval, -1 );
               } else {
                  ov[ j ] = Tcl_NewDoubleObj( outpos[ j * npos + i ] );
               }
            }
            Tcl_ListObjAppendElement( interp, result,
                                      Tcl_NewListObj( nout, ov ) );
            free( ov );
         }

/* Tidy up. */
         free( inpos );
         free( outpos );
         ASTCALL(
            astEnd;
         )
      }

/**********************************************************************/
/* No known command.                                                  */
/**********************************************************************/
      else {

/* Exit with a helpful message and error status. */
         result = Tcl_NewStringObj( "", 0 );
         Tcl_AppendObjToObj( result, objv[ 0 ] );
         Tcl_AppendStringsToObj( result, " ", (char *) NULL );
         Tcl_AppendObjToObj( result, objv[ 1 ] );
         Tcl_AppendStringsToObj( result, ": ndfset object command not known.  ",
                               "Should be one of -",
                               "\n    addframe baseframe coeffs ?newdomain?",
                               "\n    bbox frame",
                               "\n    centroid x y frame zoom",
                               "\n    destroy",
                               "\n    display ?-resamp? device loperc hiperc "
                                             "frame ?plotstyle?",
                               "\n    frameatt attname ?frame?",
                               "\n    mapped ?setmap?",
                               "\n    name",
                               "\n    ndfdo index|all command ?args ...?",
                               "\n    nndf",
                               "\n    pixelsize frame",
                               "\n    validndfset",
                               "\n    wcstran ?-format? from to pos ?pos ...?",
                               (char *) NULL );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* If we've got here, we want to exit with the result in result, and
   success status. */
      Tcl_SetObjResult( interp, result );
      return TCL_OK;
   }


/**********************************************************************/
   int Ndf_Init( Tcl_Interp *interp ) {
/**********************************************************************/
      PercHashKey hkey;
      PercHashValue hval;

/* Check that the way we have used hashes to encode percentile information
   will work with word sizes here.  If not, it will be necessary to
   do recoding of the percentile hash business using more pointers.
   That's pretty unlikely on any machine with sensible word sizes though. */
      if ( sizeof( hkey.data ) > sizeof( hkey.hash ) ||
           sizeof( hval.data ) > sizeof( hval.hash ) ) {
         Tcl_Obj *result;
         result = Tcl_NewStringObj(
                     "Uh oh - assumptions about type sizes are wrong", -1 );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* Create the "ndf" object creation command. */
      Tcl_CreateObjCommand( interp, "ndf", NdfCmd,
                            (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

/* Create the "ndfset" object creation command. */
      Tcl_CreateObjCommand( interp, "ndfset", NdfsetCmd,
                            (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL );

/* The module has initialised successfully. */
      return TCL_OK;
   }



/**********************************************************************/
/* Auxiliary functions.                                               */
/**********************************************************************/

/**********************************************************************/
   int newNdf( Tcl_Interp *interp, Tcl_Obj *ndfnameobj, Ndf **pndf ) {
/**********************************************************************/
      char *ndfname;
      int i;
      int nleng;
      Ndf *ndf;
      Ndf1 *ndf1;

/* Get the name of the NDF. */
      ndfname = Tcl_GetStringFromObj( ndfnameobj, &nleng );

/* Allocate the data structure to hold the client data. */
      ndf = (Ndf *) malloc( sizeof( Ndf ) );
      ndf->name = (char *) malloc( nleng + 1 );
      ndf->plotarray = (Plotarray *) malloc( sizeof( Plotarray ) );
      ndf->content.ndf1 = (Ndf1 *) malloc( sizeof( Ndf1 ) );
      if ( tclmemok( interp, ndf ) != TCL_OK ||
           tclmemok( interp, ndf->name ) != TCL_OK ||
           tclmemok( interp, ndf->plotarray ) != TCL_OK ||
           tclmemok( interp, ndf->content.ndf1 ) != TCL_OK ) {
         return TCL_ERROR;
      }

/* Initialise some elements of the data structure. */
      strcpy( ndf->name, ndfname );
      ndf->nmember = 0;
      ndf1 = ndf->content.ndf1;
      ndf1->mapped = 0;
      ndf1->bad = 1;
      ndf1->data = NULL;
      *ndf1->mtype = '\0';
      ndf->plotarray->exists = 0;
      ndf->plotarray->data = NULL;
      ndf1->fits.loaded = 0;
      ndf1->fits.ncard = 0;
      ndf1->fits.data = NULL;
      Tcl_InitHashTable( &ndf1->perchash, TCL_ONE_WORD_KEYS );

      STARCALL(

/* Open the NDF. */
         ndfFind( NULL, ndfname, &ndf1->identifier, status );

/* Get the bounds of the NDF. */
         ndfBound( ndf1->identifier, NDF__MXDIM, ndf1->lbnd, ndf1->ubnd,
                   &ndf1->ndim, status );

/* Get the HDS type of the DATA array. */
         ndfType( ndf1->identifier, "DATA", ndf1->ntype, DAT__SZTYP, status );

/* Get a pointer to the WCS component. */
         ndfGtwcs( ndf1->identifier, &ndf->wcs, status );
      )

/* Set the number of elements. */
      ndf1->nel = 1;
      for ( i = 0; i < ndf1->ndim; i++ ) {
         ndf1->nel *= ndf1->ubnd[ i ] - ndf1->lbnd[ i ] + 1;
      }

/* Successful return. */
      *pndf = ndf;
      return TCL_OK;
   }


/**********************************************************************/
   void forgetNdf( Ndf *ndf ) {
/**********************************************************************/
      Ndf1 *ndf1 = ndf->content.ndf1;
      int status[] = { SAI__OK };
      int valid;

/* Release the NDF resources associated with the object.  There is no
   procedure for recovering from errors in a Tcl_CmdDeleteProc (which
   is who is likely to be calling this routine), so if there is a
   problem we just have to annul the error status. */
      errMark();
      if ( ndf1->fits.loaded && ndf1->fits.ncard ) {
	datAnnul( &ndf1->fits.loc, status );
      }
      ndf->wcs = astAnnul( ndf->wcs );
      ndfValid( ndf1->identifier, &valid, status );
      if ( valid ) ndfAnnul( &ndf1->identifier, status );
      if ( *status != SAI__OK ) errAnnul( status );
      errRlse();

/* Release memory we (may) have allocated for the object.  Some or all of
   these could be NULL, but if so, free() will not complain. */
      Tcl_DeleteHashTable( &ndf1->perchash );
      free( ndf->name );
      free( ndf->plotarray->data );
      free( ndf->plotarray );
      free( ndf->content.ndf1 );
      free( ndf );
   }


/**********************************************************************/
   int tclmemok( Tcl_Interp *interp, void *ptr ) {
/**********************************************************************/
/* This routine is intended to check the result of a memory allocation call.
   If the given memory value is NULL, it puts a suitable message into
   the result of the interpreter, and returns TCL_ERROR.  Otherwise, it
   just returns TCL_OK. */
      if ( ptr == NULL ) {
         Tcl_SetObjResult( interp,
                           Tcl_NewStringObj( "memory allocation failed", -1 ) );
         return TCL_ERROR;
      }
      else {
         return TCL_OK;
      }
   }


/**********************************************************************/
   int dcompare( const void *a, const void *b ) {
/**********************************************************************/
/* Comparison function for qsort. */
      double diff = *( (double *) a ) - *( (double *) b );
      return diff == 0.0 ? 0 : ( diff < 0.0 ? -1 : 1 );
   }


/**********************************************************************/
   void domapdata( Ndf1 *ndf1, int *status ) {
/**********************************************************************/
/* Perform mapping of the DATA component of an ndf.  Use the data's native
   type for efficiency (another possiblity would be to use a smaller type,
   say _REAL if native type were _DOUBLE, for reduced memory consumption). */
      int nel;
      if ( *status != SAI__OK ) return;
      ndfMap( ndf1->identifier, "DATA", ndf1->ntype, "READ", &ndf1->data, &nel,
              status );
      if ( *status == SAI__OK ) {
         strncpy( ndf1->mtype, ndf1->ntype, DAT__SZTYP );
         ndf1->mapped = 1;
      }
   }


/**********************************************************************/
   void dounmapdata( Ndf1 *ndf1, int *status ) {
/**********************************************************************/
/* Perform unmapping of the DATA component of an NDF. */
      if ( *status != SAI__OK ) return;
      ndfUnmap( ndf1->identifier, "DATA", status );
      if ( *status == SAI__OK ) {
         ndf1->data = NULL;
         *ndf1->mtype = '\0';
         ndf1->mapped = 0;
      }
   }


/**********************************************************************/
   double getpixelsize( NdfOrNdfset *ndf, int iframe, int *status ) {
/**********************************************************************/
/* Return the approximate linear size of a unit pixel in the given frame,
   in terms of Base frame (GRID) pixels.  This only properly makes sense
   if the mapping between the two frames contains just zooms, rotations
   and translations, but for most non-drastic mappings it is a definite
   enough result to be useful for certain purposes.  It only looks at
   the first dimension, and calculates the size of a pixel in the middle
   of the first member NDF (for an ndfset object) or of the only NDF
   (for an ndf object).
*/
      AstMapping *map;
      AstFrame *bfrm;
      AstFrame *ffrm;
      AstFrameSet *wcs;
      Ndf1 *ndf1;
      int i;
      int *old_status;
      double bpoint[ NDF__MXDIM ][ 2 ];
      double fpoint[ NDF__MXDIM ][ 2 ];
      double ratio = 1.0;

/* Get a pointer to an Ndf1 data structure; either the one associated
   with this ndf object, or the first one associated with an ndfset
   object. */
      ndf1 = ndf->nmember ? ndf->content.ndfs[ 0 ]->content.ndf1
                          : ndf->content.ndf1;
      wcs = ndf->wcs;

/* Arrange for AST to use the inherited status variable for status checking. */
      old_status = astWatch( status );
      astBegin;

/* Construct a point at opposite corners of the Base frame. */
      for ( i = 0; i < ndf1->ndim; i++ ) {
         bpoint[ i ][ 0 ] = 0.5;
         bpoint[ i ][ 1 ] = ndf1->ubnd[ i ] - ndf1->lbnd[ i ] + 1.5;
      }

/* Construct a point at opposite corners of the selected frame. */
      bfrm = astGetFrame( wcs, AST__BASE );
      ffrm = astGetFrame( wcs, iframe );
      map = astGetMapping( wcs, AST__BASE, iframe );
      astTranN( map, 2, astGetI( map, "Nin" ), 2, (const double *) bpoint,
                1, astGetI( map, "Nout" ), 2, (double *) fpoint );

/* Work out the ratio of distances. */
      if ( *status == SAI__OK ) {
         double bp0[ NDF__MXDIM ];
         double bp1[ NDF__MXDIM ];
         double fp0[ NDF__MXDIM ];
         double fp1[ NDF__MXDIM ];
         for ( i = 0; i < astGetI( map, "Nin" ); i++ ) {
            bp0[ i ] = bpoint[ i ][ 0 ];
            bp1[ i ] = bpoint[ i ][ 1 ];
         }
         for ( i = 0; i < astGetI(map, "Nout" ); i++ ) {
            fp0[ i ] = fpoint[ i ][ 0 ];
            fp1[ i ] = fpoint[ i ][ 1 ];
         }
         ratio = fabs( astDistance( ffrm, fp0, fp1 ) /
                       astDistance( bfrm, bp0, bp1 ) );
      }

/* End the AST context. */
      astEnd;
      astWatch( old_status );

/* Return the result. */
      return ratio;
   }


/**********************************************************************/
   void getbbox( NdfOrNdfset *ndfset, int iframes[], double *lbox,
                 double *ubox, int *status ) {
/**********************************************************************/
/* Get the bounding box for an NDF resampled from the Base (GRID) frame
   into another, given, frame.
*/
      int i;
      int j;
      double lbnd[ NDF__MXDIM ];
      double lower;
      double ubnd[ NDF__MXDIM ];
      double upper;
      AstMapping *map;
      int *old_status;
      int nndf;
      Ndf **ndfs;
      Ndf1 *ndf1;

/* Arrange for AST to use the inherited status variable for status checking. */
      astBegin;
      old_status = astWatch( status );

/* Get a list of ndf objects to work with. */
      if ( ndfset->nmember ) {
         nndf = ndfset->nmember;
         ndfs = ndfset->content.ndfs;
      }
      else {
         nndf = 1;
         ndfs = &ndfset;
      }

/* Loop over the ndf objects. */
      for ( j = 0; j < nndf; j++ ) {
         ndf1 = ndfs[ j ]->content.ndf1;

/* Get the bounding values in the GRID (Base) frame. */
         for ( i = 0; i < ndf1->ndim; i++ ) {
            lbnd[ i ] = 0.5;
            ubnd[ i ] = ndf1->ubnd[ i ] - ndf1->lbnd[ i ] + 1.5;
         }

/* Convert the GRID bounding values to the selected frame. */
         map = astGetMapping( ndfs[ j ]->wcs, AST__BASE, iframes[ j ] );
         for ( i = 1; i <= ndf1->ndim; i++ ) {
            astMapBox( map, (const double *) lbnd, (const double *) ubnd,
                       1, i, &lower, &upper,
                       (double *) NULL, (double *) NULL );

/* Update the current upper and lower bounds. */
            if ( j == 0 || lower < lbox[ i - 1 ] ) lbox[ i - 1 ] = lower;
            if ( j == 0 || upper > ubox[ i - 1 ] ) ubox[ i - 1 ] = upper;
         }
      }

/* End AST context. */
      astWatch( old_status );
      astEnd;

      return;
   }




/**********************************************************************/
   int ndfdisplay( ClientData clientData, Tcl_Interp *interp, int objc,
                   Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      int bfrm;
      int cfrm;
      Tcl_Obj *result;
      AstPlot *plot;

/* Set values from the structure passed in the argument list. */
      struct ndfdisplay_args *pargs = objv[ 0 ]->internalRep.otherValuePtr;
      char *device = pargs->device;
      char *settings = pargs->settings;
      int *pixbloc = pargs->pixbloc;
      float *gbox = pargs->gbox;
      float *vp = pargs->vp;
      double *bbox = pargs->bbox;
      double zoom = pargs->zoom;
      AstFrameSet *wcs = pargs->wcs;
      int *iframes = pargs->iframes;
      int *pframes = pargs->pframes;
      int xpix = pargs->xpix;
      int ypix = pargs->ypix;

/* Open PGPLOT.  Use the same invocations as in the driver routine. */
      if ( cpgopen( device ) <= 0 ) {
         Tcl_SetObjResult( interp,
             Tcl_NewStringObj( "Failed to open plotting device", -1 ) );
         return TCL_ERROR;
      }
      cpgsvp( vp[ 0 ], vp[ 1 ], vp[ 2 ], vp[ 3 ] );
      cpgswin( gbox[ 0 ], gbox[ 2 ], gbox[ 1 ], gbox[ 3 ] );

/* Begin PGPLOT buffering. */
      cpgbbuf();

/* Plot the image. */
/* Although it would be more straightforward to use the four elements of
   gbox to set the plotting limits here, by doing it using the pixel
   lengths we can ensure that there are just the same number of pixels in
   the array as on the plotting surface, which makes cpgpixl() much
   more efficient. */
      cpgpixl( pixbloc, xpix, ypix, 1, xpix, 1, ypix,
               gbox[ 0 ] + 0.5 / zoom,
               gbox[ 0 ] + ( xpix - 0.5 ) / zoom,
               gbox[ 1 ] + 0.5 / zoom,
               gbox[ 1 ] + ( ypix - 0.5 ) / zoom );

/* Get AST to plot the axes etc. */
      ASTCALL(
         bfrm = astGetI( wcs, "Base" );
         cfrm = astGetI( wcs, "Current" );
         astSetI( wcs, "Base", pframes[ 0 ] );
         astSetI( wcs, "Current", iframes[ 0 ] );
         plot = astPlot( wcs, gbox, bbox, settings );
         astGrid( plot );
         astSetI( wcs, "Base", bfrm );
         astSetI( wcs, "Current", cfrm );
         plot = astAnnul( plot );
      )

/* End PGPLOT buffering. */
      cpgebuf();

/* Close the plotting device. */
      cpgclos();

/* No result is returned. */
      result = Tcl_NewStringObj( "", 0 );
      return TCL_OK;
   }



/**********************************************************************/
   int *getpixbloc( NdfOrNdfset *ndfset, int iframes[], double zoom,
                    double loperc, double hiperc, int locolour, int hicolour,
                    int badcolour, int *status ) {
/**********************************************************************/
/* Return the address of a PGPLOT-friendly array of integers representing
   the DATA array of an NDF.  This routine will cause a new such array
   to be written if necessary, but if a suitable one already exists it
   won't bother to reconstruct it.  The returned array is not the
   property of the caller, and the caller does not have responsiblity
   for freeing it.
   It does the resampling from the Base frame of the NDF into the frame
   given by the frame index iframe, and following that, if zoom is
   not (very nearly) unity, it adds a magnification on afterwards.
*/
      int iframe = iframes[ 0 ];

/* Check the inherited status. */
      if ( *status != SAI__OK ) return NULL;

/* See whether we need to construct a new array.  If no array currently
   exists, or if the scale values are different from the ones
   we've been asked for, then we'll have to construct it.  Otherwise
   we can just use the one we've already got. */
      if ( ! ndfset->plotarray->exists || ndfset->plotarray->loperc != loperc
                                       || ndfset->plotarray->hiperc != hiperc
                                       || ndfset->plotarray->iframe != iframe
                                       || ndfset->plotarray->zoom != zoom ) {
         int i;
         int ilbnd[ 2 ];
         int iubnd[ 2 ];
         int ischeme;
         int ival;
         int ix;
         int iy;
         int maxpix;
         int nndf;
         int olbnd[ 2 ];
         int oubnd[ 2 ];
         int xb;
         int xbase;
         int xd;
         int xdim;
         int xndf;
         int yb;
         int ybase;
         int yd;
         int ydim;
         int yndf;
         int *data;
         int *old_status;
         double dval;
         double factor;
         double iparams[ 10 ];
         double lbox[ NDF__MXDIM ];
         double percs[ 2 ];
         double psize;
         double scale;
         double tol;
         double ubox[ NDF__MXDIM ];
         double vals[ 2 ];
         void *work;
         AstMapping *map;
         Ndf **ndfs;
         Ndf *ndf;
         Ndf1 *ndf1;

/* Initialise some things. */
         percs[ 0 ] = loperc;
         percs[ 1 ] = hiperc;

/* Get the size of the output array. */
         psize = getpixelsize( ndfset, iframes[ 0 ], status );
         getbbox( ndfset, iframes, lbox, ubox, status );
         xbase = zoom * lbox[ 0 ];
         ybase = zoom * lbox[ 1 ];
         xdim = zoom * ( ubox[ 0 ] - lbox[ 0 ] );
         ydim = zoom * ( ubox[ 1 ] - lbox[ 1 ] );

/* Set the resampling scheme.  If we are resampling onto a much coarser
   grid, then use a scheme which reblocks the data (averages over all pixels
   of a square), otherwise just use a simple linear interpolation.
   The reblocking can be a bit slow, but if the image is a bit noisy it
   smooths it out quite significantly. */
         factor = zoom * psize;
         if ( factor > 0.35 ) {
            ischeme = AST__NEAREST;
         }
         else {
            ischeme = AST__BLOCKAVE;
            iparams[ 0 ] = floor( ( ( 1.0 / factor ) - 0.5 ) / 2.0 );
         }

/* Validate the percentile values. */
         if ( loperc >= hiperc ) {
            *status = SAI__ERROR;
            errRep( "NDF_BADLIM", "Maximum must be greater than minimum",
                    status );
            return NULL;
         }

/* If a previous plotarray exists but is the wrong size, we will need to
   deallocate that memory prior to allocating more. */
         if ( ndfset->plotarray->exists &&
              ( ndfset->plotarray->xdim != xdim ||
                ndfset->plotarray->ydim != ydim ) ) {
            free( ndfset->plotarray->data );
            ndfset->plotarray->exists = 0;
         }

/* If we don't now have memory to put the pixel array in, allocate some. */
         if ( ! ndfset->plotarray->exists ) {
            ndfset->plotarray->data =
               malloc( sizeof( int ) * ( xdim + 1 ) * ( ydim + 1 ) );
            if ( ndfset->plotarray->data == NULL ) {
               *status = SAI__ERROR;
               errRep( " ", "Memory allocation failed", status );
               return NULL;
            }
            ndfset->plotarray->exists = 1;
         }
         data = ndfset->plotarray->data;

/* Initialise the pixel array with bad values. */
         for ( i = 0; i < ( xdim + 1 ) * ( ydim + 1 ); i++ ) {
            data[ i ] = badcolour;
         }

/* Get a list of ndf objects to work with. */
         if ( ndfset->nmember ) {
            nndf = ndfset->nmember;
            ndfs = ndfset->content.ndfs;
         }
         else {
            nndf = 1;
            ndfs = &ndfset;
         }

/* Begin an AST context. */
         astBegin;
         old_status = astWatch( status );

/* Now loop over each of the ndf objects, resampling it onto the output
   grid. */
         for ( i = 0; i < nndf; i++ ) {
            ndf = ndfs[ i ];
            ndf1 = ndf->content.ndf1;
            xndf = ndf1->ubnd[ 0 ] - ndf1->lbnd[ 0 ] + 1;
            yndf = ndf1->ubnd[ 1 ] - ndf1->lbnd[ 1 ] + 1;

/* Get the pixel values corresponding to the low and high percentile
   limits we have. */
            getpercentiles( ndf1, 2, percs, vals, status );

/* Work out the scaling factor. */
            scale = ( hicolour - locolour ) / ( vals[ 1 ] - vals[ 0 ] );

/* Get the dimensions of the output array for this ndf only. */
            getbbox( ndf, &iframes[ i ], lbox, ubox, status );
            xb = zoom * lbox[ 0 ];
            yb = zoom * lbox[ 1 ];
            xd = zoom * ( ubox[ 0 ] - lbox[ 0 ] );
            yd = zoom * ( ubox[ 1 ] - lbox[ 1 ] );
            ilbnd[ 0 ] = 1;
            ilbnd[ 1 ] = 1;
            iubnd[ 0 ] = xndf;
            iubnd[ 1 ] = yndf;
            olbnd[ 0 ] = xb;
            olbnd[ 1 ] = yb;
            oubnd[ 0 ] = xb + xd - 1;
            oubnd[ 1 ] = yb + yd - 1;
            maxpix = xndf + yndf;
            tol = 0.5 * min( ( (float) xd / (float) xndf ),
                             ( (float) yd / (float) yndf ) );

/* Get the transformation mapping for the resample operation. */
            map = astGetMapping( ndf->wcs, AST__BASE, iframes[ i ] );
            if ( fabs( zoom - 1.0 ) > 1. / (double) ( xndf + yndf ) ) {
               map = (AstMapping *)
                     astCmpMap( map, (AstMapping *) astZoomMap( 2, zoom, " " ),
                                1, " " );
            }
            map = astSimplify( map );

/* If the array is not currently mapped, we have to map it. */
            if ( ! ndf1->mapped ) domapdata( ndf1, status );

/* Define a macro to resample the ndf's data array into the output array
   according to data type. */
#define CASE_RESAMP(Xccdtype,Xctype,Xasttype,Xbadval) \
               case Xccdtype: \
\
/* Allocate space for the intermediate resampled data. */ \
                  work = malloc( ( xd + 1 ) * ( yd + 1 ) * sizeof( Xctype ) ); \
                  if ( work == NULL ) { \
                     *status = SAI__ERROR; \
                     errRep( "NDF_NOMEM", "Memory allocation failed", \
                             status ); \
                     return NULL; \
                  } \
\
/* Resample this NDF onto its new grid, using the same type. */ \
                  astResample##Xasttype( map, 2, ilbnd, iubnd, \
                                         (Xctype *) ndf1->data, NULL, \
                                         ischeme, NULL, iparams, AST__USEBAD, \
                                         tol, maxpix, Xbadval, 2, \
                                         olbnd, oubnd, olbnd, oubnd, \
                                         (Xctype *) work, NULL ); \
\
/* Copy the resampled image to its proper place on the output array, \
   converting it to integer, rescaling it between the selected lower \
   and upper limiting values, and combining it with whatever is there. */ \
                  { \
                     Xctype xval; \
                     for ( iy = 0; iy < yd; iy++ ) { \
                        for ( ix = 0; ix < xd; ix++ ) { \
                           xval = ( (Xctype *) work )[ ix + iy * xd ]; \
                           if ( xval != Xbadval ) { \
                              dval = (double) xval; \
                              if ( dval <= vals[ 0 ] ) { \
                                 ival = locolour; \
                              } \
                              else if ( dval >= vals[ 1 ] ) { \
                                 ival = hicolour; \
                              } \
                              else { \
                                 ival = locolour + \
                                    (int) ( 0.5 + \
                                            ( dval - vals[ 0 ] ) * scale ); \
                              } \
                              data[ ( ix + xb - xbase ) + \
                                    ( iy + yb - ybase ) * xdim ] = ival; \
                           } \
                        } \
                     } \
                  } \
\
/* Free the workspace array. */ \
                  free( work ); \
                  break;

/* Use the above macro to do the calculations using the correct type. */
            switch ( CCD_TYPE( ndf1->mtype ) ) {
               CASE_RESAMP(CCD_TYPE_B,F77_BYTE_TYPE,B,VAL__BADB)
               CASE_RESAMP(CCD_TYPE_UB,F77_UBYTE_TYPE,UB,VAL__BADUB)
               CASE_RESAMP(CCD_TYPE_W,F77_WORD_TYPE,S,VAL__BADS)
               CASE_RESAMP(CCD_TYPE_UW,F77_UWORD_TYPE,US,VAL__BADUS)
               CASE_RESAMP(CCD_TYPE_I,F77_INTEGER_TYPE,I,VAL__BADI)
               CASE_RESAMP(CCD_TYPE_R,F77_REAL_TYPE,F,VAL__BADF)
               CASE_RESAMP(CCD_TYPE_D,F77_DOUBLE_TYPE,D,VAL__BADD)
               default:
                  *status = SAI__ERROR;
                  errRep( "NDF_BADTYPE", "Bad type value for mapped data",
                          status );
            }

/* Undefine the macro. */
#undef CASE_RESAMP
         }

/* End the AST context. */
         astWatch( old_status );
         astEnd;

/* Store characteristics of the generated array. */
         ndfset->plotarray->xdim = xdim;
         ndfset->plotarray->ydim = ydim;
         ndfset->plotarray->iframe = iframe;
         ndfset->plotarray->zoom = zoom;
         ndfset->plotarray->loperc = loperc;
         ndfset->plotarray->hiperc = hiperc;
      }

/* Return the address of the prepared array. */
      return ndfset->plotarray->data;
   }


/**********************************************************************/
   void getpercentiles( Ndf1 *ndf1, int nperc, double *percs, double *values,
                        int *status ) {
/**********************************************************************/
/* Gets a list of percentile values for an Ndf1 structure.  It does
   plenty of work to cache things so that subsequent calls ought to
   get the same results quickly.  Much better to call it once with
   many percentiles than one invocation for each value.
*/
      double *afracs;
      double *apercs;
      double *avals;
      int first;
      int i;
      int nfrac;
      int nextra;
      Tcl_HashSearch hsearch;
      double initset[] = {  0.,  1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  9.,
                           10., 11., 12., 13., 14., 15., 16., 17., 18., 19.,
                           20., 21., 22., 23., 24., 25., 26., 27., 28., 29.,
                           30., 31., 32., 33., 34., 35., 36., 37., 38., 39.,
                           40., 41., 42., 43., 44., 45., 46., 47., 48., 49.,
                           50., 51., 52., 53., 54., 55., 56., 57., 58., 59.,
                           60., 61., 62., 63., 64., 65., 66., 67., 68., 69.,
                           70., 71., 72., 73., 74., 75., 76., 77., 78., 79.,
                           80., 81., 82., 83., 84., 85., 86., 87., 88., 89.,
                           90., 91., 92., 93., 94., 95., 96., 97., 98., 99.,
                           0.5, 99.5, 0.1, 99.9, 100. };

/* If we have not got any percentiles before, then initialise the table
   with a useful set.  It makes sense to do this because it is nearly as
   fast to work out many percentiles as just one.  If we can predict
   most of the ones which might be likely to be needed, we can probably
   save time on subsequent calls. */
      nextra = 0;
      first = ( Tcl_FirstHashEntry( &ndf1->perchash, &hsearch ) == NULL );
      if ( first ) {
         nextra = sizeof( initset ) / sizeof( initset[ 0 ] );
      }

/* Allocate arrays in which to store percentiles and values. */
      apercs = malloc( ( nperc + nextra ) * sizeof( double ) );
      afracs = malloc( ( nperc + nextra ) * sizeof( double ) );
      avals = malloc( ( nperc + nextra ) * sizeof( double ) );
      if ( apercs == NULL || afracs == NULL || avals == NULL ) {
         *status = SAI__ERROR;
         errRep( " ", "getpercentile: memory allocation failed", status );
         return;
      }

/* Copy all the passed arguments, and all the extra values, into the
   work array. */
      for ( i = 0; i < nperc; i++ ) {
         apercs[ i ] = percs[ i ];
      }
      for ( i = 0; i < nextra; i++ ) {
         apercs[ nperc + i ] = initset[ i ];
      }

/* Check that all the requested values are in the correct range. */
      for ( i = 0; i < nperc + nextra; i++ ) {
         if ( apercs[ i ] < 0.0 || apercs[ i ] > 100.0 ) {
            *status = SAI__ERROR;
            errRep( " ", "getpercentile: argument out of range (0..100)",
                    status );
            return;
         }
      }

/* Find out which of these percentiles we already have values for.
   If there are any missing, write these into a new array which we
   can feed to the percentile calculation routine. */
      nfrac = 0;
      for ( i = 0; i < ( nperc + nextra ); i++ ) {
         PercHashKey hkey;
         hkey.hash = NULL;
         hkey.data = apercs[ i ];
         if ( Tcl_FindHashEntry( &ndf1->perchash, hkey.hash ) == NULL ) {
            afracs[ nfrac++ ] = apercs[ i ] * 0.01;
         }
      }

/* Unless all the requested values were in the hash table, we will need
   to work some more out. */
      if ( nfrac > 0 ) {
         double *work;
         DECLARE_CHARACTER( type, DAT__SZTYP );
         F77_LOGICAL_TYPE bad;
         F77_POINTER_TYPE ipdata;

/* We have to sort the array of new percentiles to be worked out, since
   the percentile calculation routine likes them that way. */
         qsort( afracs, nfrac, sizeof( afracs[ 0 ] ), dcompare );

/* Now do the work of calcluating the percentiles. */
         work = malloc( nfrac * sizeof( double ) );
         if ( work == NULL ) {
            *status = SAI__ERROR;
            errRep( " ", "getpercentile: memory allocation failed", status );
            return;
         }

/* If the array is not currently mapped, we have to map it. */
         if ( ! ndf1->mapped ) domapdata( ndf1, status );

/* Massage some arguments for fortran. */
         bad = F77_ISTRUE(ndf1->bad);
         cnfExprt( ndf1->mtype, type, DAT__SZTYP );
         ipdata = cnfFptr( ndf1->data );

/* Call the routine which does the calculations. */
         F77_CALL(ccd1_fra)( CHARACTER_ARG(type), INTEGER_ARG(&ndf1->nel),
                             POINTER_ARG(&ipdata),
                             INTEGER_ARG(&nfrac), DOUBLE_ARG(afracs),
                             LOGICAL_ARG(&bad), DOUBLE_ARG(work),
                             DOUBLE_ARG(avals), INTEGER_ARG(status)
                             TRAIL_ARG(type) );

/* We can update the bad flag as a bonus. */
         ndf1->bad = F77_ISTRUE(bad);
         free( work );

/* Store the new percentile results in the hash table. */
         for ( i = 0; i < nfrac; i++ ) {
            PercHashKey hkey;
            PercHashValue hval;
            Tcl_HashEntry *entry;
            int new;
            hkey.hash = NULL;
            hkey.data = afracs[ i ] * 100.0;
            entry = Tcl_CreateHashEntry( &ndf1->perchash, hkey.hash, &new );
            hval.hash = (ClientData) NULL;
            hval.data = avals[ i ];
            Tcl_SetHashValue( entry, hval.hash );
         }
      }

/* We should now have all the values in the hash.  Get them out and write
   them into the result vector. */
      for ( i = 0; i < nperc; i++ ) {
         PercHashKey hkey;
         PercHashValue hval;
         Tcl_HashEntry *entry;
         hkey.hash = NULL;
         hkey.data = percs[ i ];
         entry = Tcl_FindHashEntry( &ndf1->perchash, hkey.hash );
         if ( entry == NULL ) {    /* kludge - sorry */
            hkey.data = ( percs[ i ] * 0.01 ) * 100.0;
            entry = Tcl_FindHashEntry( &ndf1->perchash, hkey.hash );
         }
         hval.hash = Tcl_GetHashValue( entry );
         values[ i ] = hval.data;
      }

/* Tidy up. */
      free( apercs );
      free( afracs );
      free( avals );

/* Return with success status. */
      return;
   }


/**********************************************************************/
   int NdfGetNdfFromObj( Tcl_Interp *interp, Tcl_Obj *obj,
                         NdfOrNdfset **ndf ) {
/**********************************************************************/
/* Converts an object into a pointer to an NdfOrNdfset structure.
   Returns TCL_OK for success or TCL_ERROR for failure.
   On failure the value at ndf is set to NULL and a suitable error message
   is left in the interpreter's result object.
*/
      Tcl_CmdInfo cmdinfo;
      Tcl_Obj *result;
      Tcl_Obj *ov[ 2 ];
      int valid;

      *ndf = (NdfOrNdfset *) NULL;
      if ( Tcl_GetCommandInfo( interp, Tcl_GetString( obj ), &cmdinfo ) == 1 ) {

/* See if it is a valid ndf object. */
         ov[ 0 ] = obj;
         ov[ 1 ] = Tcl_NewStringObj( "validndf", -1 );
         if ( Tcl_EvalObjv( interp, 2, ov, 0 ) == TCL_OK &&
              Tcl_GetBooleanFromObj( interp, Tcl_GetObjResult( interp ),
                                     &valid ) == TCL_OK &&
              valid ) {
            *ndf = (NdfOrNdfset *) cmdinfo.objClientData;
         }

/* See if it is a valid ndfset object. */
         else {
            ov[ 0 ] = obj;
            ov[ 1 ] = Tcl_NewStringObj( "validndfset", -1 );
            if ( Tcl_EvalObjv( interp, 2, ov, 0 ) == TCL_OK &&
                 Tcl_GetBooleanFromObj( interp, Tcl_GetObjResult( interp ),
                                        &valid ) == TCL_OK &&
                 valid ) {
               *ndf = (NdfOrNdfset *) cmdinfo.objClientData;
            }
         }
      }

/* Set an error result if it's not valid. */
      if ( *ndf == NULL ) {
         char rbuf[ 200 ];
         *rbuf = '\0';
         sprintf( rbuf, "\"%s\" is not an ndf or ndfset object",
                  Tcl_GetString( obj ) );
         result = Tcl_NewStringObj( rbuf, -1 );
         Tcl_SetObjResult( interp, result );
         return TCL_ERROR;
      }

/* Return successfully. */
      return TCL_OK;
   }



/**********************************************************************/
   int NdfGetIframeFromObj( Tcl_Interp *interp, Tcl_Obj *obj,
                            AstFrameSet *fset, int *iframe ) {
/**********************************************************************/
/* Converts an object into a frame index which references a frame in the
   given frameset.  For an ndf object, a single value is stored.
   Returns TCL_OK for success or TCL_ERROR for failure.
   On failure, at least the value at iframe is set to AST__NOFRAME,
   and a suitable error message is left in the interpreter's result object.
*/
      int leng;
      char *text;
      char rbuf[ 200 ];
      Tcl_Obj *result;

/* Initialise some values. */
      *iframe = AST__NOFRAME;
      text = Tcl_GetStringFromObj( obj, &leng );
      *rbuf = '\0';

/* Begin an AST context. */
      ASTCALL(
         astBegin;

/* See if it is a special string. */
         if ( ! strcmp( text, "BASE" ) ) {
            *iframe = astGetI( fset, "Base" );
         }
         else if ( ! strcmp( text, "CURRENT" ) ) {
            *iframe = astGetI( fset, "Current" );
         }

/* See if it is an integer. */
         else if ( Tcl_GetIntFromObj( interp, obj, iframe ) == TCL_OK ) {
            if ( *iframe < 1 || *iframe > astGetI( fset, "Nframe" ) ) {
               sprintf( rbuf, "numeric frame index %d out of range", *iframe );
               *iframe = AST__NOFRAME;
            }
         }

/* See if it is a domain name. */
         else {
            char *norm;
            char c;
            int i;
            AstFrame *frame;
            norm = malloc( leng + 1 );
            if ( tclmemok( interp, norm ) != TCL_OK ) {
               return TCL_ERROR;
            }
            for ( i = 0; i <= leng; i++ ) {
               c = text[ i ];
               norm[ i ] = ( c >= 'a' && c <= 'z' ) ? c + 'A' - 'a' : c;
            }
            for ( i = 1; i <= astGetI( fset, "Nframe" ); i++ ) {
               frame = astGetFrame( fset, i );
               if ( ! strcmp( norm, astGetC( frame, "Domain" ) ) ) {
                  *iframe = i;
                  break;
               }
            }
            free( norm );
            if ( *iframe == AST__NOFRAME ) {
               sprintf( rbuf, "\"%s\" is not a frame", text );
            }
         }

/* End AST context. */
         astEnd;
      )

/* Return value. */
      result = Tcl_NewStringObj( rbuf, -1 );
      Tcl_SetObjResult( interp, result );
      return *iframe == AST__NOFRAME ? TCL_ERROR : TCL_OK;
   }



/**********************************************************************/
   int NdfGetIframesFromObj( Tcl_Interp *interp, Tcl_Obj *obj,
                             NdfOrNdfset *ndfset, int *ifrms ) {
/**********************************************************************/
/* Does the same as NdfGetIframeFromObj, but writes an array of iframe
   values, one for each member ndf object of the passed ndfset object.
   It is the caller's responsibility to ensure that the iframe array
   is large enough to hold the output - it must have at least
   ndfset->nmember elements.
*/
      int i;
      int nndf;
      Ndf **ndfs;

/* Get a list of ndf objects to work with. */
      if ( ndfset->nmember ) {
         nndf = ndfset->nmember;
         ndfs = ndfset->content.ndfs;
      }
      else {
         nndf = 1;
         ndfs = &ndfset;
      }

/* Get the frame index for each NDF. */
      for ( i = 0; i < nndf; i++ ) {
         if ( NdfGetIframeFromObj( interp, obj, ndfs[ i ]->wcs,
                                   &ifrms[ i ] ) != TCL_OK ) {
            return TCL_ERROR;
         }
      }

/* Return success. */
      return TCL_OK;
   }


/* $Id$ */

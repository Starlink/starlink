/*
*+
*  Name:
*     ndf

*  Type of Module:
*     C extension to Tcl8.

*  Language:
*     ANSI C.

*  Purpose:
*     Provides a new command "ndf" for use with Tcl.

*  Description:
*     This file provides an object to use within Tcl programs for
*     manipulating NDFs.  When the object is created the NDF is 
*     opened and when it is deleted the NDF is annulled.  To calling
*     Tcl scripts it can provide the NDF dimensions, name and so on.
*     It also provides some other services, such as mapping the NDF and
*     generating an array suitable for feeding to PGPLOT for plotting
*     on a display device, which cannot sensibly be used from within
*     Tcl.  Such commands are designed to be used from other C 
*     extensions to Tcl.

*  Usage:
*     set object [ndf name]

*  Constructor:
*     ndf name
*        Creates an NDF object to manipulate the NDF which has the filename
*        "name".  Returns the name of a new object command which can 
*        be used to manipulate the NDF object.

*  Object commands:
*
*     object bbox frame
*        Returns a list giving the extent of a bounding box large enough
*        to hold the NDF when resampled from Base (Grid) coordinates into
*        the indicated frame and plotted.  The returned value is a list 
*        of the form {{lo hi} {lo hi} .. } with one pair for each of the
*        NDF's dimensions.
*
*        The frame may be specified either as a numerical frame index, 
*        as a domain name, or as one of the special strings "BASE" or 
*        "CURRENT".
*
*     object bounds
*        Returns a list giving the bounds of the NDF.  There are ndim
*        elements in the list, where ndim is the number of dimensions 
*        of the NDF.  Each element is a two element list giving the 
*        lower and upper pixel index bound.
*
*     object destroy
*        Releases all resources associated with the object.
*
*     object display device loval hival frame options
*        Plots the NDF on the named PGPLOT device, with colour cutoffs
*        given by the loval and hival arguments, and axes drawn according
*        to the frame argument.  The options string may contain any 
*        plotting options desired, in astSet format.
*
*        The may be specified either as a numerical frame index, as a 
*        domain name, or as one of the special strings "BASE" or "CURRENT".
*
*     object fitshead ?key? ...
*        This method allows access to any FITS headers contained in the 
*        .MORE.FITS extension of the NDF.  If any optional key arguments
*        are supplied, then it will return an ordered list containing 
*        the values of the FITS headers with the named keywords.  Absent
*        headers are represented by empty strings.  If no key arguments
*        are supplied, it will return a list of all the headers; each 
*        element is the whole header card, except that trailing spaces
*        and empty comments are stripped.
*
*     object frameatt attname ?frame?
*        Returns frame attribute values from the WCS component of the NDF.
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
*     object name
*        Returns a string which is the name of the NDF.  This is the same
*        string as was initially used to open the NDF.
*
*     object percentile perc ?perc? ...
*        This calculates the value(s) of the data in the NDF which 
*        correspond(s) to the given percentile value(s).  It returns a 
*        list containing as many elements as the number of perc values 
*        specified.  It is more efficient to calculate a multiple 
*        percentile values using a single call to this method than 
*        multiple calls.  It caches previously requested values, so 
*        that requesting a percentile which has been requested at any
*        previous time in the lifetime of this NDF object is fast.
*
*     object mapped ?setmap?
*        This method controls mapping of the NDF's DATA component.
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
*     object pixelsize frame
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
*     object polygon frame
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
*     object validndf
*        Returns true if the NDF identifier is valid.  It should always 
*        return true; this method can be used and caught to see whether
*        a command represents an object belonging to this class.
*
*     object wcstran ?options? from to pos ?pos ...?
*        Transforms a set of points between coordinate frames in 
*        the WCS component of the NDF.
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

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     16-AUG-2000 (MBT):
*        Initial version.
*-
*/


#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <math.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf.h"
#include "tcl.h"
#include "mers.h"
#include "cnf.h"
#include "tclndf.h"
#include "ast.h"
#include "cpgplot.h"


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


/* General purpose static buffer.
*/
   char buffer[ 4096 ];



/**********************************************************************/
   int NdfCmd( ClientData clientData, Tcl_Interp *interp, int objc, 
               Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      Ndf *ndf;
      static int counter = 0;
      char *ndfname;
      int nleng;
      int i;
      Tcl_Obj *op;

/* Check command arguments.  There should be a single extra argument for
   the constructor, the name of the NDF. */ 
      if ( objc != 2 ) {
         Tcl_WrongNumArgs( interp, 1, objv, "ndfName" );
         return TCL_ERROR;
      }

/* Allocate the data structure to hold the client data. */
      ndf = (Ndf *) malloc( sizeof( Ndf ) );
      if ( tclmemok( interp, ndf ) != TCL_OK ) {
         return TCL_ERROR;
      }
      ndfname = Tcl_GetStringFromObj( objv[ 1 ], &nleng );
      ndf->name = malloc( nleng + 1 );
      if ( tclmemok( interp, ndf->name ) != TCL_OK ) {
         return TCL_ERROR;
      }
      strcpy( ndf->name, ndfname );

/* Initialise some elements of the data structure. */
      ndf->mapped = 0;
      ndf->bad = 1;
      ndf->data = NULL;
      *ndf->mtype = '\0';
      ndf->plotarray.exists = 0;
      ndf->plotarray.data = NULL;
      ndf->fits.loaded = 0;
      ndf->fits.ncard = 0;
      ndf->fits.data = NULL;
      Tcl_InitHashTable( &ndf->perchash, TCL_ONE_WORD_KEYS );

      STARCALL( 

/* Open the NDF. */
         ndfFind( DAT__ROOT, ndf->name, &ndf->identifier, status );

/* Get the bounds of the NDF. */
         ndfBound( ndf->identifier, NDF__MXDIM, ndf->lbnd, ndf->ubnd, 
                   &ndf->ndim, status );

/* Get the HDS type of the DATA array. */
         ndfType( ndf->identifier, "DATA", ndf->ntype, DAT__SZTYP, status );

/* Get a pointer to the WCS component. */
         ndfGtwcs( ndf->identifier, &ndf->wcs, status );
      )

/* Set the number of elements. */
      ndf->nel = 1;
      for ( i = 0; i < ndf->ndim; i++ ) {
         ndf->nel *= ndf->ubnd[ i ] - ndf->lbnd[ i ] + 1;
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
      int status[ 1 ] = { SAI__OK };
      int valid;

/* Release the NDF resources associated with the object.  There is no
   procedure for recovering from errors in a Tcl_CmdDeleteProc, so if
   there is a problem we just have to annul the error status. */
      errMark();
      if ( ndf->fits.loaded && ndf->fits.ncard ) {
         DECLARE_CHARACTER( locator, DAT__SZLOC );
         cnfExprt( ndf->fits.loc, locator, DAT__SZLOC );
         F77_CALL(dat_annul)( CHARACTER_ARG(locator), INTEGER_ARG(status)
                              TRAIL_ARG(locator) );
      }
      astAnnul( ndf->wcs );
      ndfValid( ndf->identifier, &valid, status );
      if ( valid ) ndfAnnul( &ndf->identifier, status );
      if ( *status != SAI__OK ) errAnnul( status );
      errRlse();

/* Release memory we (may) have allocated for the object.  Some or all of
   these could be NULL, but if so, free() will not complain. */
      Tcl_DeleteHashTable( &ndf->perchash );
      free( ndf->name );
      free( ndf->plotarray.data );
      free( ndf );
   }



/**********************************************************************/
   int ObjectNdfCmd( ClientData clientData, Tcl_Interp *interp, int objc, 
                     Tcl_Obj *CONST objv[] ) {
/**********************************************************************/
      Ndf *ndf = (Ndf *) clientData;
      Tcl_Obj *result;
      Tcl_Obj *op;
      char *command = Tcl_GetString( objv[ 1 ] );
      int i;
      int j;


/**********************************************************************/
/* "bbox" command                                                     */
/**********************************************************************/
      if ( ! strcmp( command, "bbox" ) ) {
         Tcl_Obj *ov[ 2 ];
         double lbox[ NDF__MXDIM ];
         double ubox[ NDF__MXDIM ];
         int iframe;

/* Check syntax. */
         if ( objc != 3 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "frame" );
            return TCL_ERROR;
         }

/* Get frame argument. */
         if ( NdfGetIframeFromObj( interp, objv[ 2 ], ndf->wcs, &iframe ) 
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get the bounding box. */
         STARCALL(
            getbbox( ndf, iframe, lbox, ubox, status );
         )

/* Set the result object. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
         for ( i = 0; i < ndf->ndim; i++ ) {
            ov[ 0 ] = Tcl_NewDoubleObj( lbox[ i ] );
            ov[ 1 ] = Tcl_NewDoubleObj( ubox[ i ] );
            Tcl_ListObjAppendElement( interp, result, Tcl_NewListObj( 2, ov ) );
         }
      }


/**********************************************************************/
/* "bounds" command                                                   */
/**********************************************************************/
      else if ( ! strcmp( command, "bounds" ) ) {

/* Check syntax. */
         if ( objc != 2 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "" );
            return TCL_ERROR;
         }

/* Set result. */
         result = Tcl_NewListObj( 0, NULL );
         for ( i = 0; i < ndf->ndim; i++ ) {
            sprintf( buffer, "%d %d", ndf->lbnd[ i ], ndf->ubnd[ i ] );
            op = Tcl_NewStringObj( buffer, -1 );
            Tcl_ListObjAppendElement( interp, result, op );
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
         int ifrm;
         int locolour;
         int hicolour;
         int ofrm;
         int xpix;
         int ypix;
         int *pixbloc;
         char *device;
         char *settings;
         float factor;
         float gbox[ 4 ];
         float xplo;
         float xphi; 
         float yplo;
         float yphi;
         double bbox[ 4 ];
         double loval;
         double hival;
         double zoom;
         AstPlot *plot;

/* Check syntax. */
         if ( objc != 7 ) {
            Tcl_WrongNumArgs( interp, 2, objv, 
                              "device loval hival frame options" );
                            /* 1      2     3     4     5        */
            return TCL_ERROR;
         }

/* Get string arguments. */
         device = Tcl_GetString( objv[ 2 ] );
         settings = Tcl_GetString( objv[ 6 ] );

/* Get numeric arguments. */
         if ( Tcl_GetDoubleFromObj( interp, objv[ 3 ], &loval ) != TCL_OK ||
              Tcl_GetDoubleFromObj( interp, objv[ 4 ], &hival ) != TCL_OK ) {
            return TCL_ERROR;
         }
           
/* Get frame argument. */
         if ( NdfGetIframeFromObj( interp, objv[ 5 ], ndf->wcs, &ifrm ) 
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Open the PGPLOT plotting device. */
         if ( cpgopen( device ) < 0 ) {
            result = Tcl_NewStringObj( "Failed to open plotting device", -1 );
            Tcl_SetObjResult( interp, result );
            return TCL_ERROR;
         }

/* Query the plotting device for highest and lowest available colour indices. */
         cpgqcir( &locolour, &hicolour );

/* Set the plotting coordinate limits. */
         gbox[ 0 ] = bbox[ 0 ] = 0.0;
         gbox[ 1 ] = bbox[ 1 ] = 0.0;
         gbox[ 2 ] = bbox[ 2 ] = ndf->ubnd[ 0 ] - ndf->lbnd[ 0 ];
         gbox[ 3 ] = bbox[ 3 ] = ndf->ubnd[ 1 ] - ndf->lbnd[ 1 ];

/* Set the viewport to use all of the available surface, within the 
   constraint that the correct aspect ratio is retained. */
         cpgsvp( 0.0, 1.0, 0.0, 1.0 );
         cpgwnad( gbox[ 0 ], gbox[ 2 ], gbox[ 1 ], gbox[ 3 ] );

/* Get the viewport size in pixels. */
         cpgqvp( 3, &xplo, &xphi, &yplo, &yphi );

/* Set the zoom factor so that PGPLOT can optimise the plotting.  Basically
   this entails making the pixel array the same shape as the plotting 
   surface.  However, making it a lot bigger than the plotting surface
   would be a waste of memory. */
         factor = 1.0;
         zoom = 10.0;
         while ( zoom > 2.0 ) {
            zoom = ( xphi - xplo ) / ( bbox[ 2 ] - bbox[ 0 ] ) / factor++;
            printf( "zoom: %f\n", zoom );
         }

/* Prepare the image in a PGPLOT-friendly form. */
         STARCALL(
            pixbloc = getpixbloc( ndf, 1, zoom, loval, hival, locolour,
                                  hicolour, badcolour, status );
         )
         xpix = ndf->plotarray.xdim;
         ypix = ndf->plotarray.ydim;

/* Begin PGPLOT buffering. */
         cpgbbuf();

/* Plot the image. */
         cpgpixl( pixbloc, xpix, ypix, 1, xpix, 1, ypix, 
                  gbox[ 0 ], gbox[ 2 ], gbox[ 1 ], gbox[ 3 ] );

/* Get AST to plot the axes etc. */
         ASTCALL(
            ofrm = astGetI( ndf->wcs, "Current" );
            astSetI( ndf->wcs, "Current", ifrm );
            plot = astPlot( ndf->wcs, gbox, bbox, settings );
            astSetI( ndf->wcs, "Current", ofrm );
            astGrid( plot );
            astAnnul( plot );
         )

/* End PGPLOT buffering. */
         cpgebuf();

/* Close the plotting device. */
         cpgclos();

/* No result is returned. */
         result = Tcl_NewStringObj( "", 0 );
      }


/**********************************************************************/
/* "fitshead" command                                                 */
/**********************************************************************/
      else if ( ! strcmp( command, "fitshead" ) ) {

/* If we have not attempted to load the FITS extension before, do so now. */
         if ( ! ndf->fits.loaded ) {
            STARCALL( 
               int there;

/* See if a FITS extension exists. */
               ndfXstat( ndf->identifier, "FITS", &there, status );

/* If it does, then map it. */
               if ( F77_ISTRUE(there) ) {
                  char loc[ DAT__SZLOC ];
                  DECLARE_CHARACTER( floc, DAT__SZLOC );
                  DECLARE_CHARACTER( ftype, DAT__SZTYP );
                  DECLARE_CHARACTER( faccess, 6 );

/* Get an HDS locator. */
                  ndfXloc( ndf->identifier, "FITS", "READ", loc, status );
                  cnfExpch( loc, floc, floc_length );
                  cnfExprt( "READ", faccess, faccess_length );
                  cnfExprt( "_CHAR*80", ftype, ftype_length );

/* Map the data and store the pointer in the ndf data structure. */
                  F77_CALL(dat_mapv)( CHARACTER_ARG(floc), 
                                      CHARACTER_ARG(ftype), 
                                      CHARACTER_ARG(faccess),
                                      (F77_POINTER_TYPE *) &ndf->fits.data,
                                      INTEGER_ARG(&ndf->fits.ncard),
                                      INTEGER_ARG(status)
                                      TRAIL_ARG(floc) TRAIL_ARG(ftype) 
                                      TRAIL_ARG(faccess) );
               }

/* There was no FITS extension; record this as effectively a mapped FITS
   block of zero length. */
               else {
                  ndf->fits.ncard = 0;
               }

/* Record that we have got any FITS headers there are to get. */
               ndf->fits.loaded = 1;
            )
         }

/* Initialise result object. */
         result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );

/* No further arguments: return the whole FITS block as a list of lines. */
         if ( objc == 2 ) {
            char *card;
            char *pc;
            Tcl_Obj *cardobj;
            for ( j = 0; j < ndf->fits.ncard; j++ ) {
               card = ndf->fits.data + j * 80;
               pc = card + 79;
               while ( *pc == ' ' && pc >= card ) pc--;
               while ( *pc == '/' && pc >= card ) pc--;
               while ( *pc == ' ' && pc >= card ) pc--;
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
                  F77_POINTER_TYPE ipfits;
                  DECLARE_CHARACTER( fkey, 80 );
                  DECLARE_CHARACTER( fvalue, 80 );
                  cnfExprt( key, fkey, 80 );
                  ipfits = cnfFptr( ndf->fits.data );
                  F77_CALL(ccd1_ftget)( INTEGER_ARG(&ndf->fits.ncard),
                                        (F77_POINTER_TYPE *) &ipfits,
                                        INTEGER_ARG(&one),
                                        CHARACTER_ARG(fkey),
                                        CHARACTER_ARG(fvalue),
                                        INTEGER_ARG(&icard), 
                                        INTEGER_ARG(status)
                                        TRAIL_ARG(fkey) TRAIL_ARG(fvalue) );
                  if ( *status == SAI__OK ) {
                     pc = fvalue + fvalue_length - 1;
                     while ( *pc == ' ' && pc >= fvalue ) pc--;
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
            if ( NdfGetIframeFromObj( interp, objv[ 3 ], ndf->wcs, &ifrm ) 
                 != TCL_OK ) {
               return TCL_ERROR;
            }
         }

/* Begin AST context. */
         astBegin;

/* Get values and place them in the result object. */
         if ( ifrm != AST__NOFRAME ) {
            if ( getindex ) {
               result = Tcl_NewIntObj( ifrm );
            }
            else {
               ASTCALL( 
                  frame = astGetFrame( ndf->wcs, ifrm );
                  aval = astGetC( frame, attname );
               )
               result = Tcl_NewStringObj( aval, -1 );
            }
         }
         else {
            Tcl_Obj *ob;
            result = Tcl_NewListObj( 0, (Tcl_Obj **) NULL );
            for ( ifrm = 1; ifrm <= astGetI( ndf->wcs, "Nframe" ); ifrm++ ) {
               if ( getindex ) {
                  ob = Tcl_NewIntObj( ifrm );
               }
               else {
                  ASTCALL(
                     frame = astGetFrame( ndf->wcs, ifrm );
                  )
                  ASTCALL(
                     ob = Tcl_NewStringObj( astGetC( frame, attname ), -1 );
                     if ( ! astOK ) {
                        astClearStatus;
                        ob = Tcl_NewDoubleObj( astGetD( frame, attname ) );
                     }
                  )
               }
               Tcl_ListObjAppendElement( interp, result, ob );
            }
         }

/* End AST context. */
         astEnd;
      }


/**********************************************************************/
/* "mapped" command                                                   */
/**********************************************************************/
      else if ( ! strcmp( command, "mapped" ) ) {
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
         result = Tcl_NewBooleanObj( ndf->mapped );
         Tcl_SetObjResult( interp, result );

/* Now we check whether a state different from the current state has been 
   requested.  If not, the following code will just fall through to the
   exit. */

/* A mapped state has been requested, and we are not already mapped. */
         if ( objc == 3 && requested && ! ndf->mapped ) {
            STARCALL( 
               domapdata( ndf, status );
            )
         }

/* An unmapped state has been requested, and we are currently mapped. */
         else if ( objc == 3 && ! requested && ndf->mapped ) {
            STARCALL( 
               dounmapdata( ndf, status );
            )
         }
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
         result = Tcl_NewStringObj( ndf->name, -1 );
      }


/**********************************************************************/
/* "percentile" command                                               */
/**********************************************************************/
      else if ( ! strcmp( command, "percentile" ) ) {
         double *fracs;
         double *percs;
         double *vals;
         int nfrac;
         int nperc;

/* Check syntax. */
         nperc = objc - 2;
         if ( nperc < 1 ) {
            Tcl_WrongNumArgs( interp, 2, objv, "perc ?perc? ..." );
            return TCL_ERROR;
         }

/* Allocate arrays in which to store percentiles and values. */
         percs = malloc( nperc * sizeof( double ) );
         fracs = malloc( nperc * sizeof( double ) );
         vals = malloc( nperc * sizeof( double ) );
         if ( tclmemok( interp, percs ) != TCL_OK ||
              tclmemok( interp, fracs ) != TCL_OK ||
              tclmemok( interp, vals ) != TCL_OK ) {
            return TCL_ERROR;
         }

/* Store each of the extra arguments in the percentiles array.  Check that
   each is a number in the correct range as we go, and bail out with an
   error if not. */
         for ( i = 0; i < nperc; i++ ) {
            if ( Tcl_GetDoubleFromObj( interp, objv[ i + 2 ], percs + i ) 
                 != TCL_OK || percs[ i ] < 0.0 || percs[ i ] > 100.0 ) {
               result = Tcl_NewStringObj( "ndf percentile: argument \"", -1 );
               Tcl_AppendObjToObj( result, objv[ i + 2 ] );
               Tcl_AppendStringsToObj( result, "\" should be a number ",
                                       "between 0 and 100", (char *) NULL );
               Tcl_SetObjResult( interp, result );
               return TCL_ERROR;
            }
         }

/* Find out which of these percentiles we already have values for.
   If there are any missing, write these into a new array which we
   can feed to the percentile calculation routine. */
         nfrac = 0;
         for ( i = 0; i < nperc; i++ ) {
            PercHashKey hkey;
            hkey.data = percs[ i ];
            if ( Tcl_FindHashEntry( &ndf->perchash, hkey.hash ) == NULL ) {
               fracs[ nfrac++ ] = percs[ i ] * 0.01;
            }
         }

/* Unless all the requested values were in the hash table, we will need
   to work some more out. */
         if ( nfrac > 0 ) {
            double *work;

/* We have to sort the array of new percentiles to be worked out, since
   the percentile calculation routine likes them that way. */
            qsort( fracs, nfrac, sizeof( fracs[ 0 ] ), dcompare );

/* Now do the work of calcluating the percentiles. */
            work = malloc( nfrac * sizeof( double ) );
            if ( tclmemok( interp, work ) != TCL_OK ) {
               return TCL_ERROR;
            }
            STARCALL(
               DECLARE_CHARACTER( type, DAT__SZTYP );
               F77_LOGICAL_TYPE bad;

/* If the array is not currently mapped, we have to map it. */
               if ( ! ndf->mapped ) domapdata( ndf, status );

/* Massage some arguments for fortran. */
               bad = F77_ISTRUE(ndf->bad);
               cnfExprt( ndf->mtype, type, DAT__SZTYP );

/* Call the routine which does the calculations. */
               F77_CALL(ccd1_fra)( CHARACTER_ARG(type),
                                   INTEGER_ARG(&ndf->nel),
                                   (F77_POINTER_TYPE) &ndf->data,
                                   INTEGER_ARG(&nfrac), DOUBLE_ARG(fracs),
                                   LOGICAL_ARG(&bad), DOUBLE_ARG(work),
                                   DOUBLE_ARG(vals), INTEGER_ARG(status)
                                   TRAIL_ARG(type) );

/* We can update the bad flag as a bonus. */
               ndf->bad = F77_ISTRUE(bad);
            )
            free( work );

/* Store the new percentile results in the hash table. */
            for ( i = 0; i < nfrac; i++ ) {
               PercHashKey hkey;
               PercHashValue hval;
               Tcl_HashEntry *entry;
               int new;
               hkey.data = fracs[ i ] * 100.0;
               entry = Tcl_CreateHashEntry( &ndf->perchash, hkey.hash, &new );
               hval.data = vals[ i ];
               Tcl_SetHashValue( entry, hval.hash );
            }
         }

/* We should now have all the values in the hash.  Get them out and write
   them into the result vector. */
         for ( i = 0; i < nperc; i++ ) {
            PercHashKey hkey;
            PercHashValue hval;
            Tcl_HashEntry *entry; 
            hkey.data = percs[ i ];
            entry = Tcl_FindHashEntry( &ndf->perchash, hkey.hash );
            if ( entry == NULL ) {    /* kludge - sorry */
               hkey.data = ( percs[ i ] * 0.01 ) * 100.0;
               entry = Tcl_FindHashEntry( &ndf->perchash, hkey.hash );
            }
            hval.hash = Tcl_GetHashValue( entry );
            vals[ i ] = hval.data;
         }

/* Store the answers as a list in a result object. */
         result = Tcl_NewListObj( 0, NULL );
         for ( i = 0; i < nperc; i++ ) {
            op = Tcl_NewDoubleObj( vals[ i ] );
            Tcl_ListObjAppendElement( interp, result, op );
         }

/* Tidy up. */
         free( fracs );
         free( percs );
         free( vals );
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
         if ( NdfGetIframeFromObj( interp, objv[ 2 ], ndf->wcs, &iframe )
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Get the pixel unit size. */
         STARCALL(
            size = getpixelsize( ndf, iframe, status );
         )

/* Set the result object. */
         result = Tcl_NewDoubleObj( size );
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
         if ( NdfGetIframeFromObj( interp, objv[ 2 ], ndf->wcs, &iframe ) 
              != TCL_OK ) {
            return TCL_ERROR;
         }

/* Set positions of the corners in the base frame. */
         xb[ 0 ] = xb[ 3 ] = 0.5;
         xb[ 1 ] = xb[ 2 ] = ndf->ubnd[ 0 ] - ndf->lbnd[ 0 ] + 0.5;
         yb[ 0 ] = yb[ 1 ] = 0.5;
         yb[ 2 ] = yb[ 3 ] = ndf->ubnd[ 1 ] - ndf->lbnd[ 1 ] + 0.5;

/* Get the positions of the corners in the requested frame. */
         ASTCALL(
            astBegin;
            map = astGetMapping( ndf->wcs, AST__BASE, iframe );
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
            ndfValid( ndf->identifier, &valid, status );
         )

/* Set result value. */
         result = Tcl_NewBooleanObj( F77_ISTRUE(valid) );
      }


/**********************************************************************/
/* "wcstran" command                                                  */
/**********************************************************************/
      else if ( ! strcmp( command, "wcstran" ) ) {
         int form;
         int iframe[ 2 ];
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
              ! strcmp( Tcl_GetString( objv[ 2 ] + nflag ), "-format" ) ) {
            form = 1;
            nflag++;
         }

/* Check syntax. */
         if ( objc + nflag < 4 ) {
            Tcl_WrongNumArgs( interp, 2, objv, 
                              "?options from to pos ?pos ...?" );
            return TCL_ERROR;
         }

/* Get frame arguments. */
         for ( i = 0; i < 2; i++ ) {
            if ( NdfGetIframeFromObj( interp, objv[ nflag + 2 + i ], ndf->wcs,
                                      &iframe[ i ] ) != TCL_OK ) {
               return TCL_ERROR;
            }
         }

/* Get number of positions to transform. */
         npos = objc - nflag - 4;

/* Begin AST context. */
         astBegin;

/* Get the mapping between the two frames. */
         ASTCALL(
            imap = astGetMapping( ndf->wcs, iframe[ 0 ], iframe[ 1 ] );
            outframe = astGetFrame( ndf->wcs, iframe[ 1 ] );
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
            astTranN( imap, npos, nin, npos, (const double (*)[]) inpos, 
                      1, nout, npos, (double (*)[]) outpos );
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
         astEnd;
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
                               "\n    bbox frame",
                               "\n    bounds", 
                               "\n    destroy",
                               "\n    fitshead ?key? ...",
                               "\n    frameatt attname ?frame?",
                               "\n    mapped ?setmap?",
                               "\n    name",
                               "\n    percentile perc ?perc? ...",
                               "\n    pixelsize frame",
                               "\n    polygon frame",
                               "\n    validndf",
                               "\n    wcstran ?options? from to pos ?pos ...?",
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

/* The module has initialised successfully. */
      return TCL_OK;
   }



/**********************************************************************/
/* Auxiliary functions.                                               */
/**********************************************************************/

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
      return diff == 0.0 ? 0 : ( diff < 1 ? -1 : 1 );
   }


/**********************************************************************/
   void domapdata( Ndf *ndf, int *status ) {
/**********************************************************************/
/* Perform mapping of the DATA component of an ndf.  Use the data's native
   type for efficiency (another possiblity would be to use a smaller type,
   say _REAL if native type were _DOUBLE, for reduced memory consumption). */
      int nel;
      if ( *status != SAI__OK ) return;
      ndfMap( ndf->identifier, "DATA", ndf->ntype, "READ", &ndf->data, &nel,
              status );
      if ( *status == SAI__OK ) {
         strncpy( ndf->mtype, ndf->ntype, DAT__SZTYP );
         ndf->mapped = 1;
      }
   }


/**********************************************************************/
   void dounmapdata( Ndf *ndf, int *status ) {
/**********************************************************************/
/* Perform unmapping of the DATA component of an NDF. */
      if ( *status != SAI__OK ) return;
      ndfUnmap( ndf->identifier, "DATA", status );
      if ( *status == SAI__OK ) {
         ndf->data = NULL;
         *ndf->mtype = '\0';
         ndf->mapped = 0;
      }
   }


/**********************************************************************/
   double getpixelsize( Ndf *ndf, int iframe, int *status ) {
/**********************************************************************/
/* Return the approximate linear size of a unit pixel in the given frame,
   in terms of Base frame (GRID) pixels.  This only properly makes sense 
   if the mapping between the two frames contains just zooms, rotations 
   and translations, but for most non-drastic mappings it is a definite 
   enough result to be useful for certain purposes.  It only looks at 
   the first dimension.
*/
      AstMapping *map;
      AstFrame *bfrm;
      AstFrame *ffrm;
      int i;
      int *old_status;
      double bpoint[ NDF__MXDIM ][ 2 ];
      double fpoint[ NDF__MXDIM ][ 2 ];
      double ratio = 1.0;

/* Arrange for AST to use the inherited status variable for status checking. */
      old_status = astWatch( status );
      astBegin;

/* Construct a point at opposite corners of the Base frame. */
      for ( i = 0; i < ndf->ndim; i++ ) {
         bpoint[ i ][ 0 ] = 1.0;
         bpoint[ i ][ 1 ] = ndf->ubnd[ i ] - ndf->lbnd[ i ];
      }

/* Construct a point at opposite corners of the selected frame. */
      bfrm = astGetFrame( ndf->wcs, AST__BASE );
      ffrm = astGetFrame( ndf->wcs, iframe );
      map = astGetMapping( ndf->wcs, AST__BASE, iframe );
      astTranN( map, 2, astGetI( map, "Nin" ), 2, (const double (*)[]) bpoint, 
                1, astGetI( map, "Nout" ), 2, fpoint );

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
   void getbbox( Ndf *ndf, int iframe, double *lbox, double *ubox, 
                 int *status ) {
/**********************************************************************/
/* Get the bounding box for an NDF resampled from the Base (GRID) frame
   into another, given, frame. 
*/
      int i;
      double lbnd[ NDF__MXDIM ];
      double ubnd[ NDF__MXDIM ];
      AstMapping *map;
      int *old_status;

/* Get the bounding values in the GRID (Base) frame. */
      for ( i = 0; i < ndf->ndim; i++ ) {
         lbnd[ i ] = 0.5;
         ubnd[ i ] = ndf->ubnd[ i ] - ndf->lbnd[ i ] + 0.5;
      }

/* Arrange for AST to use the inherited status variable for status checking. */
      astBegin;
      old_status = astWatch( status );

/* Convert the GRID bounding values to the selected frame. */
      map = astGetMapping( ndf->wcs, AST__BASE, iframe );
      for ( i = 1; i <= ndf->ndim; i++ ) {
         astMapBox( map, (const double *) lbnd, (const double *) ubnd,
                    1, i, lbox + i - 1, ubox + i - 1, 
                    (double *) NULL, (double *) NULL );
      }

/* End AST context. */
      astWatch( old_status );
      astEnd;

      return;
   }


/**********************************************************************/
   int *getpixbloc( Ndf *ndf, int iframe, double zoom, double loval, 
                    double hival, int locolour, int hicolour, int badcolour,
                    int *status ) {
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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* See whether we need to construct a new array.  If no array currently
   exists, or if the scale values are different from the ones 
   we've been asked for, then we'll have to construct it.  Otherwise
   we can just use the one we've already got. */
      if ( ! ndf->plotarray.exists || ndf->plotarray.loval != loval
                                   || ndf->plotarray.hival != hival 
                                   || ndf->plotarray.iframe != iframe
                                   || ndf->plotarray.zoom != zoom ) {
         int xdim;
         int xbase;
         int xndf;
         int ybase;
         int ydim;
         int yndf;
         int *old_status;
         double lbox[ NDF__MXDIM ];
         double ubox[ NDF__MXDIM ];
         AstMapping *map;
         DECLARE_CHARACTER( ftype, DAT__SZTYP );

         xndf = ndf->ubnd[ 0 ] - ndf->lbnd[ 0 ] + 1;
         yndf = ndf->ubnd[ 1 ] - ndf->lbnd[ 1 ] + 1;
         getbbox( ndf, iframe, lbox, ubox, status );
         xbase = zoom * lbox[ 0 ];
         ybase = zoom * lbox[ 1 ];
         xdim = zoom * ( ubox[ 0 ] - lbox[ 0 ] ) + 1;
         ydim = zoom * ( ubox[ 1 ] - lbox[ 1 ] ) + 1;

/* If a previous plotarray exists but is the wrong size, we will need to
   deallocate that memory prior to allocating more. */
         if ( ndf->plotarray.exists && ( ndf->plotarray.xdim != xdim ||
                                         ndf->plotarray.ydim != ydim ) ) {
            free( ndf->plotarray.data );
            ndf->plotarray.exists = 0;
         }

/* If we don't now have memory to put the pixel array in, allocate some. */
         if ( ! ndf->plotarray.exists ) {
            ndf->plotarray.data = malloc( sizeof( int ) * xdim * ydim );
            if ( ndf->plotarray.data == NULL ) {
               *status = SAI__ERROR;
               errRep( " ", "Memory allocation failed", status );
               return NULL;
            }
            ndf->plotarray.exists = 1;
         }

/* Store characteristics of generated array. */
         ndf->plotarray.xdim = xdim;
         ndf->plotarray.ydim = ydim;
         ndf->plotarray.iframe = iframe;
         ndf->plotarray.zoom = zoom;
         ndf->plotarray.loval = loval;
         ndf->plotarray.hival = hival;

/* If the array is not currently mapped, we have to map it. */
         if ( ! ndf->mapped ) domapdata( ndf, status );

/* Massage some arguments for fortran. */
         cnfExprt( ndf->mtype, ftype, DAT__SZTYP );

/* Begin an AST context. */
         astBegin;
         old_status = astWatch( status );

/* Get the mapping for the resample operation. */
         map = astGetMapping( ndf->wcs, AST__BASE, iframe );
         if ( fabs( zoom - 1.0 ) > 1. / (double) ( xndf + yndf ) ) {
            map = (AstMapping *) 
                     astCmpMap( map, (AstMapping *) astZoomMap( 2, zoom, "" ),
                                1, "" );
         }
         map = astSimplify( map );

/* Call the routine which does the data scaling. */
         F77_CALL(ccd1_mkimg)( (F77_POINTER_TYPE) &ndf->data,
                               CHARACTER_ARG(ftype), INTEGER_ARG(&xndf),
                               INTEGER_ARG(&yndf), INTEGER_ARG(&map),
                               INTEGER_ARG(&xbase), INTEGER_ARG(&ybase),
                               INTEGER_ARG(&xdim), INTEGER_ARG(&ydim),
                               DOUBLE_ARG(&loval), DOUBLE_ARG(&hival),
                               INTEGER_ARG(&locolour), INTEGER_ARG(&hicolour),
                               INTEGER_ARG(&badcolour),
                               INTEGER_ARG(ndf->plotarray.data),
                               INTEGER_ARG(status)
                               TRAIL_ARG(ftype) );
                               
/* End the AST context. */
         astWatch( old_status );
         astEnd;
      }

/* Return the address of the prepared array. */
      return ndf->plotarray.data;
   }


/**********************************************************************/
   int NdfGetNdfFromObj( Tcl_Interp *interp, Tcl_Obj *obj, Ndf **ndf ) {
/**********************************************************************/
/* Converts an object into a pointer to an NDF.
   Returns TCL_OK for success or TCL_ERROR for failure.
   On failure the value at ndf is set to NULL and a suitable error message
   is left in the interpreter's result object.
*/ 
      Tcl_CmdInfo cmdinfo;
      Tcl_Obj *result;
      Tcl_Obj *ov[ 2 ];

      *ndf = (Ndf *) NULL;
      if ( Tcl_GetCommandInfo( interp, Tcl_GetString( obj ), &cmdinfo ) == 1 ) {
         ov[ 0 ] = obj;
         ov[ 1 ] = Tcl_NewStringObj( "validndf", -1 );
         if ( Tcl_EvalObjv( interp, 2, ov, 0 ) == TCL_OK ) {
            *ndf = (Ndf *) cmdinfo.objClientData;
         }
      }
      if ( *ndf == NULL ) {
         char rbuf[ 200 ];
         *rbuf = '\0';
         sprintf( rbuf, "\"%s\" is not an ndf object", Tcl_GetString( obj ) );
         result = Tcl_NewStringObj( rbuf, -1 );
         return TCL_ERROR;
      }
      return TCL_OK;
   }
   


/**********************************************************************/
   int NdfGetIframeFromObj( Tcl_Interp *interp, Tcl_Obj *obj, 
                            AstFrameSet *fset, int *iframe ) {
/**********************************************************************/
/* Converts an object into a frame index which references a frame in the
   given frameset.  
   Returns TCL_OK for success or TCL_ERROR for failure.
   On failure, the value at iframe is set to AST__NOFRAME, and a suitable
   error message is left in the interpreter's result object.
*/
      int leng;
      char *text;
      char rbuf[ 200 ];
      Tcl_Obj *result;

/* Initialise some values. */
      *iframe = AST__NOFRAME;
      text = Tcl_GetStringFromObj( obj, &leng );
      *rbuf = '\0';

/* Check that AST is not in an error state. */
      if ( astOK ) {

/* Begin an AST context. */
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
      }

/* If there's an AST error, clear it but consider we have failed. */
      if ( ! astOK ) {
         astClearStatus;
         *iframe = AST__NOFRAME;
         sprintf( rbuf, "AST in error state - no frame found for \"%s\"",
                  text );
      }

/* Return value. */
      result = Tcl_NewStringObj( rbuf, -1 );
      Tcl_SetObjResult( interp, result );
      return *iframe == AST__NOFRAME ? TCL_ERROR : TCL_OK;
   }


/* $Id$ */

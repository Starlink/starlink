/*+
 *  Name:
 *     gaiaNDF

 *  Purpose:
 *     Primitive access to NDFs for GAIA.

 *  Language:
 *     C

 *  Description:
 *     Interface to the NDF functions that GAIA requires for all the various
 *     access modes. That's simple access to images, multiple images
 *     per container-file and the final evolution, proper NDF access
 *     which is mainly used in the GAIA3D extensions (which work in
 *     parallel to the image access modes).

 *  Copyright:
 *     Copyright (C) 1995-2005 Central Laboratory of the Research Councils
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council
 *     Copyright (C) 2008 Science and Technology Facilities Council
 *     Copyright (C) 2014 Cornell University.
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

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham
 *      TIMJ: Tim Jenness - Cornell University

 *   History:
 *      18-JAN-1995 (PWD):
 *         Original version.
 *      24-JUL-1998 (PWD):
 *         Changed CNF macro F77_CREATE_CHARACTER_ARRAY to new calling
 *         sequence.
 *      11-JAN-2000 (PWD):
 *         Changed to use C-code for copying data. NDF library now has
 *         a C interface, so new code should be written using this
 *         (and old code changed as time permits).
 *      12-JAN-2000
 *         Added changes for "HDU" support.
 *      30-MAY-2001
 *         Now supports double precision data type correctly.
 *      31-MAY-2001
 *         Made NDF name handling more robust to very long names
 *         (>MAXNDFNAME).
 *      21-NOV-2005 (PWD):
 *         Changed to use public HDS C interface.
 *      02-SEP-2014 (TIMJ):
 *         Use DAT__FLEXT rather than explicit ".sdf"
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <tcl.h>
#include "sae_par.h"
#include "prm_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems_par.h"
#include "ems.h"
#include "merswrap.h"
#include "star/hds.h"
#include "dat_par.h"
#include "dat_err.h"
#include "ndf.h"
#include "gaiaNDF.h"
#include "gaiaUtils.h"

/*  Maximum number of pixels to copy during chunking. */
#define MXPIX 2000000

/*  TCL_OK and TCL_ERROR */
#define TCL_OK 0
#define TCL_ERROR 1

/*  Prototypes for external Fortran routines */
extern void F77_EXTERNAL_NAME(rtd_rdndf)( CHARACTER(ndfname),
                                          INTEGER(type),
                                          INTEGER(ndfid),
                                          INTEGER(width),
                                          INTEGER(height),
                                          POINTER(charPtr),
                                          INTEGER(header_length),
                                          INTEGER(status)
                                          TRAIL(ndfname) );

extern void F77_EXTERNAL_NAME(rtd_wrndf)( CHARACTER(ndfname),
                                          INTEGER(type),
                                          INTEGER(ndfid),
                                          POINTER(data),
                                          INTEGER(width),
                                          INTEGER(height),
                                          CHARACTER(comp),
                                          CHARACTER_ARRAY(fhead),
                                          INTEGER(nhead),
                                          INTEGER(status )
                                          TRAIL(ndfname)
                                          TRAIL(comp)
                                          TRAIL(fhead) );

extern void F77_EXTERNAL_NAME(rtd_cpdat)( INTEGER(ndfid),
                                          POINTER(data),
                                          CHARACTER(comp),
                                          INTEGER(status )
                                          TRAIL(comp) );

extern F77_SUBROUTINE(rtd1_aqual)( INTEGER(ndfId),
                                   LOGICAL(grab),
                                   LOGICAL(haveQual),
                                   POINTER(q) );

/*
 *  Local structure to store information about an NDF.
 */
struct NDFinfo {
    char name[MAXNDFNAME]; /*  NDF name (within file)*/
    int ndfid;             /*  NDF identifier */
    int ncomp;             /*  Number of data components */
    int type;              /*  NDF data type (as bitpix) */
    int nx;                /*  First dimension of NDF */
    int ny;                /*  Second dimension of NDF */
    int readonly;          /*  Readonly access */
    int havevar;           /*  Variance component exists */
    int havequal;          /*  Quality array exists */
    char *header;          /*  Pointer to FITS headers */
    int hlen;              /*  Length of header */
    struct NDFinfo *next;  /*  Pointer to next NDF with displayables */
};
typedef struct NDFinfo NDFinfo;

/* Trace siblings of an NDF rooted at baseloc. */
static NDFinfo *traceMNDFs( NDFinfo **headinfo, HDSLoc *baseloc, int baseid,
                            int deepsearch, const char *slice );

/*  Trace NDFs stored in the extension of an NDF. */
static NDFinfo *traceMNDFExtensions( int ndfid, NDFinfo **headinfo,
                                     const char *slice );

/*  Trace NDFs rooted at a given locator. */
static void traceSiblings( HDSLoc *baseloc, int ndfid, int deepsearch,
                           Tcl_DString *dsPtr, const char *prefix,
                           int *nsiblings );

/*  Trace NDFs stored in the extension of an NDF. */
static void traceSiblingExtensions( int ndfid, Tcl_DString *dsPtr,
                                    const char *prefix, int *nsiblings );

/**
 * ===================================
 * Simple Skycat-like access routines.
 * ===================================
 */

/**
 *  Name:
 *     gaiaAccessNDF
 *
 *  Purpose:
 *     Access an NDF by name.
 *
 * Description:
 *     Accesses an NDF by name (filename or HDS pathname) returning
 *     its data type (as FITS bitpix), width and height, a pseudo FITS
 *     header (including any WCS information) and the NDF identifier.
 *
 *     The error message (if generated) must be "free"d by the caller.
 *     Releasing the header is also the caller's responsibility.
 */
int gaiaAccessNDF( const char *filename, int *type, int *width, int *height,
                   char **header, int *header_length, int *ndfid,
                   char **error_mess )
{
   DECLARE_CHARACTER(ndfname, MAXNDFNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(tmpname, MAXNDFNAME);   /* Local copy of filename (F77) */
   DECLARE_INTEGER(status);                  /* Global status */
   DECLARE_POINTER(charPtr);                 /* Pointer to F77 character array */
   char *tmpPtr;

   /* Convert the file name into an F77 string */
   if ( strlen( filename ) <= MAXNDFNAME ) {
       strncpy( tmpname, filename, tmpname_length );
       cnfExprt( tmpname, ndfname, ndfname_length );

       /* Attempt to open the NDF. */
       emsMark();
       F77_CALL( rtd_rdndf )( CHARACTER_ARG(ndfname),
                              INTEGER_ARG(type),
                              INTEGER_ARG(ndfid),
                              INTEGER_ARG(width),
                              INTEGER_ARG(height),
                              POINTER_ARG(&charPtr),
                              INTEGER_ARG(header_length),
                              INTEGER_ARG(&status)
                              TRAIL_ARG(ndfname)
           );
       if ( status != SAI__OK ) {
           *error_mess = gaiaUtilsErrMessage();
           emsRlse();
           return 0;
       }

       /* Convert the FITS headers into a C string */
       F77_IMPORT_POINTER( charPtr, tmpPtr );
       *header = cnfCreib( tmpPtr, 80 * (*header_length) );
       cnfFree( (void *)tmpPtr );
       emsRlse();
       return 1;
   }
   else {

       /* Filename too long. */
       *error_mess = strdup( "NDF specification is too long" );
       return 0;
   }
}

/**
 *  Name:
 *     gaiaWriteNDF
 *
 *  Purpose:
 *     Create an NDF from an existing NDF, replacing the named
 *     component with the data given.
 */
int gaiaWriteNDF( const char *filename, int type, int width, int height,
                  void *data , int ndfid, const char *component,
                  const char *header, size_t lheader, char **error_mess )
{
   DECLARE_CHARACTER(ndfname,MAXNDFNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(tmpname,MAXNDFNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(comp, 20);             /* NDF component to use (F77) */
   DECLARE_INTEGER(status);                 /* Global status */
   DECLARE_POINTER(dataPtr);                /* Pointer to F77 memory */
   DECLARE_CHARACTER_ARRAY_DYN(fhead);
   int dims[1];

   /* Convert the file name into an F77 string */
   strncpy( tmpname, filename, tmpname_length );
   cnfExprt( tmpname, ndfname, ndfname_length );

   /* Convert the NDF component into a F77 string */
   cnfExprt( (char *) component, comp, comp_length);

   /* Convert C pointer to Fortran pointer */
   F77_EXPORT_POINTER( data, dataPtr );

   /*  If we have a FITS header then write it out with the NDF. This
       also contains any new WCS information that might be
       available. */
   if ( lheader > 0) {

      /*  Convert the C string into a Fortran array. */
      dims[0] = (int) lheader / 80;
      F77_CREATE_CHARACTER_ARRAY( fhead, 80, dims[0] );
      cnfExprta( (char *) header, 80, fhead, 80, 1, dims );
   }
   else {
       fhead = NULL;
       fhead_length = 0;
   }

   /* Attempt to open the NDF. */
   emsMark();
   F77_CALL( rtd_wrndf )( CHARACTER_ARG(ndfname),
                          INTEGER_ARG(&type),
                          INTEGER_ARG(&ndfid),
                          POINTER_ARG(&dataPtr),
                          INTEGER_ARG(&width),
                          INTEGER_ARG(&height),
                          CHARACTER_ARG(comp),
                          CHARACTER_ARRAY_ARG(fhead),
                          INTEGER_ARG(dims),
                          INTEGER_ARG(&status)
                          TRAIL_ARG(ndfname)
                          TRAIL_ARG(comp)
                          TRAIL_ARG(fhead)
      );

   /* Free the header copy. */
   F77_FREE_CHARACTER( fhead );

   if ( status != SAI__OK ) {
      *error_mess = gaiaUtilsErrMessage();
      emsRlse();
      return 0;
   }
   emsRlse();
   return 1;
}

/**
 *  Name:
 *     gaiaFreeNDF
 *
 *  Purpose:
 *     Free an NDF, by anulling its identifier and freeing any
 *     locally allocated resources.
 */
int gaiaFreeNDF( int ndfid )
{
   DECLARE_LOGICAL(haveQual);
   DECLARE_POINTER(qualPtr);
   DECLARE_LOGICAL(grab);
   int status = SAI__OK;

   grab = F77_FALSE;
   emsMark();

   /*  Free any quality associated with this NDF (should be safe under
    *  any circumstances).
    */
   if ( ndfid != 0 ) {
      F77_CALL( rtd1_aqual)( INTEGER_ARG(&ndfid), LOGICAL_ARG(&grab),
                             LOGICAL_ARG(&haveQual),
                             POINTER_ARG(&qualPtr));
   }

   /* Free the NDF */
   ndfAnnul( &ndfid, &status );
   if ( status != SAI__OK ) {
       emsAnnul( &status );
   }
   emsRlse();
   return 1;
}

/**
 *   Name:
 *      gaiaCopyComponent
 *
 *   Purpose:
 *      Copy an NDF data component into an array of previously
 *      allocated memory. The routine uses NDF chunking to minimize
 *      the total memory footprint.
 */
int gaiaCopyComponent( int ndfid, void **data, const char* component,
                       char **error_mess )
{
   char dtype[NDF__SZTYP+1];
   int chunkid;
   int el;
   int i;
   int j;
   int nchunk;
   int status = SAI__OK;
   void *ptr[1];

   /* Get the type of the NDF component. */
   emsMark();
   ndfBegin();
   ndfType( ndfid, component, dtype, NDF__SZTYP+1, &status );

   /*  Determine the number of chunks needed to copy the data. */
   ndfNchnk( ndfid, MXPIX, &nchunk, &status );

   /*  Using the appropriate data type, access the NDF chunks and copy
    *  data. Use a macro to save repetition.
    */
#define CHUNK_COPY(fromtype,totype)                                  \
   fromtype *fromPtr;                                                \
   totype *toPtr = *data;                                            \
   for ( i = 1; i <= nchunk; i++ ) {                                 \
      ndfChunk( ndfid, MXPIX, i, &chunkid, &status );                \
      ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );\
      fromPtr = (fromtype *)ptr[0];                                  \
      for( j = 0; j < el; j++ ) {                                    \
          *toPtr++ = (totype) *fromPtr++;                            \
      }                                                              \
   }                                                                 \

   if ( strncmp( dtype, "_DOUBLE", 7 ) == 0 ) {
       CHUNK_COPY(double, double)
   }
   else if ( strncmp( dtype, "_REAL", 5 ) == 0 ) {
       CHUNK_COPY(float, float)
   }
   else if ( strncmp( dtype, "_INT64", 8 ) == 0 ) {
       CHUNK_COPY(INT64, INT64)
   }
   else if ( strncmp( dtype, "_INTEGER", 8 ) == 0 ) {
       CHUNK_COPY(int, int)
   }
   else if ( strncmp( dtype, "_WORD", 5 ) == 0 ) {
       CHUNK_COPY(short, short)
   }
   else if ( strncmp( dtype, "_UWORD", 5 ) == 0 ) {
       CHUNK_COPY(unsigned short, unsigned short)
   }
   else if ( strncmp( dtype, "_BYTE", 5 ) == 0 ) {
      /*  Cannot display this type, so mapping is to short */
      CHUNK_COPY(char, short)
   }
   else if ( strncmp( dtype, "_UBYTE", 6 ) == 0 ) {
       CHUNK_COPY(unsigned char, unsigned char)
   }

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
      *error_mess = gaiaUtilsErrMessage();
      ndfEnd( &status );
      emsRlse();
      return 0;
   }
   ndfEnd( &status );
   emsRlse();
   return 1;
}

/**
 *   Name:
 *      gaiaMapComponent
 *
 *   Purpose:
 *      Map an NDF data component for the given access access, returning a
 *      pointer to the mapped memory. The memory must be unmapped either
 *      directly or by annuling the NDF identifier, before the program
 *      exits.
 *
 *   Note:
 *      If the data values require modification, then a copy should be
 *      made instead.
 */
int gaiaMapComponent( int ndfid, void **data, const char* component,
                      const char *access, char **error_mess )
{
   char dtype[NDF__SZTYP+1];
   int el;
   int status = SAI__OK;
   void *ptr[1];

   /* Get the type of the NDF component. */
   emsMark();
   ndfBegin();

   /* Allow ERROR as synonym for VARIANCE. */
   if ( strcasecmp( component, "ERROR" ) == 0 ) {
       ndfType( ndfid, "VARIANCE", dtype, NDF__SZTYP+1, &status );
   }
   else {
       ndfType( ndfid, component, dtype, NDF__SZTYP+1, &status );
   }

   /*  Trap _BYTE and map _WORD */
   if ( strncmp( dtype, "_BYTE", 7 ) == 0 ) {
      strcpy( dtype, "_WORD" );
   }

   /*  Take care to not pass back a random pointer if this fails
    *  (corrupt data component or invalid ERROR values). */
   *data = NULL;
   if ( status == SAI__OK ) {
       ndfMap( ndfid, component, dtype, access, ptr, &el, &status );
       if ( status == SAI__OK ) {
           *data = ptr[0];
       }
   }

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
      *error_mess = gaiaUtilsErrMessage();
      ndfEnd( &status );
      emsRlse();
      return 0;
   }
   ndfEnd( &status );
   emsRlse();
   return 1;
}

/**
 *  Name:
 *     gaiaCreateNDF.
 *
 *  Purpose:
 *     Create a new simple NDF.
 *
 *  Description:
 *     Create a new NDF with the given type (HDS format) and bounds.
 *     Add the given WCS (NULL for none). The components can be accessed using
 *     gaiaMapComponent.
 */
int gaiaCreateNDF( const char *filename, int ndim, int lbnd[], int ubnd[],
                   const char *type, AstFrameSet *wcs,
                   int *indf, char **error_mess )
{
    int place;
    int status = SAI__OK;

    emsMark();

    /* Open NDF, also creates it */
    ndfOpen( NULL, filename, "WRITE", "NEW", indf, &place, &status );

    /* Create the NDF */
    ndfNew( type, ndim, lbnd, ubnd, &place, indf, &status );

    /* Insert the WCS */
    if ( wcs != NULL ) {
        ndfPtwcs( wcs, *indf, &status );
    }

    /* If an error occurred construct the message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return 0;
    }
    emsRlse();
    return 1;
}

/**
 *  Name:
 *     gaiaCopyNDF.
 *
 *  Purpose:
 *     Create a new NDF by selective copying of parts of an existing NDF.
 *
 *  Description:
 *     Create a new NDF and populate it with some of the content of an
 *     existing NDF using the ndfScopy() routine. The filename is for the new
 *     NDF. The dimensionality and data type of the array components can be
 *     set, and a new (matching) wcs established.
 *
 *     If an axis value is given (i.e. has positive value) then this
 *     forces the "axis" AXIS component to be copied to the first AXIS
 *     component, if an AXIS component is present (use when extracting a
 *     single axis and the AXIS component is still required, i.e. spectral
 *     extraction). In this case "AXIS" must be in the component list.
 */
int gaiaCopyNDF( const char *filename, int indf, const char *clist,
                 int ndim, int lbnd[], int ubnd[], const char *type,
                 AstFrameSet *wcs, int axis, int *ondf, char **error_mess )
{
    char value[132];
    const char *c[] = { "CENTRE", "WIDTH", "VARIANCE" };
    const char *cc[] = { "LABEL", "UNITS" };
    void *inptr;
    void *outptr;
    int exists;
    int haveaxis;
    int i;
    int nel;
    int place;
    int status = SAI__OK;

    emsMark();

    /* Open NDF, overwrites an existing NDF and returns a place holder */
    ndfOpen( NULL, filename, "WRITE", "NEW", ondf, &place, &status );

    /* Create the NDF by copying the components */
    ndfScopy( indf, clist, &place, ondf, &status );

    /* If new dimensionalities are given, set those */
    if ( ndim > 0 ) {
        ndfSbnd( ndim, lbnd, ubnd, *ondf, &status );
    }

    /* Copy AXIS component, if needed. Note if axis == 1 this isn't needed,
     * scopy will have done the correct thing. Note need to do this after
     * dimensioning as axis size should match after that. */
    if ( axis > 1 && status == SAI__OK ) {

        ndfState( *ondf, "AXIS", &haveaxis, &status );
        if ( haveaxis ) {
            /*  AXIS component was copied. */

            /*  UNITS and LABEL. */
            for ( i = 0; i < 2; i++ ) {
                ndfAstat( indf, cc[i], axis, &exists, &status );
                if ( exists ) {
                    ndfAcget( indf, cc[i], axis, value, 132, &status );
                    ndfAcput( value, *ondf, cc[i], 1, &status );
                }
            }

            /*  CENTRE, WIDTH, VARIANCE */
            for ( i = 0; i < 3; i++ ) {
                ndfAstat( indf, c[i], axis, &exists, &status );
                if ( exists ) {
                    ndfAmap( indf, c[i], axis, "_DOUBLE", "READ", &inptr, &nel,
                             &status );
                    ndfAmap( *ondf, c[i], 1, "_DOUBLE", "WRITE", &outptr, &nel,
                             &status );
                    if ( status == SAI__OK ) {
                        /*  Offset raw cube by lower bound. */
                        inptr = ((double *)inptr) + lbnd[0] - 1;
                        memcpy( outptr, inptr, nel *  sizeof( double ) );
                    }
                    ndfAunmp( indf, "CENTRE", axis, &status );
                    ndfAunmp( *ondf, "CENTRE", 1, &status );
                }
            }
        }
    }

    /* And the data type */
    if ( type != NULL ) {
        ndfStype( type, *ondf, "DATA,VARIANCE", &status );
    }

    /* Insert the WCS, if given */
    if ( wcs != NULL ) {
        ndfPtwcs( wcs, *ondf, &status );
    }

    /* If an error occurred construct the message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return 0;
    }
    emsRlse();
    return 1;
}


/**
 *  =====================================
 *  Multiple NDFs per container interface
 *  =====================================
 */


/*
 *   Name:
 *      setState
 *
 *   Purpose:
 *      Set the state of an NDFinfo object.
 *
 *   Notes:
 *      Actually queries the NDF about the existence of components.
 */
static void setState( struct NDFinfo *state, int ndfid, const char *name,
                      int type, int nx, int ny, char *header,
                      int hlen, int *status )
{
   int ncomp = 1;
   int exists = 0;

   strncpy( state->name, name, MAXNDFNAME );
   state->ndfid = ndfid;
   state->type = type;
   state->nx = nx;
   state->ny = ny;
   state->readonly = 1;
   state->header = header;
   state->hlen = hlen;

   /*  Query NDF about other components */
   ndfState( ndfid, "Variance", &exists, status );
   if ( exists ) {
      ncomp++;
      state->havevar = 1;
   }
   else {
      state->havevar = 0;
   }

   ndfState( ndfid, "Quality", &exists, status );
   if ( exists ) {
      ncomp++;
      state->havequal = 1;
   }
   else {
      state->havequal = 0;
   }

   /*  No next structure yet */
   state->next = (NDFinfo *)NULL;

   /*  Number of components */
   state->ncomp = ncomp;
}

/*
 *  Name:
 *     getNDFInfo
 *
 *  Purpose:
 *     Locate NDF in the given info structure. Returns pointer, or
 *     NULL if not found.
 */
static NDFinfo *getNDFInfo( const void *handle, const int index )
{
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;
   int status = 1;

   if ( current ) {
      for ( count = 1; count < index; count++ ) {
         current = current->next;
         if ( current == NULL ) {
            status = 0;
            break;
         }
      }
      if ( status ) {
         return current;
      }
   }
   return NULL;
}

/*
 *  Name:
 *     gaiaInitMNDF
 *
 *  Purpose:
 *     Initialise access to an NDF and/or any related NDFs. This
 *     routine should be called before any others in the multiple
 *     image NDFs per container file interface.
 *
 *  Description:
 *     Categorise and store displayable information for the given NDF and
 *     any related ones.
 *
 *     Displayables in this context are either array components of the
 *     given NDF, or the components of any other NDFs which are stored at
 *     the same "level" (i.e. HDS path) within a HDS container file.
 *     This can also be extended to include searching of the NDF extension
 *     for other "related" NDFs. These are also limited to being
 *     children of an extension (i.e. no recursive deep searches).
 *
 *     The return from this function is a pointer to an initialised
 *     information structure about the NDFs and displayables
 *     available. If this fails then zero is returned.
 *
 */
int gaiaInitMNDF( const char *name, int deepsearch, void **handle,
                  char **error_mess )
{
    HDSLoc *baseloc = NULL;
    HDSLoc *tmploc = NULL;
    NDFinfo *currentinfo = NULL;
    NDFinfo *headinfo = NULL;
    NDFinfo *nextinfo = NULL;
    char *emess = NULL;
    char *filename = NULL;
    char *header = NULL;
    char *left = NULL;
    char *path = NULL;
    char *right = NULL;
    char slice[MAXNDFNAME];
    int baseid = 0;
    int height;
    int hlen;
    int ndfid;
    int status = SAI__OK;
    int type;
    int valid;
    int width;

    /*  Check that we're not going to have problems with the name
        length */
    if ( strlen( name ) > MAXNDFNAME ) {
        *error_mess = (char *) malloc( (size_t) MAXNDFNAME );
        sprintf( *error_mess, "NDF specification is too long "
                 "(limit is %d characters)", MAXNDFNAME );
        return 0;
    }

    /*  Mark the error stack */
    emsMark();

    /*  Attempt to open the given name as an NDF */
    if ( gaiaAccessNDF( name, &type, &width, &height, &header, &hlen,
                        &ndfid, &emess ) ) {

        /*  Name is an NDF. */
        baseid = ndfid;

        /*  Get the locator to the NDF and hence to its parent. */
        tmploc = NULL;
        ndfLoc( ndfid, "READ", &tmploc, &status );
        datParen( tmploc, &baseloc, &status );

        if ( status == DAT__OBJIN ) {

            /*  Means we're at the top level of the container file, so no
                parent available. */
            emsAnnul( &status );
            datClone( tmploc, &baseloc, &status );
        }

        /*  Store info about this NDF. Note path is set to "." */
        headinfo = (NDFinfo *) malloc( sizeof( NDFinfo ) );
        setState( headinfo, ndfid, ".", type, width, height, header, hlen,
                  &status );
        currentinfo = headinfo;
        nextinfo = headinfo;

        /*  Check for a slice. If found all components in this file will also
            have that slice applied. */
        left = strrchr( name, '(');
        right = strrchr( name, ')');
        if ( left && right ) {
            strcpy( slice, left );
        }
        else {
            slice[0] = '\0';
        }

        /*  If in error trap latest message for the report. */
        if ( status != SAI__OK ) {
            emess = gaiaUtilsErrMessage();
        }
        else {
            /*  If requested search NDF extensions for more related NDFs. */
            if ( deepsearch ) {
                currentinfo = traceMNDFExtensions( ndfid, &headinfo, slice );
            }
        }
    }
    else {
        /*  Not an NDF. May just be a HDS container name with NDFs at this
            level. Need to parse down to a filename and an HDS path. */
        filename = strdup( name );
        path = strstr( filename, DAT__FLEXT );
        if ( path ) {
            int i;
            for ( i = 0; i < DAT__SZFLX; i++ ) {
                *path++ = ' '; /* Have ".sdf" in name, cut it out. */
            }
        }
        path = strrchr( filename, '/' );  /* Last / in name */
        if ( ! path ) {
            path = strchr( filename, '.' );
        }
        else {
            path = strchr( path, '.' );
        }
        if ( path ) {
            *path = '\0';  /*  Remove "." from front, rest is HDS path. */
            path++;
        }

        /*  Attempt to open the file and obtain a base locator. */
        hdsOpen( filename, "READ", &tmploc, &status );

        /*  Now look for the object specified by the PATH. */
        if ( path && status == SAI__OK ) {
            hdsFind( tmploc, path, "READ", &baseloc, &status );
        }
        else {
            /*  No PATH, so re-use tmploc as baseloc. */
            datClone( tmploc, &baseloc, &status );
        }

        /*  If all is well, cancel pending error message (from initial
            attempt to open). */
        if ( status == SAI__OK && emess != NULL ) {
            free( emess );
            emess = NULL;
        }
        free( filename );

        /*  No slice for non-NDF specification. */
        slice[0] = '\0';
    }
    valid = 0;
    datValid( baseloc, &valid, &status );
    if ( valid && status == SAI__OK ) {

        /*  Look for additional NDFs at baseloc and append these to the
         *  info structs. If only one component, must be this NDF. */
        if ( headinfo == NULL ) {
            (void) traceMNDFs( &headinfo, baseloc, baseid, deepsearch, slice );
        }
        else {
            (void) traceMNDFs( &currentinfo, baseloc, baseid, deepsearch,
                               slice );
        }
    }
    else {

        /* Invalid locator with no error message? */
        if ( status == SAI__OK && ! emess ) {
            emess = strdup( "Invalid base locator for this NDF" );
        }

        /*  Initialisation failed (no such NDF, or container file/path
         *  doesn't have any NDFs in it).
         */
        if ( emess ) {
            *error_mess = emess;
            emess = NULL;
        }
        else {
            *error_mess = strdup( "Failed to locate any NDFs" );
        }
        emsRlse();
        return 0;
    }

    /*  Return list of NDFinfos */
    *handle = headinfo;
    emsRlse();

    /*  No error messages should get past here! */
    if ( emess ) {
        free( emess );
    }

    /*  Release locators */
    datAnnul( &baseloc, &status );
    datAnnul( &tmploc, &status );

    return 1;
}

/*
 *  Name:
 *     traceMNDFExtensions
 *
 *  Purpose:
 *     Looks for NDFs that are children of the extensions of an NDF and
 *     add their displayables to a multiple NDF info struct list.
 *
 */
NDFinfo *traceMNDFExtensions( int ndfid, NDFinfo **headinfo, const char *slice )
{
    HDSLoc *moreloc = NULL;
    NDFinfo *currentinfo;
    NDFinfo *nextinfo;
    char name[NDF__SZXNM+1];
    int i;
    int nmore = 0;
    int status = SAI__OK;

    emsMark();

    currentinfo = *headinfo;

    ndfXnumb( ndfid, &nmore, &status );
    if ( nmore > 0 && status == SAI__OK ) {
        for ( i = 1; i <= nmore; i++ ) {
            ndfXname( ndfid, i, name, NDF__SZXNM+1, &status );
            ndfXloc( ndfid, name, "READ", &moreloc, &status );
            nextinfo = traceMNDFs( &currentinfo, moreloc, NDF__NOID, 1,
                                   slice );
            currentinfo = nextinfo;
            datAnnul( &moreloc, &status );
        }
    }

    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();

    return currentinfo;
}

/*
 *  Name:
 *     traceMNDFs
 *
 *  Purpose:
 *     Given a locator to an NDF locate siblings and add to the multiple NDF
 *     structure given (this will be created if given NULL). Does work for
 *     gaiaInitMNDF. Exists so that we can recurse looking for NDFs in
 *     extensions. Returns the address of the last struct added, and (as the
 *     headptr value, the address of the first struct).
 *
 */
NDFinfo *traceMNDFs( NDFinfo **headptr, HDSLoc *baseloc, int baseid,
                     int deepsearch, const char *slice )
{
    HDSLoc *newloc = NULL;
    NDFinfo *laststate = NULL;
    NDFinfo *newstate = NULL;
    NDFinfo *state = NULL;
    char *emess = NULL;
    char *ftype = NULL;
    char *header = NULL;
    char *path = NULL;
    char ndffile[MAXNDFNAME];
    char ndfpath[MAXNDFNAME];
    int height;
    int hlen;
    int i;
    int isect;
    int level;
    int ncomp = 0;
    int ndfid;
    int same;
    int status = SAI__OK;
    int type;
    int width;

    emsMark();

    /* Initialise the result. */
    state = *headptr;
    laststate = NULL;

    datNcomp( baseloc, &ncomp, &status );
    if ( status != SAI__OK ) {
        ncomp = 0;
    }

    for ( i = 1; i <= ncomp; i++ ) {
        datIndex( baseloc, i, &newloc, &status );

        /*  Get full name of component and see if it is an NDF */
        hdsTrace( newloc, &level, ndfpath, ndffile, &status,
                  MAXNDFNAME, MAXNDFNAME );

        ftype = strstr( ndffile, DAT__FLEXT ); /* Strip .sdf from filename */
        if ( ftype ) *ftype = '\0';

        path = strstr( ndfpath, "." );   /* Now find first component */
        if ( path == NULL ) {            /* and remove it (not used as */
            strcat( ndffile, "." );      /* part of NDF name) */
            path = ndfpath;
        }
        strcat( ndffile, path );        /*  Join filename and HDS
                                            path to give NDF full
                                            name */
        if ( slice[0] != '\0' ) {
            strcat( ndffile, slice );    /*  Add the NDF slice, if set */
            strcat( path, slice );
        }

        /*  Attempt to open NDF to see if it exists (could check for
         *  DATA_ARRAY component) */
        emess = NULL;
        if ( gaiaAccessNDF( ndffile, &type, &width, &height,
                            &header, &hlen, &ndfid, &emess ) ) {

            /*  Check that this isn't the base NDF by another name */
            if ( ndfid != 0 && baseid != 0 ) {
                ndfSame( baseid, ndfid, &same, &isect, &status );
            }
            else {
                same = 0;
            }
            if ( ! same ) {
                /*  NDF exists so add details to the list of NDFs. */
                newstate = (NDFinfo *) malloc( sizeof( NDFinfo ) );
                if ( state != NULL ) {
                    state->next = newstate;
                    laststate = state;
                }
                else {
                    /*  No existing info structs, so start a list. */
                    *headptr = newstate;
                }
                state = newstate;
                setState( state, ndfid, path, type, width, height, header,
                          hlen, &status );

                /*  Search extension for NDFs, if requested. */
                if ( deepsearch ) {
                    state = traceMNDFExtensions( ndfid, &state, slice );
                }
            }
            else {
                /* Same NDF as before, release it and continue */
                gaiaFreeNDF( ndfid );
            }
        }
        if ( emess != NULL ) {
            free( emess );
            emess = NULL;
        }

        datAnnul( &newloc, &status );
        if ( status != SAI__OK ) {
            /*  Something very bad happened. Truncate the search and release
             *  current state (this may have forced something to be exhausted
             *  and stop further processing, so we're better off without
             *  it). */
            if ( laststate != NULL ) {
                if ( state != NULL ) {
                    gaiaReleaseMNDF( state );
                }
                state = laststate;
                state->next = NULL;
                fprintf( stderr, "WARNING: truncated number of NDFs accessed "
                         "to %d (found %d)\n", i, ncomp );
            }
            break;
        }
    }

    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
    return state;
}

/*
 *  Name:
 *     gaiaCheckMNDF
 *
 *  Purpose:
 *     See if the given NDF (specified by its index) contains the
 *     required component. If not found then a 0 is returned.
 */
int gaiaCheckMNDF( const void *handle, int index, const char *component )
{
   NDFinfo *current = NULL;
   int status = 1;

   /*  Get pointer to relevant NDF */
   current = getNDFInfo( handle, index );

   /*  See if the component exists (note ERROR is synonym for VARIANCE). */
   if ( current ) {
      switch ( component[0] ) {
         case 'd':
         case 'D': {
            status = 1;
         }
         break;

         case 'v':
         case 'V':
         case 'e':
         case 'E': {
            if ( current->havevar ) {
               status = 1;
            }
            else {
               status = 0;
            }
         }
         break;

         case 'q':
         case 'Q': {
            if ( current->havequal ) {
               status = 1;
            }
            else {
               status = 0;
            }
         }
         break;

         default: {
            status = 0;
         }
         break;
      }
   }
   else {

      /*  Bad NDF index */
      status = 0;
   }
   return status;
}

/*
 *  Name:
 *     gaiaGetIdMNDF
 *
 *  Purpose:
 *     Get NDF identifier for a particular NDF.
 */
int gaiaGetIdMNDF( const void *handle, int index  )
{
   NDFinfo *current = NULL;

   current = getNDFInfo( handle, index );
   if ( current ) {
       return current->ndfid;
   }
   return NDF__NOID;
}

/*
 *  Name:
 *     gaiaGetInfoMNDF
 *
 *  Purpose:
 *     Get information about a particular NDF.
 */
void gaiaGetInfoMNDF( const void *handle, int index, char **name,
                      int *type, int *width, int *height,
                      char **header, int *hlen, int *ndfid,
                      int *hasvar, int *hasqual )
{
   NDFinfo *current = NULL;

   current = getNDFInfo( handle, index );
   if ( current ) {
      *name = current->name;
      *type = current->type;
      *width = current->nx;
      *height = current->ny;
      *header = current->header;
      *hlen = current->hlen;
      *ndfid = current->ndfid;
      *hasvar = current->havevar;
      *hasqual = current->havequal;
   }
}

/*
 *  Name:
 *     gaiaGetMNDF
 *
 *  Purpose:
 *     Obtained a copy of an NDF data component.
 *
 *  Description:
 *     This routine obtains access to a named NDF data component and
 *     returns either a copy or a pointer to mapped memory depending
 *     the readonly state of the NDF.
 *
 *     If readonly is true then a mapped pointer is returned,
 *     otherwise a copy is made into some supplied memory (may want to
 *     offer malloc version?).
 */
int gaiaGetMNDF( const void *handle, int index, const char *component,
                 void **data, char **error_mess )
{
   NDFinfo *current = NULL;
   current = getNDFInfo( handle, index );

   if ( current ) {

      /*  Either copy the data or obtained a mapped pointer according
       *  to the readonly status */
       if ( current->readonly ) {
           return gaiaMapComponent( current->ndfid, data, component,
                                    "READ", error_mess );
       } else {
           return gaiaCopyComponent( current->ndfid, data, component,
                                     error_mess );
       }
   }

   /*  Arrive here only when NDF isn't available */
   *error_mess = strdup( "No such NDF is available" );
   return 0;
}

/*
 *  Name:
 *     gaiaSetReadMNDF
 *
 *  Purpose:
 *     Set the access method for the NDF data components.
 *     The value is 1 for readonly and 0 for writable memory.
 *
 *  Notes:
 *     Existing memory is not affected by this call, you will need to
 *     recall gaiaFreeMNDF and gaiaGetMNDF make the change.
 */
void gaiaSetReadMNDF( const void *handle, int index, int readonly )
{
   NDFinfo *current = getNDFInfo( handle, index );

   if ( current ) {
      current->readonly = readonly;
   }
}

/*
 *  Name:
 *     gaiaGetReadMNDF
 *
 *  Purpose:
 *     Get the access method for the NDF data components.
 *
 *  Notes:
 *     Existing memory is not affected by this call, you will need to
 *     recall gaiaFreeMNDF and gaiaGetMNDF make the change.
 */
int gaiaGetReadMNDF( const void *handle, int index )
{
   NDFinfo *current = getNDFInfo( handle, index );

   if ( current ) {
      return current->readonly;
   }
   return -1;
}

/*
 *  Name:
 *     gaiaCountMNDFs
 *
 *  Purpose:
 *     Return the number of NDFs available.
 */
int gaiaCountMNDFs( const void *handle )
{
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;

   if ( current ) {
      for ( ; current->next; current = current->next ) count++;
      count++;
   }
   return count;
}

/*
 *  Name:
 *     gaiaReleaseMNDF
 *
 *  Purpose:
 *     Release all NDF resources associated with given handle.
 */
void gaiaReleaseMNDF( const void *handle )
{
   NDFinfo *current = (NDFinfo *) handle;
   NDFinfo *next = NULL;

   do {
       /* Free NDF and header block */
       gaiaFreeNDF( current->ndfid );
       cnfFree( current->header );

       /* Free the NDFinfo structure that stored these */
       next = current->next;
       free( current );

       /* And process next in chain */
       current = next;
   }
   while( current != NULL );
}

/*
 *  Name:
 *     gaiaFreeMNDF
 *
 *  Purpose:
 *     Release components accessed for an NDF.
 */
void gaiaFreeMNDF( void *handle, int index )
{
   int status = SAI__OK;

   /*  Access the appropriate NDF information structure */
   NDFinfo *current = getNDFInfo( handle, index );

   if ( current ) {
      emsMark();
      ndfUnmap( current->ndfid, "*", &status );
      emsRlse();
   }
}

/*
 *  Name:
 *     gaiaCloneMNDF
 *
 *  Purpose:
 *     Create a copy of an NDFinfo handle structure with cloned NDF
 *     identifiers and header copy. Use this when independent access
 *     of the container file is required (i.e. when you need to map in
 *     more than one NDF in a single container file at a time).
 *
 *     Obviously you should avoid mixed mode access to this file
 *     otherwise treat it as independent (i.e. close it sometime).
 */
void *gaiaCloneMNDF( const void *handle )
{
    NDFinfo *current = (NDFinfo *) handle;
    NDFinfo *newInfo = NULL;
    NDFinfo *newState = NULL;
    NDFinfo *lastState = NULL;
    int first = 1;
    int status = SAI__OK;

    if ( current ) {
        for ( ; current; current = current->next ) {

            /*  Make a copy of the current NDFinfo structure. */
            newState = (NDFinfo *) malloc( sizeof( NDFinfo ) );
            memcpy( newState, current, sizeof( NDFinfo ) );

            /*  Clone it's NDF identifier. */
            ndfClone( current->ndfid, &(newState->ndfid), &status );

            /*  And copy the headers. */
            newState->header = cnfCreat( current->hlen * 80 );
            memcpy( newState->header, current->header,
                    current->hlen * 80  );

            /*  And add this to the new super structure. */
            if ( first ) {
                newInfo = newState;
                first = 0;
            } else {
                lastState->next = newState;
            }
            lastState = newState;
        }
    }
    return newInfo;
}

/*
 *  =============================================
 *  Straight-forward NDF access, with no 2D bias.
 *  =============================================
 *
 *  These are the "cube" and "spectral" access routines. Note these all return
 *  0 for success and 1 for failure to match the TCL_OK and TCL_ERROR values.
 */

/**
 * Name:
 *    gaiaNDFOpen
 *
 * Purpose:
 *    Open an NDF and return the identifier.
 */
int gaiaNDFOpen( char *ndfname, int *ndfid, char **error_mess )
{
    int place;
    int status = SAI__OK;

    emsMark();
    ndfOpen( NULL, ndfname, "READ", "OLD", ndfid, &place, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        ndfid = NDF__NOID;
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFClose
 *
 * Purpose:
 *    Close an NDF.
 */
int gaiaNDFClose( int *ndfid )
{
    int status = SAI__OK;
    emsMark();
    ndfAnnul( ndfid, &status );
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFType
 *
 * Purpose:
 *    Query the data type of a component.
 */
int gaiaNDFType( int ndfid, const char* component, char *type,
                 int type_length, char **error_mess )
{
   int status = SAI__OK;

   emsMark();
   ndfType( ndfid, component, type, type_length, &status );

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
       *error_mess = gaiaUtilsErrMessage();
       emsRlse();
       return TCL_ERROR;
   }
   emsRlse();
   return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFCGet
 *
 * Purpose:
 *    Query a character component (label, units, title).
 */
int gaiaNDFCGet( int ndfid, const char* component, char *value,
                 int value_length, char **error_mess )
{
   int status = SAI__OK;

   emsMark();
   ndfCget( ndfid, component, value, value_length, &status );

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
       *error_mess = gaiaUtilsErrMessage();
       emsRlse();
       return TCL_ERROR;
   }
   emsRlse();
   return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFCPut
 *
 * Purpose:
 *    Set the value of a character component (label, units, title).
 */
int gaiaNDFCPut( int ndfid, const char* component, const char *value,
                 char **error_mess )
{
    int status = SAI__OK;

    emsMark();
    ndfCput( value, ndfid, component, &status );

    /* If an error occurred return an error message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFMap
 *
 * Purpose:
 *    Map an NDF component with a given data type. Returns data and number of
 *    elements.
 */
int gaiaNDFMap( int ndfid, char *type, const char *access,
                const char* component, void **data, int *el,
                char **error_mess )
{
   int status = SAI__OK;
   void *ptr[1];

   /* Do the mapping */
   emsMark();
   ndfMap( ndfid, component, type, access, ptr, el, &status );
   *data = ptr[0];

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
       *error_mess = gaiaUtilsErrMessage();
       ndfid = NDF__NOID;
       emsRlse();
       return TCL_ERROR;
   }
   emsRlse();
   return TCL_OK;
}

/*
 * Name:
 *    gaiaNDFUnmap
 *
 * Purpose:
 *    Unmap the named NDF component.
 */
int gaiaNDFUnmap( int ndfid, const char *component, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    ndfUnmap( ndfid, component, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}


/**
 * Name:
 *    gaiaNDFGtWcs
 *
 * Purpose:
 *    Get the AST frameset that defines the NDF WCS. Returns the address of
 *    the frameset.
 *
 *    To match the behaviour in KAPPA (and the Fortran part of GAIA), if the
 *    WCS component isn't present we look for a FITS WCS and attempt to use
 *    that. If that fails a default NDF WCS is returned.
 */
int gaiaNDFGtWcs( int ndfid, AstFrameSet **iwcs, char **error_mess )
{
    AstFrameSet *fitswcs;
    AstPermMap *permMap;
    HDSLoc *fitsloc = NULL;
    double zero[1];
    int ndfaxes;
    int fitsaxes;
    int exists = 0;
    int i;
    int inperm[NDF__MXDIM];
    int outperm[NDF__MXDIM];
    int status = SAI__OK;
    size_t ncard = 0;
    void *fitsptr = NULL;
    int ndfframes;
    int fitscurrent;

    emsMark();

    /* Check if the WCS component exists */
    ndfState( ndfid, "WCS", &exists, &status );

    /* Read it, we always need it */
    ndfGtwcs( ndfid, iwcs, &status );

    /* If there's no WCS, we just have the PIXEL, AXES and GRID domains, there
     * may be some addition WCS information in the FITS extension, see if we
     * can use that. */
    if ( ! exists ) {
        ndfXstat( ndfid, "FITS", &exists, &status );
        if ( exists ) {
            ndfXloc( ndfid, "FITS", "READ", &fitsloc, &status );
            datMapV( fitsloc, "_CHAR*80", "READ", &fitsptr, &ncard, &status );
            if ( gaiaUtilsGtFitsWcs( (char *) fitsptr, (int) ncard,
                                     NULL, &fitswcs ) ) {

                /* Read a WCS, we need to join this to the NDF WCS via the
                 * base frame. This will use a UnitMap, extra dimensions will
                 * be just thrown away using a PermMap (or could give up?). */
                ndfaxes = astGetI( *iwcs, "Nin" );
                fitsaxes = astGetI( fitswcs, "Nin" );

                for ( i = 0; i < ndfaxes; i++ ) {
                    inperm[i] = i + 1;
                    outperm[i] = i + 1;
                }

                if ( fitsaxes > ndfaxes ) {
                    for ( i = ndfaxes; i < fitsaxes; i++ ) {
                        inperm[i] = -1;
                        outperm[i] = -1;
                    }
                }
                else if ( fitsaxes < ndfaxes ) {
                    for ( i = fitsaxes; i < ndfaxes; i++ ) {
                        inperm[i] = -1;
                        outperm[i] = -1;
                    }
                }

                /* Use PermMap to join FITS base frame to NDF base frame */
                zero[0] = 0.0;
                permMap = astPermMap( fitsaxes, inperm, ndfaxes, outperm,
                                      zero, " " );

                ndfframes = astGetI( *iwcs, "Nframe" );
                fitscurrent = astGetI( fitswcs, "Current" );

                /* Join the base frames together (GRID to GRID) */
                astInvert( fitswcs );
                astAddFrame( *iwcs, AST__BASE, permMap, fitswcs );

                /* Current frame of FITS WCS becomes the Current frame */
                astSetI( *iwcs, "Current", ndfframes + fitscurrent );
                fitswcs = (AstFrameSet *) astAnnul( fitswcs );
           }
           datAnnul( &fitsloc, &status );
       }
   }

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
       *error_mess = gaiaUtilsErrMessage();
       *iwcs = (AstFrameSet *) NULL;
       emsRlse();
       return TCL_ERROR;
   }
   emsRlse();
   return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFGtAxisWcs
 *
 * Purpose:
 *    Get an AST frameset that describes the coordinates of a given axis.
 *    Axes are in the AST sense, i.e. start at 1.
 */
int gaiaNDFGtAxisWcs( int ndfid, int axis, AstFrameSet **iwcs,
                      char **error_mess )
{
   AstFrameSet *fullwcs;
   int result;

   if ( gaiaNDFGtWcs( ndfid, &fullwcs, error_mess ) == 1 ) {
       return TCL_ERROR;
   }
   result = gaiaUtilsGtAxisWcs( fullwcs, axis, 0, iwcs, error_mess );
   fullwcs = (AstFrameSet *) astAnnul( fullwcs );
   return result;
}

/**
 * Name:
 *    gaiaNDFQueryDims
 *
 * Purpose:
 *    Query the dimensions of an NDF.
 */
int gaiaNDFQueryDims( int ndfid, int ndimx, int dims[], int *ndim,
                      char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    ndfDim( ndfid, ndimx, dims, ndim, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        ndfid = NDF__NOID;
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFQueryBounds
 *
 * Purpose:
 *    Query the bounds of an NDF.
 */
int gaiaNDFQueryBounds( int ndfid, int ndimx, int lbnd[], int ubnd[],
                        int *ndim, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    ndfBound( ndfid, ndimx, lbnd, ubnd, ndim, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        ndfid = NDF__NOID;
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFExists
 *
 * Purpose:
 *    Determine whether an NDF component exists.
 */
int gaiaNDFExists( int ndfid, const char *component, int *exists,
                   char **error_mess )
{
    int status = SAI__OK;

    emsMark();
    ndfState( ndfid, component, exists, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        exists = 0;
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFGetFitsChan
 *
 * Purpose:
 *    Return an existing FITS extension of an NDF as an AST FITS channel.
 *    Returns an empty channel if the extension doesn't exist.
 */
int gaiaNDFGetFitsChan( int ndfid, AstFitsChan **fitschan, char **error_mess )
{
    int status = SAI__OK;
    int exists;
    void *fitsptr = NULL;
    size_t ncards;
    HDSLoc *fitsloc = NULL;

    emsMark();
    ndfXstat( ndfid, "FITS", &exists, &status );
    if ( exists ) {
        ndfXloc( ndfid, "FITS", "READ", &fitsloc, &status );
        datMapV( fitsloc, "_CHAR*80", "READ", &fitsptr, &ncards, &status );
        if ( ! gaiaUtilsGtFitsChan( fitsptr, ncards, fitschan ) ) {
            if ( status == SAI__OK ) {
                *error_mess = strdup( "Failed to read NDF FITS headers" );
            }
        }
        datAnnul( &fitsloc, &status );
    }
    else {
        *fitschan = astFitsChan( NULL, NULL, " " );
    }
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFWriteFitsChan
 *
 * Purpose:
 *    Write a new FITS extension to an NDF using the contents of a FITS
 *    channel.
 */
int gaiaNDFWriteFitsChan( int ndfid, AstFitsChan *fitschan, char **error_mess )
{
    HDSLoc *fitsloc = NULL;
    char card[81];
    int i;
    int ncard[1];
    int status = SAI__OK;
    size_t ncards;
    void *fitsptr = NULL;

    emsMark();

    /*  How many cards */
    ncard[0] = astGetI( fitschan, "Ncard" );
    if ( ncard[0] == 0 ) {
        /* Nothing to do */
        return TCL_OK;
    }

    /*  Erase the current extension, if it exists. */
    ndfXdel( ndfid, "FITS", &status );

    /*  Create a new with the correct size */
    ndfXnew( ndfid, "FITS", "_CHAR*80", 1, ncard, &fitsloc, &status );

    /*  And map it in */
    datMapV( fitsloc, "_CHAR*80", "WRITE", &fitsptr, &ncards, &status );

    /*  Write the FITS channel to it */
    if ( status == SAI__OK ) {
        astClear( fitschan, "Card" );
        for ( i = 0; i < ncards; i++ ) {
            astFindFits( fitschan, "%f", card, 1 );
            memcpy( fitsptr, card, 80 );
            fitsptr += 80;
        }

    }

    /*  Free locator (and flush to disk). */
    datAnnul( &fitsloc, &status );

    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFCanWrite
 *
 * Purpose:
 *    Return whether an NDF has been opened with write access.
 */
int gaiaNDFCanWrite( int ndfid )
{
    int status = SAI__OK;
    int writable = 0;

    emsMark();
    ndfIsacc( ndfid, "WRITE", &writable, &status );
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
    return writable;
}

/**
 * Name:
 *    gaiaNDFExtensionExists
 *
 * Purpose:
 *    Return whether an extension exists.
 */
int gaiaNDFExtensionExists( int ndfid, const char *name )
{
    int status = SAI__OK;
    int exists;

    emsMark();
    ndfXstat( ndfid, name, &exists, &status );
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
    return exists;
}

/**
 * Name:
 *    gaiaNDFGetProperty
 *
 * Purpose:
 *     Get the value of a primitive stored in an extension.
 */
int gaiaNDFGetProperty( int ndfid, const char *extension, const char *name,
                        char *value, int value_length, char **error_mess )
{
    int status = SAI__OK;
    int exists;

    emsMark();

    /* If the value doesn't exist, return a blank */
    value[0] = ' ';
    value[1] = '\0';

    /* Get value */
    ndfXstat( ndfid, extension, &exists, &status );
    if ( exists ) {
        ndfXgt0c( ndfid, extension, name, value, value_length, &status );
    }

    /* If an error occurred return an error message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFGetDoubleProperty
 *
 * Purpose:
 *    Get the value of a double precision primitive stored in an extension.
 */
int gaiaNDFGetDoubleProperty( int ndfid, const char *extension,
                              const char *name, double *value,
                              char **error_mess )
{
    int status = SAI__OK;
    int exists;

    emsMark();

    /* If the value doesn't exist, return VAL__BADD */
    *value = VAL__BADD;

    /* Get value */
    ndfXstat( ndfid, extension, &exists, &status );
    if ( exists ) {
        ndfXgt0d( ndfid, extension, name, value, &status );
    }

    /* If an error occurred return an error message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/**
 * Name:
 *    gaiaNDFGetPropertyDims
 *
 * Purpose:
 *    Get the dimensions of a component stored in an extension. The dimensions
 *    are returned in the dims array which should be at least NDF__MXDIM.
 */
int gaiaNDFGetPropertyDims( int ndfid, const char *extension,
                            const char *name, hdsdim dims[], int *ndim,
                            char **error_mess )
{
    HDSLoc *extloc = NULL;
    HDSLoc *comloc = NULL;
    int exists;
    int i;
    int status = SAI__OK;

    emsMark();

    *ndim = 0;
    for ( i = 0; i < NDF__MXDIM; i++ ) {
        dims[i] = 1;
    }

    ndfXstat( ndfid, extension, &exists, &status );
    if ( exists ) {
        ndfXloc( ndfid, extension, "READ", &extloc, &status );
        hdsFind( extloc, name, "READ", &comloc, &status );
        datShape( comloc, NDF__MXDIM, dims, ndim, &status );
        datAnnul( &extloc, &status );
        datAnnul( &comloc, &status );
    }

    /* If an error occurred return an error message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();
    return TCL_OK;
}

/*
 * Name:
 *    gaiaNDFFindChild
 *
 * Purpose:
 *    Search an HDS path for any children that are NDFs and return an
 *    identifier to the first one. Usually name is the name of an HDS
 *    container file being presented for opening.
 */
int gaiaNDFFindChild( const char *name, int *ndfid, char **error_mess )
{
    HDSLoc *baseloc = NULL;
    HDSLoc *newloc = NULL;
    HDSLoc *tmploc = NULL;
    char *filename;
    char *ftype;
    char *path = NULL;
    char ndffile[MAXNDFNAME];
    char ndfpath[MAXNDFNAME];
    int i;
    int level;
    int ncomp = 0;
    int status = SAI__OK;

    /*  Check that we're not going to have problems with the name
        length */
    if ( strlen( name ) > MAXNDFNAME ) {
        *error_mess = (char *) malloc( (size_t) MAXNDFNAME );
        sprintf( *error_mess, "NDF specification is too long "
                 "(limit is %d characters)", MAXNDFNAME );
        return 0;
    }

    /*  Mark the error stack */
    emsMark();
    *error_mess = NULL;

    /*  Need to parse down to a filename and an HDS path. */
    filename = strdup( name );
    path = strstr( filename, DAT__FLEXT );
    if ( path ) {
      for ( i = 0; i < DAT__SZFLX; i++ ) {
            *path++ = ' '; /* Have ".sdf" in name, cut it out. */
        }
    }
    path = strrchr( filename, '/' );  /* Last / in name */
    if ( ! path ) {
        path = strchr( filename, '.' );
    }
    else {
        path = strchr( path, '.' );
    }
    if ( path ) {
        *path = '\0';  /*  Remove "." from filename, rest is HDS path */
        path++;
    }

    /*  Attempt to open the file and obtain a base locator. */
    hdsOpen( filename, "READ", &tmploc, &status );

    /*  Now look for the object specified by the PATH */
    if ( path ) {
        hdsFind( tmploc, path, "READ", &baseloc, &status );
    }
    else {
        datClone( tmploc, &baseloc, &status );
    }

    /*  Look for additional NDFs at baseloc. */
    datNcomp( baseloc, &ncomp, &status );
    if ( status == SAI__OK ) {
        for ( i = 1; i <= ncomp; i++ ) {
            *ndfid = NDF__NOID;
            datIndex( baseloc, i, &newloc, &status );

            /*  Get full name of component and see if it is an NDF */
            hdsTrace( newloc, &level, ndfpath, ndffile, &status,
                      MAXNDFNAME, MAXNDFNAME );

            ftype = strstr( ndffile, DAT__FLEXT ); /* Strip .sdf from filename */
            if ( ftype ) *ftype = '\0';

            path = strstr( ndfpath, "." );  /* Now find first component */
            if ( path == NULL ) {           /* and remove it (not used as */
                strcat( ndffile, "." );     /* part of NDF name) */
                path = ndfpath;
            }
            strcat( ndffile, path );        /*  Join filename and HDS path to
                                             *  give NDF full name */

            /*  Attempt to open NDF to see if it exists */
            if ( gaiaNDFOpen( ndffile, ndfid, error_mess ) == TCL_OK ) {
                datAnnul( &newloc, &status );
                break;
            }
            if ( *error_mess ) {
                free( *error_mess );
                *error_mess = NULL;
            }

            datAnnul( &newloc, &status );
        }

        /*  Release locators */
        datAnnul( &baseloc, &status );
        datAnnul( &tmploc, &status );
    }
    else {
        *ndfid = NDF__NOID;
    }

    /* If an error occurred return an error message */
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return TCL_ERROR;
    }
    emsRlse();

    /*  Return bad if no child NDF was found */
    if ( *ndfid == NDF__NOID ) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 * Name:
 *    gaiaNDFSiblingSearch
 *
 * Purpose:
 *    Search an HDS container file for sibling NDFs. Siblings are at the same
 *    HDS level as the given NDF and may include any NDFs stored in the NDF's
 *    extensions, if requested (deepsearch is true). The number of sibling
 *    located and a Tcl list of their basic properties is returned (HDS path
 *    and dimensionality followed by the first three dimensions).
 */
int gaiaNDFSiblingSearch( int ndfid, int deepsearch, int *nsiblings,
                          char **props )
{
    HDSLoc *baseloc = NULL;
    HDSLoc *ndfloc = NULL;
    Tcl_DString dsPtr;
    char buffer[EMS__SZMSG+1];
    char name[DAT__SZNAM+3];
    int added;
    int dims[NDF__MXDIM];
    int ndims;
    int status = SAI__OK;

    /*  Mark the error stack */
    emsMark();

    /*  Get the locator to the NDF. */
    ndfloc = NULL;
    ndfLoc( ndfid, "READ", &ndfloc, &status );

    /*  Create a string for the Tcl list of properties. The first entry is
     *  for the given NDF. Use Tcl_DString. */
    Tcl_DStringInit( &dsPtr );

    /*  Gather basic information. */
    ndfDim( ndfid, NDF__MXDIM, dims, &ndims, &status );

    /*  Get locator to the NDF parent. */
    datParen( ndfloc, &baseloc, &status );
    if ( status == DAT__OBJIN ) {
        /* No parent, means at toplevel, so no siblings and no path. */
        name[0] = '\0';
        baseloc = NULL;
        emsAnnul( &status );
    }
    else {
        datName( ndfloc, name, &status );
    }
    *nsiblings = 1; /* self */
    added = sprintf( buffer, "{{%d} {NDF} {.%s} {%d} {%d} {%d} {%d}} ",
                     *nsiblings, name, ndims, dims[0], dims[1], dims[2] );
    Tcl_DStringAppend( &dsPtr, buffer, added );

    /*  Check this NDF for related NDFs in its extensions. */
    if ( deepsearch ) {
        buffer[0] = '.'; buffer[1] = '\0';
        if ( baseloc == NULL ) {
            strcat( buffer, "MORE." );
        }
        else {
            strcat( buffer, name );
            strcat( buffer, ".MORE." );
        }
        traceSiblingExtensions( ndfid, &dsPtr, buffer, nsiblings );
    }

    /*  Look for additional NDFs at baseloc or ndfloc if at top-level. */
    if ( status == SAI__OK ) {
        if ( baseloc == NULL ) {
            traceSiblings( ndfloc, ndfid, deepsearch, &dsPtr, "", nsiblings );
        }
        else {
            traceSiblings( baseloc, ndfid, deepsearch, &dsPtr, "", nsiblings );
        }
    }

    /*  Get final result. */
    *props = (char *) malloc( Tcl_DStringLength( &dsPtr ) + 1 );
    strncpy( *props, Tcl_DStringValue( &dsPtr ),
             Tcl_DStringLength( &dsPtr ) + 1 );
    Tcl_DStringFree( &dsPtr );

    datAnnul( &ndfloc, &status );
    if ( baseloc != NULL ) {
        datAnnul( &baseloc, &status );
    }
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }

    emsRlse();
    return TCL_OK;
}

/*
 * Name:
 *    traceSiblingExtensions
 *
 * Purpose:
 *    Looks for NDFs that are children of the extensions of an NDF and
 *    add their displayables to the list of displayables.
 *
 */
void traceSiblingExtensions( int ndfid, Tcl_DString *dsPtr, const char *prefix,
                             int *nsiblings )
{
    HDSLoc *moreloc = NULL;
    char name[NDF__SZXNM+1];
    char buf[EMS__SZMSG+1];
    int i;
    int nmore = 0;
    int status = SAI__OK;

    emsMark();

    ndfXnumb( ndfid, &nmore, &status );
    if ( nmore > 0 && status == SAI__OK ) {
        for ( i = 1; i <= nmore; i++ ) {
            ndfXname( ndfid, i, name, NDF__SZXNM+1, &status );
            ndfXloc( ndfid, name, "READ", &moreloc, &status );
            strcpy( buf, prefix );
            strcat( buf, name );
            traceSiblings( moreloc, ndfid, 1, dsPtr, buf, nsiblings );
            datAnnul( &moreloc, &status );
        }
    }

    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
}

/*
 *  Do the work for gaiaNDFSiblingSearch by tracing the properties of any
 *  siblings to the main NDF. Also searches for additional NDFs in the
 *  extensions (mainly exists so that this can be done recursively).
 */
void traceSiblings( HDSLoc *baseloc, int ndfid, int deepsearch,
                    Tcl_DString *dsPtr, const char *prefix, int *nsiblings )
{
    HDSLoc *newloc = NULL;
    char buffer[EMS__SZMSG+1];
    char name[DAT__SZNAM+1];
    char namedprefix[EMS__SZMSG+1];
    int added;
    int dims[NDF__MXDIM];
    int i;
    int isect;
    int ncomp = 0;
    int ndims;
    int newid;
    int place;
    int same;
    int status = SAI__OK;
    static const char *fmt = "{{%d} {NDF} {%s.%s} {%d} {%d} {%d} {%d}} ";

    emsMark();

    datNcomp( baseloc, &ncomp, &status );

    if ( status == SAI__OK ) {
        for ( i = 1; i <= ncomp; i++ ) {
            newloc = NULL;
            datIndex( baseloc, i, &newloc, &status );

            /*  See if newloc is an NDF by attempting to open it. */
            ndfOpen( newloc, "", "READ", "OLD", &newid, &place, &status );
            if ( status == SAI__OK ) {

                /*  Don't include self twice. */
                ndfSame( ndfid, newid, &same, &isect, &status );
                if ( ! same ) {
                    (*nsiblings)++;

                    /* Add description. */
                    ndfDim( newid, NDF__MXDIM, dims, &ndims, &status );
                    datName( newloc, name, &status );
                    added = sprintf( buffer, fmt, *nsiblings, prefix, name,
                                     ndims, dims[0], dims[1], dims[2] );
                    Tcl_DStringAppend( dsPtr, buffer, added );

                    /*  Check this NDF any extensions. */
                    if ( deepsearch ) {
                        namedprefix[0] = '\0';
                        strcpy( namedprefix, prefix );
                        strcat( namedprefix, "." );
                        strcat( namedprefix, name );
                        strcat( namedprefix, ".MORE." );
                        traceSiblingExtensions( newid, dsPtr, namedprefix,
                                                nsiblings );
                    }
                }
                ndfAnnul( &newid, &status );
            }
            datAnnul( &newloc, &status );
            if ( status != SAI__OK ) {
                emsAnnul( &status );
            }
        }
    }
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();
}

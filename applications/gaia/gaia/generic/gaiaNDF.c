/*+
 *   Name:
 *      gaiaNDF
 *
 *   Purpose:
 *      Primitive access to NDFs for GAIA.
 *
 *   Language:
 *      C
 *
 * Notes:
 *      These routines initially existed as using CNF to call
 *      Fortran from C++ seemed very difficult (CNF preprocessor
 *      problems), but they are now the C interface to the NDF
 *      functions that GAIA requires.
 *
 *  Copyright:
 *     Copyright (C) 2000-2001 Central Laboratory of the Research Councils
 *
 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham
 *
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
 *      {enter_changes_here}
 *-
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <float.h>
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems_par.h"
#include "ems.h"
#include "dat_par.h"
#define DAT__OBJIN 147358563  /* Not available in HDS include files */
#include "ndf.h"
#include "gaiaNDF.h"

/*  Maximum number of pixels to copy during chunking. */
#define MXPIX 2000000

/*
 *  Structure to store information about an NDF.
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

/*  Local prototypes */
static void datAnnul( const char *loc, int *status );

static void hdsOpen( const char *name, const char *mode, char *loc,
                     int *status );

static void hdsTrace( const char *loc, int *level, char *path,
                      int path_len, char *file, int file_len,
                      int *status );

static void datFind( const char *loc1, const char *path, char *loc2,
                     int *status );

static void datParen( const char *loc1, char *loc2, int *status );

static void datNcomp( const char *loc, int *ncomp, int *status );

static void datIndex( const char *loc1, int index, char *loc2,
                      int *status );

static void datClone( const char *loc1, char *loc2, int *status );

static void hdsShow( const char *name, int *status );

static char *errMessage( int *status );

static int datValid( const char *loc, int *status );

/*
 *  Name:
 *     errMessage
 *
 *  Purpose:
 *     Copy the current EMS error message into a dynamic string for
 *     returning. Make sure status is set before calling this and
 *     release the memory used for the string when complete. On
 *     return status is reset to SAI__OK and the error stack is
 *     empty.
 */
static char *errMessage( int *status )
{
    char buffer[EMS__SZMSG+1];
    char param[EMS__SZPAR+1];
    char *result_message = NULL;
    char *result_copy = NULL;
    int status_copy;
    int buffer_length = 0;
    int mess_length = 0;
    int param_length = 0;
    int result_length = 0;

    if ( status != SAI__OK ) {
        /* Need to get error message in one pass. Since we don't know how
           big this is going to be we need to realloc memory used */

        /* Recover error status from EMS */
        emsStat( &status_copy );

        /* Loop until all error has been read */
        while ( status_copy != SAI__OK ) {

            /* Load error message and start reading it */
            emsEload( param, &param_length, buffer, &buffer_length,
                      &status_copy );

            /* If all read then stop */
            if ( status_copy != SAI__OK ) {

                /* Else initialise or increase the result buffer to
                   make room for new part, plus 2 for newline and
                   null. */
                mess_length += ( buffer_length + 2 );
                result_copy = (char *) realloc( (void *) result_message,
                                                (size_t) mess_length );
                if ( result_copy == NULL ) {

                    /* Realloc failed so use the fragment already
                       recovered in  result_message */
                    status_copy = SAI__OK;
                }
                else {
                    /* Realloc suceeded */
                    result_message = result_copy;

                    /* Add a newline to end of string to wrap last
                       line, unless this is first, in which initialise
                       it */
                    if ( mess_length == ( buffer_length + 2 ) ) {
                        result_message[0] = '\0';
                    }
                    else {
                        strcat( result_message, "\n" );
                    }

                    /* Concatenate buffer into result string */
                    strncat( result_message, buffer, buffer_length );
                }
            }
        }
    }
    else {
        result_message = strdup( "Bad status value (gaiaNDF:errMessage)" );
    }
    return result_message;
}

/*
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
           *error_mess = errMessage( &status );
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

/*
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
      *error_mess = errMessage( &status );
      emsRlse();
      return 0;
   }
   emsRlse();
   return 1;
}

/*
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

/*
 *   Name:
 *      gaiaCopyNDF
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
    *  data.
    */
   if ( strncmp( dtype, "_DOUBLE", 7 ) == 0 ) {
      double *fromPtr;
      double *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (double *)ptr[0];
         for( j = 0; j < el; j++ ) {
             *toPtr++ = *fromPtr++;
         }
      }
   }
   else if ( strncmp( dtype, "_REAL", 5 ) == 0 ) {
      float *fromPtr;
      float *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (float *)ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   }
   else if ( strncmp( dtype, "_INTEGER", 8 ) == 0 ) {
      int *fromPtr;
      int *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (int *)ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   }
   else if ( strncmp( dtype, "_WORD", 5 ) == 0 ||
               strncmp( dtype, "_UWORD", 6 ) == 0 ) {
      unsigned short *fromPtr;
      unsigned short *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (unsigned short *) ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   }
   else if ( strncmp( dtype, "_BYTE", 5 ) == 0 ) {

      /*  Cannot represent this type, so mapping is to short */
      unsigned char *fromPtr;
      unsigned short *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (unsigned char *) ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = (unsigned short) *fromPtr++;
         }
      }
   }
   else if ( strncmp( dtype, "_UBYTE", 6 ) == 0 ) {

      unsigned char *fromPtr;
      unsigned char *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (unsigned char *) ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   }

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
      *error_mess = errMessage( &status );
      ndfEnd( &status );
      emsRlse();
      return 0;
   }
   ndfEnd( &status );
   emsRlse();
   return 1;
}

/*
 *   Name:
 *      gaiaMapComponent
 *
 *   Purpose:
 *      Map an NDF data component for READ access, returning a pointer
 *      to the mapped memory. The memory must be unmapped either
 *      directly or by annuling the NDF identifier, before the program
 *      exits.
 *
 *   Note:
 *      If the data values require modification, then a copy should be
 *      made instead.
 */
int gaiaMapComponent( int ndfid, void **data, const char* component,
                char **error_mess )
{
   char dtype[NDF__SZTYP+1];
   int el;
   int status = SAI__OK;
   void *ptr[1];

   /* Get the type of the NDF component. */
   emsMark();
   ndfBegin();
   ndfType( ndfid, component, dtype, NDF__SZTYP+1, &status );

   /*  Trap _BYTE and map _WORD */
   if ( strncmp( dtype, "_BYTE", 7 ) == 0 ) {
      strcpy( dtype, "_WORD" );
   }
   ndfMap( ndfid, component, dtype, "READ", ptr, &el, &status );
   *data = ptr[0];

   /* If an error occurred return an error message */
   if ( status != SAI__OK ) {
      *error_mess = errMessage( &status );
      ndfEnd( &status );
      emsRlse();
      return 0;
   }
   ndfEnd( &status );
   emsRlse();
   return 1;
}

/*
 *   Name:
 *      gaiaNDFUnmap
 *
 *   Purpose:
 *      Unmaps the named NDF component.
 *
 *   Notes:
 *      Component name can be "*" which unmaps everything.
 */
int gaiaNDFUnmap( int ndfid, const char *component, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    ndfUnmap( ndfid, component, &status );
    if ( status != SAI__OK ) {
        *error_mess = errMessage( &status );
        ndfEnd( &status );
        emsRlse();
        return 0;
    }
    ndfEnd( &status );
    emsRlse();
    return 1;
}

/*  ===================================== */
/*  Multiple NDFs per container interface */
/*  ===================================== */

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
 *     NDFs per container file interface.
 *
 *  Description:
 *     Categorise and store displayable information for the given NDF and
 *     any related ones.
 *
 *     Displayables in this context are either array components of the
 *     given NDF, or the components of any other NDFs which are stored at
 *     the same "level" (i.e. HDS path) within a HDS container file.
 *
 *     The return from this function is a pointer to an initialised
 *     information structure about the NDFs and displayables
 *     available. If this fails then zero is returned.
 *
 */
int gaiaInitMNDF( const char *name, void **handle, char **error_mess )
{
    NDFinfo *head = (NDFinfo *) NULL;
    NDFinfo *newstate = (NDFinfo *) NULL;
    NDFinfo *state = (NDFinfo *) NULL;
    char *emess = NULL;
    char *filename = NULL;
    char *ftype = NULL;
    char *header = NULL;
    char *left = NULL;
    char *path = NULL;
    char *right = NULL;
    char baseloc[DAT__SZLOC];
    char ndffile[MAXNDFNAME];
    char ndfpath[MAXNDFNAME];
    char newloc[DAT__SZLOC];
    char slice[MAXNDFNAME];
    char tmploc[DAT__SZLOC];
    int baseid = 0;
    int first;
    int height;
    int hlen;
    int i;
    int isect;
    int istop = 0;
    int level;
    int ncomp = 0;
    int ndfid;
    int same;
    int status = SAI__OK;
    int type;
    int width;

    /*  Mark the error stack */
    emsMark();

    /*  Check that we're not going to have problems with the name
        length */
    if ( strlen( name ) > MAXNDFNAME ) {
        *error_mess = (char *) malloc( (size_t) MAXNDFNAME );
        sprintf( *error_mess, "NDF specification is too long "
                 "(limit is %d characters)", MAXNDFNAME );
        return 0;
    }

    /*  Attempt to open the given name as an NDF */
    if ( gaiaAccessNDF( name, &type, &width, &height, &header, &hlen,
                        &ndfid, &emess ) ) {

        /*  Name is an NDF. Need a HDS path to its top-level so we can
            check for other NDFs at this level, also need to check for the
            existence of other displayables in the NDF */
        baseid = ndfid;
        head = (NDFinfo *) malloc( sizeof( NDFinfo ) );
        setState( head, ndfid, name, type, width, height, header, hlen,
                  &status );

        /*  Get the locator to the NDF and hence to its parent. */
        ndfLoc( ndfid, "READ", tmploc, &status );
        datParen( tmploc, baseloc, &status );

        /*  See if the NDF has a slice. If so all components and NDFs in
            this file will also have that slice applied */
        left = strrchr( name, '(');
        right = strrchr( name, ')');
        if ( left && right ) {
            strcpy( slice, left );
        }
        else {
            slice[0] = '\0';
        }
        if ( status == DAT__OBJIN ) {

            /*  At top level, so no parent available */
            emsAnnul( &status );
            datClone( tmploc, baseloc, &status );
        }
        else {
            /*  Some other error occured, just let it go */
            emess = errMessage( &status );
        }
    }
    else {

        /*  May just be a HDS container name with NDFs at this level. Need
         *  to parse down to a filename and a HDS path.
         */
        filename = strdup( name );
        path = strstr( filename, ".sdf" );
        if ( path ) {
            *path++ = ' '; /* Have ".sdf" in name, cut it out. */
            *path++ = ' ';
            *path++ = ' ';
            *path++ = ' ';
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
        hdsOpen( filename, "READ", tmploc, &status );

        /*  Now look for the object specified by the PATH */
        if ( path && status == SAI__OK ) {
            datFind( tmploc, path, baseloc, &status );
        }
        else {
            datClone( tmploc, baseloc, &status );
        }

        /*  If all is well, cancel pending error message (from initial
            attempt to open) */
        if ( status == SAI__OK && emess != NULL ) {
            free( emess );
            emess = NULL;
        }
        free( filename );

        /*  Top level container file, so safe to strip .sdf from name
         *  (if this was an NDF then .sdf.gz might be used).
         */
        istop = 1;

        /*
         *  No slice for plain top-level.
         */
        slice[0] = '\0';
    }
    if ( status == SAI__OK && datValid( baseloc, &status ) ) {

        /*  Look for additional NDFs at baseloc, ignore tmploc which is
         *  NDF itself (or baseloc). If only one component, must be this NDF.
         */
        datNcomp( baseloc, &ncomp, &status );
        if ( status != SAI__OK || ncomp == 1 ) {
            ncomp = 0;
        }
        first = 1;
        for ( i = 1; i <= ncomp; i++ ) {
            datIndex( baseloc, i, newloc, &status );

            /*  Get full name of component and see if it is an NDF */
            hdsTrace( newloc, &level, ndfpath, MAXNDFNAME, ndffile,
                      MAXNDFNAME, &status );

            ftype = strstr( ndffile, ".sdf" ); /* Strip .sdf from filename */
            if ( ftype ) *ftype = '\0';

            path = strstr( ndfpath, "." );  /* Now find first component */
            if ( path == NULL ) {           /* and remove it (not used as */
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
                DATA_ARRAY component) */
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
                    /*  NDF exists so add its details to the list of NDFs */
                    newstate = (NDFinfo *) malloc( sizeof( NDFinfo ) );
                    if ( first ) {
                        if ( head ) {
                            head->next = newstate;
                        }
                        else {
                            head = newstate;
                        }
                        first = 0;
                    }
                    else {
                        state->next = newstate;
                    }
                    state = newstate;
                    setState( state, ndfid, path, type, width, height, header,
                              hlen, &status );
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
            datAnnul( newloc, &status );
        }

        /*  Release locators */
        datAnnul( baseloc, &status );
        datAnnul( tmploc, &status );
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
    *handle = head;
    emsRlse();

    /*  No error messages should get past here! */
    if ( emess ) {
        free( emess );
    }
    return 1;
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

   /*  See if the component exists */
   if ( current ) {
      switch ( component[0] ) {
         case 'd':
         case 'D': {
            status = 1;
         }
         break;

         case 'v':
         case 'V': {
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
void gaiaGetIdMNDF( const void *handle, int index, int *ndfid )
{
   NDFinfo *current = NULL;

   current = getNDFInfo( handle, index );
   if ( current ) {
      *ndfid = current->ndfid;
   }
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
                                    error_mess );
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
void *gaiaCloneMNDF( void *handle )
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
                    current->hlen * 80 );

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

/*  ============ */
/*  HDS wrappers */
/*  ============ */

/*
 *  Name:
 *     datAnnul
 *
 *  Purpose:
 *     Annul a HDS locator, releasing all associated resources.
 *
 *  Params:
 *    loc = HDS locator to component
 *    status = global status
 */
extern void F77_EXTERNAL_NAME(dat_annul)( CHARACTER(loc),
                                          INTEGER(status)
                                          TRAIL(loc) );
static void datAnnul( const char *loc, int *status )
{
   DECLARE_CHARACTER(floc, DAT__SZLOC);  /* Fortran locator */

   /* Convert the input locator into an F77 character array */
   F77_EXPORT_LOCATOR( loc, floc );
   F77_CALL( dat_annul )( CHARACTER_ARG(floc),
                          INTEGER_ARG(status)
                          TRAIL_ARG(floc) );
}

/*
 *  Name:
 *     hdsOpen
 *
 *  Purpose:
 *     Open an HDS container file.
 *
 *  Params:
 *     name = name of container file
 *     mode = access mode
 *     loc = HDS locator to container file
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( hds_open )(
   CHARACTER( fname ),
   CHARACTER( fmode ),
   CHARACTER( floc ),
   INTEGER( fstatus )
   TRAIL( fname )
   TRAIL( fmode )
   TRAIL( floc ) );
static void hdsOpen( const char *name, const char *mode, char *loc,
                     int *status )
{
   DECLARE_CHARACTER(floc,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(fname);
   DECLARE_CHARACTER_DYN(fmode);
   DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER( fname, strlen( name ) );
   F77_EXPORT_CHARACTER( name, fname, fname_length );
   F77_CREATE_CHARACTER( fmode, strlen( mode ) );
   F77_EXPORT_CHARACTER( mode, fmode, fmode_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( hds_open )( CHARACTER_ARG( fname ),
                         CHARACTER_ARG( fmode ),
                         CHARACTER_ARG( floc ),
                         INTEGER_ARG( &fstatus )
                         TRAIL_ARG( fname )
                         TRAIL_ARG( fmode )
                         TRAIL_ARG( floc ) );

   F77_FREE_CHARACTER( fmode );
   F77_FREE_CHARACTER( fname );
   F77_IMPORT_LOCATOR( floc, loc );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     datFind
 *
 *  Purpose:
 *     Obtain a locator to a named component
 *
 *  Params:
 *     loc1 = structure locator
 *     name = component name
 *     loc2 = component locator
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( dat_find )(
   CHARACTER( floc1 ),
   CHARACTER( fname ),
   CHARACTER( floc2 ),
   INTEGER( fstatus )
   TRAIL( floc1 )
   TRAIL( fname )
   TRAIL( floc2 ) );
static void datFind( const char *loc1, const char *name, char *loc2,
                     int *status )
{
   DECLARE_CHARACTER(floc1,DAT__SZLOC);
   DECLARE_CHARACTER(floc2,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(fname);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1);
   F77_CREATE_CHARACTER( fname, strlen( name ) );
   F77_EXPORT_CHARACTER( name, fname, fname_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_find )( CHARACTER_ARG( floc1 ),
                         CHARACTER_ARG( fname ),
                         CHARACTER_ARG( floc2 ),
                         INTEGER_ARG( &fstatus )
                         TRAIL_ARG( floc1 )
                         TRAIL_ARG( fname )
                         TRAIL_ARG( floc2 ) );

   F77_FREE_CHARACTER( fname );
   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     datParen
 *
 *  Purpose:
 *     Returns locator to the parent structure of an HDS object.
 *
 *  Params:
 *     loc1 = object locator
 *     loc2 = parent structure
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( dat_paren )(
   CHARACTER( floc1 ),
   CHARACTER( floc2 ),
   INTEGER( fstatus )
   TRAIL( floc1 )
   TRAIL( floc2 ) );
static void datParen( const char *loc1, char *loc2, int *status )
{
   DECLARE_CHARACTER(floc1,DAT__SZLOC);
   DECLARE_CHARACTER(floc2,DAT__SZLOC);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1);
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_paren )( CHARACTER_ARG( floc1 ),
                          CHARACTER_ARG( floc2 ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc1 )
                          TRAIL_ARG( floc2 ) );

   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     datNcomp
 *
 *  Purpose:
 *     Enquire number of components.
 *
 *  Params:
 *     loc = structure locator
 *     ncomp = number of components
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( dat_ncomp )(
   CHARACTER( floc ),
   INTEGER( fncomp ),
   INTEGER( fstatus )
   TRAIL( floc ) );
static void datNcomp( const char *loc, int *ncomp, int *status )
{
   DECLARE_CHARACTER(floc,DAT__SZLOC);
   DECLARE_INTEGER(fncomp);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc);
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_ncomp )( CHARACTER_ARG( floc ),
                          INTEGER_ARG( &fncomp ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc ) );

   F77_IMPORT_INTEGER( fncomp, *ncomp );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     datIndex
 *
 *  Purpose:
 *     Index into component list
 *
 *  Params:
 *     loc1 = structire locator
 *     index = list position
 *     loc2 = component locator
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( dat_index )(
   CHARACTER( floc1 ),
   INTEGER( findex ),
   CHARACTER( floc2 ),
   INTEGER( fstatus )
   TRAIL( floc1 )
   TRAIL( floc2 ) );
static void datIndex( const char *loc1, int index, char *loc2,
                      int *status )
{
   DECLARE_CHARACTER(floc1,DAT__SZLOC);
   DECLARE_CHARACTER(floc2,DAT__SZLOC);
   DECLARE_INTEGER(findex);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1);
   F77_EXPORT_INTEGER( index, findex );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_index )( CHARACTER_ARG( floc1 ),
                          INTEGER_ARG( &findex ),
                          CHARACTER_ARG( floc2 ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc1 )
                          TRAIL_ARG( floc2 ) );

   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     hdsTrace
 *
 *  Purpose:
 *     Trace an object path
 *
 *  Params:
 *     loc = HDS locator to object
 *     level = number of path levels
 *     path = object path with container file
 *     file = container file
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( hds_trace )(
   CHARACTER( floc ),
   INTEGER( flevel ),
   CHARACTER( fpath ),
   CHARACTER( ffile ),
   INTEGER( fstatus )
   TRAIL( floc )
   TRAIL( fpath )
   TRAIL( ffile ) );
static void hdsTrace( const char *loc, int *level, char *path,
                      int path_len, char *file, int file_len,
                      int *status )
{
   DECLARE_CHARACTER(floc,DAT__SZLOC);
   DECLARE_CHARACTER_DYN(fpath);
   DECLARE_CHARACTER_DYN(ffile);
   DECLARE_INTEGER(flevel);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc, floc);
   F77_CREATE_CHARACTER( fpath, path_len-1 );
   F77_CREATE_CHARACTER( ffile, file_len-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( hds_trace )( CHARACTER_ARG( floc ),
                          INTEGER_ARG( &flevel ),
                          CHARACTER_ARG( fpath ),
                          CHARACTER_ARG( ffile ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc )
                          TRAIL_ARG( fpath )
                          TRAIL_ARG( ffile ) );

   F77_IMPORT_CHARACTER( fpath, fpath_length, path );
   F77_FREE_CHARACTER( fpath );
   F77_IMPORT_CHARACTER( ffile, ffile_length, file );
   F77_FREE_CHARACTER( ffile );

   F77_IMPORT_INTEGER( flevel, *level );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

/*
 *  Name:
 *     datClone
 *
 *  Purpose:
 *     Clone a locator.
 *
 *  Params:
 *     loc1 = object locator
 *     loc2 = new object locator
 *     status = global status
 *
 */
extern void F77_EXTERNAL_NAME( dat_clone )(
   CHARACTER( floc1 ),
   CHARACTER( floc2 ),
   INTEGER( fstatus )
   TRAIL( floc1 )
   TRAIL( floc2 ) );
static void datClone( const char *loc1, char *loc2, int *status )
{
   DECLARE_CHARACTER(floc1,DAT__SZLOC);
   DECLARE_CHARACTER(floc2,DAT__SZLOC);
   DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOCATOR( loc1, floc1);
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_clone )( CHARACTER_ARG( floc1 ),
                          CHARACTER_ARG( floc2 ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc1 )
                          TRAIL_ARG( floc2 ) );

   F77_IMPORT_LOCATOR( floc2, loc2 );
   F77_IMPORT_INTEGER( fstatus, *status );
   return;
}

extern void F77_EXTERNAL_NAME( hds_show )(
   CHARACTER( fname ),
   INTEGER( fstatus )
   TRAIL( fname ) );

static void hdsShow( const char *name, int *status )
{
   DECLARE_CHARACTER_DYN(fname);
   DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER( fname, strlen( name ) );
   F77_EXPORT_CHARACTER( name, fname, fname_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( hds_show )( CHARACTER_ARG( fname ),
                         INTEGER_ARG( &fstatus )
                         TRAIL_ARG( fname ) );

   F77_FREE_CHARACTER( fname );
   return;
}

/*
 *  Name:
 *     datValid
 *
 *  Purpose:
 *     Enquire if a locator is valid.
 *
 *  Params:
 *     loc = structure locator
 *     status = global status
 *
 *  Return:
 *     1 for valid locator, 0 for bad.
 *
 */
extern void F77_EXTERNAL_NAME( dat_valid )(
   CHARACTER( floc ),
   LOGICAL( freply ),
   INTEGER( fstatus )
   TRAIL( floc ) );
static int datValid( const char *loc, int *status )
{
   DECLARE_CHARACTER(floc,DAT__SZLOC);
   DECLARE_LOGICAL(freply);
   DECLARE_INTEGER(fstatus);
   int reply;

   F77_EXPORT_LOCATOR( loc, floc);
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_CALL( dat_valid )( CHARACTER_ARG( floc ),
                          LOGICAL_ARG( &freply ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc ) );

   F77_IMPORT_LOGICAL( freply, reply );
   F77_IMPORT_INTEGER( fstatus, *status );
   return reply;
}


/*
 *  =============================================
 *  Straight-forward NDF access, with no 2D bias.
 *  =============================================
 *
 *  These are only used to query NDF bounds, so that we can section up cubes.
 */

/**
 * Open an NDF and return the identifier.
 */
int gaiaSimpleOpenNDF( char *ndfname, int *ndfid, char **error_mess )
{
    int place;
    int status = SAI__OK;

    emsMark();
    ndfOpen( DAT__ROOT, ndfname, "READ", "OLD", ndfid, &place, &status );
    if ( status != SAI__OK ) {
        *error_mess = errMessage( &status );
        ndfid = NDF__NOID;
        emsRlse();
        return 1;
    }
    return 0;
}

/**
 * Close an NDF.
 */
int gaiaSimpleCloseNDF( int *ndfid )
{
    int status = SAI__OK;
    emsMark();
    ndfAnnul( ndfid, &status );
    emsRlse();
    return 0;
}

/**
 * Query the bounds of an NDF.
 */
int gaiaSimpleQueryBounds( int ndfid, int ndimx, int lbnd[], int ubnd[], 
                           int *ndim, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    ndfBound( ndfid, ndimx, lbnd, ubnd, ndim, &status );
    if ( status != SAI__OK ) {
        *error_mess = errMessage( &status );
        ndfid = NDF__NOID;
        emsRlse();
        return 1;
    }
    emsRlse();
    return 0;
}

/**
 * Query the equivalent world coordinate of a pixel coordinate along the
 * given axis. 
 *
 * Returns a formatted value pointed at by *coord that has the value (trailed
 * by the axis units and label if requested). The position along the axis is
 * identified using ncoords pixel coordinates, where ncoords must match the
 * dimensionality of the NDF. The coordinate returned is matched to the
 * requested axis by being the coordinate with the largest change wrt to a
 * single pixel step. We need to use this logic as the relationship between
 * the NDF axes and the world coordinates may not be straight-forward. Note
 * that the value returned by the coord argument should be immediately copied.
 */
int gaiaSimpleQueryCoord( int ndfid, int axis, double *coords, int trailed, 
                          int ncoords, char **coord, char **error_mess )
{
    AstFrameSet *frameSet = NULL;
    char *domain;
    char *units;
    char *label;
    static char buf[256];
    double diff;
    double tmp;
    double out1[7];
    double out2[7];
    int base;
    int current;
    int caxis;
    int i;
    int pixel;
    int status = SAI__OK;

    emsMark();

    ndfGtwcs( ndfid, &frameSet, &status );
    if ( status != SAI__OK ) {
        *error_mess = errMessage( &status );
        emsRlse();
        *coord = NULL;
        return 1;
    }

    //  Find the PIXEL domain.
    base = astGetI( frameSet, "Base" );
    current = astGetI( frameSet, "Current" );
    pixel = 0;
    for ( i = 1; i <= ncoords; i++ ) {
        astSetI( frameSet, "Current", i );
        domain = (char *)astGetC( frameSet, "Domain" );
        if ( strcasecmp( domain, "PIXEL" ) == 0 ) {
            pixel = i;
            break;
        }
    }
    astSetI( frameSet, "Current", current );
    if ( pixel == 0 ) {
        frameSet = (AstFrameSet *) astAnnul( frameSet );
        *error_mess = strdup( "Cannot locate the PIXEL domain" );
        *coord = NULL;
        emsRlse();
        return 1;
    }

    //  This becomes the BASE frame.
    astSetI( frameSet, "Base", pixel );

    //  Transform the position from pixel coordinates to world coordinates for
    //  this position, and a position one pixel offset along the given axis.
    coords[axis-1] += 1.0;
    astTranN( frameSet, 1, ncoords, 1, coords, 1, ncoords, 1, out1  );
    coords[axis-1] -= 1.0;
    astTranN( frameSet, 1, ncoords, 1, coords, 1, ncoords, 1, out2  );

    //  Select the axis with the largest shift as equivalent in world
    //  coordinates.
    caxis = axis;
    diff = 0.0;
    for ( i = 0; i < ncoords; i++ ) {
        tmp = fabs( out1[i] - out2[i] );
        if ( tmp > diff ) {
            caxis = i + 1;
            diff = tmp;
        }
    }

    //  Format the value along that axis.
    *coord = (char *) astFormat( frameSet, caxis, out2[caxis - 1] );
    if ( ! astOK ) {
        astClearStatus;
        *coord = NULL;
        *error_mess = 
            strdup( "Failed to convert pixel index to world coordinate" );
        astSetI( frameSet, "Base", base );
        frameSet = (AstFrameSet *) astAnnul( frameSet );
        errRlse();
        return 1;
    }

    //  Add the axis units and label if requested.
    if ( trailed ) {
        sprintf( buf, "unit(%d)", caxis );
        units = (char *) astGetC( frameSet, buf );
        sprintf( buf, "label(%d)", caxis );
        label = (char *) astGetC( frameSet, buf );
        sprintf( buf, "%s %s (%s)", *coord, label, units );
        *coord = buf;
    }

    //  Restore the base frame.
    astSetI( frameSet, "Base", base );
    frameSet = (AstFrameSet *) astAnnul( frameSet );
    emsRlse();
    return 0;
}


/*
 *   Name:
 *      rtdNDF
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
 *     Copyright (C) 2000 Central Laboratory of the Research Councils
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
 *      {enter_changes_here}
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
#include "ndf.h"
#include "rtdNDF.h"

/*  Maximum number of pixels to copy during chunking. */
#define MXPIX 500000

/*  Maximum name of NDF filename or HDS path */
#define MXNAME 256

/*
 *  Structure to store information about an NDF.
 */
struct NDFinfo {
      char name[MXNAME];    /*  NDF name (within file)*/
      int ndfid;            /*  NDF identifier */
      int ncomp;            /*  Number of data components */
      int type;             /*  NDF data type (as bitpix) */
      int nx;               /*  First dimension of NDF */
      int ny;               /*  Second dimension of NDF */
      int havevar;          /*  Variance component exists */
      int havequal;         /*  Quality array exists */
      char *header;         /*  Pointer to FITS headers */
      int hlen;             /*  Length of header */
      struct NDFinfo *next; /*  Pointer to next NDF with displayables */
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
                                          TRAIL(ndfname));

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
                                          TRAIL(fhead));

extern void F77_EXTERNAL_NAME(rtd_cpdat)( INTEGER(ndfid),
                                          POINTER(data),
                                          CHARACTER(comp),
                                          INTEGER(status )
                                          TRAIL(comp) );

extern F77_SUBROUTINE(rtd1_aqual)( INTEGER(ndfId),
                                   LOGICAL(grab),
                                   LOGICAL(haveQual),
                                   POINTER(q));

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

static char *errMessage( int *status );

/*
 *  Copy the current EMS error message into a dynamic string for
 *  returning. Make sure status is set before calling this and release
 *  the memory used for the string when complete. On return status is
 *  reset to SAI__OK and the error stack is empty.
 */
static char *errMessage( int *status )
{
   char *opPtr = (char *) NULL;
   char *opStr = (char *) NULL;
   char param[EMS__SZPAR+1];
   int errcount = 1;
   int i;
   int j;
   int used = 0;

   while ( *status != SAI__OK ) {
      opStr = (char *)realloc( (void *)opStr,
                               (size_t) EMS__SZMSG * sizeof(char) * errcount++ );
      opPtr = opStr + used;
      emsStat( status );
      emsEload( param, &i, opPtr, &j, status);
      used += j;
      opStr[used++] ='\n';
   }
   opStr[used] = '\0';
   emsAnnul( status );
   return opStr;
}

/*
 *  Access an NDF returning pseudo FITS headers and the NDF
 *  identifier.
 */

int rtdAccessNDF( const char *filename, int *type, int *width, int *height,
                  char **header, int *header_length, int *ndfid,
                  char **error_mess )
{
   DECLARE_CHARACTER(ndfname, MXNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(tmpname, MXNAME);   /* Local copy of filename (F77) */
   DECLARE_INTEGER(status);             /* Global status */
   DECLARE_POINTER(charPtr);            /* Pointer to F77 character array */
   char *opStr;
   char *opPtr;
   int used, i, j, errcount;

   /* Convert the file name into an F77 string */
   strcpy( tmpname, filename );
   cnf_exprt( tmpname, ndfname, ndfname_length );

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
   *header = cnf_creib( (char *)charPtr, 80 * (*header_length) );
   cnf_free( (void *)charPtr );
   emsRlse();
   return 1;
}


/*
 *  Create an NDF from an existing NDF, replacing the given
 *  component.
 */
int rtdWriteNDF( const char *filename, int type, int width, int height,
                 void *data , int ndfid, const char *component,
                 const char *header, int lheader, char **error_mess )
{
   DECLARE_CHARACTER(ndfname,MXNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(tmpname,MXNAME);   /* Local copy of filename (F77) */
   DECLARE_CHARACTER(comp, 20);      /* NDF component to use (F77) */
   DECLARE_INTEGER(status);          /* Global status */
   DECLARE_POINTER(dataPtr);         /* Pointer to F77 memory */
   DECLARE_CHARACTER_ARRAY_DYN(fhead);
   char *opStr;
   char *opPtr;
   int used, i, j, errcount;
   int dims[1];

   /* Convert the file name into an F77 string */
   strcpy( tmpname, filename );
   cnf_exprt( tmpname, ndfname, ndfname_length );

   /* Convert the NDF component into a F77 string */
   cnf_exprt( (char *) component, comp, comp_length);

   /* Convert C pointer to Fortran pointer */
   dataPtr = (F77_POINTER_TYPE) data;

   /*  If we have a FITS header then write it out with the NDF. This
       also contains any new WCS information that might be
       available. */
   if ( lheader > 0) {

      /*  Convert the C string into a Fortran array. */
      dims[0] = lheader / 80;
      F77_CREATE_CHARACTER_ARRAY( fhead, 80, dims[0] );
      cnf_exprta( (char *) header, 80, fhead, 80, 1, dims );
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
 *  Free an NDF.
 */
int rtdFreeNDF( int ndfid )
{
   DECLARE_LOGICAL(haveQual);
   DECLARE_POINTER(qualPtr);
   DECLARE_LOGICAL(grab);
   int status = SAI__OK;

   grab = F77_FALSE;
   emsMark();

   /* Free any quality associated with this NDF (should be safe under
    * any circumstances). */
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
 *  Copy an NDF data component into some given memory.
 */
int rtdCopyNDF( int ndfid, void **data, const char* component,
                char **error_mess )
{
   char *opPtr;
   char *opStr;
   char dtype[NDF__SZTYP+1];
   int chunkid;
   int el;
   int errcount;
   int i;
   int j;
   int nchunk;
   int status = SAI__OK;
   int used;
   void *ptr[1];

   /* Get the type of the NDF component. */
   emsMark();
   ndfBegin();

   ndfType( ndfid, component, dtype, NDF__SZTYP+1, &status );
   ndfMap( ndfid, component, dtype, "READ", ptr, &el, &status ); 
   *data = ptr[0];
   ndfEnd( &status );
   return 1;

   /*  Determine the number of chunks needed to copy the data. */
   ndfNchnk( ndfid, MXPIX, &nchunk, &status );

   /*  Using the appropriate data type, access the NDF chunks and copy
    *  data.
    */
   if ( strncmp( dtype, "_DOUBLE", 8 ) == 0 ){

      /*  Double type not available, so use float, check for values that
          cannot be represented and set bad */
      double *fromPtr;
      float *toPtr = *data;
      double hi = FLT_MAX;
      double lo = -FLT_MAX;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = (double *) ptr[0];
         for( j = 0; j < el; j++ ) {
            if ( *fromPtr <= hi && *fromPtr > lo ) {
               *toPtr++ = (float)*fromPtr++;
            } else {
               *toPtr++ = -FLT_MAX;
            }
         }
      }
   } else if ( strncmp( dtype, "_REAL", 5 ) == 0 ) {
      float *fromPtr;
      float *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   } else if ( strncmp( dtype, "_INTEGER", 9 ) == 0 ) {
      int *fromPtr;
      int *toPtr = *data;
      for ( i = 1; i <= nchunk; i++ ) {
         ndfChunk( ndfid, MXPIX, i, &chunkid, &status );
         ndfMap( chunkid, component, dtype, "READ", ptr, &el, &status );
         fromPtr = ptr[0];
         for( j = 0; j < el; j++ ) {
            *toPtr++ = *fromPtr++;
         }
      }
   } else if ( strncmp( dtype, "_WORD", 5 ) == 0 ||
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
   } else if ( strncmp( dtype, "_BYTE", 5 ) == 0 ) {
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
   } else if ( strncmp( dtype, "_UBYTE", 6 ) == 0 ) {

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
 *  Set the state of an NDFinfo object.
 *  Queries the NDF about the existence of components.
 */
static void setState( struct NDFinfo *state, int ndfid, const char *name,
                      int type, int nx, int ny, char *header,
                      int hlen, int *status )
{
   int ncomp = 1;
   int exists = 0;

   strncpy( state->name, name, MXNAME );
   state->ndfid = ndfid;
   state->type = type;
   state->nx = nx;
   state->ny = ny;
   state->header = header;
   state->hlen = hlen;

   /*  Query NDF about other components */
   ndfState( ndfid, "Variance", &exists, status );
   if ( exists ) {
      ncomp++;
      state->havevar = 1;
   } else {
      state->havevar = 0;
   }

   ndfState( ndfid, "Quality", &exists, status );
   if ( exists ) {
      ncomp++;
      state->havequal = 1;
   } else {
      state->havequal = 0;
   }

   /*  No next structure yet */
   state->next = (NDFinfo *)NULL;

   /*  Number of components */
   state->ncomp = ncomp;
}

/*
 *  Categorise and store displayable information for the given NDF and
 *  any related ones.
 *
 *  Displayables in this context are either array components of the
 *  given NDF, or the components of any other NDFs which are stored at
 *  the same "level" (i.e. HDS path) within a HDS container file.
 *
 *  The return from this function is a pointer to an initialised
 *  information structure about the NDFs and displayables
 *  available. If this fails then zero is returned.
 */
int rtdInitNDF( const char *name, void **handle, char **error_mess )
{
   NDFinfo *head = (NDFinfo *) NULL;
   NDFinfo *newstate = (NDFinfo *) NULL;
   NDFinfo *state = (NDFinfo *) NULL;
   char *emess;
   char *filename;
   char *ftype;
   char *header;
   char *left;
   char *path;
   char *right;
   char baseloc[DAT__SZLOC];
   char ndffile[MXNAME];
   char ndfpath[MXNAME];
   char newloc[DAT__SZLOC];
   char slice[MXNAME];
   char tmploc[DAT__SZLOC];
   int baseid = 0;
   int count = 0;
   int first;
   int height;
   int hlen;
   int i;
   int isect;
   int istop = 0;
   int level;
   int ncomp;
   int ndfid;
   int same;
   int status = SAI__OK;
   int type;
   int width;

   /*  Mark the error stack */
   emsMark();

   /*  Attempt to open the given name as an NDF */
   if ( rtdAccessNDF( name, &type, &width, &height, &header, &hlen,
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
      } else {
         slice[0] = '\0';
      }
      if ( status != SAI__OK ) {

         /*  At top level already */
         emsAnnul( &status );
         datClone( tmploc, baseloc, &status );
      }
   } else {

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
      } else {
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
      } else {
         datClone( tmploc, baseloc, &status );
      }

      /*  If all this well, cancel pending error message */
      if ( status != SAI__OK ) {
         free( emess );
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
   if ( status == SAI__OK ) {

      /*  Look for additional NDFs at baseloc, ignore tmploc which is
       *  NDF itself (or baseloc).
       */
      datNcomp( baseloc, &ncomp, &status );
      first = 1;
      for ( i = 1; i <= ncomp; i++ ) {
         datIndex( baseloc, i, newloc, &status );

         /*  Get full name of component and see if it is an NDF */
         hdsTrace( newloc, &level, ndfpath, MXNAME, ndffile, MXNAME, &status );

         ftype = strstr( ndffile, ".sdf" ); /* Strip off .sdf from filename */
         if ( ftype ) *ftype = '\0';

         path = strstr( ndfpath, "." );  /* Now find first component */
         if ( path == NULL ) {           /* and remove it (not used as */
            strcat( ndffile, "." );      /* part of NDF name) */
            path = ndfpath;
         }
         strcat( ndffile, path );        /*  Join filename and HDS
                                             path to give NDF full
                                             name */
         if ( slice ) {
            strcat( ndffile, slice );    /*  Add the NDF slice, if set */
            strcat( path, slice );
         }

         /*  Attempt to open NDF to see if it exists (could check for
             DATA_ARRAY component) */
         if ( rtdAccessNDF( ndffile, &type, &width, &height, &header, &hlen,
                            &ndfid, &emess ) ) {

            /*  Check that this isn't the base NDF by another name */
            if ( ndfid != 0 && baseid != 0 ) {
               ndfSame( baseid, ndfid, &same, &isect, &status ); 
            } else {
               same = 0;
            }
            if ( ! same ) {
               /*  NDF exists so add its details to the list of NDFs */
               newstate = (NDFinfo *) malloc( sizeof( NDFinfo ) );
               if ( first ) {
                  if ( head ) {
                     head->next = newstate;
                  } else {
                     head = newstate;
                  }
                  first = 0;
               } else {
                  state->next = newstate;
               }
               state = newstate;
               setState( state, ndfid, path, type, width, height, header,
                         hlen, &status );
            }
         }
      }

      /*  Release locators */
      datAnnul( baseloc, &status );
      datAnnul( tmploc, &status );
   } else {

      /*  Initialisation failed (no such NDF, or container file/path
       *  doesn't have any NDFs in it). 
       */
      if ( emess ) {
         *error_mess = emess;
      } else {
         *error_mess = strdup( "Failed to locate any NDFs" );
      }
      emsRlse();
      return 0;
   }

   /*  Return list of NDFinfos */
   *handle = head;
   emsRlse();
   return 1;
}

/*
 *  See if the given NDF contains a named displayable component.
 *
 *  If not found then a 0 is returned.
 *
 */
int rtdCheckDisplayable( const void *handle, int index, const char *component )
{
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;
   int status = 1;

   if ( current ) {

      /*  Offset into the structure by the given amount. */
      for ( count = 1; count < index; count++ ) {
         current = current->next;
         if ( current == NULL ) {
            status = 0;
            break;
         }
      }
      if ( status ) {

         /*  See if the component exists */
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
               } else {
                  status = 0;
               }
            }
            break;

            case 'q':
            case 'Q': {
               if ( current->havequal ) {
                  status = 1;
               } else {
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
   } else {
      status = 0;
   }
   return status;
}

/*
 *  Return useful information about an NDF.
 */
void rtdGetNDFInfo( const void *handle, int index,
                    char **name, int *type, int *width,
                    int *height, char **header, int *hlen,
                    int *ndfid, int *hasvar, int *hasqual )
{
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;
   int status = 1;
   if ( current ) {

      /*  Offset into the structure by the given amount. */
      for ( count = 1; count < index; count++ ) {
         current = current->next;
         if ( current == NULL ) {
            status = 0;
            break;
         }
      }
      if ( status ) {
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
}

/*
 *  Copy a displayables data component into some memory.
 */
int rtdCopyDisplayable( const void *handle, int index, const char *component,
                        void **data, char **error_mess )
{
   /*  Access the appropriate NDF information structure */
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
         /*  Copy the data */
         return rtdCopyNDF( current->ndfid, data, component,
                            error_mess );
      }
   }

   /*  Arrive here only when NDF isn't available */
   *error_mess = strdup( "No such NDF is available" );
   return 0;
}

/*
 *  Return the number of NDFs available.
 */
int rtdCountNDFs( const void *handle )
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
 *  Release all NDF resources.
 */
void rtdReleaseNDF( const void *handle )
{
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;
   if ( current ) {
      for ( ; current->next; current = current->next ) {
         rtdFreeNDF( current->ndfid );

         /*  Free the headers that remain allocated for each query */
         cnf_free( current->header ); /*  Memory allocated by PSX_ routines */
      }
   }
}

/*
 *  Release displayables accessed for an NDF.
 */
void rtdFreeDisplayable( void *handle, int index ) 
{   
   /*  Access the appropriate NDF information structure */
   NDFinfo *current = (NDFinfo *) handle;
   int count = 0;
   int located = 1;
   int status = SAI__OK;

   if ( current ) {
      for ( count = 1; count < index; count++ ) {
         current = current->next;
         if ( current == NULL ) {
            located = 0;
            break;
         }
      }
      if ( located ) {
         
         /*  Free the NDF array components */
         emsMark();
         ndfUnmap( current->ndfid, "*", &status );
         emsRlse();
      }
   }
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
                          INTEGER_ARG( flevel ),
                          CHARACTER_ARG( fpath ),
                          CHARACTER_ARG( ffile ),
                          INTEGER_ARG( &fstatus )
                          TRAIL_ARG( floc )
                          TRAIL_ARG( fpath )
                          TRAIL_ARG( ffile ) );

   F77_IMPORT_CHARACTER( fpath, fpath_length, path );
   F77_IMPORT_CHARACTER( ffile, ffile_length, file );
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

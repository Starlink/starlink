#include <time.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "f77.h"
#include "ast.h"
#include "mers.h"
#include "ems.h"
#include "sae_par.h"

#define FNMLEN 255
#define ATEND "%%BoundingBox: (atend)"
#define BBOX "%%BoundingBox: "
#define ORIENT "%%Orientation: "
#define DOCSTART "BEGINAGIFILE"
#define DOCEND "ENDAGIFILE"

/* Structure holding a single EPS picture */
typedef struct EPS {
   char **data;       /* Array of pointers to strings holding the lines of text */
   int nline;         /* Number of lines */
   double bbox[4];    /* The four values in the bounding box */
   int orient;        /* Index of forst %%Orientation line */
} EPS;

/* Structure holding a list of EPS pictures */
typedef struct EPSList {
   const char *path;  /* Path to the EPS file */
   EPS **data;        /* Array of pointers to EPS structures holding the data */
   int ndata;         /* Number of EPS structures in list */
   double bbox[4];    /* The four values in the total bounding box */
} EPSList;

/* Prototypes for local functions */
static EPS *agp1FreeEPS( EPS *eps );
static EPSList *agp1FreeEPSList( EPSList *eps );
static EPS *agp1ReadEPS( FILE *fd, int *status );
static EPSList *agp1ReadFile( const char *file, int *status );
static void agp1WriteEPS( EPS *eps, FILE *fd, int enc, int *status );
static void agp1WriteEPSList( EPSList *eps, const char *file, int *status );


F77_SUBROUTINE(agp1_merge)( CHARACTER(OLDEPS), CHARACTER(NEWEPS),
                            LOGICAL(CLEAR), INTEGER(STATUS) TRAIL(OLDEPS) TRAIL(NEWEPS) ){
/*
*+
*  Name:
*     AGP1_MERGE

*  Purpose:
*     Merge two EPS files.

*  Description:
*     This routine handles the merging of Postscript output produced by
*     AGI's "accumulating" Postscript devices. The contents of the Postscript
*     file with name "NEWEPS" are appended to the end of the Postscript
*     file with name "OLDEPS", and the "NEWEPS" file is then deleted. If
*     the "OLDEPS" file does not exist, or is not a Postscript file, it
*     is deleted and the "NEWEPS" file is simply renamed to "OLDEPS".
*
*     Some care is taken to ensure that old pictures that have zero size,
*     or are obscured by new opaque pictures, are not included in the
*     output EPS file.

*  Parameters:
*     OLDEPS = CHARACTER * ( * ) (Given)
*       The name of the old EPS file into which the new file is to be copied.
*     NEWEPS = CHARACTER * ( * ) (Given)
*       The name of the new EPS file containing the data to be copied.
*     CLEAR = LOGICAL (Given)
*       Was the picture described by the new EPS file cleared before the
*       plot was produced? In other words, does it have an opaque
*       background?
*     STATUS = INTEGER (Given & Returned)
*       Inherited status

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-2013 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

   GENPTR_CHARACTER(OLDEPS)
   GENPTR_CHARACTER(NEWEPS)
   GENPTR_LOGICAL(CLEAR)
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   char oldeps[ FNMLEN + 1 ];
   char neweps[ FNMLEN + 1 ];
   EPSList *epsold, *epsnew;
   EPS *thiseps;
   int i, irej, j;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Import the FORTRAN file names to C strings  */
   cnfImpn( OLDEPS, OLDEPS_length, FNMLEN,  oldeps );
   cnfImpn( NEWEPS, NEWEPS_length, FNMLEN,  neweps );

/* Attempt to read the new EPS file into memory. */
   epsnew = agp1ReadFile( neweps, STATUS );
   if( epsnew ) {

/* Attempt to read the old EPS file into memory. */
      epsold = agp1ReadFile( oldeps, STATUS );

/* If the old file exists, we append the contents of the new file to the
   end of the old file, then delete the new file. */
      if( epsold ) {

/* If the total bounding box of the new file is larger than or roughly equal
   to the total bounding box of the old file, and the new picture was cleared
   (i.e. has an opaque background), the new one replaces the old one. */
         if( F77_ISTRUE( *CLEAR ) ) {
            if( epsold->bbox[0] - epsnew->bbox[0] > -20.0 &&
                epsold->bbox[1] - epsnew->bbox[1] > -20 &&
                epsnew->bbox[2] - epsold->bbox[2] > -20.0 &&
                epsnew->bbox[3] - epsold->bbox[3] > -20.0 ) {
               epsold = agp1FreeEPSList( epsold );
               epsold = epsnew;
               epsnew = NULL;

/* If the bounding box of any of the individual EPS pictures in the old
   file are less than or roughly equal in size to the total bounding box
   of the new file, and the new picture was cleared (i.e. has an opaque
   background), then we can remove the obscured old EPS pictures from the
   list of pictures to be included in the merged file. */
            } else {

/* Check the bounding box of each EPS picture in the old file. */
               irej = -1;
               for( i = 0; i < epsold->ndata; i++ ) {
                  thiseps = (epsold->data)[ i ];

/* If it is obscured by the new EPS picture, free its resources, and
   replace it with a NULL pointer in the list. Note the index of the first
   EPS picture to be removed. */
                  if( thiseps->bbox[0] - epsnew->bbox[0] > -20.0 &&
                      thiseps->bbox[1] - epsnew->bbox[1] > -20 &&
                      epsnew->bbox[2] - thiseps->bbox[2] > -20.0 &&
                      epsnew->bbox[3] - thiseps->bbox[3] > -20.0 ) {
                     (epsold->data)[ i ] = agp1FreeEPS( thiseps );
                     if( irej < 0 ) irej = i;
                  }
               }

/* If any were removed, shuffle down the higher remaining EPS pictures
   to fill the gaps. We do not need to check the docs prior to the first
   one removed above. */
               if( irej >= 0 ) {
                  j = irej;
                  irej = 0;
                  for( i = j; i < epsold->ndata; i++ ) {
                     if( (epsold->data)[ i ] != NULL ) {
                        (epsold->data)[ j++ ] = (epsold->data)[ i ];
                     } else {
                        irej++;
                     }
                  }

/* Fill any unused space at the end of the list with NULLs. */
                  for( i = j; i < epsold->ndata; i++ ) {
                     (epsold->data)[ j ] = NULL;
                  }

/* Adjust the number of EPS pictures in the list. */
                  epsold->ndata -= irej;
               }
            }
         }

/* Otherwise, append all the EPS pictures in the new file to the end
   of the old file, and update its bounding box to encompass both files. */
         if( epsnew && epsnew->ndata > 0 ) {
            j = epsold->ndata;
            epsold->ndata += epsnew->ndata;
            epsold->data = astGrow( epsold->data, epsold->ndata,
                                    sizeof( *epsold->data ) );
            for( i = 0; i < epsnew->ndata; i++ ) {
               (epsold->data)[ j++ ] = (epsnew->data)[ i ];
               (epsnew->data)[ i ] = NULL;
            }
            epsnew->ndata = 0;

            if( epsnew->bbox[0] < epsold->bbox[0] ) epsold->bbox[0] = epsnew->bbox[0];
            if( epsnew->bbox[1] < epsold->bbox[1] ) epsold->bbox[1] = epsnew->bbox[1];
            if( epsnew->bbox[2] > epsold->bbox[2] ) epsold->bbox[2] = epsnew->bbox[2];
            if( epsnew->bbox[3] > epsold->bbox[3] ) epsold->bbox[3] = epsnew->bbox[3];
         }

/* If the old file did not exist, just use the new file in its place. */
      } else {
         epsold = epsnew;
         epsnew = NULL;
      }

/* If it is not empty, write out the total picture using the old
   file name. If it is empty, delete the file. */
      if( epsold->ndata > 0 ) {
         agp1WriteEPSList( epsold, oldeps, STATUS );
      } else {
         remove( oldeps );
      }

/* Delete the new file. */
      remove( neweps );

/* Free the in-memory structures. */
      epsold = agp1FreeEPSList( epsold );
   }
   epsnew = agp1FreeEPSList( epsnew );
}

static EPS *agp1FreeEPS( EPS *eps ){
/*
*  Name:
*     agp1FreeEPS

*  Purpose:
*     Free the resource sused by an EPS structure.

*  Parameters:
*     eps
*       Pointer to the structure to be freed.

*  Returned Value:
*     A NULL pointer is always returned.
*/
   int i;

   if( eps ) {
      if( eps->data ) {
         for( i = 0; i < eps->nline; i++ ) {
            (eps->data)[ i ] = astFree( (eps->data)[ i ] );
         }
      }
      eps->nline = 0;
   }

   return astFree( eps );
}

static EPSList *agp1FreeEPSList( EPSList *list ){
/*
*  Name:
*     agp1FreeEPSList

*  Purpose:
*     Free the resource used by an EPSList structure.

*  Parameters:
*     eps
*       Pointer to the structure to be freed.

*  Returned Value:
*     A NULL pointer is always returned.
*/
   int i;

   if( list ) {
      list->path = NULL;
      if( list->data ) {
         for( i = 0; i < list->ndata; i++ ) {
            (list->data)[ i ] = agp1FreeEPS( (list->data)[ i ] );
         }
      }
      list->ndata = 0;
   }

   return astFree( list );
}

static EPSList *agp1ReadFile( const char *name, int *status ){

/*
*  Name:
*     agp1ReadFile

*  Purpose:
*     Read the contents of an EPS file into memory.

*  Description:
*     This function attempts to read the supplied file into an EPSList
*     structure. If the file does not exist, or is not an EPS file, NULL
*     is returned without error.

*  Parameters:
*     name
*       The name of the EPS file.
*     status
*       A pointer to the inherited status value.

*  Returned Value:
*     A pointer to a newly allocated EPSList structure containing the
*     contents of the EPS file. It should be freed using agp1FreeEPSList
*     when no longer needed. NULL is returned if an error occurs.

*/

/* Local Variables; */
   EPSList *result = NULL;
   EPS *eps = NULL;
   FILE *fd;
   char *linebuf;
   int state;
   int linelen;
   int c;
   int iat;
   int nc;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Open the file. */
   fd = fopen( name, "r" );
   if( fd ) {

/* Allocate the returned structure, filling it with zeros. */
      result = astCalloc( 1, sizeof( *result ) );
      if( result ) {
         result->path = name;

/* Attempt to read the file as a single EPS picture. */
         eps = agp1ReadEPS( fd, status );

/* If successfull, store the EPS in the returned list, copying the
   bounding box. */
         if( eps ) {
            result->data = astMalloc( sizeof( *result->data ) );
            if( result->data ) {
               result->data[ 0 ] = eps;
               result->ndata = 1;
               memcpy( result->bbox, eps->bbox, 4*sizeof( double ) );
            }

/* If the file does not contain a single EPS picture, expect a header to
   come first, and then we read a list of EPS pictures. */
         } else {

/* Rewind the file so that we read from the start. */
            rewind( fd );

/* Initialise the dynamic buffer holding the current line being read from
   the PS file. */
            linebuf = NULL;
            linelen = 0;

/* Read all characters from the file. */
            state = 0;
            while( ( c = fgetc( fd ) ) != EOF ) {

/* If we have reached the end of a non-empty line... */
               if( ( c == '\r' || c == '\n') && linelen > 0 ) {

/* Append a null character to the line buffer. */
                  iat = linelen++;
                  linebuf = astGrow( linebuf, linelen, 1 );
                  linebuf[ iat ] = 0;

/* If this is the first line, check it looks right. If not, break and
   return NULL. */
                  nc = astChrLen( linebuf );
                  if( state == 0 ) {
                     if( strncmp( linebuf, "%!PS-Adobe-", 11 ) ||
                         linebuf[ 12 ] != '.' ||
                         strncmp( linebuf + 14, " EPSF-", 6 ) ||
                         linebuf[ 21 ] != '.' ) {
                        result = agp1FreeEPSList( result );
                        break;
                     }
                     state = 1;

/* If this is a "%%BoundingBox: (atend)" line, skip it. */
                  } else if( state == 1 ) {
                     if( nc == strlen(ATEND) && !strncmp( linebuf, ATEND, nc ) ) {

/* If this is a real "%%BoundingBox: ..." line parse it and store in the
   returned bbox. */
                     } else if( !strncmp( linebuf, BBOX, strlen(BBOX) ) ) {
                        sscanf( linebuf + strlen(BBOX), "%lf %lf %lf %lf",
                                result->bbox, result->bbox + 1,
                                result->bbox + 2, result->bbox + 3 );

/* If this is a "BEGINAGIFILE" line, indicate we should read an EPS
   picture after the subsequent comment. */
                     } else if( !strncmp( linebuf, DOCSTART, strlen(DOCSTART) ) ) {
                        state = 2;
                     }

/* Time to read the sub-picture. */
                  } else if( state == 2 ) {
                     eps = agp1ReadEPS( fd, status );

/* Break if it could not be read. */
                     if( !eps ) {
                        result = agp1FreeEPSList( result );
                        break;

/* Otherwise, add the EPS to the returned EPSList. */
                     } else {
                        iat = result->ndata++;
                        result->data = astGrow( result->data,
                                                result->ndata,
                                                sizeof( *result->data ) );
                        result->data[ iat ] = eps;
                     }

/* Read through stuff prior to the next EPS sub-picture. */
                     state = 1;
                  }

/* Reset the line buffer variables ready for the next line. */
                  linebuf = NULL;
                  linelen = 0;

/* Otherwise just add the next character to the end of the line buffer. */
               } else {
                  iat = linelen++;
                  linebuf = astGrow( linebuf, linelen, 1 );
                  linebuf[ iat ] = c;
               }
            }
         }
      }

/* Close the file. */
      fclose( fd );
   }

/* Return the result. */
   return result;
}

static void agp1WriteEPS( EPS *eps, FILE *fd, int enc, int *status ){
/*
*  Name:
*     agp1WriteEPS

*  Purpose:
*     Write an EPS structure out to a file handle.

*  Description:
*     This function writes the supplied EPS structure out to a file handle.

*  Parameters:
*     eps
*       The EPS structure to write out.
*     fd
*       File handle for output file.
*     inc
*       If non-zero, include headers and footers to enclose the EPS in a
*       higher level file.
*     status
*       A pointer to the inherited status value.

*/

/* Local Variables; */
   int iline;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Header */
   if( enc ) {
      fprintf( fd, "%s\n", DOCSTART );
      fprintf( fd, "%%%%BeginFile: \n" );
   }

/* Data */
   for( iline = 0; iline < eps->nline; iline++ ) {
      fprintf( fd, "%s\n", (eps->data)[ iline ] );
   }

/* Footer */
   if( enc ) {
      fprintf( fd, "%%%%EndFile\n" );
      fprintf( fd, "%s\n", DOCEND );
   }
}



static EPS *agp1ReadEPS( FILE *fd, int *status ){

/*
*  Name:
*     agp1ReadEPS

*  Purpose:
*     Read a single EPS picture into memory.

*  Description:
*     This function attempts to read an EPS picture starting at
*     the current position in the supplied file. If successfull
*     it returns a newly allocated EPS structure holding the
*     picture. Otherwise, it returns NULL.

*     size bounding box, NULL is returned.

*  Parameters:
*     fd
*       FILE to read from.
*     status
*       A pointer to the inherited status value.

*  Returned Value:
*     A pointer to a newly allocated EPS structure containing the
*     contents of the EPS file. It should be freed using agp1FreeEPS
*     when no longer needed. NULL is returned if an error occurs.

*/

/* Local Variables; */
   EPS *result = NULL;
   char *linebuf;
   int atend;
   int bbox;
   int iline;
   int linelen;
   int c;
   int iat;
   int nc;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Allocate the returned structure, filling it with zeros. */
   result = astCalloc( 1, sizeof( *result ) );
   if( result ) {

/* Iniitlaise the dynamic buffer holding the current line being read from
   the PS file. */
      linebuf = NULL;
      linelen = 0;
      atend = 0;
      bbox = 0;

/* Read all characters from the PS file. */
      while( ( c = fgetc( fd ) ) != EOF ) {

/* If we have reached the end of a non-empty line... */
         if( ( c == '\r' || c == '\n') && linelen > 0 ) {

/* Append a null character to the line buffer. */
            iat = linelen++;
            linebuf = astGrow( linebuf, linelen, 1 );
            linebuf[ iat ] = 0;

/* Append the line buffer to the "data" array in the returned structure. */
            iat = (result->nline)++;
            result->data = astGrow( result->data, result->nline,
                                    sizeof( *(result->data) ) );
            (result->data)[ iat ] = linebuf;

/* If this is the first line, check it looks right. If not, break and
   return NULL. */
            nc = astChrLen( linebuf );
            if( iat == 0 ) {
               if( strncmp( linebuf, "%!PS-Adobe-", 11 ) ||
                   linebuf[ 12 ] != '.' ||
                   strncmp( linebuf + 14, " EPSF-", 6 ) ||
                   linebuf[ 21 ] != '.' ) {
                  result = agp1FreeEPS( result );
                  break;
               }

/* If this line contains the DOCSTART string then we are not reading
   a single EPS picture. */
            } else if( strstr( linebuf, DOCSTART ) ) {
               result = agp1FreeEPS( result );
               break;

/* If this line contains the DOCEND string then we have reached the
   end of the picture. Remove the line, plus the previous comment line. */
            } else if( strstr( linebuf, DOCEND ) ) {
               (result->nline)--;
               result->data[ result->nline ] = astFree( result->data[ result->nline ] );
               if( strstr( result->data[ result->nline - 1 ], "%%EndFile" ) ) {
                  (result->nline)--;
                  result->data[ result->nline ] = astFree( result->data[ result->nline ] );
               }
               break;

/* If this is the first "%%BoundingBox: (atend)" line to be found, record
   its index. */
            } else if( nc == strlen(ATEND) && !strncmp( linebuf, ATEND, nc ) ) {
               if( atend == 0 ) atend = iat;

/* If this is the first "%%Orientation:" line to be found, record its index. */
            } else if( !strncmp( linebuf, ORIENT, strlen(ORIENT) ) ) {
               if( result->orient == 0 ) result->orient = iat;

/* If this is a real "%%BoundingBox: ..." line record its index. This
   will leave us at the end with the index of the last such line. */
            } else if( !strncmp( linebuf, BBOX, strlen(BBOX) ) ) {
               bbox = iat;
            }

/* Reset the line buffer variables ready for the next line. */
            linebuf = NULL;
            linelen = 0;

/* Otherwise just add the next character to the end of the line buffer. */
         } else {
            iat = linelen++;
            linebuf = astGrow( linebuf, linelen, 1 );
            linebuf[ iat ] = c;
         }
      }

/* Parse the last bounding box line. */
      if( result && bbox ) {
         if( sscanf( (result->data)[ bbox ] + strlen(BBOX),
                     "%lf %lf %lf %lf",
                     result->bbox, result->bbox + 1,
                     result->bbox + 2, result->bbox + 3 ) == 4 ) {

/* We ignore any EPS file with a zero sized bounding box. */
            if( result->bbox[0] == result->bbox[2] ||
                result->bbox[1] == result->bbox[3] ) {
               result = agp1FreeEPS( result );

/* If we found both bounding box lines, replace the first one with the
   last one. */
            } else if( atend ){
               (void) astFree( (result->data)[ atend ] );
               (result->data)[ atend ] = (result->data)[ bbox ];

               for( iline = bbox + 1; iline < result->nline; iline++ ) {
                  (result->data)[ iline - 1 ] = (result->data)[ iline ];
               }
               (result->data)[ iline - 1 ] = NULL;
               result->nline--;
            }
         }
      }
   }

/* Return the result. */
   return result;
}

static void agp1WriteEPSList( EPSList *list, const char *file, int *status ){
/*
*  Name:
*     agp1WriteEPSList

*  Purpose:
*     Write an EPSList structure out to a file

*  Description:
*     This function writes the supplied EPS structure out to a file handle.

*  Parameters:
*     list
*       The EPSList structure to write out.
*     file
*       The file path to create.
*     status
*       A pointer to the inherited status value.

*/

/* Local Variables; */
   int i;
   time_t ticks;
   struct tm *timeptr;
   char *date;
   FILE *fd;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Open the file for writing. */
   fd = fopen( file, "w" );
   if( fd ) {

/* If it has only one picture, write it out as is. */
      if( list->ndata == 1 ) {
         agp1WriteEPS( list->data[ 0 ], fd, 0, status );

/* If it has more than one picture, write them out with appropriate
   headers and footers. */
      } else {

/* Write the header for the combined EPS file, including the bounding
   box. */
         EPS *first = (list->data)[0];
         fprintf( fd, "%s\n", (first->data)[ 0 ] );
         fprintf( fd, "%s\n", (first->data)[ first->orient ] );
         fprintf( fd, "%%%%Creator: AGI\n" );
         ticks = time( 0 );
         timeptr = localtime( &ticks );
         date = asctime( timeptr );
         fprintf( fd, "%%%%Creation date: %s", date );
         fprintf( fd, "%%%%Pages: 1\n" );
         fprintf( fd, "%%%%pictureFonts: (atend)\n" );
         fprintf( fd, "%%%%BoundingBox: %d %d %d %d\n", (int) list->bbox[0],
                  (int) list->bbox[1], (int) list->bbox[2], (int) list->bbox[3] );
         fprintf( fd, "%%%%EndComments\n" );

/* Write definitions of BEGINAGIFILE and ENDAGIFILE. These are taken from
   the ENCAPSULATED Postscript FILES picture V2.0 with the addition of
   the re-definition of defaultmatrix which the GKS postscript driver uses.
   (this may be a bug in GKS). */
         fprintf( fd, "/%s{\n", DOCSTART );
         fprintf( fd, "    /EPSFsave save def\n");
         fprintf( fd, "    0 setgray 0 setlinecap 1 setlinewidth 0 setlinejoin\n");
         fprintf( fd, "        10 setmiterlimit [] 0 setdash\n" );
         fprintf( fd, "    newpath\n" );
         fprintf( fd, "    /showpage {} def\n" );
         fprintf( fd, "    /defaultmatrix {currentmatrix} def\n" );
         fprintf( fd, "} bind def\n" );
         fprintf( fd, "/%s{\n", DOCEND );
         fprintf( fd, "    EPSFsave restore\n" );
         fprintf( fd, "} bind def\n" );

/* Append each of the EPS pictures in the list. */
         for( i = 0; i < list->ndata; i++ ) {
            agp1WriteEPS( list->data[ i ], fd, 1, status );
         }

/* Write the footer for the combined EPS file. */
         fprintf( fd, "showpage\n" );
         fprintf( fd, "%%%%Trailer\n");
      }

/* Close the output file. */
      fclose( fd );
   }
}


#include "dat_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "sae_par.h"
#include "star/util.h"
#include <stdlib.h>
#include <string.h>

void ndf1Opfor( HDSLoc *loc, const char *name, const char *mode,
                NdfACB **acb, int *status ){
/*
*+
*  Name:
*     ndf1Opfor

*  Purpose:
*     Open an NDF, possibly converting from a foreign format file.

*  Synopsis:
*     void ndf1Opfor( HDSLoc *loc, const char *name, const char *mode,
*                     NdfACB **acb, int *status )

*  Description:
*     This function opens an existing NDF for a specified form of access,
*     importing it into the NDF_ system and returning an ACB index for it.
*     If the object specified does not exist in native NDF format, then a
*     search may be made (according to the contents of the Format
*     Conversion Block and the Tuning Control Block) for a suitable foreign
*     format file which may be converted into NDF format and then accessed.
*     If a foreign format file is accessed, then the NDF name supplied is
*     interpreted as the name of that file, although in all cases a
*     subscript expression may also be appended.

*  Parameters:
*     loc
*        Locator which, in conjunction with the "name" value, identifies
*        the NDF to be opened. A value of NULL may be given to
*        indicate that "name" contains the absolute name of an NDF object
*        or of a foreign format file.
*     name
*        Pointer to a null terminated string holding the name of the NDF to
*        be opened. If "loc" is set to NULL, this should be an
*        absolute NDF name, or the name of a foreign format file (with an
*        optional subscript expression appended). Otherwise it should be a
*        relative NDF name.
*     mode
*        Pointer to a null terminated string holding the required mode of
*        access to the NDF ("READ", "UPDATE" or "WRITE").
*     *acb
*        Pointer to the new NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     If this function is called with "status" set, then a value of zero
*     will be returned for the "acb" parameter, although no further
*     processing will occur.  The same value will also be returned if the
*     function should fail for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.
*     21-NOV-2019 (DSB):
*        Reduce the time that this function has the TCB mutex locked, so
*        that other threads can proceed more rapidly.

*-
*/

/* Local Variables: */
   HDSLoc *ndfloc = NULL;/* Locator for associated NDF */
   NdfACB *acbt;         /* Pointer to temporary ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   NdfFCB *fcb;          /* FCB code for foreign formats */
   char *substring = NULL;/* Pointer to dynamic memory holding substring */
   char expfil[ NDF__SZFIL + 1 ];  /* Expanded file name string */
   char fext[ 21 ];      /* Foreign filename extension */
   char forfil[ NDF__SZFIL + 1 ];  /* Foreign file name */
   char forid[ NDF__SZFID + 1 ];   /* Foreign format file ID */
   char ndfnam[ NDF__SZREF + 1 ];  /* Name of associated native NDF */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode string */
   int active;           /* NDF is already in use? */
   int cvt;              /* Conversion required? */
   int direct;           /* Open th NDF directly as a native HDS object? */
   int found;            /* Input file identified? */
   int islot;            /* Slot index */
   int keep;             /* Keep the native format object? */
   int next;             /* Next DCB entry to consider */
   int report;           /* Report error if file does not exist? */
   int url;              /* Name is URL */
   size_t d1;            /* First character of directory field */
   size_t d2;            /* Last character of directory field */
   size_t lexp;          /* Length of expanded file name string */
   size_t lfor;          /* Number of characters in file name */
   size_t lnam;          /* Number of characters in NDF name */
   size_t n1;            /* First character of name field */
   size_t n2;            /* Last character of name field */
   size_t o1;            /* First character of object name */
   size_t o2;            /* Last character of object name */
   size_t s1;            /* First character of subscripts */
   size_t s2;            /* Last character of subscripts */
   size_t t1;            /* First character of type field */
   size_t t2;            /* Last character of type field */
   size_t tmin;          /* Anticipated start of type field */
   size_t v1;            /* First character of version field */
   size_t v2;            /* Last character of version field */
   size_t x1;            /* First character of foreign extension field */
   size_t x2;            /* Last character of foreign extension field */
   size_t xx1;           /* First character of foreign extension field */
   size_t xx2;           /* Last character of foreign extension field */

/* Set an initial null value for the "acb" parameter. */
   *acb = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Validate the access mode string. */
   ndf1Vmod( mode, vmode, sizeof( vmode ), status );

/* Initialise. */
   cvt = 0;
   url = 0;

/* Ensure the current thread has exclusive access to the global TCB
   values. */
   NDF__TCB_LOCK_MUTEX;

/* If an active input locator has been supplied (indicating that we
   need not consider a foreign format file), or there are no foreign
   data formats to be recognised on input, or the "Ndf_TCB_docvt" flag is
   zero indicating that foreign format conversions are not required,
   then flag that the NDF shouldbe opened directly as a native format object. */
   direct = ( loc || !Ndf_TCB_forin || !Ndf_TCB_docvt );

/* See if the native format object is to be kept. */
   keep = Ndf_TCB_keep;

/* Allow other threads to access the tuning parameters. */
   NDF__TCB_UNLOCK_MUTEX;

/* No foreign formats.
   ==================
   If required, open the NDF directly as a native format object. */
   if( *status == SAI__OK ) {
      if( direct ){
         ndf1Nfind( loc, name, vmode, acb, status );

/* Foreign formats.
   ===============
   If there are foreign formats to be recognised, then first check for URI"s
   and construct an appropriate standard foreign file format, then split the
   NDF name into an object name and an (optional) subscript expression. */
      } else {
         if( strstr( name, "://" ) ) url = 1;
         ndf1Nsplt( name, 0, &o1, &o2, &s1, &s2, status );
         if( *status == SAI__OK ) {

/* The object name found above may include a foreign extension specifier
   (e.g. a FITS extension). Locate the start and and of any such string. */
            ndf1Forxt( name, 0, o2, &x1, &x2, status );

/* At the moment, foreign extension specifiers can only be used when
   converting from a foreign format to NDF, not the other way round.
   Therefore, report an error if the object name includes a foreign
   extension specifier and the access mode is not READ. */
            if( strcmp( vmode, "READ" ) && x1 <= x2 && *status == SAI__OK ) {
               *status = NDF__ACDEN;
               msgSetc( "MODE", vmode );
               msgSetc( "FILE", name );
               errRep( " ", "Unable to open the foreign format file "
                       "'^FILE' for ^MODE access.", status );
               errRepf( " ", "Extension specifiers such as '%.*s' may only "
                       "be used if the foreign file is accessed "
                       "read-only.", status, (int)( x2 - x1 + 1 ), name + x1 );
            }

/* Adjust the index of the end of the object name to exclude any foreign
   extension specifier. If there is no foreign extension specifier, this
   will leave "o2" unchanged. */
            o2 = x1 - 1;

/* Mark the error stack and attempt to expand the object name as if it
   was a normal file name. If this doesn't succeed (we may actually have
   an NDF name whose syntax is not valid for the host operating system,
   for instance), then annul the error and use the original name as
   supplied. */
            errMark();
            if( url ) {
               lexp = NDF_MIN( NDF_MAX( 1, o2 ), sizeof( expfil ) - 1 );
               star_strlcpy( expfil, name + o1 - 1, sizeof( expfil ) );
            } else {
               substring = ndf1Strip( substring, name, o1, o2, NULL, NULL, status );
               ndf1Expfn( substring, 0, expfil, sizeof( expfil ), forid,
                          sizeof( forid ), status );
               lexp = NDF_MAX( 1, strlen(expfil) );
               if( *status != SAI__OK ) {
                  errAnnul( status );
                  lexp = NDF_MIN( NDF_MAX( 1, o2 - o1 + 1 ),
                                  sizeof( expfil ) - 1 );
                  star_strlcpy( expfil, name + o1 - 1, sizeof( expfil ) );
               }
            }
            errRlse();

/* Split the resulting name into directory, name, type and version
   fields (any of which may be absent). */
            ndf1Fsplt( expfil, 1, 0, &d1, &d2, &n1, &n2, &t1, &t2, &v1, &v2,
                       status );
         }

/* File type present.
   =================
   If a file type field appears to be present, then we must determine
   whether it identifies a foreign format file. Loop to test against
   each recognised foreign input format. */
         if( *status == SAI__OK ) {
            if( url || ( t1 <= t2 ) ) {
               found = 0;

               next = 0;
               islot = -1;
               NDF__FCB_LOCK_MUTEX;
               fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               while( *status == SAI__OK && next != -1 ){
                  islot = next;
                  if( fcb->infmt ) {

/* Since the file extension may contain a "." character, it may actually
   be longer than identified above (i.e. the end of the name field may
   still contain the first part of the file extension). Find the first
   character position at which the full file extension field could
   start (allowing it to extend into the name field, if present, but not
   into the directory field). */
                     tmin = t1;
                     if( n2 >= n1 ) tmin = n1;

/* Adjust the anticipated starting position for the expected file
   extension. */
                     tmin = NDF_MIN( NDF_MAX( tmin, t2 - strlen( fcb->ext ) + 1 ), t1 );

/* Test if the file extension field matches (be case sensitive if
   necessary). We pretend that the a file type of ".URL" has been
   supplied if the expanded file looks like a "url". This is just a trick
   for picking up the correct conversion command. */
                     if( url ) {
                        star_strlcpy( fext, ".URL", sizeof( fext ) );
                     } else {
                        star_strlcpy( fext, expfil + tmin - 1,
                                      sizeof( fext ) );
                     }
                     ndf1Cmpfl( fext, 1, 0, fcb->ext, &found, status );

/* Quit searching if a match is found or an error occurs. */
                     if( found || ( *status != SAI__OK ) ) break;

                  }

/* Get a pointer to the next FCB. */
                  fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
               }
               NDF__FCB_UNLOCK_MUTEX;

/* If the file name extension was not recognised, then open the NDF
   directly as a native format object, using the original name string. */
               if( *status == SAI__OK ) {
                  if( !found ) {
                     ndf1Nfind( NULL, name, vmode, acb, status );

/* Otherwise, a foreign format file has probably been specified. Flag
   that conversion is required, mark the error stack and check whether
   the foreign file exists and is accessible (request an error message
   if it is not). */
                  } else {
                     cvt = 1;
                     errMark();

                     if( url ) {
                        report = 0;
                        ndfloc = NULL;
                     } else {
                        report = 1;
                     }

                     substring = ndf1Strip( substring, expfil, 0, lexp, NULL, NULL, status );
                     ndf1Filex( substring, vmode, report, &found, status );

/* If no foreign file was found, then report contextual information. */
                     if( *status != SAI__OK ) {
                        msgSetc( "FMT", fcb->name + 1 );
                        errRep( " ", "Error searching for ^FMT format "
                                "file.", status );

/* Attempt to open the NDF as a native format object instead, using the
   original name string. Begin a new error reporting environment, since
   we are attempting to recover from an earlier error. */
                        errBegin( status );
                        ndf1Nfind( NULL, name, vmode, acb, status );

/* Note if a native NDF object was found, otherwise annul the error. End
   the error reporting environment. */
                        if( *status == SAI__OK ) {
                           cvt = 0;
                        } else {
                           errAnnul( status );
                        }
                        errEnd( status );
                     }

/* If a native NDF was found, then annul the previous error (failure to
   find a foreign file). */
                     if( !cvt ) errAnnul( status );
                     errRlse();

/* Conversion required.
   ===================
   If conversion from a foreign file is required, then re-expand the
   original file name, this time requesting that a file identification
   code be returned (now that we know the file exists and is
   accessible). Save the results for later use. */
                     if( cvt ) {
                        if( url ) {
                           star_strlcpy( forfil, expfil, sizeof( forfil ) );
                           lfor = lexp;
                           star_strlcpy( forid, " ", sizeof( forid ) );
                        } else {
                           substring = ndf1Strip( substring, name, o1, o2, NULL, NULL, status );
                           ndf1Expfn( substring, 1, forfil, sizeof( forfil ),
                                      forid, sizeof( forid ), status );
                        }
                        lfor = NDF_MAX( 1, lfor );
                     }
                  }
               }

/* No file type present.
   ====================
   If no file type appears to be present, then first mark the error
   stack and try to open the NDF directly as a native format object.
   Note if a native format NDF is found. */
            } else {
               found = 0;
               errMark();
               ndf1Nfind( NULL, name, vmode, acb, status );
               if( *status == SAI__OK ) {
                  found = 1;

/* If there is no native format NDF with this name, then annul the
   error. We must now search for a foreign format file. */
               } else if( ndf1Absnt( *status ) ) {
                  errAnnul( status );
               }
               errRlse();

/* If necessary, loop to look for a file with each of the recognised
   foreign input formats in turn, until one is found. */
               if( ( *status == SAI__OK ) && ( !found ) ) {
                  next = 0;
                  islot = -1;
                  NDF__FCB_LOCK_MUTEX;
                  fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
                  while( *status == SAI__OK && next != -1 ){
                     islot = next;
                     if( fcb->infmt ) {

/* Construct the name of the file by appending the foreign format file
   type field to the rest of the file name. Also append the version
   number field if present. */
                        if( d1 <= d2 ) {
                           substring = ndf1Strip( substring, expfil, d1, d2, NULL, NULL, status );
                           star_strlcat( forfil, substring, sizeof( forfil ) );
                        }
                        if( n1 <= n2 ) {
                           substring = ndf1Strip( substring, expfil, n1, n2, NULL, NULL, status );
                           star_strlcat( forfil, substring, sizeof( forfil ) );
                        }
                        star_strlcat( forfil, fcb->ext, sizeof( forfil ) );
                        if( v1 <= v2 ) {
                           substring = ndf1Strip( substring, expfil, v1, v2, NULL, NULL, status );
                           star_strlcat( forfil, substring, sizeof( forfil ) );
                        }
                        lfor = strlen( forfil );

/* Check whether the file exists (do not report an error if it does
   not). */
                        ndf1Filex( forfil, " ", 0, &found, status );

/* If a suitable file has been found, check whether it is accessible
   (this time, request an error report if it is not). */
                        if( ( *status == SAI__OK ) && found ) {
                           ndf1Filex( forfil, vmode, 1, &found, status );

/* If it is not accessible, then report contextual information. */
                           if( *status != SAI__OK ) {
                              msgSetc( "FMT", fcb->name );
                              errRep( " ", "Error searching for ^FMT format "
                                      "file.", status );
                           }
                        }

/* Quit searching when a suitable file is found, or if an error occurs. */
                        if( ( *status != SAI__OK ) || found ) break;
                     }

/* Get a pointer to the next FCB. */
                     fcb = ndf1Nxtsl( NDF__FCBTYPE, islot, &next, status );
                  }
                  NDF__FCB_UNLOCK_MUTEX;

/* If no suitable file was found, then report an error. */
                  if( *status == SAI__OK ) {
                     if( !found ) {
                        *status = NDF__FILNF;
                        msgSetc( "FILE", expfil );
                        msgSetc( "MODE", vmode );
                        errRep( " ", "Unable to open the file '^FILE' for "
                                "^MODE access.", status );
                        errRep( " ", "No file exists with this name and a "
                                "recognised file type extension.", status );

/* Otherwise, flag that conversion is required. */
                     } else {
                        cvt = 1;

/* Expand the identified file name, requesting that a file
   identification code be returned. Save the results for later use. */
                        lexp = lfor + 1;
                        star_strlcpy( expfil, forfil, sizeof( expfil ) );
                        ndf1Expfn( expfil, 1, forfil, sizeof( forfil ),
                                   forid, sizeof( forid ), status );
                        lfor = NDF_MAX( 1, lfor );
                     }
                  }
               }
            }
         }
      }
   }

/* Conversion required.
   ===================
   If conversion of a foreign format file appears to be required, then
   search through the DCB to determine whether any NDF data object
   which is currently in use already has this foreign format file
   and extension associated with it. */
   if( *status == SAI__OK ) {
      if( cvt ) {
         active = 0;
         next = 0;
         islot = -1;
         NDF__DCB_LOCK_MUTEX;
         dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );
         while( ( *status == SAI__OK ) && ( next != -1 ) ){
            islot = next;

/* Search for DCB entries with the same foreign file identification code
   and format (ignore blank identification codes, which indicate that
   identification information could not be obtained for the file). */
            if( ( !strcmp( dcb->forid, forid ) ) && ( astChrLen( forid ) > 0 )
                && ( dcb->fcb == fcb ) ) {

/* Now check that the DCB entry refers to the same foreign extension.
   First locate the bounds of the foreign extension specifier within the
   existing DCB entry. */
               ndf1Forxt( dcb->forfl, 1, 0, &xx1, &xx2, status );

/* If neither the new entry, nor the existing entry refer to a specific
   foreign extension, indicate that the require structure is already
   active. */
               if( xx1 > xx2 && x1 > x2 ) {
                  active = 1;

/* If one but not both entries refer to a specific foreign extension,
   indicate that the require structure is not already active. */
               } else if( xx1 > xx2 || x1 > x2 ) {
                  active = 0;

/* If both entries refer to the same foreign extension, indicate that the
   require structure is already active. */
               } else {
                  active = !strncmp( dcb->forfl+xx1 - 1, name, xx2 - xx1 + 1 );
               }

/* Leave the loop if we have found that the new entry is already active. */
               if( active ) break;

            }
            dcb = ndf1Nxtsl( NDF__DCBTYPE, islot, &next, status );

         }
         NDF__DCB_UNLOCK_MUTEX;

/* If the foreign file is already associated with an NDF, then that NDF
   will contain a converted copy, so there is no need to repeat the
   conversion. */
         if( *status == SAI__OK ) {
            if( active ) {

/* Check whether the required mode of access is compatible with the
   existing access mode. If not, then the data object must be
   re-imported (involving re-opening its container file).  Find the
   name of the object from its DCB locator and use this name to open it
   with the required access mode. */
               if( ( strcmp( vmode, "READ" ) ) && ( strcmp( dcb->mod, "UPDATE" ) ) ) {
                  datRef( dcb->loc, ndfnam, sizeof(ndfnam), status );
                  if( *status == SAI__OK ) {
                     ndf1Nfind( NULL, ndfnam, vmode, acb, status );
                  }

/* If the access modes are compatible, then simply create a new base
   NDF entry in the ACB to refer to the existing DCB entry. */
               } else {
                  ndf1Crnbn( dcb, acb, status );
               }

/* If conversion of a foreign format file is definitely required... */
            } else {

/* Append any foreign extension specifier to the foreign format file spec. */
               if( x1 <= x2 ) {
                  substring = ndf1Strip( substring, name, x1, x2, NULL, NULL, status );
                  star_strlcat( forfil, substring, sizeof( forfil ) );
               }

/* If conversion of a foreign format file is definitely required, then
   identify the native format NDF object which is to be associated with
   it and will hold the converted data. */
               ndf1Ntfor( forfil, fcb, keep, &ndfloc, ndfnam,
                          sizeof( ndfnam ), &lnam, status );

/* If we are getting an "SDF" file (i.e. no file type) from a "url" there will
   be no true conversion step to produce the NDF specified by ndf1Ntfor, so
   we force "ndfnam" to the name of the NDF being retrieved but preceded by
   "URL". This NDF will be affected by the KEEP tuning parameter. */
               if( url ) {
                  if( t2 <= t1 ) {
                     ndfloc = NULL;
                     star_strlcpy( ndfnam, "URL", sizeof( ndfnam ) );
                     star_strlcat( ndfnam, expfil, sizeof( ndfnam ) );
                     lnam = n2 - n1 + 4;
                  }
               }

/* Convert the foreign file. */
               if( *status == SAI__OK ) {
                  ndf1Cvfor( forfil, fcb, ndfloc, ndfnam, 1, status );

/* If conversion appears to have succeeded, then mark the error stack
   and open the resulting native format NDF object, obtaining an ACB
   index for the new base NDF. Specify UPDATE access unless the object
   is to be kept, since we will later need to delete it. */
                  if( *status == SAI__OK ) {
                     errMark();
                     if( keep ) {
                        ndf1Nfind( ndfloc, ndfnam, vmode, acb, status );
                     } else {
                        ndf1Nfind( ndfloc, ndfnam, "UPDATE", acb, status );
                     }

/* If this fails, then format conversion has probably gone wrong. Annul
   any "object not found" type errors, but let others remain (to help
   diagnose any unanticipated problems). */
                     if( *status != SAI__OK ) {
                        if( ndf1Absnt( *status ) ) errAnnul( status );

/* Report (additional) context information, extracting the name of the
   foreign format from the FCB format list string. */
                        *status = NDF__CVTER;
                        msgSetc( "FMT", fcb->name + 1 );

                        msgSetc( "FOR", forfil );
                        if( ndfloc ) {
                           datMsg( "NDF", ndfloc );
                           msgSetc( "NDF", "." );
                        }

                        msgSetc( "NDF", ndfnam );
                        errRep( " ", "Failed to convert the ^FMT format "
                                "file '^FOR' to NDF format in the object "
                                "^NDF.", status );
                     }

/* Release the error stack. */
                     errRlse();

/* If OK, obtain an index to the data object entry in the DCB. */
                     if( *status == SAI__OK ) {
                        dcb = (*acb)->dcb;

/* If we have the NDF object open for UPDATE access (either because it
   must later be deleted or because it resides in a temporary file
   which was previously opened with this access mode) then the DCB
   access mode entry will reflect this. If UPDATE access to the
   object's contents is not actually required, then modify this DCB
   entry, since it will otherwise cause the NDF's contents to be
   written back to the foreign file (with format conversion) when it is
   released. */
                        if( !strcmp( vmode, "READ" ) ) {
                           star_strlcpy( dcb->mod, "READ",
                                         sizeof( dcb->mod ) );
                        }

/* Save the name and identification code of the associated foreign file
   and the FCB index identifying its data format in the DCB. Also note
   that the foreign file existed before we accessed it, and record
   whether the NDF copy of the foreign file is to be kept. */
                        star_strlcpy( dcb->forfl, forfil,
                                      sizeof( dcb->forfl ) );
                        star_strlcpy( dcb->forid, forid,
                                      sizeof( dcb->forid ) );
                        dcb->fcb = fcb;
                        dcb->forex = 1;
                        dcb->forkp = keep;

/* If the converted NDF object is not being kept, then mark its file as
   a scratch file to ensure it will be deleted when finished with. */
                        if( !keep ) ndf1Hscrt( dcb->loc, status );
                     }
                  }
               }

/* Annul the locator used to identify the native format NDF object. */
               if( ndfloc ) datAnnul( &ndfloc, status );
            }

/* If a subscript expression was supplied, then cut the appropriate
   section from the converted NDF and annul the original ACB entry. */
            if( *status == SAI__OK ) {
               if( s1 <= s2 ) {
                  substring = ndf1Strip( substring, name, s1, s2, NULL, NULL, status );
                  ndf1Ncut( *acb, substring, &acbt, status );
                  ndf1Anl( acb, status );
                  *acb = acbt;
                  acbt = NULL;
               }
            }
         }
      }
   }

/* Assign the name of the data file to the MSG token "NDF_EVENT" */
   if( *acb ) ndf1Evmsg( "NDF_EVENT", (*acb)->dcb );

/* Raise an NDF event, describing the opening of an existing NDF. */
   if( !strcmp( vmode, "READ" ) ) {
      ndf1Event( "READ_EXISTING_NDF", status );

   } else if( !strcmp( vmode, "WRITE" ) ) {
      ndf1Event( "WRITE_EXISTING_NDF", status );

   } else if( !strcmp( vmode, "UPDATE" ) ) {
      ndf1Event( "UPDATE_EXISTING_NDF", status );
   }

/* Free dynamic strings. */
   substring = astFree( substring );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Opfor", status );

}


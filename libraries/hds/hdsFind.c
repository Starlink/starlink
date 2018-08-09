#include <stdlib.h>
#include "sae_par.h"
#include "hds.h"
#include "ems.h"
#include "chr.h"
#include "dat_err.h"
#include <string.h>

int hdsFind( HDSLoc *loc1, const char *name, const char *mode,
             HDSLoc **loc2, int *status ){
/*
*+
*  Name:
*     hdsFind

*  Synopsis:
*     int hdsFind( HDSLoc *loc1, const char *name, const char *mode,
*                  HDSLoc **loc2, int *status )

*  Description:
*     This function finds an existing HDS object. Its purpose is similar to
*     that of the HDS function "datFind", except that it permits the
*     component name to contain a series of component fields separated by
*     "." and also allows dimension bounds expressions (e.g. "(1:2,3,,:6)")
*     to be appended to non-scalar HDS objects in order to select a cell or
*     slice from them. Thus, if all the necessary HDS objects exist, a
*     component name such as "MYSTRUCT.AXIS(2).DATA_ARRAY.DATA(20:)" could
*     be given for this function to find. If a blank component name is
*     supplied, then the initial locator supplied is simply cloned.
*     If a NULL input locator is given, then the "name" parameter is
*     assumed to contain a complete object specification, including a
*     container file name. The filename can be specified in " " if a non-
*     standard file extension is being used.

*  Parameters:
*     loc1
*        Locator to an existing HDS object (or NULL if "name" contains a
*        complete object specification).
*     name
*        Pointer to a null terminated string holding the relative path name
*        of the component to be found within the structure (may be blank,
*        or set to the complete object specification if NULL is given for
*        "loc1").
*     mode
*        Pointer to a null terminated string holding the mode of access
*        required to the object: "READ", "UPDATE" or "WRITE". This
*        parameter is only used if "loc1" is NULL, otherwise the mode of
*        access is derived from the input locator.
*     *loc2
*        Returned holding the locator to the object found. This will be a
*        primary locator if "loc1" was set to NULL, otherwise it will
*        be a secondary locator.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then NULL will be
*     returned via the "loc2" parameter. The same value will also be returned
*     if the function should fail for any reason.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *loc = NULL;   /* Temporary locator */
   char *dp;             /* Pointer to '.' in section specification */
   char *lname;          /* Local copy of "name" */
   char c;               /* Saved character */
   hdsbool_t there;      /* Does required component exist? */
   int again;            /* Loop to process another field? */
   int dotted;           /* Path name begins with a '.'? */
   int nfield;           /* Number of component name fields */
   int prim;             /* Primary locator required? */
   size_t ep;            /* End posn for search */
   size_t f1;            /* First character in file name */
   size_t f2;            /* Last character in file name */
   size_t f;             /* First non-blank component character */
   size_t i1;            /* Start of component field */
   size_t i2;            /* End of component field */
   size_t iend;          /* Last non-blank path name character */
   size_t iname;         /* Last component name character */
   size_t l;             /* Last non-blank component character */
   size_t lp;            /* Posn. of left parenthesis */
   size_t rp;            /* Posn. of right parenthesis */
   size_t sp;            /* Start posn for search */

/* Initialise the returned locator. */
   *loc2 = NULL;

/* Fix warnings */
   dotted = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return *status;

/* Create a local copy of the supplied "name" value so that we can modify it. */
   l = strlen( name );
   lname = malloc( l + 1 );
   if( !lname ) {
      emsRepf( " ", "Failed to allocate %zu bytes of memory", status, l + 1 );
   } else {
      strcpy( lname, name );

/* If the input locator is NULL, then the "name" value contains a complete
   object name, including a container file name. Split it into its file name
   and HDS path fields and open the container file. Teminate the local
   copy of "name" temporarily in order to exclude any HDS component path. */
      if( !loc1 ) {
         hdsSplit( lname, &f1, &f2, &i1, &iend, status );
         if( *status == SAI__OK ) {
            c = lname[ f2 + 1 ];
            lname[ f2 + 1 ] = 0;
            hdsOpen( lname + f1, mode, loc2, status );
            lname[ f2 + 1 ] = c;
         }

/* Otherwise, clone the input locator and find the first and last
   non-blank characters in the HDS path string. */
      } else {
         datClone( loc1, loc2, status );
         chrFandl( lname, &i1, &iend );
      }

/* If the HDS path string is not blank, then check to see if it starts
   with a ".". If so, then skip over it. */
      if( ( *status == SAI__OK ) && ( i1 <= iend ) ) {
         dotted = ( lname[ i1 ] == '.' );
         if( dotted ) i1++;
      }

/* In the following code, we assume that "." is the component separator.
   Since we would like to tell the difference between a bad subsection
   and any other error, we need to trap the case where the "." lies
   inside a section. "." is only allowed within an NDF section and not an
   HDS section. Look for matching parens, then look inside for ".". We do this
   in a separate loop to overcome the later INDEX on the whole string. */
      if( ( *status == SAI__OK ) && ( i1 <= iend ) ) {
         again = 1;
         sp = i1;
         ep = iend;
         while( again ) {

/* Look for parens */
            c = lname[ ep + 1 ];
            lname[ ep + 1 ] = 0;
            chrFparx( lname + sp, '(', ')', &lp, &rp );
            lname[ ep + 1 ] = c;

/* Correct the positions for the offset */
            lp += sp;
            rp += sp;

/* Stop looping if we found none */
            if( lp > rp ) {
               again = 0;
            } else {

/* Found something - look for a "." */
               c = lname[ rp + 1 ];
               lname[ rp + 1 ] = 0;
               dp = strchr( lname + lp, '.' );
               if( dp ) {

/* Found a "." so trigger error condition */
                  *status = DAT__SUBIN;
                  emsSetc( "SECT", lname + lp );
                  emsRep( " ", "Invalid section specification. \".\" is not "
                          "allowed in HDS section in ^SECT", status );
                  again = 0;

               } else {
                  lname[ rp + 1 ] = c;

/* Increment search location */
                  sp = rp + 1;
                  ep = iend;
                  if( sp >= ep ) again = 0;
               }

            }
         }
      }

/* If the HDS path string is still not blank, then loop to extract each
   HDS component field from the path. Count the fields as they are
   processed. */
      if( ( *status == SAI__OK ) && ( i1 <= iend  ) ) {
         nfield = 0;
         again = 1;
         while( ( *status == SAI__OK ) && again ){
            nfield++;

/* If we are still within the bounds of the name string, then find the
   end of the next path name field (the last character before a "." or
   end of string). Note if a "." does not terminate the field, in which
   case there are no more fields to process. */
            if( i1 <= iend ) {
               c = lname[ iend + 1 ];
               lname[ iend + 1 ] = 0;
               dp = strchr( lname + i1, '.' );
               lname[ iend + 1 ] = c;
               if( !dp ) {
                  i2 = iend;
                  again = 0;
               } else {
                  i2 = dp - lname - 1;
               }

/* If we are beyond the end of the string, but are making another pass
   to process the (blank) field following a final ".", then use the
   string length as the end of the field and note there are no more
   fields to process. */
            } else {
               i2 = iend;
               again = 0;
            }

/* If the field is missing (two consecutive "." characters appear or
   the string finishes with ".") then report an error. */
            if( i1 > i2 ) {
               *status = DAT__NOCMP;
               emsSetc( "NAME", name );
               emsRep( " ", "Missing field in HDS component name '^NAME'.",
                       status );

/* Find the first and last non-blank characters in the field. Report an
   error if the field is entirely blank. */
            } else {
               c = lname[ i2 + 1 ];
               lname[ i2 + 1 ] = 0;
               chrFandl( lname + i1, &f, &l );
               lname[ i2 + 1 ] = c;
               if( f > l ) {
                  *status = DAT__NOCMP;
                  emsSetc( "NAME", name );
                  emsRep( " ", "Missing field in HDS component name '^NAME'.",
                          status );

/* Search for a parenthesised expression within the field (the dimension
   bounds expression). */
               } else {
                  f += i1;
                  l += i1;

                  c = lname[ l + 1 ];
                  lname[ l + 1 ] = 0;
                  chrFparx( lname + f, '(', ')', &lp, &rp );
                  lname[ l + 1 ] = c;

                  if( lp < rp ) {
                     lp += f;
                     rp += f;

/* Check if there is a component name in front of the opening
   parenthesis. If not, then report an error unless this is the first
   field and the path name string did not begin with a "." (a dimension
   bounds expression alone is allowed as the first field in the path
   name). */
                     c = lname[ l + 1 ];
                     lname[ l + 1 ] = 0;
                     if( ( lp <= f ) && ( dotted || ( nfield != 1 ) ) ) {
                        *status = DAT__NOCMP;
                        emsSetc( "FIELD", lname + f  );
                        emsRep( " ", "Missing name in HDS component field "
                                "'^FIELD'.", status );

/* Check that there are no characters following the closing
   parenthesis. Report an error if there are. */
                     } else if( rp != l ) {
                        *status = DAT__SUBIN;
                        emsSetc( "FIELD", lname + f );
                        emsRep( " ", "Unknown character(s) following subset "
                                "expression in HDS component field '^FIELD'.",
                                status );
                     }
                     lname[ l + 1 ] = c;

/* Note where the component name ends (if present). */
                     iname = lp - 1;
                  } else {
                     iname = l;
                  }

/* If a dimension bounds expression exists in the absence of a
   component name (only permitted in the first field), then select the
   appropriate subset of the object. Promote the resulting locator if
   necessary. */
                  if( *status == SAI__OK ) {
                     if( ( lp < rp ) && ( lp <= f ) ) {
                        c = lname[ rp + 1 ];
                        lname[ rp + 1 ] = 0;
                        datCut( *loc2, lname + lp, &loc, status );
                        lname[ rp + 1 ] = c;
                        if( !loc1 ) {
                           prim = 1;
                           datPrmry( 1, &loc, &prim, status );
                        }

/* Annul the object locator and replace it with the subset locator. */
                        datAnnul( loc2, status );
                        *loc2 = loc;
                        loc = NULL;

/* If a component name exists, then check it for validity and see if
   the required component exists within the current HDS structure. */
                     } else {
                        c = lname[ iname + 1 ];
                        lname[ iname + 1 ] = 0;
                        datChscn( lname + f, status );
                        datThere( *loc2, lname + f, &there, status );
                        if( *status == SAI__OK ) {

/* Report an error if the component does not exist. */
                           if( !there ) {
                              *status = DAT__NAMIN;
                              emsSetc( "NAME", lname + f );
                              datMsg( "STRUCT", *loc2 );
                              emsRep( " ", "There is no '^NAME' component in "
                                      "the HDS structure ^STRUCT", status );

/* Otherwise, locate the required component. Promote the resulting
   locator if necessary. */
                           } else {
                              datFind( *loc2, lname + f, &loc, status );
                              if( !loc1 ) {
                                 prim = 1;
                                 datPrmry( 1, &loc, &prim, status );
                              }

/* Annul the structure locator and replace it with the new object
   locator. */
                              datAnnul( loc2, status );
                              *loc2 = loc;
                              loc = NULL;
                           }

/* Restore the terminating character. Do it here as there is no need to do
   so if an error has occurred. */
                           lname[ iname + 1 ] = c;

/* If a dimension bounds expression exists, then select the appropriate
   subsection from the object. Promote the resulting locator if
   necessary. */
                           if( *status == SAI__OK ) {
                              if( lp < rp ) {
                                 c = lname[ rp + 1 ];
                                 lname[ rp + 1 ] = 0;
                                 datCut( *loc2, lname + lp, &loc, status );
                                 if( !loc1 ) {
                                    prim = 1;
                                    datPrmry( 1, &loc, &prim, status );
                                 }
                                 lname[ rp + 1 ] = c;

/* Annul the object locator and replace it with the subset locator. */
                                 datAnnul( loc2, status );
                                 *loc2 = loc;
                                 loc = NULL;
                              }
                           }
                        }
                     }
                  }
               }
            }

/* Update the character pointer to the next component name field and
   return to process it. */
            i1 = i2 + 2;
         }
      }

/* Free the local copy of the supplied "name". */
      free( lname );
   }

/* If an error has occurred, then annul the returned locator. */
   if( *status != SAI__OK ) datAnnul( loc2, status );

/* Return the status value */
   return *status;

}


#include <stdlib.h>
#include <stdio.h>
#include "sae_par.h"
#include "hds.h"
#include "ems.h"
#include "chr.h"
#include "dat_err.h"
#include <string.h>

static void dat1Pshde( char *, int, const hdsdim[], hdsdim[], hdsdim[], int * );
static void dat1Pshdf( char *, int, hdsdim *, hdsdim *, int * );
static void dat1Pshdb( char *, int, hdsdim *, int * );


int datCut( HDSLoc *loc1, char *str, HDSLoc **loc2, int *status ){
/*
*+
*  Name:
*     datCut

*  Purpose:
*     Cut a cell or slice from an HDS object.

*  Synopsis:
*     int datCut( HDSLoc *loc1, char *str, HDSLoc **loc2, int *status )

*  Description:
*     This function selects a sub-section of an HDS object, generating a
*     new locator to a cell or a slice of the object, as appropriate. The
*     dimension bounds defining the subsection are supplied as a
*     parenthesised character string via the "str" parameter (e.g.
*     "(256,256)", "(,,3)", "(3:5,8:)" or "(,7,,:6)", etc. The number of
*     dimensions implied by this string must match the number of HDS object
*     dimensions and the dimension bounds must lie within the object's
*     bounds. If this string is blank, then the function returns a locator
*     to the whole object by cloning the locator given.

*  Parameters:
*     loc1
*        Locator to HDS object.
*     str
*        Pointer to a null terminated string holding the dimension bounds
*        expression. Note, on exit the supplied string will be terminated
*        to exclude any railing spaces.
*     *loc2
*        Returned holding the locator to the specified sub-section of the
*        HDS object.
*     *status
*        The global status.

*  Notes:
*     -  The syntax of the "str" string will be fully validated by this
*     function. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this function is called with "status" set, then NULL will be
*     returned via the "loc2" parameter. The same value will also be
*     returned if the function should fail for any reason.

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
   char c;               /* Terminating character */
   hdsdim dim[ DAT__MXDIM ];       /* Default upper dimension bounds */
   hdsdim lbnd[ DAT__MXDIM ];      /* Lower dimension bounds */
   hdsdim ubnd[ DAT__MXDIM ];      /* Upper dimension bounds */
   int cell;             /* Is an HDS cell required? */
   int i;                /* Loop counter for dimensions */
   int ndim;             /* Number of object dimensions */
   size_t f;             /* First non-blank character position */
   size_t l;             /* Last non-blank character position */

/* Initialise the returned locator. */
   *loc2 = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return *status;

/* Find the first and last non-blank characters in the dimension bounds
   string. Terminate the supplied string to exclude any trailing spaces. */
   chrFandl( str, &f, &l );
   str[ l + 1 ] = 0;

/* If the string is blank, then simply clone the object's locator. */
   if( f > l ) {
      datClone( loc1, loc2, status );

/* Otherwise, check that the string has enclosing parentheses and report
   an error if it does not. */
   } else if( ( str[ f ] != '(' ) || ( str[ l ] != ')' ) ) {
      *status = DAT__SUBIN;
      emsSetc( "SUBSET", str );
      datMsg( "OBJECT", loc1 );
      emsRep( " ", "Invalid subset '^SUBSET' specified for the HDS object "
              "^OBJECT -- enclosing parenthesis missing.", status );

/* Otherwise, obtain the object's shape. */
   } else {
      datShape( loc1, DAT__MXDIM, dim, &ndim, status );
      if( *status == SAI__OK ) {

/* If the number of dimensions is zero, then report an error, since no
   dimension bounds can be applied (a blank dimension bound string has
   already been checked for above). */
         if( ndim == 0 ) {
            *status = DAT__SUBIN;
            emsSetc( "SUBSET", str );
            datMsg( "OBJECT", loc1 );
            emsRep( " ", "Invalid subset '^SUBSET' specified for the HDS "
                    "object ^OBJECT -- this object is scalar.", status );

/* Remove the enclosing parentheses (report an error if "()" was
   specified) and parse the dimension bounds expression. */
         } else {
            if( !strcmp( str+f, "()" ) ) {
               *status = DAT__DIMIN;
               emsSetc( "SUBSET", str );
               emsSeti( "NDIM", ndim );
               emsRep( " ", "Too few dimensions given in the subset "
                       "expression '(^SUBSET)'; the associated object "
                       "is ^NDIM-dimensional.", status );

            } else {
               c = str[ l ];
               str[ l ] = 0;
               dat1Pshde( str + f + 1, ndim, dim, lbnd, ubnd, status );
               str[ l ] = c;
            }

/* If an error occurs, then report contextual information. */
            if( *status != SAI__OK ) {
               datMsg( "OBJECT", loc1 );
               emsRep( " ", "Unable to select the specified subset of the "
                       "HDS object ^OBJECT", status );

/* Otherwise, loop to test if the lower and upper bounds of each
   dimension are equal. If so, then an HDS cell must be selected. */
            } else {
               cell = 1;
               for( i = 0; i < ndim; i++ ){
                  if( lbnd[ i ] != ubnd[ i ] ) {
                     cell = 0;
                     break;
                  }
               }

/* Select a cell or a slice from the object, as appropriate. */
               if( cell ) {
                  datCell( loc1, ndim, lbnd, loc2, status );
               } else {
                  datSlice( loc1, ndim, lbnd, ubnd, loc2, status );
               }
            }
         }
      }
   }

/* If an error occurred, then return a null locator. */
   if( *status != SAI__OK ) *loc2 = NULL;

/* Return the status */
   return *status;

}

static void dat1Pshde( char *str, int ndim, const hdsdim dim[], hdsdim lbnd[],
                       hdsdim ubnd[], int *status ){
/*
*+
*  Name:
*     dat1Pshde

*  Purpose:
*     Parse an HDS dimension bounds expression.

*  Synopsis:
*     void dat1Pshde( char *str, int ndim, const hdsdim dim[],
*                     hdsdim lbnd[], hdsdim ubnd[], int *status )

*  Description:
*     This function parses an HDS dimension bound expression (such as
*     "1:10,2", "3:,,:7" or "3,,6") and returns the lower and upper bounds
*     for each dimension, using supplied defaults wherever appropriate.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string containing
*        the expression to be parsed. This may be terminated on exit to
*        exclude any trailing spaces.
*     ndim
*        Number of dimension bounds expected.
*     dim
*        Object dimension sizes. The supplied "dim" array should have at
*        least "ndim" elements.
*     lbnd
*        Returned holding the lower bounds for each dimension. The supplied
*        "lbnd" array should have at least "ndim" elements.
*     ubnd
*        Returned holding the upper bounds for each dimension. The supplied
*        "ubnd" array should have at least "ndim" elements.
*     *status
*        The global status.

*  Notes:
*     -  The number of dimension bounds implied by the expression supplied
*     (one more than the number of separating commas which it contains)
*     must match the number specified via the "ndim" parameter. An error
*     will result if this is not the case.
*     -  A value of "ndim"=0 is not permitted; dimension bounds cannot be
*     used with a scalar object.

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
   const char *dc;       /* Pointer to comma */
   char c;               /* Terminating character */
   int comma;            /* Comma terminated a field? */
   int nbnd;             /* Number of dimension bounds */
   size_t f;             /* First non-blank character in field */
   size_t i1;            /* First character position in field */
   size_t i2;            /* Last character position in field */
   size_t l;             /* Last non-blank character in field */
   size_t nc;            /* Length of supplied string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   nbnd = 0;
   i1 = 0;
   comma = 1;
   nc = strlen( str );

/* Loop to extract each dimension bound field from the expression. */
   while( ( *status == SAI__OK ) && comma ){

/* If we are still within the bounds of the expression string, then
   search for the end of the next field (the last character before a
   comma or end of string). Note if a comma did not terminate this
   field. */
      if( i1 < nc ) {
         dc = strchr( str + i1, ',' );
         if( !dc ) {
            i2 = nc - 1;
            comma = 0;
         } else {
            i2 = dc - str - 1;
         }

/* If we are outside the bounds of the expression, but have to make one
   more pass to process the (blank) field following a final comma, then
   use the end of string as the end of the field. */
      } else {
         i2 = nc - 1;
         comma = 0;
      }

/* Increment the count of dimension bounds and report an error if this
   exceeds the expected number of dimensions. */
      nbnd++;
      if( nbnd > ndim ) {
         *status = DAT__DIMIN;
         emsSetc( "SUBSET", str );
         emsSeti( "NDIM", ndim );
         emsRep( " ", "Too many dimensions given in the subset expression "
                 "'(^SUBSET)'; the associated object is "
                 "^NDIM-dimensional.", status );

/* If the field does not exist (i.e. there are two consecutive commas
   or a comma at the start or end of the string) then use the default
   bounds for the current dimension. */
      } else {
         if( i1 > i2 ) {
            lbnd[ nbnd - 1 ] = 1;
            ubnd[ nbnd - 1 ] = dim[ nbnd - 1 ];

/* Otherwise, find the first and last non-blank characters in the
   current dimension field. */
         } else {
            c = str[ i2 + 1 ];
            str[ i2 + 1 ] = 0;
            chrFandl( str + i1, &f, &l );
            str[ i2 + 1 ] = c;

/* If the field is blank, then apply the default bounds. */
            if( f > l ) {
               lbnd[ nbnd - 1 ] = 1;
               ubnd[ nbnd - 1 ] = dim[ nbnd - 1 ];

/* Otherwise, parse the field to determine the dimension bounds. */
            } else {
               f += i1;
               l += i1;
               c = str[ l + 1 ];
               str[ l + 1 ] = 0;
               dat1Pshdf( str + f, dim[ nbnd - 1 ], lbnd + nbnd - 1,
                          ubnd + nbnd - 1, status );
               str[ l + 1 ] = c;

/* Make a contextual error report if an error occurs. */
               if( *status != SAI__OK ) {
                  emsSeti( "NBND", nbnd );
                  emsSetc( "SUBSET", str );
                  emsRep( " ", "Error in dimension ^NBND of the subset "
                          "expression '(^SUBSET)'.", status );
               }
            }
         }
      }

/* Increment the pointer to the start of the next field and return to
   process it. */
      i1 = i2 + 2;
   }

/* If the number of dimension bounds obtained is less than expected,
   then report an error. */
   if( ( *status == SAI__OK ) && ( nbnd < ndim ) ) {
      *status = DAT__DIMIN;
      emsSetc( "SUBSET", str );
      emsSeti( "NDIM", ndim );
      emsRep( " ", "Too few dimensions given in the subset expression "
              "'(^SUBSET)'; the associated object is ^NDIM-dimensional.",
              status );
   }

}

static void dat1Pshdf( char *str, int dim, hdsdim *lbnd, hdsdim *ubnd,
                       int *status ){
/*
*+
*  Name:
*     dat1Pshdf

*  Purpose:
*     Parse an HDS dimension bound field.

*  Synopsis:
*     void dat1Pshdf( char *str, int dim, hdsdim *lbnd, hdsdim *ubnd,
*                     int *status )

*  Description:
*     This function parses a dimension bound field for an HDS array object
*     to determine the lower and upper bounds to be applied when selecting
*     a subset in that dimension. The lower and upper bounds are separated
*     by a colon ":" (e.g. "10:20").  Suitable default values are returned
*     if either or both halves of the field are omitted (e.g. "100:",
*     ":100", ":", etc.). If no colon is present, then the upper bound is
*     set to equal the lower bound (unless the string is blank, which is
*     equivalent to ":").

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        parsed. It may be terminated on exit to exclude any trailing
*        spaces.
*     dim
*        The object dimension size.
*     *lbnd
*        Returned holding the lower bound.
*     *ubnd
*        Returned holding the upper bound.
*     *status
*        The global status.

*  Notes:
*     -  The values obtained by parsing the string are constrained to lie
*     within the object bounds. An error will be reported, and "status"
*     set, if they do not.
*     -  An error will be reported, and "status" set, if the lower bound
*     exceeds the upper bound.

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
   char c;               /* Terminating character */
   char *pc;             /* Pointer to colon */
   size_t colon;         /* Character position of colon ':' */
   size_t f;             /* Position of first non-blank character */
   size_t l;             /* Position of last non-blank character */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find the first and last non-blank characters in the string. */
   chrFandl( str, &f, &l );

/* If the string is blank, then return the default lower and upper
   bounds. */
   if( f > l ) {
      *lbnd = 1;
      *ubnd = dim;

/* Otherwise, locate the colon ":" which separates the lower and upper
   bounds. */
   } else {
      pc = strchr( str, ':' );
      if( !pc ) {
         colon = l + 1;
      } else {
         colon = pc - str;
      }

/* If a colon appears at the start, then use the default lower bound. */
      if( colon <= f ) {
         *lbnd = 1;

/* Otherwise, parse the string in front of the colon to obtain the
   lower bound. */
      } else {
         c = str[ colon ];
         str[ colon ] = 0;
         dat1Pshdb( str + f, 1, lbnd, status );
         str[ colon ] = c;
         if( *status == SAI__OK ) {

/* Check that the value obtained lies inside the object bounds and
   report an error if it does not. */
            if( ( *lbnd < 1 ) || ( *lbnd > dim ) ) {
               *status = DAT__BOUND;
               emsSeti( "LBND", *lbnd );
               emsSeti( "DIM", dim );
               emsRep( " ", "Lower bound (^LBND) lies outside object "
                       "bounds (1:^DIM).", status );
            }
         }
      }

/* If there is no colon present, then the upper bound equals the lower
   bound. */
      if( colon > l ) {
         *ubnd = *lbnd;

/* Otherwise, if the colon appears at the end of the string, then use
   the default upper bound. */
      } else if( colon == l ) {
         *ubnd = dim;

/* Otherwise, parse the string which follows the colon to determine the
   upper bound. */
      } else {
         c = str[ l + 1 ];
         str[ l + 1 ] = 0;
         dat1Pshdb( str + colon + 1, dim, ubnd, status );
         str[ l + 1 ] = c;
         if( *status == SAI__OK ) {

/* Check that the value obtained lies inside the object bounds and
   report an error if it does not. */
            if( ( *ubnd < 1 ) || ( *ubnd > dim ) ) {
               *status = DAT__BOUND;
               emsSeti( "UBND", *ubnd );
               emsSeti( "DIM", dim );
               emsRep( " ", "Upper bound (^UBND) lies outside object  "
                       "bounds (1:^DIM).", status );
            }
         }
      }
   }

/* If the lower bound exceeds the upper bound, then report an error. */
   if( ( *status == SAI__OK ) && ( *lbnd > *ubnd ) ) {
      *status = DAT__BOUND;
      emsSeti( "LBND", *lbnd );
      emsSeti( "UBND", *ubnd );
      emsRep( " ", "Lower bound (^LBND) exceeds upper bound (^UBND).", status );
   }

}

#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "dat_err.h"

static void dat1Pshdb( char *str, int def, hdsdim *value, int *status ){
/*
*+
*  Name:
*     dat1Pshdb

*  Purpose:
*     Parse an HDS object dimension bound.

*  Synopsis:
*     void dat1Pshdb( char *str, int def, hdsdim *value, int *status )

*  Description:
*     This function parses a string representing an upper or lower
*     dimension bound of an HDS array object. If the string is blank, then
*     a default value is returned. Leading and trailing spaces are ignored.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the string to be
*        parsed.
*     def
*        Default value to be returned if the string is blank.
*     *value
*        Returned holding the dimension bound value.
*     *status
*        The global status.

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
*     RFWS: R."f". Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   size_t f;             /* Position of first non-blank character */
   size_t l;             /* Position of last non-blank character */
   size_t slen;          /* Length of string */
   int nc;               /* Number of characters read */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Find the first and last non-blank characters in the string. */
   chrFandl( str, &f, &l );

/* If the input string is blank, then return the default value. */
   if( f > l ) {
      *value = def;

/* Otherwise, attempt to convert the string to an integer. */
   } else {
      slen = strlen( str );
      if( nc = 0, ( sscanf( str, "%" HDS_DIM_FORMAT "%n", value, &nc ) < 1 || nc < slen ) ){

/* If the attempt fails, then report an error message. */
         if( *status != SAI__OK ) {
            *status = DAT__DIMIN;
            emsSetc( "BADBOUND", str );
            emsRep( " ", "Invalid dimension bound '^BADBOUND' specified; bad "
                    "syntax.", status );
         }
      }
   }
}


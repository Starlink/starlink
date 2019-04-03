#include <string.h>
#include "sae_par.h"
#include "star/util.h"
#include "dat_par.h"
#include "ndf_ast.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include "star/util.h"

void ndf1Pscpx( const char *str, int mxextn, char extn[][ DAT__SZNAM + 1 ],
                int *nextn, int cpf[], int *status ){
/*
*+
*  Name:
*     ndf1Pscpx

*  Purpose:
*     Parse an NDF component propagation expression.

*  Synopsis:
*     void ndf1Pscpx( const char *str, int mxextn, char extn[][ DAT__SZNAM
*                     + 1 ], int *nextn, int cpf[], int *status )

*  Description:
*     This function parses an expression specifying which components of an
*     NDF are to be propagated when a new NDF is created based on an
*     existing template. The expression should contain a comma separated
*     list of component names (optionally abbreviated) or component names
*     prefixed with "NO" (to indicate that the specified component should
*     not be propagated). By default the HISTORY, LABEL and TITLE
*     components are propagated. All extensions are also propagated by
*     default except for any that have had a zero value assigned to the
*     corresponding "PXT..." tuning parameter using ndfTune. Named
*     extensions may be included or excluded (over-riding the defaults set
*     by the "PXT..." tuning parameters) by specifying EXTENSION( ) or
*     NOEXTENSION( ) as one of the list items with a list of the extensions
*     to be affected contained within the parentheses. The same component
*     name may appear more than once in the list, and the effects of each
*     occurrence are cumulative (i.e. the latter occurrence takes
*     precedence).  An asterisk (*) can be used as a wild card to match all
*     extensions. The function returns an array of logical component
*     propagation flags and a list of the names of extensions which are not
*     to be propagated.

*  Parameters:
*     str
*        Pointer to a null terminated string holding the expression to be
*        parsed.
*     mxextn
*        Maximum number of names to be returned in the "extn" array (i.e.
*        the declared size of this array).
*     extn
*        On entry, a list of the names of all available NDF extensions. On
*        exit, a list of the names of NDF extensions which are not to be
*        propagated. The supplied "extn" array should have at least
*        "mxextn" elements.
*     *nextn
*        On entry, the number of available extension names supplied in the
*        "extn" array. On exit, the number of extension names returned in
*        the "extn" array.
*     cpf
*        Returned holding the array of component propagation flags.
*        Symbolic constants are defined in the header file "ndf1.h" to
*        identify the elements of this array. The supplied "cpf" array
*        should have at least "NDF__MXCPF" elements.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  License:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program (see "slaConditions"); if not, write to the
*     Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
*     Boston, MA  02110-1301  USA

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA 02111-1307,
*     USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   AstKeyMap *pxt;       /* KeyMap holding extension flags */
   const char *key;      /* Extension name */
   int i;                /* Index into KeyMap */
   int includ;           /* Non-zero if extension is to be copied */
   int nkey;             /* Number of keys in the KeyMap */
   int recog;            /* Whether item was recognised */
   size_t f;             /* Position of start of name to test */
   size_t i1;            /* Character position of start of item */
   size_t i2;            /* Character position of end of item */
   size_t j1;            /* Position of opening parenthesis */
   size_t j2;            /* Position of closing parenthesis */
   size_t l;             /* Position of end of name to test */
   size_t slen;          /* Length of supplied string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the TCB contains an AST KeyMap holding default propagation flags
   for NDF extensions, then take a copy of it so we can modify it without
   making any permanatent changes. Otherwise, create a new empty KeyMap.
   Also, store the names of all available extensions in the KeyMap. Each
   is given an associated value of one, indicating that the extension
   should be copied. Any "NOEXTENSION" elements in the supplied expression
   may cause some of these values to be set to zero, indicating that the
   extension is not to be copied. */
   NDF__TCB_LOCK_MUTEX;
   if( Ndf_TCB_pxt ) {
      pxt = astCopy( Ndf_TCB_pxt );
      for( i = 0; i < *nextn; i++ ){
         if( !astMapHasKey( pxt, extn[ i ] ) ) astMapPut0I( pxt, extn[ i ], 1, " " );
      }
   } else {
      pxt = astKeyMap( " " );
      for( i = 0; i < *nextn; i++ ){
         astMapPut0I( pxt, extn[ i ], 1, " " );
      }
   }
   NDF__TCB_UNLOCK_MUTEX;

/* Initialise the count of returned extensions. */
   *nextn = 0;

/* Initialise the component propagation flags. */
   cpf[ NDF__ACPF ] = 0;
   cpf[ NDF__DCPF ] = 0;
   cpf[ NDF__HCPF ] = 1;
   cpf[ NDF__LCPF ] = 1;
   cpf[ NDF__QCPF ] = 0;
   cpf[ NDF__TCPF ] = 1;
   cpf[ NDF__UCPF ] = 0;
   cpf[ NDF__VCPF ] = 0;
   cpf[ NDF__WCPF ] = 0;

/* Initialise a pointer to the start of the "current" item in the
   component list. */
   i1 = 0;

/* Loop to process each item in the list. */
   slen = strlen( str );
   while( i1 < slen && *status == SAI__OK ){

/* Find the end of the current item (the last character before the next
   unparenthesised comma or end of string). */
      i2 = i1;
      if( ndf1Indxp( str, ',', &i2 ) ) {
         i2--;
      } else {
         i2 = slen - 1;
      }

/* Find the first and last characters in the item (excluding surrounding
   blanks). */
      if( i1 <= i2 ) {
         astFandl( str, i1, i2, &f, &l );

/* Check the item is not completely blank. */
         if( f <= l ) {

/* Compare the item with each permitted value in turn, allowing
   abbreviation. Set the appropriate component propagation flag values
   and note if the item is recognised. */
            recog = 0;

/* ...AXIS. */
            if( ndf1Simlr( str, f, l, "AXIS", NDF__MINAB ) ) {
               cpf[ NDF__ACPF ] = 1;
               recog = 1;

/* ...NOAXIS. */
            } else if( ndf1Simlr( str, f, l, "NOAXIS", NDF__MINAB + 2 ) ) {
               cpf[ NDF__ACPF ] = 0;
               recog = 1;

/* ...DATA. */
            } else if( ndf1Simlr( str, f, l, "DATA", NDF__MINAB ) ) {
               cpf[ NDF__DCPF ] = 1;
               recog = 1;

/* ...NODATA. */
            } else if( ndf1Simlr( str, f, l, "NODATA", NDF__MINAB + 2 ) ) {
               cpf[ NDF__DCPF ] = 0;
               recog = 1;

/* ...HISTORY. */
            } else if( ndf1Simlr( str, f, l, "HISTORY", NDF__MINAB ) ) {
               cpf[ NDF__HCPF ] = 1;
               recog = 1;

/* ...NOHISTORY. */
            } else if( ndf1Simlr( str, f, l, "NOHISTORY", NDF__MINAB + 2 ) ) {
               cpf[ NDF__HCPF ] = 0;
               recog = 1;

/* ...LABEL. */
            } else if( ndf1Simlr( str, f, l, "LABEL", NDF__MINAB ) ) {
               cpf[ NDF__LCPF ] = 1;
               recog = 1;

/* ...NOLABEL. */
            } else if( ndf1Simlr( str, f, l, "NOLABEL", NDF__MINAB + 2 ) ) {
               cpf[ NDF__LCPF ] = 0;
               recog = 1;

/* ...QUALITY. */
            } else if( ndf1Simlr( str, f, l, "QUALITY", NDF__MINAB ) ) {
               cpf[ NDF__QCPF ] = 1;
               recog = 1;

/* ...NOQUALITY. */
            } else if( ndf1Simlr( str, f, l, "NOQUALITY", NDF__MINAB + 2 ) ) {
               cpf[ NDF__QCPF ] = 0;
               recog = 1;

/* ...TITLE. */
            } else if( ndf1Simlr( str, f, l, "TITLE", NDF__MINAB ) ) {
               cpf[ NDF__TCPF ] = 1;
               recog = 1;

/* ...NOTITLE. */
            } else if( ndf1Simlr( str, f, l, "NOTITLE", NDF__MINAB + 2 ) ) {
               cpf[ NDF__TCPF ] = 0;
               recog = 1;

/* ...UNITS. */
            } else if( ndf1Simlr( str, f, l, "UNITS", NDF__MINAB ) ) {
               cpf[ NDF__UCPF ] = 1;
               recog = 1;

/* ...NOUNITS. */
            } else if( ndf1Simlr( str, f, l, "NOUNITS", NDF__MINAB + 2 ) ) {
               cpf[ NDF__UCPF ] = 0;
               recog = 1;

/* ...VARIANCE. */
            } else if( ndf1Simlr( str, f, l, "VARIANCE", NDF__MINAB ) ) {
               cpf[ NDF__VCPF ] = 1;
               recog = 1;

/* ...NOVARIANCE. */
            } else if( ndf1Simlr( str, f, l, "NOVARIANCE", NDF__MINAB + 2 ) ) {
               cpf[ NDF__VCPF ] = 0;
               recog = 1;

/* ...WCS. */
            } else if( ndf1Simlr( str, f, l, "WCS", NDF__MINAB ) ) {
               cpf[ NDF__WCPF ] = 1;
               recog = 1;

/* ...NOWCS. */
            } else if( ndf1Simlr( str, f, l, "NOWCS", NDF__MINAB + 2 ) ) {
               cpf[ NDF__WCPF ] = 0;
               recog = 1;

/* If the item did not match any of the above, then it may be an
   EXTENSION specification, followed by a parenthesised list of
   extension names. Search for a parenthesesed expression. */
            } else {
               astBrackets( str, f, l, '(', ')', 1, &j1, &j2, NULL, NULL, NULL );

/* If found, then test the characters lying in front of the opening
   parenthesis (if there are any). */
               if( j1 <= j2 ) {
                  if( j1 > f ) {
                     if( ndf1Simlr( str, f, j1 - 1, "EXTENSION", NDF__MINAB ) ) {

/* If this is an EXTENSION specification, then update the KeyMap to
   ensure that the named extensions have a non-zero value and will thus
   be propagated. */
                        recog = 1;
                        ndf1Pxlst( 1, str, j1 + 1, j2 - 1, pxt, status );

/* Perform the appropriate updating operation if this is a NOEXTENSION
   specification. */
                     } else if( ndf1Simlr( str, f, j1 - 1, "NOEXTENSION", NDF__MINAB ) ) {
                        recog = 1;
                        ndf1Pxlst( 0, str, j1 + 1, j2 - 1, pxt, status );
                     }
                  }
               }
            }

/* If the list item was not ecognised, then report an error. */
            if( !recog ) {
               *status = NDF__CNMIN;
               errRepf( " ", "Invalid component name '%.*s' specified "
                        "(possible programming error).", status,
                        (int)(l - f + 1), str + f );
            }
         }
      }

/* Increment the pointer to the start of the next list item and return
   to process it. */
      i1 = i2 + 2;
   }

/* Obtain the list of NDF extensions that are not to be propagated. To
   do this, go through all entries in the KeyMap looking for any that
   have an associated value of zero. The keys associated with such
   entries are the names of the extensions that are not to be propagated. */
   nkey = astMapSize( pxt );
   for( i = 0; i < nkey; i++ ){
      key = astMapKey( pxt, i );
      if( astMapGet0I( pxt, key, &includ ) ) {

         if( includ == 0 ) {

            if( *nextn < mxextn ) {
               (*nextn)++;
               star_strlcpy( extn[ *nextn - 1 ], key, DAT__SZNAM + 1 );

            } else if( *status == SAI__OK ) {
               *status = NDF__XSEXT;
               msgSeti( "MXEXTN", mxextn );
               errRep( " ", "The maximum number of extension names "
                       "(^MXEXTN) has been exceeded.", status );
            }
         }
      }
   }

/* Annul the KeyMap used to hold extension propagation flags. */
   pxt = astAnnul( pxt );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Pscpx", status );

}


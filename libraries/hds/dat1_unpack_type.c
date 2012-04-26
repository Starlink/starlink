#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   int dat1_unpack_type( const char ptype[ DAT__SZTYP ], struct PDD *pdd )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_unpack_type                                                      */

/* Purpose:                                                                 */
/*    Unpack data type information.                                         */

/* Invocation:                                                              */
/*    dat1_unpack_type( ptype, pdd )                                        */

/* Description:                                                             */
/*    This function unpacks the information in a packed data type           */
/*    specification and enters it into the attribute fields of a primitive  */
/*    data descriptor (PDD) structure.                                      */

/* Parameters:                                                              */
/*    const char ptype[ DAT__SZTYP ]                                        */
/*       Pointer to an array of DAT__SZTYP characters containing the type   */
/*       information to be unpacked.                                        */
/*    struct PDD *pdd                                                       */
/*       Pointer to a PDD structure whose attribute fields will be filled   */
/*       in with the unpacked information.                                  */

/* Returned Value:                                                          */
/*    int dat1_unpack_type                                                  */
/*       The global status value current on exit.                           */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    4-JUL-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    10-SEP-1992 (RFWS):                                                   */
/*       Added writing of format string for use by sscanf.                  */
/*    20-APR-2012 (TIMJ):                                                   */
/*       Add _INT64 support.                                                */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      char fmt[ 31 ];            /* Format string for use by sscanf         */
      const unsigned char *utype; /* Unsigned pointer to packed information */
      int clen;                  /* Character string length                 */
      int lenok;                 /* Character string length valid?          */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return hds_gl_status;

/* If the first character of the type specification is not an underscore,   */
/* then it is a structure type, so return its attributes (some attribute    */
/* fields are not used in this situation).                                  */
      if ( ptype[ 0 ] != '_' )
      {
         pdd->class  = DAT__STRUCTURE;
         pdd->length = SZSRV;
      }

/* If the first character is an underscore, but the second isn't, then we   */
/* have an old-style primitive data type specification (these are retained  */
/* for compatibility with container files written by earlier versions of    */
/* HDS). Test for the string representing each primitive type and assign    */
/* the appropriate attributes. (These describe the VAX/VMS number           */
/* representations used by the original implementation.)                    */
      else if ( ptype[ 1 ] != '_' )
      {

/* _DOUBLE:                                                                 */
         if ( strncmp( ptype + 1, "DOUBLE", 6 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 8;
            pdd->dtype = DAT__D;
            pdd->format = DAT__VAXD; /* VAX "D" floating-point format       */
            pdd->order = DAT__MSB;
         }

/* _REAL                                                                    */
         else if ( strncmp( ptype + 1, "REAL", 4 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 4;
            pdd->dtype = DAT__R;
            pdd->format = DAT__VAXF; /* VAX "F" floating-point format       */
            pdd->order = DAT__MSB;
         }

/* _INTEGER                                                                 */
         else if ( strncmp( ptype + 1, "INTEGER", 7 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 4;
            pdd->dtype = DAT__I;
            pdd->format = DAT__2COMP; /* 2's complement signed integer      */
            pdd->order = DAT__LSB;
         }

/* _INT64                                                                   */
         else if ( strncmp( ptype + 1, "INT64", 5 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 8;
            pdd->dtype = DAT__K;
            pdd->format = DAT__2COMP; /* 2's complement signed integer      */
            pdd->order = DAT__LSB;
         }

/* _WORD                                                                    */
         else if ( strncmp( ptype + 1, "WORD", 4 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 2;
            pdd->dtype = DAT__W;
            pdd->format = DAT__2COMP; /* 2's complement signed integer      */
            pdd->order = DAT__LSB;
         }

/* _UWORD                                                                   */
         else if ( strncmp( ptype + 1, "UWORD", 5 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 2;
            pdd->dtype = DAT__UW;
            pdd->format = DAT__BINARY; /* Binary unsigned integer           */
            pdd->order = DAT__LSB;
         }

/* _BYTE                                                                    */
         else if ( strncmp( ptype + 1, "BYTE", 4 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 1;
            pdd->dtype = DAT__B;
            pdd->format = DAT__2COMP; /* 2's complement signed integer      */
            pdd->order = DAT__MSB;
         }

/* _UBYTE                                                                   */
         else if ( strncmp( ptype + 1, "UBYTE", 5 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 1;
            pdd->dtype = DAT__UB;
            pdd->format = DAT__BINARY; /* Binary unsigned integer           */
            pdd->order = DAT__MSB;
         }

/* _LOGICAL                                                                 */
         else if ( strncmp( ptype + 1, "LOGICAL", 7 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 4;
            pdd->dtype = DAT__L;
            pdd->format = DAT__BIT0; /* Bit zero: 1 --> TRUE, 0 ==> FALSE   */
            pdd->order = DAT__LSB;
         }

/* _CHAR                                                                    */
         else if ( strncmp( ptype + 1, "CHAR", 4 ) == 0 )
         {
            pdd->class = DAT__PRIMITIVE;
            pdd->length = 1;
            pdd->dtype = DAT__C;
            pdd->format = DAT__ASCII; /* ASCII encoded character string     */
            pdd->order = DAT__MSB;

/* If a character-type specification is followed by a length (i.e. number   */
/* of characters) specification, then write a format string (to specify the */
/* field width), read this length and check its validity. Otherwise the     */
/* length defaults to 1.                                                    */
            if ( ptype[ 5 ] == '*' )
            {
               (void) sprintf( fmt, "%%%dd", DAT__SZTYP - 6 );
               lenok = ( sscanf( ptype + 6, fmt, &clen ) == 1 );
               if ( lenok ) lenok = ( ( clen >= 1 ) && ( clen <= DAT__MXCHR ) );
               if ( !lenok )
               {
                  hds_gl_status = DAT__TYPIN;
                  emsSetnc( "TYPE", ptype, DAT__SZTYP );
                  emsSeti( "MXCHR", DAT__MXCHR );
                  emsRep( "DAT1_UNPACK_TYPE_1",
                             "Invalid length encountered in the character \
type specification \'^TYPE\'; this should be in the range 1 to ^MXCHR \
(possible corrupt container file or internal programming error).",
                             &hds_gl_status );
               }
               else
               {
                  pdd->length = clen;
               }
            }
         }

/* If a possible old-style type specification is not recognised, then       */
/* report an error.                                                         */
         else
         {
            hds_gl_status = DAT__TYPIN;
            emsSetnc( "TYPE", ptype, DAT__SZTYP );
            emsRep( "DAT1_UNPACK_TYPE_2",
                       "Unrecognised HDS data type \'^TYPE\' encountered \
(possible corrupt container file or internal programming error).",
                       &hds_gl_status );
         }
      }

/* If the type specification begins with two underscores, then it contains  */
/* a packed record of the primitive data attributes. Unpack this            */
/* information (this is the reverse of the packing process performed by     */
/* dat1_check_type).                                                        */
      else
      {
         utype = (const unsigned char *) ptype;
         pdd->class = DAT__PRIMITIVE;
         pdd->length = ( utype[ 3 ] << 8 ) | utype[ 2 ];
         pdd->dtype = utype[ 4 ];
         pdd->format = utype[ 5 ];
         pdd->order = utype[ 6 ];
      }

/* Return the current global status value.                                  */
      return hds_gl_status;
   }

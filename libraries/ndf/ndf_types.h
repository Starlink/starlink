#if !defined( _NDF_TYPES_INCLUDED )	/* Protect against multiple inclusion	    */
#define _NDF_TYPES_INCLUDED 1
/*
*+
* Name:
*    ndf_types.h

* Purpose:
*    Defines public data types and constants used by NDF.

*  Description:
*     This file defines all the public data types and constants
*     used within the C version of NDF.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     19-MAR-2018 (DSB):
*        Initial version, extracted from ndf.h included in the F77 verion
*        of NDF.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory.
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

*/

/*  Constants.                                                              */
/*  ==========                                                              */
/*  General.                                                                */
/*  --------                                                                */
/*  Maximum number of NDF dimensions.                                       */
#define NDF__MXDIM 7

/*  Value which is never used as an NDF identifier, to which an invalid     */
/*  identifier may be set.                                                  */
#define NDF__NOID 0

/*  Value which is never used as an NDF placeholder, to which an invalid    */
/*  placeholder may be set.                                                 */
#define NDF__NOPL 0

/*  String lengths.                                                         */
/*  ---------------                                                         */
/*  Maximum size of a string describing an NDF access type, e.g.            */
/*  'DELETE'.                                                               */
#define NDF__SZACC 6

/*  Recommended maximum length of the name of the currently-executing       */
/*  application.                                                            */
#define NDF__SZAPP 80

/*  Maximum length of a string describing the storage form of an NDF        */
/*  array component, e.g. 'SIMPLE'.                                         */
#define NDF__SZFRM 10

/*  Maximum length of a string describing the full data type of an NDF      */
/*  array component (including whether it is complex), e.g.                 */
/*  'COMPLEX_REAL'.                                                         */
#define NDF__SZFTP 15

/*  Maximum length of a history component date/time string.                 */
#define NDF__SZHDT 24

/*  Recommended length of a line of history text.                           */
#define NDF__SZHIS 72

/*  Maximum length of a line of history text (this limit is determined      */
/*  primarily by the use of MSG_ routines for expanding message tokens,     */
/*  so is set equal to MSG__SZMSG).                                         */
#define NDF__SZHMX 300

/*  Recommended maximum length of the host machine node name recorded in    */
/*  NDF history records.                                                    */
#define NDF__SZHST 80

/*  Maximum length of a history update mode string, e.g. 'DISABLED'.        */
#define NDF__SZHUM 8

/*  Maximum length of a string describing the "mapping mode" used to map    */
/*  an NDF array component for access, e.g. 'WRITE/ZERO'.                   */
#define NDF__SZMMD 11

/*  Recommended length of a character variable that is to hold the full     */
/*  "reference name" of an NDF dataset.                                     */
#define NDF__SZREF 512

/*  Maximum length of a string describing the numeric type of an NDF        */
/*  array component, e.g. '_INTEGER'.                                       */
#define NDF__SZTYP 8

/*  Recommended maximum length of the user name recorded in NDF history     */
/*  records.                                                                */
#define NDF__SZUSR 80

/*  Maximum length of a string containing an NDF extension name.            */
#define NDF__SZXNM 15


/* A type for a pointer to a function that can be called to handle an
   NDF event. */
typedef void (* NdfEventHandler)( const char *evname, const char *text,
                                  int *status );



#endif

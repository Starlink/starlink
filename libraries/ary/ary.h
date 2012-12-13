#if !defined( _ARY_INCLUDED )	/* Protect against multiple inclusion	    */
#define _ARY_INCLUDED 1
/*
*+
* Name:
*    ary.h

* Purpose:
*    Public C definitions for the ARY library.

* Language:
*    ANSI C

* Type of Module:
*    Package public include file.

* Description:
*    This file contains definitions which are used by the ARY system and
*    which may also be needed by software which calls routines from this
*    system.

* Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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

* Authors:
*     DSB: David S Berry (JAC, Hawaii)
*    <{enter_new_authors_here}>

* History:
*    13-DEC-2012 (DSB):
*       Original version, based on ndf.h. Needs to be extended as
*       further ARY routines are needed.
*    <{enter_further_changes_here}>

*-
*/

/* External interfaces.                                                     */
/* ====================                                                     */
#include "star/hds_types.h"      /* HDS typedefs                            */

/*  Constants.                                                              */
/*  ==========                                                              */
/*  General.                                                                */
/*  --------                                                                */
/*  Maximum number of ARY dimensions.                                       */
#define ARY__MXDIM 7

/*  Value which is never used as an ARY identifier, to which an invalid     */
/*  identifier may be set.                                                  */
#define ARY__NOID 0

/*  Value which is never used as an ARY placeholder, to which an invalid    */
/*  placeholder may be set.                                                 */
#define ARY__NOPL 0

/*  String lengths.                                                         */
/*  ---------------                                                         */
/*  Maximum size of a string describing an ARY access type, e.g.            */
/*  'DELETE'.                                                               */
#define ARY__SZACC 6

/*  Maximum length of a string describing the storage form of an ARY        */
/*  array component, e.g. 'SIMPLE'.                                         */
#define ARY__SZFRM 10

/*  Maximum length of a string describing the full data type of an ARY      */
/*  array component (including whether it is complex), e.g.                 */
/*  'COMPLEX_REAL'.                                                         */
#define ARY__SZFTP 20

/*  Maximum length of a string describing the "mapping mode" used to map    */
/*  an ARY array component for access, e.g. 'WRITE/ZERO'.                   */
#define ARY__SZMMD 11

/*  Maximum length of a string describing the numeric type of an ARY        */
/*  array component, e.g. '_INTEGER'.                                       */
#define ARY__SZTYP 8

/* ARY_ error codes.                                                        */
/* =================                                                        */
/* N.B. This should be the only place in the ARY_ library where the         */
/* "ary_err.h" include file is referenced. It is used only during           */
/* development and software builds. Include it only if it has not already   */
/* been pasted on to the front of this file (as happens during software     */
/* installation).                                                           */
#if !defined( ARY_ERROR_DEFINED )
#include "ary_err.h"             /* ARY_ error codes                        */
#endif

/* Function prototypes.                                                     */
/* ====================                                                     */
void aryAnnul( int *iary,
               int *status );

void aryFind( const HDSLoc * loc,
              const char *name,
              int *iary,
              int *status );

void aryMap( int iary,
             const char *type,
             const char *mmod,
             void *pntr[],
             int *el,
             int *status );

void arySect( int iary1,
              int ndim,
              const int lbnd[],
              const int ubnd[],
              int *iary2,
              int *status );

void arySize( int iary,
              int *npix,
              int *status );

#endif

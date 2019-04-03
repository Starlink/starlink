#if !defined( _NDF_INCLUDED )	/* Protect against multiple inclusion	    */
#define _NDF_INCLUDED 1
/*
*+
* Name:
*    ndf.h

* Purpose:
*    Public C definitions for the NDF library.

* Language:
*    ANSI C

* Type of Module:
*    Package public include file.

* Description:
*    This file contains definitions which are used by the NDF system and
*    which may also be needed by software which calls routines from this
*    system.

* Copyright:
*    Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*    RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC, UCLan)
*    <{enter_new_authors_here}>

* History:
*    8-OCT-1993 (RFWS:
*       Original version.
*    30-JAN-1995 (RFWS):
*       Moved error code definitions out into separate file ndf_err.h.
*    30-SEP-1998 (RFWS)
*       Added public C interface.
*    18-NOV-2005 (TIMJ):
*       Use HDSLoc* rather than char [DAT__SZLOC]
*     23-JAN-2009 (DSB):
*        Added ndfHsdat.
*    <{enter_further_changes_here}>

*-
*/

/* External interfaces.                                                     */
/* ====================                                                     */
#include "ast.h"                 /* AST world coordinate system handling    */
#include "star/hds_types.h"      /* HDS typedefs                            */

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
#define NDF__SZHMX 200

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

/* NDF_ error codes.                                                        */
/* =================                                                        */
/* N.B. This should be the only place in the NDF_ library where the         */
/* "ndf_err.h" include file is referenced. It is used only during           */
/* development and software builds. Include it only if it has not already   */
/* been pasted on to the front of this file (as happens during software     */
/* installation).                                                           */
#if !defined( NDF_ERROR_DEFINED )
#include "ndf_err.h"             /* NDF_ error codes                        */
#endif

/* Function prototypes.                                                     */
/* ====================                                                     */
void ndfAcget( int indf,
               const char *comp,
               int iaxis,
               char *value,
               int value_length,
               int *status );

void ndfAclen( int indf,
               const char *comp,
               int iaxis,
               int *length,
               int *status );

void ndfAcmsg( const char *token,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAcput( const char *value,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAcre( int indf,
              int *status );

void ndfAform( int indf,
               const char *comp,
               int iaxis,
               char *form,
               int form_length,
               int *status );

void ndfAmap( int indf,
              const char *comp,
              int iaxis,
              const char *type,
              const char *mmod,
              void *pntr[],
              int *el,
              int *status );

void ndfAnnul( int *indf,
               int *status );

void ndfAnorm( int indf,
               int iaxis,
               int *norm,
               int *status );

void ndfArest( int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAsnrm( int norm,
               int indf,
               int iaxis,
               int *status );

void ndfAssoc( const char *param,
               const char *mode,
               int *indf,
               int *status );

void ndfAstat( int indf,
               const char *comp,
               int iaxis,
               int *state,
               int *status );

void ndfAstyp( const char *type,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAtype( int indf,
               const char *comp,
               int iaxis,
               char *type,
               int type_length,
               int *status );

void ndfAunmp( int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfBad( int indf,
             const char *comp,
             int check,
             int *bad,
             int *status );

void ndfBase( int indf1,
              int *indf2,
              int *status );

void ndfBb( int indf,
            unsigned char *badbit,
            int *status );

void ndfBegin( void );

void ndfBlock( int indf1,
               int ndim,
               const int mxdim[],
               int iblock,
               int *indf2,
               int *status );

void ndfBound( int indf,
               int ndimx,
               int lbnd[],
               int ubnd[],
               int *ndim,
               int *status );

void ndfCancl( const char *param,
               int *status );

void ndfCget( int indf,
              const char *comp,
              char *value,
              int value_length,
              int *status );

void ndfChunk( int indf1,
               int mxpix,
               int ichunk,
               int *indf2,
               int *status );

void ndfCinp( const char *param,
              int indf,
              const char *comp,
              int *status );

void ndfClen( int indf,
              const char *comp,
              int *length,
              int *status );

void ndfClone( int indf1,
               int *indf2,
               int *status );

void ndfCmplx( int indf,
               const char *comp,
               int *cmplx,
               int *status );

void ndfCmsg( const char *token,
              int indf,
              const char *comp,
              int *status );

void ndfCopy( int indf1,
              int *place,
              int *indf2,
              int *status );

void ndfCput( const char *value,
              int indf,
              const char *comp,
              int *status );

void ndfCreat( const char *param,
               const char *ftype,
               int ndim,
               const int lbnd[],
               const int ubnd[],
               int *indf,
               int *status );

void ndfCrep( const char *param,
              const char *ftype,
              int ndim,
              const int ubnd[],
              int *indf,
              int *status );

void ndfCrepl( const char *param,
               int *place,
               int *status );

void ndfDelet( int *indf,
               int *status );

void ndfDim( int indf,
             int ndimx,
             int dim[],
             int *ndim,
             int *status );

void ndfEnd( int *status );

void ndfExist( const char *param,
               const char *mode,
               int *indf,
               int *status );

void ndfFind( const HDSLoc * loc,
              const char *name,
              int *indf,
              int *status );

void ndfForm( int indf,
              const char *comp,
              char *form,
              int form_length,
              int *status );

void ndfFtype( int indf,
               const char *comp,
               char *ftype,
               int ftype_length,
               int *status );

void ndfGtune( const char *tpar,
               int *value,
               int *status );

void ndfGtwcs( int indf,
               AstFrameSet **iwcs,
               int *status );

void ndfHappn( const char *appn,
               int *status );

void ndfHcre( int indf,
              int *status );

void ndfHdef( int indf,
              const char *appn,
              int *status );

void ndfHecho( int nlines,
               char *const text[],
               int *status );

void ndfHend( int *status );

void ndfHfind( int indf,
               const int ymdhm[ 5 ],
               float sec,
               int eq,
               int *irec,
               int *status );

void ndfHinfo( int indf,
               const char *item,
               int irec,
               char *value,
               int value_length,
               int *status );

void ndfHnrec( int indf,
               int *nrec,
               int *status );

void ndfHout( int indf,
              int irec,
              void ( *routin )( int, char *const [], int * ),
              int *status );

void ndfHpurg( int indf,
               int irec1,
               int irec2,
               int *status );

void ndfHput( const char *hmode,
              const char *appn,
              int repl,
              int nlines,
              char *const text[],
              int trans,
              int wrap,
              int rjust,
              int indf,
              int *status );

void ndfHgmod( int indf,
               char *hmode,
               int hmode_length,
               int *status );

void ndfHsmod( const char *hmode,
               int indf,
               int *status );

void ndfHsdat( const char *date,
               int indf,
               int *status );

void ndfInit( int argc, char *const *argv, int *status );

void ndfIsacc( int indf,
               const char *access,
               int *isacc,
               int *status );

void ndfIsbas( int indf,
               int *isbas,
               int *status );

void ndfIsin( int indf1,
              int indf2,
              int *isin,
              int *status );

void ndfIstmp( int indf,
               int *istmp,
               int *status );

void ndfLoc( int indf,
             const char *mode,
             HDSLoc ** loc,
             int *status );

void ndfMap( int indf,
             const char *comp,
             const char *type,
             const char *mmod,
             void *pntr[],
             int *el,
             int *status );

void ndfMapql( int indf,
               int **pntr,
               int *el,
               int *bad,
               int *status );

void ndfMapz( int indf,
              const char *comp,
              const char *type,
              const char *mmod,
              void *rpntr[],
              void *ipntr[],
              int *el,
              int *status );

void ndfMbad( int badok,
              int indf1,
              int indf2,
              const char *comp,
              int check,
              int *bad,
              int *status );

void ndfMbadn( int badok,
               int n,
               const int ndfs[],
               const char *comp,
               int check,
               int *bad,
               int *status );

void ndfMbnd( const char *option,
              int *indf1,
              int *indf2,
              int *status );

void ndfMbndn( const char *option,
               int n,
               int ndfs[],
               int *status );

void ndfMsg( const char *token,
             int indf );

void ndfMtype( const char *typlst,
               int indf1,
               int indf2,
               const char *comp,
               char *itype,
               int itype_length,
               char *dtype,
               int dtype_length,
               int *status );

void ndfMtypn( const char *typlst,
               int n,
               const int ndfs[],
               const char *comp,
               char *itype,
               int itype_length,
               char *dtype,
               int dtype_length,
               int *status );

void ndfNbloc( int indf,
               int ndim,
               const int mxdim[],
               int *nblock,
               int *status );

void ndfNchnk( int indf,
               int mxpix,
               int *nchunk,
               int *status );

void ndfNew( const char *ftype,
             int ndim,
             const int lbnd[],
             const int ubnd[],
             int *place,
             int *indf,
             int *status );

void ndfNewp( const char *ftype,
              int ndim,
              const int ubnd[],
              int *place,
              int *indf,
              int *status );

void ndfNoacc( const char *access,
               int indf,
               int *status );

void ndfOpen( const HDSLoc * loc,
              const char *name,
              const char *mode,
              const char *stat,
              int *indf,
              int *place,
              int *status );

void ndfPlace( const HDSLoc * loc,
               const char *name,
               int *place,
               int *status );

void ndfProp( int indf1,
              const char *clist,
              const char *param,
              int *indf2,
              int *status );

void ndfPtszi( int scale,
               int zero,
               int indf,
               const char *comp,
               int *status );

void ndfPtszr( float scale,
              float zero,
              int indf,
              const char *comp,
              int *status );

void ndfPtszd( double scale,
              double zero,
              int indf,
              const char *comp,
              int *status );

void ndfGtszi( int indf,
               const char *comp,
               int *scale,
               int *zero,
               int *status );

void ndfGtszr( int indf,
              const char *comp,
              float *scale,
              float *zero,
              int *status );

void ndfGtszd( int indf,
              const char *comp,
              double *scale,
              double *zero,
              int *status );

#define ndfPtwcs(iwcs,indf,status) ndfPtwcs_((AstFrameSet *)(iwcs),indf,status)
void ndfPtwcs_( AstFrameSet *iwcs,
                int indf,
                int *status );

#define ndfQmask(qual,badbit) ((((unsigned char)(qual))&((unsigned char)(badbit)))==(unsigned char)0)

void ndfQmf( int indf,
             int *qmf,
             int *status );

void ndfReset( int indf,
               const char *comp,
               int *status );

void ndfSame( int indf1,
              int indf2,
              int *same,
              int *isect,
              int *status );

void ndfSbad( int bad,
              int indf,
              const char *comp,
              int *status );

void ndfSbb( unsigned char badbit,
             int indf,
             int *status );

void ndfSbnd( int ndim,
              const int lbnd[],
              const int ubnd[],
              int indf,
              int *status );

void ndfScopy( int indf1,
               const char *clist,
               int *place,
               int *indf2,
               int *status );

void ndfSect( int indf1,
              int ndim,
              const int lbnd[],
              const int ubnd[],
              int *indf2,
              int *status );

void ndfShift( int nshift,
               const int shift[],
               int indf,
               int *status );

void ndfSize( int indf,
              int *npix,
              int *status );

void ndfSqmf( int qmf,
              int indf,
              int *status );

void ndfSsary( int iary1,
               int indf,
               int *iary2,
               int *status );

void ndfState( int indf,
               const char *comp,
               int *state,
               int *status );

void ndfStype( const char *ftype,
               int indf,
               const char *comp,
               int *status );

void ndfTemp( int *place,
              int *status );

void ndfTune( int value,
              const char *tpar,
              int *status );

void ndfSctyp( int indf,
              const char *comp,
              char *type,
              int type_length,
              int *status );

void ndfType( int indf,
              const char *comp,
              char *type,
              int type_length,
              int *status );

void ndfUnmap( int indf,
               const char *comp,
               int *status );

void ndfValid( int indf,
               int *valid,
               int *status );

void ndfXdel( int indf,
              const char *xname,
              int *status );

void ndfXgt0c( int indf,
               const char *xname,
               const char *cmpt,
               char *value,
               int value_length,
               int *status );

void ndfXgt0d( int indf,
               const char *xname,
               const char *cmpt,
               double *value,
               int *status );

void ndfXgt0i( int indf,
               const char *xname,
               const char *cmpt,
               int *value,
               int *status );

void ndfXgt0l( int indf,
               const char *xname,
               const char *cmpt,
               int *value,
               int *status );

void ndfXgt0r( int indf,
               const char *xname,
               const char *cmpt,
               float *value,
               int *status );

void ndfXiary( int indf,
               const char *xname,
               const char *cmpt,
               const char *mode,
               int *iary,
               int *status );

void ndfXloc( int indf,
              const char *xname,
              const char *mode,
              HDSLoc ** loc,
              int *status );

void ndfXname( int indf,
               int n,
               char *xname,
               int xname_length,
               int *status );

void ndfXnew( int indf,
              const char *xname,
              const char *type,
              int ndim,
              const int dim[],
              HDSLoc **loc,
              int *status );

void ndfXnumb( int indf,
               int *nextn,
               int *status );

void ndfXpt0c( const char *value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXpt0d( double value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXpt0i( int value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXpt0l( int value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXpt0r( float value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXstat( int indf,
               const char *xname,
               int *there,
               int *status );


void ndfZscal( int indf1,
               const char *type,
               double scale[ 2 ],
               double zero[ 2 ],
               int *place,
               int *indf2,
               int *status );

void ndfZdelt( int indf1,
               const char *comp,
               float minrat,
               int zaxis,
               const char *type,
               int *place,
               int *indf2,
               float *zratio,
               int *status );

void ndfGtdlt( int indf,
               const char *comp,
               int *zaxis,
               char *ztype,
               int ztype_length,
               float *zratio,
               int *status );

void ndfHcopy( int indf1,
               int indf2,
               int *status );
#endif

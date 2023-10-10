#if !defined( _NDF1_INCLUDED )	/* Protect against multiple inclusion	    */
#define _NDF1_INCLUDED 1
/*
*+
* Name:
*    ndf1.h

* Purpose:
*    Private C definitions for the NDF library.

* Language:
*    ANSI C

* Type of Module:
*    Package private include file.

* Description:
*    This file contains definitions which are used internally by the NDF
*    system and which will not be needed by software that calls routines
*    from this system.

* Copyright:
*    Copyright (C) 2018 East Asian Observatory
*    All Rights Reserved.

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
*     DSB: David S Berry (JAC, UCLan)

* History:
*     29-MAY-2018 (DSB):
*       Original version.

*-
*/

/* External interfaces. */
/* ==================== */
#include "ast.h"
#include "star/hds_types.h"


/*  Constants and types. */
/*  ==================== */
#include "ndf1_types.h"


/*  Function prototypes */
/*  =================== */
void ndf1Cmpbl( int lead,
                char *string );

int ndf1Expid( NdfObject *object,
               int *status );

void *ndf1Ffs( const NdfBlockType type,
               int *status );

void *ndf1Nxtsl( const NdfBlockType type,
                 int slot,
                 int *next,
                 int *status );

void ndf1Rjust( char *string,
                int string_length );

void *ndf1Rls( NdfObject *object,
               int *status );

void ndf1A2p( int n,
              const double ax[],
              hdsdim lbnd,
              hdsdim ubnd,
              int havcen,
              int havwid,
              const double centre[],
              const double width[],
              int *inc,
              int ipix0[],
              double cent0[],
              double space0[],
              int inpix[],
              int ipix1[],
              double cent1[],
              double width1[],
              int *status );

int ndf1Absnt( int istat );

void ndf1Accok( NdfACB *acb,
                const char *access,
                int *ok,
                int *status );

void ndf1Acprp( NdfACB *acb1,
                int iccomp,
                int accpf,
                NdfDCB *dcb2,
                int *status );

void ndf1Acre( NdfDCB *dcb,
               int *status );

void ndf1Acrst( int iax,
                int iccomp,
                NdfACB *acb,
                int *status );

void ndf1Adcre( hdsdim lbnd,
                hdsdim ubnd,
                int iax,
                NdfDCB *dcb,
                int *status );

void ndf1Adext( const char *type,
                double scale,
                double zero,
                int upper,
                hdsdim pix0,
                hdsdim lbnda,
                hdsdim ubnda,
                void *pntr,
                int *status );

void ndf1Adfrm( int iax,
                NdfACB *acb,
                char *form,
                size_t form_length,
                int *status );

void ndf1Adini( const char *type,
                hdsdim lbnda,
                hdsdim ubnda,
                void *pntr,
                int *status );

void ndf1Admap( int iax,
                NdfACB *acb,
                const char *type,
                const char *mode,
                void **pntr,
                size_t *el,
                int *status );

void ndf1Adprp( NdfACB *acb1,
                int adcpf,
                NdfDCB *dcb2,
                int *status );

void ndf1Adrst( int iax,
                NdfACB *acb,
                int *status );

void ndf1Adsbn( hdsdim lbndd,
                hdsdim ubndd,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Adstp( const char *type,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Adtyp( int iax,
                NdfACB *acb,
                char *type,
                size_t type_length,
                int *status );

void ndf1Adump( int iax,
                NdfACB *acb,
                int *status );

void ndf1Affor( NdfFCB *fcb,
                int *status );

void ndf1Amap( int iaxis,
               NdfACB *acb,
               const char *comp,
               const char *type,
               const char *mmod,
               void *pntr[],
               size_t *el,
               int *status );

void ndf1Amsg( const char *token,
               NdfACB *acb );

void ndf1Anl( NdfACB **acb,
              int *status );

void ndf1Annpl( int erase,
                NdfPCB **pcb,
                int *status );

void ndf1Antmp( HDSLoc **loc,
                int *status );

void ndf1Aprp( NdfACB *acb1,
               int acpf,
               NdfDCB *dcb2,
               int *status );

void ndf1Arst( NdfACB *acb,
               int *status );

void ndf1Asbnd( int ndim,
                const hdsdim lbnd[],
                const hdsdim ubnd[],
                NdfACB *acb,
                int *status );

void ndf1Asetc( AstFrameSet *iast,
                const char *value,
                const char *attrib,
                int *status );

void ndf1Atyp( int iaxis,
               NdfACB *acb,
               const char *comp,
               int *itype,
               int *status );

void ndf1Aump( int iaxis,
               NdfACB *acb,
               const char *comp,
               int *status );

void ndf1Avcre( int iax,
                NdfDCB *dcb,
                int *status );

void ndf1Avext( const char *type,
                int upper,
                hdsdim pix0,
                hdsdim lbnda,
                hdsdim ubnda,
                void *pntr,
                int *status );

void ndf1Avfrm( int iax,
                NdfACB *acb,
                char *form,
                size_t form_length,
                int *status );

void ndf1Avmap( int iax,
                NdfACB *acb,
                const char *type,
                const char *mode,
                int stdev,
                void **pntr,
                size_t *el,
                int *status );

void ndf1Avprp( NdfACB *acb1,
                int acpf,
                NdfDCB *dcb2,
                int *status );

void ndf1Avrst( int iax,
                NdfACB *acb,
                int *status );

void ndf1Avsbn( hdsdim lbndv,
                hdsdim ubndv,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Avstp( const char *type,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Avtyp( int iax,
                NdfACB *acb,
                char *type,
                size_t type_length,
                int *status );

void ndf1Avump( int iax,
                NdfACB *acb,
                int *status );

void ndf1Awcre( int iax,
                NdfDCB *dcb,
                int *status );

void ndf1Awext( const char *type,
                int upper,
                hdsdim pix0,
                double width,
                hdsdim lbnd,
                hdsdim ubnd,
                void *pntr,
                int *status );

void ndf1Awfrm( int iax,
                NdfACB *acb,
                char *form,
                size_t form_length,
                int *status );

void ndf1Awini( const char *type,
                hdsdim lbnd,
                hdsdim ubnd,
                const double data[],
                void *pntr,
                int *status );

void ndf1Awmap( int iax,
                NdfACB *acb,
                const char *type,
                const char *mode,
                void **pntr,
                size_t *el,
                int *status );

void ndf1Awprp( NdfACB *acb1,
                int awcpf,
                NdfDCB *dcb2,
                int *status );

void ndf1Awrst( int iax,
                NdfACB *acb,
                int *status );

void ndf1Awsbn( hdsdim lbndw,
                hdsdim ubndw,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Awstp( const char *type,
                int iax,
                NdfACB *acb,
                int *status );

void ndf1Awtyp( int iax,
                NdfACB *acb,
                char *type,
                size_t type_length,
                int *status );

void ndf1Awump( int iax,
                NdfACB *acb,
                int *status );

void ndf1Axlim( int iax,
                NdfACB *acb,
                double value1,
                double value2,
                int ispix1,
                int ispix2,
                int isbnd,
                hdsdim *lbnd,
                hdsdim *ubnd,
                int *status );

void ndf1Bad( NdfACB *acb,
              const char *comp,
              int check,
              int *bad,
              int *status );

void ndf1Bpp( const char *type,
              size_t el,
              void *pntr,
              int *bad,
              int *status );

void ndf1Cbfrm( int ndim,
                const hdsdim lbnd[],
                const hdsdim ubnd[],
                char *form,
                size_t form_length,
                int *status );

void ndf1Ccpy( const char *cin,
               char *cout,
               size_t cout_length,
               int *status );

void ndf1Chacc( NdfACB *acb,
                const char *access,
                int *status );

void ndf1Chftp( const char *ftype,
                char *type,
                size_t type_length,
                int *cmplx,
                int *status );

void ndf1CheckLocker( NdfACB *acb,
                      int *status );

void ndf1Chhum( const char *hmode,
                int *hum,
                int *status );

void ndf1Chmod( NdfACB *acb,
                const char *mode,
                int *status );

void ndf1Chtim( const char *date,
                double *mjd,
                int *status );

void ndf1Chxnm( const char *xname,
                size_t start,
                size_t end,
                int *status );

void ndf1Clfor( int dispos,
                NdfDCB *dcb,
                int *status );

void ndf1Cln( NdfACB *acb1,
              NdfACB **acb2,
              int *status );

void ndf1Cmpac( NdfDCB *dcb,
                const char *comp,
                int *status );

void ndf1Cmpfl( const char *name1,
                size_t start1,
                size_t end1,
                const char *name2,
                int *same,
                int *status );

void ndf1Cpync( HDSLoc *loc1,
                const char *name,
                HDSLoc *loc2,
                int *status );

void ndf1Crfor( const char *file,
                NdfFCB *fcb,
                char *expfil,
                size_t expfil_length,
                char *fid,
                size_t fid_length,
                int *status );

void ndf1Crnbn( NdfDCB *dcb,
                NdfACB **acb,
                int *status );

void ndf1Cuserid( char *user,
                  size_t user_length,
                  int *status );

void ndf1Cut( NdfACB *acb1,
              int ndim,
              const hdsdim lbnd[],
              const hdsdim ubnd[],
              NdfACB **acb2,
              int *status );

void ndf1Cvcmd( const char *forfil,
                NdfFCB *fcb,
                HDSLoc *ndfloc,
                const char *ndfnam,
                int from,
                int report,
                int *def,
                char *cmd,
                size_t cmd_length,
                int *status );

void ndf1Cvfor( const char *forfil,
                NdfFCB *fcb,
                HDSLoc *ndfloc,
                const char *ndfnam,
                int from,
                int *status );

void ndf1Cvtok( const char *forfil,
                NdfFCB *fcb,
                HDSLoc *ndfloc,
                const char *ndfnam,
                int *status );

void ndf1Dac( int iax,
              int iccomp,
              NdfDCB *dcb,
              int *status );

void ndf1Dad( int iax,
              NdfDCB *dcb,
              int *status );

void ndf1Da( NdfDCB *dcb,
             int *status );

void ndf1Dan( int iax,
              NdfDCB *dcb,
              int *status );

void ndf1Danl( int dispos,
               NdfDCB **dcb,
               int *status );

void ndf1Dav( int iax,
              NdfDCB *dcb,
              int *status );

void ndf1Daw( int iax,
              NdfDCB *dcb,
              int *status );

void ndf1Dbad( NdfACB *acb,
               int check,
               int *bad,
               int *status );

void ndf1Dc( NdfDCB *dcb,
             int iccomp,
             int *status );

void ndf1Dcre( const char *ftype,
               int ndim,
               const hdsdim lbnd[],
               const hdsdim ubnd[],
               NdfPCB *pcb,
               NdfACB **acb,
               int *status );

void ndf1Dcrep( const char *ftype,
                int ndim,
                const hdsdim ubnd[],
                NdfPCB *pcb,
                NdfACB **acb,
                int *status );

void ndf1Dd( NdfDCB *dcb,
             int *status );

void ndf1Del( NdfACB **acb,
              int *status );

void ndf1Delob( HDSLoc **loc,
                int *status );

void ndf1Dh( NdfDCB *dcb,
             int *status );

void ndf1Dimp( HDSLoc *loc,
               NdfDCB **dcb,
               int *status );

void ndf1Dlfor( const char *file,
                NdfFCB *fcb,
                int *status );

void ndf1Dmap( NdfACB *acb,
               const char *type,
               int cmplx,
               const char *mmod,
               int mask,
               void **dpntr,
               void **ipntr,
               int *status );

void ndf1Dmsg( const char *token,
               NdfDCB *dcb );

void ndf1Dnfor( const char *forfil,
                NdfFCB *fcb,
                int keep,
                HDSLoc **ndfloc,
                char *ndfnam,
                size_t ndfnam_length,
                size_t *lnam,
                int *status );

void ndf1Docmd( const char *cmd,
                int *status );

void ndf1Dqanl( NdfDCB *dcb,
                int del,
                int *status );

void ndf1Dq( NdfDCB *dcb,
             int *status );

void ndf1Dump( NdfACB *acb,
               int *status );

void ndf1Dvanl( NdfDCB *dcb,
                int del,
                int *status );

void ndf1Dv( NdfDCB *dcb,
             int *status );

void ndf1Dw( NdfDCB *dcb,
             int *status );

void ndf1Dx( NdfDCB *dcb,
             int *status );

void ndf1Ellip( char *str,
                size_t str_length );

void ndf1Event( const char *evname,
                int *status );

void ndf1Evmsg( const char *token,
                NdfDCB *dcb );

void ndf1Expfn( const char *in,
                int getfid,
                char *out,
                size_t out_length,
                char *fid,
                size_t fid_length,
                int *status );

int ndf1Expid( NdfObject *acb,
               int *status );

void *ndf1Ffs( const NdfBlockType type,
               int *status );

void ndf1Filac( const char *fname,
                const char *mode,
                int report,
                int *ok,
                int *status );

void ndf1Filex( const char *file,
                const char *mode,
                int report,
                int *ok,
                int *status );

char **ndf1Findwords( const char *text,
                      int *el,
                      int *status );

void ndf1Fmhdt( const int ymdhm[],
                float sec,
                char *str,
                size_t str_length,
                int *status );

void ndf1Forxt( const char *name,
                size_t start,
                size_t end,
                size_t *x1,
                size_t *x2,
                int *status );

void ndf1Fr2px( int nax,
                int ndim,
                const hdsdim nlbnd[],
                const hdsdim nubnd[],
                const int isbnd[],
                double value1[],
                double value2[],
                int frame1[],
                int frame2[],
                int *status );

char **ndf1Freewords( int len, char **strings );

void ndf1Fsplt( const char *fname,
                size_t start,
                size_t end,
                size_t *d1,
                size_t *d2,
                size_t *n1,
                size_t *n2,
                size_t *t1,
                size_t *t2,
                size_t *v1,
                size_t *v2,
                int *status );

void ndf1Gadex( hdsdim lbnd,
                hdsdim ubnd,
                Ary *ary,
                int upper,
                double *scale,
                double *zero,
                int *status );

void ndf1Gawex( hdsdim lbnd,
                hdsdim ubnd,
                Ary *ary,
                int upper,
                double *width,
                int *status );

void ndf1Getap( char *appn,
                size_t appn_length,
                int *status );

void ndf1Gtarg( int iarg,
                char *arg,
                size_t arg_length,
                int *there,
                int *status );

void ndf1Gtbb( NdfACB *acb,
               unsigned char *badbit,
               int *status );

void ndf1Gtenv( const char *name,
                int *def,
                char *val,
                size_t val_length,
                size_t *lval,
                int *status );

void ndf1Gtfil( char *name,
                size_t name_length,
                int *status );

void ndf1Gthdt( NdfDCB *dcb,
                int irec,
                int ymdhm[],
                float *sec,
                int *status );

void ndf1Gtxtn( const char *name,
                int mxxtn,
                int *def,
                char *xtn,
                size_t xtn_length,
                size_t xtn1[],
                size_t xtn2[],
                int *nxtn,
                int *status );

void ndf1Hcopy( HDSLoc *loc1,
                HDSLoc *loc2,
                int *status );

void ndf1Hcpy( int nlines,
               size_t clen,
               char *hist,
               const char *text,
               int *status );

void ndf1Hdcre( NdfDCB *dcb,
                int *status );

void ndf1Hderr( NdfDCB *dcb,
                int rel,
                int *status );

void ndf1Hfwrt( NdfDCB *dcb,
                const char *appn,
                int nlines,
                char *const text[],
                int trans,
                int wrap,
                int rjust,
                int *status );

void ndf1Hincr( NdfDCB *dcb,
                int *status );

void ndf1Hlerr( int *status );

char *ndf1Hmp0C( HDSLoc *loc,
               int *status );

void ndf1Hnew( HDSLoc *loc1,
               const char *name,
               const char *type,
               int ndim,
               const hdsdim dim[],
               HDSLoc **loc2,
               int *status );

void ndf1Hprp( NdfDCB *dcb1,
               int prop,
               NdfDCB *dcb2,
               int *status );

void ndf1Hrst( HDSLoc *loc,
               int *status );

void ndf1Hscrt( HDSLoc *loc,
                int *status );

void ndf1Hsrt( NdfDCB *dcb,
               int nrec,
               double work1[],
               int work2[],
               int *status );

void ndf1Htcmp( const int ymdhm1[],
                float sec1,
                const int ymdhm2[],
                float sec2,
                int *order,
                int *status );

void ndf1Htlen( NdfDCB *dcb,
                size_t *htlen,
                int *status );

void ndf1Htop( HDSLoc *loc1,
               const char *mode,
               HDSLoc **loc2,
               int *status );

void ndf1Hunmp( HDSLoc *loc,
                int *status );

void ndf1Hwdef( NdfDCB *dcb,
                const char *appn,
                int *status );

void ndf1Hwenv( NdfDCB *dcb,
                const char *appn,
                int *status );

void ndf1Hwrt( NdfDCB *dcb,
               const char *appn,
               int nlines,
               int linelen,
               const char *text,
               int *status );

NdfObject *ndf1Id2ac( int id,
                      const int isacb );

void ndf1Imp( HDSLoc *loc,
              NdfACB **acb,
              int *status );

void ndf1Impid( int indf,
                NdfACB **acb,
                int *status );

void ndf1Imppl( int place,
                NdfPCB **pcb,
                int *status );

int ndf1Indxp( const char *str,
               char ch,
               size_t *iat );

void ndf1Infcb( int *status );

void ndf1Inifr( NdfACB *acb,
                AstFrameSet *iwcs,
                int *status );

int *ndf1Init( int *status );

void ndf1Intcb( int *status );

void ndf1InvokeF77Handler( NdfEventHandler handler,
                           const char *evname,
                           const char *shorttext,
                           int *status );

int ndf1Isalm( const char *string,
               size_t start,
               size_t end );

int ndf1Isnam( const char *string,
               size_t start,
               size_t end );

int ndf1IsValid( NdfObject *object );

int ndf1Locked( NdfACB *acb );

void ndf1LockACB( NdfACB *acb,
                  int *status );

void ndf1LockDCB( NdfDCB *dcb,
                  int *status );

void ndf1LockLoc( HDSLoc *loc,
                  int *status );

void ndf1LockAry( Ary *ary,
                  int *status );

void ndf1Map( NdfACB *acb,
              const char *comp,
              const char *type,
              int cmplx,
              const char *mmod,
              void *rpntr[],
              void *ipntr[],
              int *status );

void ndf1Mbad( int badok,
               int n,
               const int ndfs[],
               const char *comp,
               int check,
               int *bad,
               int *status );

void ndf1Mbndp( int n,
                int ndfs[],
                int *status );

void ndf1Mbndt( int n,
                int ndfs[],
                int *status );

void ndf1Mjd2t( double mjd,
                int ymdhm[ 5 ],
                float *sec,
                int *status );

void ndf1Move( const char *type,
               size_t n,
               const void *pntr1,
               void *pntr2,
               int *status );

void ndf1Mpanl( AstMapping *mapin,
                int *nmap,
                AstMapping *maps[],
                int hasinv[],
                int inmap[],
                int inind[],
                int outmap[],
                int outind[],
                AstMapping **map,
                int *status );

void ndf1Mpspt( AstMapping *map,
                AstMapping **map0,
                AstMapping **map1,
                AstMapping **map2,
                int inprm[],
                int outprm[],
                int *status );

void ndf1Mtyp( const char *typlst,
               int n,
               const int ndfs[],
               const char *comp,
               char *itype,
               size_t itype_length,
               char *dtype,
               size_t dtype_length,
               int *status );

void ndf1Mxtyp( int itype1,
                int itype2,
                int *itype,
                int *status );

void ndf1Ncut( NdfACB *acb1,
               const char *str,
               NdfACB **acb2,
               int *status );

void ndf1Nfind( HDSLoc *loc,
                const char *name,
                const char *mode,
                NdfACB **acb,
                int *status );

void ndf1Nplac( HDSLoc *loc,
                const char *name,
                NdfPCB **pcb,
                int *status );

void ndf1Nsplt( const char *name,
                int rel,
                size_t *n1,
                size_t *n2,
                size_t *s1,
                size_t *s2,
                int *status );

void ndf1Ntfor( const char *forfil,
                NdfFCB *fcb,
                int keep,
                HDSLoc **ndfloc,
                char *ndfnam,
                size_t ndfnam_length,
                size_t *lnam,
                int *status );

void *ndf1Nxtsl( const NdfBlockType type,
                 int slot,
                 int *next,
                 int *status );

void ndf1Opfor( HDSLoc *loc,
                const char *name,
                const char *mode,
                NdfACB **acb,
                int *status );

void ndf1P2a( int n,
              const int ipix[],
              hdsdim lbnd,
              hdsdim ubnd,
              int havcen,
              int havwid,
              int havvar,
              const double centre[],
              const double width[],
              const double varian[],
              double cen[],
              double wid[],
              double var[],
              int *status );

void ndf1Plcre( HDSLoc *loc,
                const char *name,
                HDSLoc **locpl,
                int *new,
                int *status );

void ndf1Pldcb( NdfPCB *pcb,
                NdfDCB *dcb,
                int *status );

void ndf1Plfor( HDSLoc *loc,
                const char *name,
                NdfPCB **pcb,
                int *status );

void ndf1Prfor( NdfACB *acb,
                NdfPCB *pcb,
                int *status );

void ndf1Prp( NdfACB *acb1,
              int nextn,
              char extn[][ DAT__SZNAM + 1 ],
              const int cpf[],
              NdfPCB *pcb,
              NdfACB **acb2,
              int *status );

void ndf1Pscpx( const char *str,
                int mxextn,
                char extn[][ DAT__SZNAM + 1 ],
                int *nextn,
                int cpf[],
                int *status );

void ndf1Psffl( const char *list,
                int mxel,
                size_t ibeg[],
                size_t iend[],
                int *el,
                int *status );

void ndf1Psfmt( const char *fmt,
                size_t *f1,
                size_t *f2,
                size_t *e1,
                size_t *e2,
                int *status );

void ndf1Pshdt( const char *str,
                int ymdhm[],
                float *sec,
                int *status );

void ndf1Psndb( const char *str,
                double def,
                int axis,
                AstFrameSet *iwcs,
                int wcssec,
                double *value,
                int *frame,
                int *isdef,
                int *isgeo,
                int *status );

void ndf1Psnde( const char *str,
                int nax,
                const double lbnd[],
                const double ubnd[],
                AstFrameSet *iwcs,
                int wcssec,
                double value1[],
                double value2[],
                int *nval,
                int frame1[],
                int frame2[],
                int isgeo[],
                int isbnd[],
                int isdef1[],
                int isdef2[],
                int *status );

void ndf1Psndf( const char *str,
                double lbnd,
                double ubnd,
                int axis,
                AstFrameSet *iwcs,
                int wcssec,
                double *value1,
                double *value2,
                int *frame1,
                int *frame2,
                int *isgeo,
                int *isbnd,
                int *isdef1,
                int *isdef2,
                int *status );

void ndf1Pstyp( const char *type,
                int *itype,
                int *status );

void ndf1Ptloc( const char *param,
                int ipar,
                const char *vmode,
                NdfACB *acb,
                int *status );

void ndf1Pxlst( int includ,
                const char *str,
                size_t start,
                size_t end,
                AstKeyMap *keymap,
                int *status );

void ndf1Qbpp( unsigned char badbit,
               size_t el,
               const unsigned char qual[],
               int *bad,
               int *status );

void ndf1Qcre( NdfACB *acb,
               int *status );

void ndf1Qfrm( NdfACB *acb,
               char *form,
               size_t form_length,
               int *status );

void ndf1Qimp( NdfACB *acb,
               int *status );

void ndf1Qityp( int dtype,
                int itype,
                int *ok,
                int *status );

void ndf1Qma( size_t el,
              const unsigned char qual[],
              unsigned char badbit,
              const char *type,
              int npntr,
              void *pntr[],
              int *bad,
              int *status );

void ndf1Qmap( NdfACB *acb,
               const char *type,
               const char *mmod,
               void **pntr,
               int *status );

void ndf1Qmlog( unsigned char badbit,
                size_t el,
                const unsigned char qual[],
                int larray[],
                int *bad,
                int *status );

void ndf1Qrst( NdfACB *acb,
               int *status );

void ndf1Qsta( NdfACB *acb,
               int *state,
               int *status );

void ndf1Qump( NdfACB *acb,
               int *status );

const char *ndf1Rdast( void );

void ndf1Rdtun( const char *name,
                int dflt,
                int *value,
                int *status );

void ndf1Rdwcs( NdfACB *acb,
                AstFrameSet **iwcs,
                int *status );

void ndf1Rjust( char *string,
                int string_length );

void *ndf1Rls( NdfObject *object,
               int *status );

void ndf1Rmblk( char *text );

void ndf1Rst( NdfACB *acb,
              const char *comp,
              int *status );

void ndf1S2v( int bad,
              const char *type,
              size_t el,
              void *pntr,
              int *dce,
              int *status );

void ndf1Sctyp( NdfACB *acb,
                const char *comp,
                int *itype,
                int *status );

int ndf1Simlr( const char *str1,
               size_t start,
               size_t end,
               const char *str2,
               int n );

void ndf1Spfor( const char *fname,
                NdfFCB *fcb,
                size_t *d1,
                size_t *d2,
                size_t *n1,
                size_t *n2,
                size_t *t1,
                size_t *t2,
                size_t *v1,
                size_t *v2,
                size_t *x1,
                size_t *x2,
                int *status );

void ndf1Spldt( const char *str,
                size_t sbeg,
                size_t send,
                const char *delim,
                int mxfld,
                int fbeg[],
                int fend[],
                int *nfield,
                int *status );

void ndf1Ssdup( Ary *ary1,
                Ary *ary2,
                Ary **ary3,
                int *status );

void ndf1Stats( size_t dim,
                const double data[],
                double *maxv,
                double *minv,
                double *mean,
                double *sigma,
                double *rms,
                size_t *ngood,
                int *status );

void ndf1StoreSqLimit( void );

char *ndf1Strip( char *mem,
                 const char *text,
                 size_t start,
                 size_t end,
                 size_t *nc,
                 size_t *nlspace,
                 int *status );

char *ndf1Substr( const char *text,
                  size_t start,
                  size_t end,
                  int *status );

void ndf1Tcnam( HDSLoc *loc,
                char *name,
                size_t name_length,
                int *status );

void ndf1Temp( const char *type,
               int ndim,
               const hdsdim dim[],
               HDSLoc **loc,
               int *status );

char *ndf1Tilde( const char *text,
                 int *status );

void ndf1Time( int ymdhm[ 5 ],
               float *sec,
               int *status );

void ndf1Trace( const char *routin,
                int *status );

void ndf1True( size_t el,
               int larray[],
               int *status );

void ndf1Twrap( const char *in,
                size_t indent,
                size_t *fp,
                size_t l,
                char *out,
                size_t out_length );

void ndf1Typ( NdfACB *acb,
              const char *comp,
              int *itype,
              int *status );

void ndf1Ump( NdfACB *acb,
              const char *comp,
              int *status );

void ndf1Uname( NdfUNAME *info,
                int *status );

void ndf1UnlockACB( NdfACB *acb,
                    int *status );

void ndf1UnlockDCB( NdfDCB *dcb,
                    int *status );

void ndf1UnlockLoc( HDSLoc *loc,
                    int *status );

void ndf1UnlockAry( Ary *ary,
                    int *status );

void ndf1V2s( int bad,
              const char *type,
              size_t el,
              void *pntr,
              int *dce,
              int *status );

void ndf1Vaccn( const char *ccomp,
                int *iccomp,
                int *status );

void ndf1Van( NdfACB *acb,
              int iaxis,
              int allok,
              int *iax1,
              int *iax2,
              int *status );

void ndf1Vbad( NdfACB *acb,
               int check,
               int *bad,
               int *status );

void ndf1Vbnd( int ndim,
               const hdsdim lbnd[],
               const hdsdim ubnd[],
               int *status );

void ndf1Vccn( const char *ccomp,
               int *iccomp,
               int *status );

void ndf1Vclst( const char *text,
                int ncomp,
                const char *cnames[],
                int cflags[],
                int *nset,
                int *status );

void ndf1Vcpx( NdfACB *acb,
               int *cmplx,
               int *status );

void ndf1Vcre( NdfACB *acb,
               int *status );

void ndf1Vdat( const int ymdhm[],
               float sec,
               int *status );

void ndf1Vfrm( NdfACB *acb,
               char *form,
               size_t form_length,
               int *status );

void ndf1Vftp( NdfACB *acb,
               char *ftype,
               size_t ftype_length,
               int *status );

void ndf1Vimp( NdfACB *acb,
               int *status );

void ndf1Vmap( NdfACB *acb,
               const char *type,
               int cmplx,
               const char *mmod,
               int stdev,
               int mask,
               void **dpntr,
               void **ipntr,
               int *status );

void ndf1Vmmd( const char *mmod,
               char *mode,
               size_t mode_length,
               char *inopt,
               size_t inopt_length,
               int *status );

void ndf1Vmod( const char *mode,
               char *vmode,
               size_t vmode_length,
               int *status );

void ndf1Vrst( NdfACB *acb,
               int *status );

void ndf1Vsbd( int bad,
               NdfACB *acb,
               int *status );

void ndf1Vsftp( const char *ftype,
                NdfACB *acb,
                int *status );

void ndf1Vsta( NdfACB *acb,
               int *state,
               int *status );

void ndf1Vstat( const char *state,
                char *vstate,
                size_t vstate_length,
                int *status );

void ndf1Vstyp( NdfACB *acb,
                char *type,
                size_t type_length,
                int *status );

void ndf1Vtyp( NdfACB *acb,
               char *type,
               size_t type_length,
               int *status );

void ndf1Vump( NdfACB *acb,
               int *status );

void ndf1Vwcs( NdfACB *acb,
               AstFrameSet *iwcs1,
               AstFrameSet **iwcs2,
               int *status );

void ndf1Wclim( AstFrameSet *iwcs,
                int nax,
                int ndim,
                const hdsdim nlbnd[],
                const hdsdim nubnd[],
                const int isdef1[],
                const int isdef2[],
                double value1[],
                double value2[],
                int isgeo[],
                int isbnd[],
                hdsdim lbnd[],
                hdsdim ubnd[],
                int *status );

void ndf1Wcspm( AstMapping *map,
                const hdsdim lbnd[],
                const hdsdim ubnd[],
                int perm[],
                int *status );

void ndf1Wplim( AstFrameSet *iwcs,
                int nax,
                const hdsdim lbndd[],
                const hdsdim ubndd[],
                double value1[],
                const double value2[],
                int ispix1[],
                const int ispix2[],
                const int isbnd[],
                const int isgeo[],
                const int isdef1[],
                const int isdef2[],
                hdsdim lbnd[],
                hdsdim ubnd[],
                int *status );

void ndf1Wrast( const char *text );

void ndf1Wrwcs( AstFrameSet *iwcs,
                NdfACB *acb,
                int *status );

void ndf1Wsbnd( int ndim,
                const hdsdim lbnd[],
                const hdsdim ubnd[],
                NdfACB *acb,
                AstFrameSet **iwcs,
                int *status );

void ndf1Wsta( NdfACB *acb,
               int *state,
               int *status );

void ndf1Wwrt( AstFrameSet *iwcs,
               NdfDCB *dcb,
               int *status );

void ndf1Xcpy( HDSLoc *xloc1,
               int nextn,
               char extn[][ DAT__SZNAM + 1 ],
               HDSLoc *loc,
               HDSLoc **xloc2,
               int *status );

void ndf1Xlst( NdfACB *acb,
               int mxextn,
               char extn[][ DAT__SZNAM + 1 ],
               int *nextn,
               int *status );

void ndf1Xtfor( const char *forfil,
                NdfFCB *fcb,
                HDSLoc *ndfloc,
                const char *ndfnam,
                int imp,
                int *status );

void ndf1Zpsca( NdfACB *acb,
                int type,
                const double scale[],
                const double zero[],
                int *status );

void ndf1Zscal( int intype,
                void *pin,
                size_t el,
                int outtype,
                double *scale,
                double *zero,
                void *pout,
                int *status );

/* Now include the expanded generic prototypes. */
#include "ndf1_cgen.h"

#endif

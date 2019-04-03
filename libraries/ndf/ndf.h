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
*    This file contains definitions needed for public use of the NDF
*    system.

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

/* External interfaces.
   ==================== */
#include "ast.h"
#include "ary.h"
#include "star/hds_types.h"


/*  Constants and types.
    ==================== */
#include "ndf_types.h"

/* The C wrapper in the F77 version of NDF includes the NDF error
   definitions in the ndf.h file, so the C version need to too to
   avoid breaking existing code. */
#if !defined( NDF_ERROR_DEFINED )
#include "ndf_err.h"
#endif



/*  Function prototypes for NDF V2
    ==============================  */

void ndfAcget_( int indf,
               const char *comp,
               int iaxis,
               char *value,
               size_t value_length,
               int *status );

void ndfAclen_( int indf,
                const char *comp,
                int iaxis,
                size_t *length,
                int *status );

void ndfAcmsg_( const char *token,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAcput_( const char *value,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAcre_( int indf,
              int *status );

void ndfAform_( int indf,
               const char *comp,
               int iaxis,
               char *form,
               size_t form_length,
               int *status );

void ndfAmap_( int indf,
               const char *comp,
               int iaxis,
               const char *type,
               const char *mmod,
               void *pntr[],
               size_t *el,
               int *status );

void ndfAnnul_( int *indf,
               int *status );

void ndfAnorm_( int indf,
               int iaxis,
               int *norm,
               int *status );

void ndfArest_( int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAsnrm_( int norm,
               int indf,
               int iaxis,
               int *status );

void ndfAssoc_( const char *param,
               const char *mode,
               int *indf,
               int *status );

void ndfAstat_( int indf,
               const char *comp,
               int iaxis,
               int *state,
               int *status );

void ndfAstyp_( const char *type,
               int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfAtype_( int indf,
               const char *comp,
               int iaxis,
               char *type,
               size_t type_length,
               int *status );

void ndfAunmp_( int indf,
               const char *comp,
               int iaxis,
               int *status );

void ndfBad_( int indf,
             const char *comp,
             int check,
             int *bad,
             int *status );

void ndfBase_( int indf1,
              int *indf2,
              int *status );

void ndfBb_( int indf,
            unsigned char *badbit,
            int *status );

void ndfBegin_( void );

void ndfBlock_( int indf1,
               int ndim,
               const hdsdim mxdim[],
               int iblock,
               int *indf2,
               int *status );

void ndfBound_( int indf,
               int ndimx,
               hdsdim lbnd[],
               hdsdim ubnd[],
               int *ndim,
               int *status );

void ndfCancl_( const char *param,
               int *status );

void ndfCget_( int indf,
              const char *comp,
              char *value,
              size_t value_length,
              int *status );

void ndfChunk_( int indf1,
               int mxpix,
               int ichunk,
               int *indf2,
               int *status );

void ndfCinp_( const char *param,
              int indf,
              const char *comp,
              int *status );

void ndfClen_( int indf,
              const char *comp,
              size_t *length,
              int *status );

void ndfClone_( int indf1,
               int *indf2,
               int *status );

void ndfCmplx_( int indf,
               const char *comp,
               int *cmplx,
               int *status );

void ndfCmsg_( const char *token,
              int indf,
              const char *comp,
              int *status );

void ndfCopy_( int indf1,
              int *place,
              int *indf2,
              int *status );

void ndfCput_( const char *value,
              int indf,
              const char *comp,
              int *status );

void ndfCreat_( const char *param,
               const char *ftype,
               int ndim,
               const hdsdim lbnd[],
               const hdsdim ubnd[],
               int *indf,
               int *status );

void ndfCrep_( const char *param,
              const char *ftype,
              int ndim,
              const hdsdim ubnd[],
              int *indf,
              int *status );

void ndfCrepl_( const char *param,
               int *place,
               int *status );

void ndfDelet_( int *indf,
               int *status );

void ndfDim_( int indf,
             int ndimx,
             hdsdim dim[],
             int *ndim,
             int *status );

void ndfEnd_( int *status );

void ndfExist_( const char *param,
               const char *mode,
               int *indf,
               int *status );

void ndfFind_( const HDSLoc *loc,
              const char *name,
              int *indf,
              int *status );

void ndfForm_( int indf,
              const char *comp,
              char *form,
              size_t form_length,
              int *status );

void ndfFtype_( int indf,
               const char *comp,
               char *ftype,
               size_t ftype_length,
               int *status );

void ndfGtdlt_( int indf,
               const char *comp,
               int *zaxis,
               char *ztype,
               size_t ztype_length,
               float *zratio,
               int *status );

void ndfGtune_( const char *tpar,
               int *value,
               int *status );

void ndfGtwcs_( int indf,
               AstFrameSet **iwcs,
               int *status );

void ndfHappn_( const char *appn,
               int *status );

void ndfHcopy_( int indf1,
               int indf2,
               int *status );

void ndfHcre_( int indf,
              int *status );

void ndfHdef_( int indf,
              const char *appn,
              int *status );

void ndfHecho_( int nlines,
               char *const text[],
               int *status );

void ndfHend_( int *status );

void ndfHfind_( int indf,
               const int ymdhm[ 5 ],
               float sec,
               int eq,
               int *irec,
               int *status );

void ndfHgmod_( int indf,
               char *hmode,
               size_t hmode_length,
               int *status );

void ndfHinfo_( int indf,
               const char *item,
               int irec,
               char *value,
               size_t value_length,
               int *status );

void ndfHndlr_( const char *evname,
               NdfEventHandler hndlr,
               int set,
               int *status );

void ndfHnrec_( int indf,
               int *nrec,
               int *status );

void ndfHout_( int indf,
              int irec,
              void ( *routin )( int, char *const [], int * ),
              int *status );

void ndfHpurg_( int indf,
               int irec1,
               int irec2,
               int *status );

void ndfHput_( const char *hmode,
              const char *appn,
              int repl,
              int nlines,
              char *const text[],
              int trans,
              int wrap,
              int rjust,
              int indf,
              int *status );

void ndfHsdat_( const char *date,
               int indf,
               int *status );

void ndfHsmod_( const char *hmode,
               int indf,
               int *status );

void ndfIsacc_( int indf,
               const char *access,
               int *isacc,
               int *status );

void ndfIsbas_( int indf,
               int *isbas,
               int *status );

void ndfIsin_( int indf1,
              int indf2,
              int *isin,
              int *status );

void ndfIstmp_( int indf,
               int *istmp,
               int *status );

void ndfLoc_( int indf,
             const char *mode,
             HDSLoc ** loc,
             int *status );

void ndfLock_( int indf,
               int *status );

int ndfLocked_( int indf,
                int *status );

void ndfMap_( int indf,
             const char *comp,
             const char *type,
             const char *mmod,
             void *pntr[],
             size_t *el,
             int *status );

void ndfMapql_( int indf,
               int **pntr,
               size_t *el,
               int *bad,
               int *status );

void ndfMapz_( int indf,
              const char *comp,
              const char *type,
              const char *mmod,
              void *rpntr[],
              void *ipntr[],
              size_t *el,
              int *status );

void ndfMbad_( int badok,
              int indf1,
              int indf2,
              const char *comp,
              int check,
              int *bad,
              int *status );

void ndfMbadn_( int badok,
               int n,
               const int ndfs[],
               const char *comp,
               int check,
               int *bad,
               int *status );

void ndfMbnd_( const char *option,
              int *indf1,
              int *indf2,
              int *status );

void ndfMbndn_( const char *option,
               int n,
               int ndfs[],
               int *status );

void ndfMsg_( const char *token,
             int indf );

void ndfMtype_( const char *typlst,
               int indf1,
               int indf2,
               const char *comp,
               char *itype,
               size_t itype_length,
               char *dtype,
               size_t dtype_length,
               int *status );

void ndfMtypn_( const char *typlst,
               int n,
               const int ndfs[],
               const char *comp,
               char *itype,
               size_t itype_length,
               char *dtype,
               size_t dtype_length,
               int *status );

void ndfNbloc_( int indf,
               int ndim,
               const hdsdim mxdim[],
               int *nblock,
               int *status );

void ndfNchnk_( int indf,
               int mxpix,
               int *nchunk,
               int *status );

void ndfNew_( const char *ftype,
             int ndim,
             const hdsdim lbnd[],
             const hdsdim ubnd[],
             int *place,
             int *indf,
             int *status );

void ndfNewp_( const char *ftype,
              int ndim,
              const hdsdim ubnd[],
              int *place,
              int *indf,
              int *status );

void ndfNoacc_( const char *access,
               int indf,
               int *status );

void ndfOpen_( const HDSLoc * loc,
              const char *name,
              const char *mode,
              const char *stat,
              int *indf,
              int *place,
              int *status );

void ndfPlace_( const HDSLoc * loc,
               const char *name,
               int *place,
               int *status );

void ndfProp_( int indf1,
              const char *clist,
              const char *param,
              int *indf2,
              int *status );

void ndfPtwcs_( const AstFrameSet *iwcs,
               int indf,
               int *status );

void ndfQmf_( int indf,
             int *qmf,
             int *status );

int ndfReport_( int indf,
                int *status );

void ndfReset_( int indf,
               const char *comp,
               int *status );

void ndfSame_( int indf1,
              int indf2,
              int *same,
              int *isect,
              int *status );

void ndfSbad_( int bad,
              int indf,
              const char *comp,
              int *status );

void ndfSbb_( unsigned char badbit,
             int indf,
             int *status );

void ndfSbnd_( int ndim,
              const hdsdim lbnd[],
              const hdsdim ubnd[],
              int indf,
              int *status );

void ndfScopy_( int indf1,
               const char *clist,
               int *place,
               int *indf2,
               int *status );

void ndfSctyp_( int indf,
               const char *comp,
               char *type,
               size_t type_length,
               int *status );

void ndfSect_( int indf1,
              int ndim,
              const hdsdim lbnd[],
              const hdsdim ubnd[],
              int *indf2,
              int *status );

void ndfShift_( int nshift,
               const hdsdim shift[],
               int indf,
               int *status );

void ndfSize_( int indf,
              size_t *npix,
              int *status );

void ndfSqmf_( int qmf,
              int indf,
              int *status );

void ndfSsary_( Ary *ary1,
               int indf,
               Ary **ary2,
               int *status );

void ndfState_( int indf,
               const char *comp,
               int *state,
               int *status );

void ndfStype_( const char *ftype,
               int indf,
               const char *comp,
               int *status );

void ndfTemp_( int *place,
              int *status );

void ndfTune_( int value,
              const char *tpar,
              int *status );

void ndfType_( int indf,
              const char *comp,
              char *type,
              size_t type_length,
              int *status );

void ndfUnlock_( int indf,
                 int *status );

void ndfUnmap_( int indf,
               const char *comp,
               int *status );

void ndfValid_( int indf,
               int *valid,
               int *status );

void ndfXdel_( int indf,
              const char *xname,
              int *status );

void ndfXgt0c_( int indf,
               const char *xname,
               const char *cmpt,
               char *value,
               size_t value_length,
               int *status );

void ndfXgt0l_( int indf,
                const char *xname,
                const char *cmpt,
                int *value,
                int *status );

void ndfXiary_( int indf,
               const char *xname,
               const char *cmpt,
               const char *mode,
               Ary **ary,
               int *status );

void ndfXloc_( int indf,
              const char *xname,
              const char *mode,
              HDSLoc ** loc,
              int *status );

void ndfXname_( int indf,
               int n,
               char *xname,
               size_t xname_length,
               int *status );

void ndfXnew_( int indf,
              const char *xname,
              const char *type,
              int ndim,
              const hdsdim dim[],
              HDSLoc **loc,
              int *status );

void ndfXnumb_( int indf,
               int *nextn,
               int *status );

void ndfXpt0c_( const char *value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status );

void ndfXpt0l_( int value,
                int indf,
                const char *xname,
                const char *cmpt,
                int *status );

void ndfXstat_( int indf,
               const char *xname,
               int *there,
               int *status );

void ndfZdelt_( int indf1,
               const char *comp,
               float minrat,
               int zaxis,
               const char *type,
               int *place,
               int *indf2,
               float *zratio,
               int *status );

void ndfZscal_( int indf1,
               const char *type,
               double scale[ 2 ],
               double zero[ 2 ],
               int *place,
               int *indf2,
               int *status );


/*  Prototypes for NDF function that are different in V1 (arguments that
    are "int" in V1 and "size_t" in V2 are not included since the int
    value supplied by the caller will be cast to a size_t automatically
    by the compiler). The changes are mainly "hdsdim"->"int" (for
    dimension sizes) and *size_t *" -> "int *" (for returned pixel counts,
    etc).
    ==================================================================  */

void ndfAmap_v1( int indf,
               const char *comp,
               int iaxis,
               const char *type,
               const char *mmod,
               void *pntr[],
               int *el,
               int *status );

void ndfBound_v1( int indf,
               int ndimx,
               int lbnd[],
               int ubnd[],
               int *ndim,
               int *status );

void ndfCreat_v1( const char *param,
               const char *ftype,
               int ndim,
               const int lbnd[],
               const int ubnd[],
               int *indf,
               int *status );

void ndfCrep_v1( const char *param,
              const char *ftype,
              int ndim,
              const int ubnd[],
              int *indf,
              int *status );

void ndfDim_v1( int indf,
             int ndimx,
             int dim[],
             int *ndim,
             int *status );

void ndfMap_v1( int indf,
             const char *comp,
             const char *type,
             const char *mmod,
             void *pntr[],
             int *el,
             int *status );

void ndfMapql_v1( int indf,
               int **pntr,
               int *el,
               int *bad,
               int *status );

void ndfMapz_v1( int indf,
              const char *comp,
              const char *type,
              const char *mmod,
              void *rpntr[],
              void *ipntr[],
              int *el,
              int *status );

void ndfNew_v1( const char *ftype,
             int ndim,
             const int lbnd[],
             const int ubnd[],
             int *place,
             int *indf,
             int *status );

void ndfNewp_v1( const char *ftype,
              int ndim,
              const int ubnd[],
              int *place,
              int *indf,
              int *status );

void ndfSbnd_v1( int ndim,
              const int lbnd[],
              const int ubnd[],
              int indf,
              int *status );

void ndfSect_v1( int indf1,
              int ndim,
              const int lbnd[],
              const int ubnd[],
              int *indf2,
              int *status );

void ndfShift_v1( int nshift,
               const int shift[],
               int indf,
               int *status );

void ndfSize_v1( int indf,
              int *npix,
              int *status );

void ndfSsary_v1( int iary1,
               int indf,
               int *iary2,
               int *status );

void ndfXiary_v1( int indf,
               const char *xname,
               const char *cmpt,
               const char *mode,
               int *iary,
               int *status );

void ndfXnew_v1( int indf,
              const char *xname,
              const char *type,
              int ndim,
              const int dim[],
              HDSLoc **loc,
              int *status );



/* Now define the macros used by application code to invoke the appropriate
   functions, depending on whether the old or new interface is required.

   Interfaced which are the same in both version... */
#define ndfAcget  ndfAcget_
#define ndfAclen  ndfAclen_
#define ndfAcmsg  ndfAcmsg_
#define ndfAcput  ndfAcput_
#define ndfAcre   ndfAcre_
#define ndfAform  ndfAform_
#define ndfAnnul  ndfAnnul_
#define ndfAnorm  ndfAnorm_
#define ndfArest  ndfArest_
#define ndfAsnrm  ndfAsnrm_
#define ndfAssoc  ndfAssoc_
#define ndfAstat  ndfAstat_
#define ndfAstyp  ndfAstyp_
#define ndfAtype  ndfAtype_
#define ndfAunmp  ndfAunmp_
#define ndfBad    ndfBad_
#define ndfBase   ndfBase_
#define ndfBb     ndfBb_
#define ndfBegin  ndfBegin_
#define ndfBlock  ndfBlock_
#define ndfCancl  ndfCancl_
#define ndfCget   ndfCget_
#define ndfChunk  ndfChunk_
#define ndfCinp   ndfCinp_
#define ndfClen   ndfClen_
#define ndfClone  ndfClone_
#define ndfCmplx  ndfCmplx_
#define ndfCmsg   ndfCmsg_
#define ndfCopy   ndfCopy_
#define ndfCput   ndfCput_
#define ndfCrepl  ndfCrepl_
#define ndfDelet  ndfDelet_
#define ndfEnd    ndfEnd_
#define ndfExist  ndfExist_
#define ndfFind   ndfFind_
#define ndfForm   ndfForm_
#define ndfFtype  ndfFtype_
#define ndfGtdlt  ndfGtdlt_
#define ndfGtune  ndfGtune_
#define ndfGtwcs  ndfGtwcs_
#define ndfHappn  ndfHappn_
#define ndfHcopy  ndfHcopy_
#define ndfHcre   ndfHcre_
#define ndfHdef   ndfHdef_
#define ndfHecho  ndfHecho_
#define ndfHend   ndfHend_
#define ndfHfind  ndfHfind_
#define ndfHgmod  ndfHgmod_
#define ndfHinfo  ndfHinfo_
#define ndfHnrec  ndfHnrec_
#define ndfHout   ndfHout_
#define ndfHpurg  ndfHpurg_
#define ndfHput   ndfHput_
#define ndfHsdat  ndfHsdat_
#define ndfHsmod  ndfHsmod_
#define ndfIsacc  ndfIsacc_
#define ndfIsbas  ndfIsbas_
#define ndfIsin   ndfIsin_
#define ndfIstmp  ndfIstmp_
#define ndfLoc    ndfLoc_
#define ndfLock   ndfLock_
#define ndfLocked ndfLocked_
#define ndfMbad   ndfMbad_
#define ndfMbadn  ndfMbadn_
#define ndfMbnd   ndfMbnd_
#define ndfMbndn  ndfMbndn_
#define ndfMsg    ndfMsg_
#define ndfMtype  ndfMtype_
#define ndfMtypn  ndfMtypn_
#define ndfNbloc  ndfNbloc_
#define ndfNchnk  ndfNchnk_
#define ndfNoacc  ndfNoacc_
#define ndfOpen   ndfOpen_
#define ndfPlace  ndfPlace_
#define ndfProp   ndfProp_
#define ndfPtwcs  ndfPtwcs_
#define ndfQmf    ndfQmf_
#define ndfReport ndfReport_
#define ndfReset  ndfReset_
#define ndfSame   ndfSame_
#define ndfSbad   ndfSbad_
#define ndfSbb    ndfSbb_
#define ndfScopy  ndfScopy_
#define ndfSctyp  ndfSctyp_
#define ndfSqmf   ndfSqmf_
#define ndfState  ndfState_
#define ndfStype  ndfStype_
#define ndfTemp   ndfTemp_
#define ndfTune   ndfTune_
#define ndfType   ndfType_
#define ndfUnlock ndfUnlock_
#define ndfUnmap  ndfUnmap_
#define ndfValid  ndfValid_
#define ndfXdel   ndfXdel_
#define ndfXgt0c  ndfXgt0c_
#define ndfXloc   ndfXloc_
#define ndfXname  ndfXname_
#define ndfXnumb  ndfXnumb_
#define ndfXpt0l  ndfXpt0l_
#define ndfXpt0c  ndfXpt0c_
#define ndfXstat  ndfXstat_
#define ndfZdelt  ndfZdelt_
#define ndfZscal  ndfZscal_

/* Version 2 interfaces with 64 bit pixel counters, etc */
#if defined( NDF_V2 )

#define ndfAmap   ndfAmap_
#define ndfBound  ndfBound_
#define ndfCreat  ndfCreat_
#define ndfCrep   ndfCrep_
#define ndfDim    ndfDim_
#define ndfMap    ndfMap_
#define ndfMapql  ndfMapql_
#define ndfMapz   ndfMapz_
#define ndfNew    ndfNew_
#define ndfNewp   ndfNewp_
#define ndfSbnd   ndfSbnd_
#define ndfSect   ndfSect_
#define ndfShift  ndfShift_
#define ndfSize   ndfSize_
#define ndfSsary  ndfSsary_
#define ndfXiary  ndfXiary_
#define ndfXnew   ndfXnew_


/* Version 1 interfaces with 32 bit pixel counters, etc */
#else

#define ndfAmap   ndfAmap_v1
#define ndfBound  ndfBound_v1
#define ndfCreat  ndfCreat_v1
#define ndfCrep   ndfCrep_v1
#define ndfDim    ndfDim_v1
#define ndfMap    ndfMap_v1
#define ndfMapql  ndfMapql_v1
#define ndfMapz   ndfMapz_v1
#define ndfNew    ndfNew_v1
#define ndfNewp   ndfNewp_v1
#define ndfSbnd   ndfSbnd_v1
#define ndfSect   ndfSect_v1
#define ndfShift  ndfShift_v1
#define ndfSize   ndfSize_v1
#define ndfSsary  ndfSsary_v1
#define ndfXiary  ndfXiary_v1
#define ndfXnew   ndfXnew_v1

#endif

/* Now include the expanded generic prototypes. */
#include "ndf_cgen.h"

/* The ndfINit funtion that was included in the F77 version of NDF is no
   longer needed. But the following null macro is provided so that source
   code need not be changed. */
#define ndfInit( argc, argv, status )

#endif

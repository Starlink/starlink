#if !defined( SOFA2AST_INCLUDED )  /* Include this file only once */
#define SOFA2AST_INCLUDED
/*
*  Name:
*     sofa2ast.h

*  Type:
*     C include file.

*  Purpose:
*     Defines new names for symbols exported by the IAU SOFA library.

*  Invocation:
*     #include "sofa2ast.h"

*  Description:
*     This include file defines a new name for each public function
*     defined by the SOFA library. The names defined by SOFA itself are
*     of the form "iauXxx" (e.g. iauPmp) - this include file defines
*     a macro that translates each such name to the form "astIauXxx"
*     (e.g. astIauPmp). This is done so that the names do not clash
*     with any external SOFA library with which the application is linked.
*
*     It should be included at the start of any AST source file that refers
*     to SOFA functions using the standard names (e.g. iauPmp).

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     16-FEB-2012 (DSB):
*        Original version.
*/

/* Rename all SOFA functions called directlty from PAL. */
#define iauAf2a    astIauAf2a
#define iauAnp	   astIauAnp
#define iauAnpm	   astIauAnpm
#define iauC2s	   astIauC2s
#define iauCal2jd  astIauCal2jd
#define iauD2tf	   astIauD2tf
#define iauDat	   astIauDat
#define iauEe06a   astIauEe06a
#define iauEpb	   astIauEpb
#define iauEpb2jd  astIauEpb2jd
#define iauEpj	   astIauEpj
#define iauEpj2jd  astIauEpj2jd
#define iauEpv00   astIauEpv00
#define iauFk5hz   astIauFk5hz
#define iauGd2gc   astIauGd2gc
#define iauGmst06  astIauGmst06
#define iauHfk5z   astIauHfk5z
#define iauIr	   astIauIr
#define iauJd2cal  astIauJd2cal
#define iauObl06   astIauObl06
#define iauP06e	   astIauP06e
#define iauPap	   astIauPap
#define iauPas	   astIauPas
#define iauPdp	   astIauPdp
#define iauPmat06  astIauPmat06
#define iauPn	   astIauPn
#define iauPnm06a  astIauPnm06a
#define iauPxp	   astIauPxp
#define iauRm2v	   astIauRm2v
#define iauRv2m	   astIauRv2m
#define iauRx	   astIauRx
#define iauRxp	   astIauRxp
#define iauRxpv	   astIauRxpv
#define iauRxr	   astIauRxr
#define iauRy	   astIauRy
#define iauRz	   astIauRz
#define iauS2c	   astIauS2c
#define iauSepp	   astIauSepp
#define iauSeps	   astIauSeps
#define iauTf2a	   astIauTf2a
#define iauTf2d	   astIauTf2d
#define iauTr	   astIauTr
#define iauTrxp    astIauTrxp


/* Rename all SOFA functions called internally within the above SOFA
   functions. */
#define iauA2af    astIauA2af
#define iauA2tf	   astIauA2tf
#define iauBi00	   astIauBi00
#define iauBp00	   astIauBp00
#define iauBp06	   astIauBp06
#define iauBpn2xy  astIauBpn2xy
#define iauC2i00a  astIauC2i00a
#define iauC2i00b  astIauC2i00b
#define iauC2i06a  astIauC2i06a
#define iauC2ibpn  astIauC2ibpn
#define iauC2ixy   astIauC2ixy
#define iauC2ixys  astIauC2ixys
#define iauC2t00a  astIauC2t00a
#define iauC2t00b  astIauC2t00b
#define iauC2t06a  astIauC2t06a
#define iauC2tcio  astIauC2tcio
#define iauC2teqx  astIauC2teqx
#define iauC2tpe   astIauC2tpe
#define iauC2txy   astIauC2txy
#define iauCp	   astIauCp
#define iauCpv	   astIauCpv
#define iauCr	   astIauCr
#define iauD2dtf   astIauD2dtf
#define iauDtdb	   astIauDtdb
#define iauDtf2d   astIauDtf2d
#define iauEe00	   astIauEe00
#define iauEe00a   astIauEe00a
#define iauEe00b   astIauEe00b
#define iauEect00  astIauEect00
#define iauEform   astIauEform
#define iauEo06a   astIauEo06a
#define iauEors	   astIauEors
#define iauEqeq94  astIauEqeq94
#define iauEra00   astIauEra00
#define iauFad03   astIauFad03
#define iauFae03   astIauFae03
#define iauFaf03   astIauFaf03
#define iauFaju03  astIauFaju03
#define iauFal03   astIauFal03
#define iauFalp03  astIauFalp03
#define iauFama03  astIauFama03
#define iauFame03  astIauFame03
#define iauFane03  astIauFane03
#define iauFaom03  astIauFaom03
#define iauFapa03  astIauFapa03
#define iauFasa03  astIauFasa03
#define iauFaur03  astIauFaur03
#define iauFave03  astIauFave03
#define iauFk52h   astIauFk52h
#define iauFk5hip  astIauFk5hip
#define iauFw2m	   astIauFw2m
#define iauFw2xy   astIauFw2xy
#define iauGc2gd   astIauGc2gd
#define iauGc2gde  astIauGc2gde
#define iauGd2gce  astIauGd2gce
#define iauGmst00  astIauGmst00
#define iauGmst82  astIauGmst82
#define iauGst00a  astIauGst00a
#define iauGst00b  astIauGst00b
#define iauGst06   astIauGst06
#define iauGst06a  astIauGst06a
#define iauGst94   astIauGst94
#define iauH2fk5   astIauH2fk5
#define iauJdcalf  astIauJdcalf
#define iauNum00a  astIauNum00a
#define iauNum00b  astIauNum00b
#define iauNum06a  astIauNum06a
#define iauNumat   astIauNumat
#define iauNut00a  astIauNut00a
#define iauNut00b  astIauNut00b
#define iauNut06a  astIauNut06a
#define iauNut80   astIauNut80
#define iauNutm80  astIauNutm80
#define iauObl80   astIauObl80
#define iauP2pv	   astIauP2pv
#define iauP2s	   astIauP2s
#define iauPb06	   astIauPb06
#define iauPfw06   astIauPfw06
#define iauPlan94  astIauPlan94
#define iauPm	   astIauPm
#define iauPmat00  astIauPmat00
#define iauPmat76  astIauPmat76
#define iauPmp	   astIauPmp
#define iauPn00	   astIauPn00
#define iauPn00a   astIauPn00a
#define iauPn00b   astIauPn00b
#define iauPn06	   astIauPn06
#define iauPn06a   astIauPn06a
#define iauPnm00a  astIauPnm00a
#define iauPnm00b  astIauPnm00b
#define iauPnm80   astIauPnm80
#define iauPom00   astIauPom00
#define iauPpp	   astIauPpp
#define iauPpsp	   astIauPpsp
#define iauPr00	   astIauPr00
#define iauPrec76  astIauPrec76
#define iauPv2p	   astIauPv2p
#define iauPv2s	   astIauPv2s
#define iauPvdpv   astIauPvdpv
#define iauPvm	   astIauPvm
#define iauPvmpv   astIauPvmpv
#define iauPvppv   astIauPvppv
#define iauPvstar  astIauPvstar
#define iauPvu	   astIauPvu
#define iauPvup	   astIauPvup
#define iauPvxpv   astIauPvxpv
#define iauS00	   astIauS00
#define iauS00a	   astIauS00a
#define iauS00b	   astIauS00b
#define iauS06	   astIauS06
#define iauS06a	   astIauS06a
#define iauS2p	   astIauS2p
#define iauS2pv	   astIauS2pv
#define iauS2xpv   astIauS2xpv
#define iauSp00	   astIauSp00
#define iauStarpm  astIauStarpm
#define iauStarpv  astIauStarpv
#define iauSxp	   astIauSxp
#define iauSxpv	   astIauSxpv
#define iauTaitt   astIauTaitt
#define iauTaiut1  astIauTaiut1
#define iauTaiutc  astIauTaiutc
#define iauTcbtdb  astIauTcbtdb
#define iauTcgtt   astIauTcgtt
#define iauTdbtcb  astIauTdbtcb
#define iauTdbtt   astIauTdbtt
#define iauTrxpv   astIauTrxpv
#define iauTttai   astIauTttai
#define iauTttcg   astIauTttcg
#define iauTttdb   astIauTttdb
#define iauTtut1   astIauTtut1
#define iauUt1tai  astIauUt1tai
#define iauUt1tt   astIauUt1tt
#define iauUt1utc  astIauUt1utc
#define iauUtctai  astIauUtctai
#define iauUtcut1  astIauUtcut1
#define iauXy06	   astIauXy06
#define iauXys00a  astIauXys00a
#define iauXys00b  astIauXys00b
#define iauXys06a  astIauXys06a
#define iauZp	   astIauZp
#define iauZpv	   astIauZpv
#define iauZr      astIaunZr

#endif

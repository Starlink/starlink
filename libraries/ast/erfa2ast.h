#if !defined( ERFA2AST_INCLUDED )  /* Include this file only once */
#define ERFA2AST_INCLUDED
/*
*  Name:
*     erfa2ast.h

*  Type:
*     C include file.

*  Purpose:
*     Defines new names for symbols exported by the ERFA library.

*  Invocation:
*     #include "erfa2ast.h"

*  Description:
*     This include file defines a new name for each public function
*     defined by the ERFA library. The names defined by ERFA itself are
*     of the form "eraXxx" (e.g. eraPmp) - this include file defines
*     a macro that translates each such name to the form "astEraXxx"
*     (e.g. astEraPmp). This is done so that the names do not clash
*     with any external ERFA library with which the application is linked.
*
*     It should be included at the start of any AST source file that refers
*     to ERFA functions using the standard names (e.g. eraPmp).

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*     
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*     
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     16-FEB-2012 (DSB):
*        Original version.
*/

/* Rename all ERFA functions called directlty from PAL. */
#define eraAf2a    astEraAf2a
#define eraAnp	   astEraAnp
#define eraAnpm	   astEraAnpm
#define eraC2s	   astEraC2s
#define eraCal2jd  astEraCal2jd
#define eraD2tf	   astEraD2tf
#define eraDat	   astEraDat
#define eraEe06a   astEraEe06a
#define eraEpb	   astEraEpb
#define eraEpb2jd  astEraEpb2jd
#define eraEpj	   astEraEpj
#define eraEpj2jd  astEraEpj2jd
#define eraEpv00   astEraEpv00
#define eraFk5hz   astEraFk5hz
#define eraGd2gc   astEraGd2gc
#define eraGmst06  astEraGmst06
#define eraHfk5z   astEraHfk5z
#define eraIr	   astEraIr
#define eraJd2cal  astEraJd2cal
#define eraObl06   astEraObl06
#define eraP06e	   astEraP06e
#define eraPap	   astEraPap
#define eraPas	   astEraPas
#define eraPdp	   astEraPdp
#define eraPmat06  astEraPmat06
#define eraPn	   astEraPn
#define eraPnm06a  astEraPnm06a
#define eraPxp	   astEraPxp
#define eraRm2v	   astEraRm2v
#define eraRv2m	   astEraRv2m
#define eraRx	   astEraRx
#define eraRxp	   astEraRxp
#define eraRxpv	   astEraRxpv
#define eraRxr	   astEraRxr
#define eraRy	   astEraRy
#define eraRz	   astEraRz
#define eraS2c	   astEraS2c
#define eraSepp	   astEraSepp
#define eraSeps	   astEraSeps
#define eraTf2a	   astEraTf2a
#define eraTf2d	   astEraTf2d
#define eraTr	   astEraTr
#define eraTrxp    astEraTrxp


/* Rename all ERFA functions called internally within the above ERFA
   functions. */
#define eraA2af    astEraA2af
#define eraA2tf	   astEraA2tf
#define eraBi00	   astEraBi00
#define eraBp00	   astEraBp00
#define eraBp06	   astEraBp06
#define eraBpn2xy  astEraBpn2xy
#define eraC2i00a  astEraC2i00a
#define eraC2i00b  astEraC2i00b
#define eraC2i06a  astEraC2i06a
#define eraC2ibpn  astEraC2ibpn
#define eraC2ixy   astEraC2ixy
#define eraC2ixys  astEraC2ixys
#define eraC2t00a  astEraC2t00a
#define eraC2t00b  astEraC2t00b
#define eraC2t06a  astEraC2t06a
#define eraC2tcio  astEraC2tcio
#define eraC2teqx  astEraC2teqx
#define eraC2tpe   astEraC2tpe
#define eraC2txy   astEraC2txy
#define eraCp	   astEraCp
#define eraCpv	   astEraCpv
#define eraCr	   astEraCr
#define eraD2dtf   astEraD2dtf
#define eraDtdb	   astEraDtdb
#define eraDtf2d   astEraDtf2d
#define eraEe00	   astEraEe00
#define eraEe00a   astEraEe00a
#define eraEe00b   astEraEe00b
#define eraEect00  astEraEect00
#define eraEform   astEraEform
#define eraEo06a   astEraEo06a
#define eraEors	   astEraEors
#define eraEqeq94  astEraEqeq94
#define eraEra00   astEraEra00
#define eraFad03   astEraFad03
#define eraFae03   astEraFae03
#define eraFaf03   astEraFaf03
#define eraFaju03  astEraFaju03
#define eraFal03   astEraFal03
#define eraFalp03  astEraFalp03
#define eraFama03  astEraFama03
#define eraFame03  astEraFame03
#define eraFane03  astEraFane03
#define eraFaom03  astEraFaom03
#define eraFapa03  astEraFapa03
#define eraFasa03  astEraFasa03
#define eraFaur03  astEraFaur03
#define eraFave03  astEraFave03
#define eraFk52h   astEraFk52h
#define eraFk5hip  astEraFk5hip
#define eraFw2m	   astEraFw2m
#define eraFw2xy   astEraFw2xy
#define eraGc2gd   astEraGc2gd
#define eraGc2gde  astEraGc2gde
#define eraGd2gce  astEraGd2gce
#define eraGmst00  astEraGmst00
#define eraGmst82  astEraGmst82
#define eraGst00a  astEraGst00a
#define eraGst00b  astEraGst00b
#define eraGst06   astEraGst06
#define eraGst06a  astEraGst06a
#define eraGst94   astEraGst94
#define eraH2fk5   astEraH2fk5
#define eraJdcalf  astEraJdcalf
#define eraNum00a  astEraNum00a
#define eraNum00b  astEraNum00b
#define eraNum06a  astEraNum06a
#define eraNumat   astEraNumat
#define eraNut00a  astEraNut00a
#define eraNut00b  astEraNut00b
#define eraNut06a  astEraNut06a
#define eraNut80   astEraNut80
#define eraNutm80  astEraNutm80
#define eraObl80   astEraObl80
#define eraP2pv	   astEraP2pv
#define eraP2s	   astEraP2s
#define eraPb06	   astEraPb06
#define eraPfw06   astEraPfw06
#define eraPlan94  astEraPlan94
#define eraPm	   astEraPm
#define eraPmat00  astEraPmat00
#define eraPmat76  astEraPmat76
#define eraPmp	   astEraPmp
#define eraPn00	   astEraPn00
#define eraPn00a   astEraPn00a
#define eraPn00b   astEraPn00b
#define eraPn06	   astEraPn06
#define eraPn06a   astEraPn06a
#define eraPnm00a  astEraPnm00a
#define eraPnm00b  astEraPnm00b
#define eraPnm80   astEraPnm80
#define eraPom00   astEraPom00
#define eraPpp	   astEraPpp
#define eraPpsp	   astEraPpsp
#define eraPr00	   astEraPr00
#define eraPrec76  astEraPrec76
#define eraPv2p	   astEraPv2p
#define eraPv2s	   astEraPv2s
#define eraPvdpv   astEraPvdpv
#define eraPvm	   astEraPvm
#define eraPvmpv   astEraPvmpv
#define eraPvppv   astEraPvppv
#define eraPvstar  astEraPvstar
#define eraPvu	   astEraPvu
#define eraPvup	   astEraPvup
#define eraPvxpv   astEraPvxpv
#define eraRefco   astEraRefco
#define eraS00	   astEraS00
#define eraS00a	   astEraS00a
#define eraS00b	   astEraS00b
#define eraS06	   astEraS06
#define eraS06a	   astEraS06a
#define eraS2p	   astEraS2p
#define eraS2pv	   astEraS2pv
#define eraS2xpv   astEraS2xpv
#define eraSp00	   astEraSp00
#define eraStarpm  astEraStarpm
#define eraStarpv  astEraStarpv
#define eraSxp	   astEraSxp
#define eraSxpv	   astEraSxpv
#define eraTaitt   astEraTaitt
#define eraTaiut1  astEraTaiut1
#define eraTaiutc  astEraTaiutc
#define eraTcbtdb  astEraTcbtdb
#define eraTcgtt   astEraTcgtt
#define eraTdbtcb  astEraTdbtcb
#define eraTdbtt   astEraTdbtt
#define eraTrxpv   astEraTrxpv
#define eraTttai   astEraTttai
#define eraTttcg   astEraTttcg
#define eraTttdb   astEraTttdb
#define eraTtut1   astEraTtut1
#define eraUt1tai  astEraUt1tai
#define eraUt1tt   astEraUt1tt
#define eraUt1utc  astEraUt1utc
#define eraUtctai  astEraUtctai
#define eraUtcut1  astEraUtcut1
#define eraXy06	   astEraXy06
#define eraXys00a  astEraXys00a
#define eraXys00b  astEraXys00b
#define eraXys06a  astEraXys06a
#define eraZp	   astEraZp
#define eraZpv	   astEraZpv
#define eraZr      astEranZr

#endif

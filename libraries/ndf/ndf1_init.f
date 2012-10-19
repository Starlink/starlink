      BLOCK DATA NDF1_INIT
*+
*  Name:
*     NDF1_INIT

*  Purpose:
*     Initialise the NDF_ system common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     The routine initialises global variables stored in the NDF_ system
*     common blocks.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     AJC: A. J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     21-SEP-1989 (RFWS):
*        Added initialisation of NDF character component names which
*        are used, in effect, as an array of global constants.
*     26-SEP-1989 (RFWS):
*        Added Placeholder Control Block initialisation.
*     26-SEP-1989 (RFWS):
*        Added initialisation of current identifier context level.
*     29-SEP-1989 (RFWS):
*        Added initialisation of NDF character axis component names.
*     6-OCT-1989 (RFWS):
*        Added initialisation of PCB_PLCNT.
*     27-NOV-1989 (RFWS):
*        Added initialisation of TCB_ETFLG.
*     4-JUL-1990 (RFWS):
*        Changed name of DCB_CAC to DCB_ACCN.
*     29-NOV-1990 (RFWS):
*        Changed initialisation of the ACB_IDCNT and ACB_PLCNT counters
*        so that they start at the (unique) value NDF__FACNO. This is
*        to reduce the chance of the NDF_ system issuing
*        identifier/placeholder values which clash with other systems.
*     4-OCT-1991 (RFWS):
*        Added initialisation of TCB_WARN flag.
*     14-MAY-1993 (RFWS):
*        Added initialisation of DCB_HAPPN.
*     2-NOV-1993 (RFWS):
*        Added new TCB values to control foreign data format
*        conversion.
*     9-MAR-1994 (RFWS):
*        Removed all references to the TCB - initialisation is now
*        performed by NDF1_INTCB.
*     20-FEB-2003 (AJC):
*        Add initialisation of NDF_TMP
*     19-OCT-2012 (DSB):
*        Add initialisation of the ADAM Parameter Block. Note, we cannot
*        include AST_PAR in a block data module because it contains EXTERNAL
*        statements. So sadly we cannot initialise APB_PARS to AST__NULL.
*        Instead, use a literal value zero.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACCN( NDF__MXACN ) = CHARACTER * ( DAT__SZNAM ) (Write)
*           Names of NDF axis character components.
*        DCB_CCN( NDF__MXCCN ) = CHARACTER * ( DAT__SZNAM ) (Write)
*           Names of NDF character components.
*        DCB_HAPPN = CHARACTER * ( NDF__SZAPP ) (Write)
*           Name of the currently-executing application for purposes of
*           recording history information.
*        DCB_USED( NDF__MXDCB ) = LOGICAL (Write)
*           Whether a DCB slot has been used.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCNT = INTEGER (Write)
*           Count of NDF identifiers issued.
*        ACB_IDCTX = INTEGER (Write)
*           "Current" identifier context level.
*        ACB_USED( NDF__MXACB ) = LOGICAL (Write)
*           Whether an ACB slot has been used.

      INCLUDE 'NDF_PCB'          ! NDF_ Placeholder Control Block
*        PCB_PLCNT = INTEGER (Write)
*           Count of placeholder identifiers issued.
*        PCB_USED( NDF__MXPCB ) = LOGICAL (Write)
*           Whether a PCB slot has been used.

      INCLUDE 'NDF_TMP'          ! NDF_ temporary files
*        COUNT = INTEGER (Write)
*           Count of temporary NDFs
*        TMPLOC = CHARACTER*(DAT__SZLOC)
*           Locator for container of temporary NDFs

      INCLUDE 'NDF_APB'          ! NDF_ ADAM Parameter Block
*        APB_PARS = INTEGER (Read and Write)
*           An AST KeyMap holding a list of parameters with associated
*           NDFs.

*  Global Data:
      DATA ACB_IDCNT / NDF__FACNO /
      DATA ACB_IDCTX / 1 /
      DATA ACB_USED / NDF__MXACB * .FALSE. /

      DATA DCB_ACCN / 'LABEL', 'UNITS' /
      DATA DCB_CCN / 'LABEL', 'TITLE', 'UNITS' /
      DATA DCB_HAPPN / ' ' /
      DATA DCB_USED / NDF__MXDCB * .FALSE. /

      DATA PCB_PLCNT / NDF__FACNO /
      DATA PCB_USED / NDF__MXPCB * .FALSE. /

      DATA COUNT /0/
      DATA TMPLOC /DAT__NOLOC/

      DATA APB_PARS / 0 /
*.

      END

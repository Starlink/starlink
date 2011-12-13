************************************************************************

      SUBROUTINE AGI_1GWNAM ( INAME, PACKGE, AGINAM, STATUS )

*+
*  Name:
*     AGI_1GWNAM
*
*  Purpose:
*     Translate the given name into an AGI workstation name
*     Replacement routine for native PGPLOT version of AGI
*
*  Invocation:
*     CALL AGI_1GWNAM( INAME, PACKGE, AGINAM, STATUS )
*
*  Description:
*     This routine converts the workstation name given by INAME into
*     a name suitable for use by AGI. This version of the routine is
*     specific to native-PGPLOT and is used in the AGP library only.
*
*  Arguments:
*     INAME = CHARACTER*(*) (Given but may be modified)
*        Name of workstation to translate
*     PACKGE = CHARACTER*(*) (Given)
*        Name of package to search. Blank means search all
*     AGINAM = CHARACTER*(DAT__SZNAM) (Returned)
*        AGI name for the physical device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*
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
*     NE: Nick Eaton (Durham University)
*     BKM: Brian McIlwrath (Starlink,RAL)
*     DSB: David Berry (Starlink)
*
*  History:
*     Aug 1988 (NE):
*        Original version for SGS/IDI/Starlink PGPLOT
*     Dec 1999 (BKM);
*        Conversion for native PGPLOT only
*     18-Jan-2001 (BKM):
*        Correct to accept possible GNS abbreviations of GKS device names
*        (for example, 'xw' for 'xwindows')
*     18-Apr-2000 (BKM):
*        Correct incorrect rejection of native-PGPLOT device names and
*        confine AGI name matching to the /GWM device type.
*     2-NOV-2001 (DSB):
*        Re-written to use AGP1_TRANS to find the AGI name.
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'

*  Arguments Given :
      CHARACTER * ( * ) INAME
      CHARACTER * ( * ) PACKGE

*  Arguments Returned :
      CHARACTER * ( DAT__SZNAM ) AGINAM

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER LPACK*64
      CHARACTER PSPEC*1
*.

*  Initialize
      AGINAM = ' '

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Copy the package argument to a local variable and convert to upper case
      LPACK = PACKGE
      CALL CHR_LDBLK( LPACK )
      CALL CHR_UCASE( LPACK )

*   If the package is not PGP, abort.
      IF ( LPACK .NE. 'PGP' .AND. LPACK .NE. ' ' ) THEN
         STATUS = AGI__NAMNR
         CALL ERR_REP( 'AGI_1GWNAM_NMNR', 'AGI internal error - '//
     :                 'incorrect use of AGI1_GWNAM by AGP', STATUS )
      END IF

*  Attempt the translation.
      CALL AGP1_TRANS( ' ', INAME, PSPEC, AGINAM, STATUS )

      END

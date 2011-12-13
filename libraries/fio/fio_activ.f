      SUBROUTINE FIO_ACTIV( STATUS )
*+
*  Name:
*     FIO_ACTIV

*  Purpose:
*     Initialise FIO library for ADAM application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_ACTIV( STATUS )

*  Description:
*     The FIO package and parameter system is initialised for the start
*     of an executable image.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is not normally needed in a simple program
*        as FIO activates itself when necessary.

*  Algorithm:
*     -  The table of assigned files in the FIO I/O common block is
*        initialised.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     AJC: A Chipperfield  (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     03-Feb-1988 (AJC):
*        Originalversion. Adapted from MAG
*     12-FEB-1992 (PMA):
*        Converted to new ADAM prologue
*        Remove all references to the exit handler that was commented
*        out anyway.
*     23-FEB-1992 (PMA):
*        Add INCLUDE 'PAR_PAR' for use on Unix systems.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*      2-AUG-2002 (AJC):
*        Split FIOGO_CMN/FIOGOPA_CMN
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO Error codes
      INCLUDE 'FIOPAR_SYS'       ! FIOPAR Internal Constants

*  Global Variables:
      INCLUDE 'FIOPA_CMN'        ! FIO Parameter Table
*        PFREE( FIO__MXPAR ) = LOGICAL (Write)
*           Whether slot used
*        PDESC( FIO__MXPAR ) = INTEGER (Write)
*           File descriptor for parameter
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Write)
*           Parameter names
*        PACMOD( FIO__MXPAR ) = CHARACTER * ( PAR__SZMOD ) (Write)
*           Parameter access modes

      INCLUDE 'FIOGOPA_CMN'        ! FIO Initialisation Switch
*        FIOSLP = LOGICAL (Write)
*           Whether package is asleep

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIOPA_BLK           ! Block data subprogram

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start stand-alone FIO.
      CALL FIO_START( STATUS )

      DO I = 1, FIO__MXPAR
         PFREE( I ) = .TRUE.
         PDESC( I ) = 0
         PTNAME( I ) = ' '
         PACMOD( I ) = ' '
      ENDDO

*  Declare FIO awake
      FIOSLP = .FALSE.

      END

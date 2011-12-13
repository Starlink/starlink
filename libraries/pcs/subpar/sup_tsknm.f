      SUBROUTINE SUBPAR_TSKNM( TSKNAM, PFNAM, IFNAM, IFC, STATUS )
*+
*  Name:
*     SUBPAR_TSKNM

*  Purpose:
*     To obtain the full names of the interface file and parameter
*     file associated with the task.
*     This routine is expected to be machine dependent

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_TSKNM( TSKNAM, PFNAM, IFNAM, IFC, STATUS )

*  Description:
*     The routine finds the full name of the executable image being
*     run then constructs from it the parameter file name, using the
*     directory defined by the SUBPAR_ADMUS routine.
*     It then searches the ADAM_IFL path to find the first occurrence
*     of a .IFL or .IFC file with the same name.

*  Arguments:
*     TSKNAM = CHARACTER*(*) (Returned)
*        The name of the task (derived from the executable name)
*     PFNAM = CHARACTER*(*) (Returned)
*        The full name of the associated parameter file
*     IFNAM = CHARACTER*(*) (Returned)
*        The full name of the interface file
*     IFC = LOGICAL (Returned)
*        TRUE if the interface module is an IFC.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
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

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1991 (AJC):
*        Original version.
*     10-FEB-1992 (AJC):
*        Use SUBPAR_ADMUS.
*     12-FEB-1992 (AJC):
*        Correct access mode for interface files
*     05-MAR-1992 (AJC):
*        Improve interface file not found report
*     12-MAR-1995 (AJC):
*        Trap error from SUBPAR_ADMUS now it tries to create directory
*     27-JUL-2004 (TIMJ):
*        Recognize if we are running from libtool build directory.
*        Important for testing purposes so that A-task tests can run
*        from a libtool/autoconf build. Should be harmless unless an
*        A-task really does start with lt- !
*     11-AUG-2005 (TIMJ):
*        Not all fortran compilers have the ACCESS intrinsic and not
*        all compilers allow an intrinsic to be marked as EXTERNAL.
*        To remove these portability concerns, now call PSX_ACCESS
*        rather than the ACCESS intrinsic. A version was present in
*        pcs/misc/access.c but it was not being used on all compilers.
*     15-FEB-2006 (TIMJ):
*        Change calling scheme for PSX_ACCESS
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SUBPAR_ERR'       ! SUBPAR error values

*  Arguments Returned:
      CHARACTER*(*) TSKNAM
      CHARACTER*(*) PFNAM
      CHARACTER*(*) IFNAM
      LOGICAL IFC

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used lenth of string
      EXTERNAL STRING_IANYR
      INTEGER STRING_IANYR       ! Index search backwards

*  Local Variables:
      CHARACTER*(256) ARGV0      ! Argument 0 of command
      CHARACTER*(256) EXENAM     ! Full executable image name
      CHARACTER*(256) ADMUSR     ! ADAM_USER directory
      INTEGER EXELEN             ! Used length of EXENAM
      INTEGER AULEN              ! Length of ADAM_USER directory name
      INTEGER STNM               ! Start of file name in description
      INTEGER ENDNM              ! End of file name in description
      INTEGER IND                ! Index for FIFIL
      INTEGER REASON             ! Reason for ACCESS failure
      LOGICAL ACCIFL             ! IFL file accessible
      LOGICAL ACCIFC             ! IFC file accessible
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set error reportin level
      CALL EMS_MARK

*  Get the command name
      CALL GETARG (0, ARGV0)

*  If it is a full pathname - use it as is
      IF (INDEX (ARGV0, '/') .NE. 0) THEN
         EXENAM = ARGV0

*  Otherwise find the full name of the executable
      ELSE
         CALL SUBPAR_FIFIL ( 'PATH', ARGV0, ' ', 'x',
     :    EXENAM, IND, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL EMS_REP( 'SUP_TSKNM1',
     :      'Failed to find executable image name', STATUS )
         ENDIF

      ENDIF

      IF ( STATUS .EQ. SAI__OK ) THEN
*     Find the name part of the file description
         EXELEN = CHR_LEN( EXENAM )
         STNM = STRING_IANYR( EXENAM(1:EXELEN), '/' ) + 1
         ENDNM = STRING_IANYR( EXENAM(STNM:EXELEN), '.' ) - 1
*     Adjust ENDNM to absolute position, allowing for case of no '.' in
*     name.
         IF ( ENDNM .LE. 0 ) THEN
            ENDNM = EXELEN
         ELSE
            ENDNM = STNM + ENDNM
         ENDIF

*     Save the task name
         TSKNAM = EXENAM(STNM:ENDNM)

*     If we are running from a libtool build directory, this
*     file name may well be adorned with a leading 'lt-' string.
*     Assume that any task name beginning with 'lt-' falls in this
*     category and clean up the task name so that we can find
*     IFL files.
*     Note that the discovery that we are running as a libtool
*     executable should probably lead us to automatically searching
*     ../ for the ifc file...
         IF (TSKNAM(1:3) .EQ. 'lt-') THEN
            TSKNAM = TSKNAM(4:CHR_LEN(TSKNAM))
         ENDIF

*     Construct the parameter file name
*     First find the directory to be used
         CALL SUBPAR_ADMUS ( ADMUSR, AULEN, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            PFNAM = ADMUSR(1:AULEN) // TSKNAM

*        Find the interface module
*        First look along path ADAM_IFL for a .ifc or .ifl with read
*        access
            CALL SUBPAR_FIFIL ( 'ADAM_IFL', TSKNAM, '.ifc!.ifl', 'r',
     :       IFNAM, IND, STATUS )

*        If not found, look in same directory as executable
            IF ( STATUS .NE. SAI__OK ) THEN

*           First annul the error messages from _FIFIL
               CALL EMS_ANNUL ( STATUS )
               CALL PSX_ACCESS( EXENAM(1:ENDNM)//'.ifc',
     :              'R', ACCIFC, REASON, STATUS )

               IF ( ACCIFC ) THEN

*              An IFC is found
                  IFC = .TRUE.
                  IFNAM = EXENAM(1:ENDNM)//'.ifc'

               ELSE

                  CALL PSX_ACCESS( EXENAM(1:ENDNM)//'.ifl',
     :                 'R', ACCIFL, REASON, STATUS )

                  IF ( ACCIFL ) THEN

*     An IFL is found
                     IFC = .FALSE.
                     IFNAM = EXENAM(1:ENDNM)//'.ifl'

                  ELSE
*     No interface module found
                     STATUS = SUBPAR__IFNF
                     CALL EMS_SETC( 'TSKNAM', TSKNAM )
                     CALL EMS_REP( 'SUP_TSKNM1',
     :                    'Interface file for ^TSKNAM not found',
     :                    STATUS )

                  END IF
               ENDIF

            ELSE
*           Interface module was found along ADAM_IFL - set IFC appropriately
               IF ( IND .EQ. 1 ) THEN
                  IFC = .TRUE.
               ELSE
                  IFC = .FALSE.
               ENDIF

            ENDIF

         ENDIF

      ENDIF

      CALL EMS_RLSE

      END

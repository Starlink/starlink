      SUBROUTINE NDF2DIPSO (STATUS)
*+
*  Name:
*     NDF2DIPSO

*  Purpose:
*     Converts an NDF to a DIPSO-format file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2DIPSO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine converts a 1-dimensional NDF data file into a
*     DIPSO-format file.  The resultant file can be imported into DIPSO
*     by its READ command.  See SUN/50. The rules for the conversion
*     are listed in the Notes.

*  Usage:
*     NDF2DIPSO IN OUT

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure.  A file extension must not be given
*        after the name.  The suggested default is the current NDF if
*        one exists, otherwise it is the current value.
*     OUT = FILENAME (Write)
*        Output DIPSO file.  A default file extension of ".DAT" is
*        appended when parameter OUT contains no file extension.

*  Examples:
*     NDF2DIPSO OLD NEW
*        This converts the NDF called OLD (in file OLD.SDF) to the
*        DIPSO file called NEW.DAT.
*     NDF2DIPSO SPECTRE SPECTRE.DAT
*        This converts the NDF called SPECTRE (in file SPECTRE.SDF) to
*        the DIPSO file SPECTRE.DAT.

*  Notes:
*     -  The NDF TITLE object is to the DIPSO file.
*     -  The NDF data array becomes the main array in the DIPSO file.
*     Bad pixels found in the NDF result in `breaks' in the DIPSO file.
*     -  The axis centres becomes the x-axis array in the DIPSO file.
*     -  Most NDF components are not supported by the DIPSO format,
*     and therefore anything but the data array, axis centres, and
*     data title will not be copied.

*  Related Applications:
*     CONVERT: DIPSO2NDF; DIPSO: commands OREAD and OWRITE.

*  Implementation Status:
*     -  If the NDF data array exceeds the DIPSO limits of 28000
*     elements or 1000 breaks in the data, the application will abort
*     with an appropriate error message.
*     -  If the NDF does not have a title, "Data from NDF" is written
*     as the DIPSO file's title.
*     -  If the NDF does not have an axis, the application will abort
*     with an appropriate error message.
*     -  Only available on VMS platforms.

*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990 August 21st (JM):
*        Original version.
*     1990 November 15th (JM):
*        NDF map axis data call included.
*     1992 February 4 (MJC):
*        Renamed the parameters IN and OUT for consistency with other
*        applications and with the paper documentation.  Added Usage,
*        Implementation Status, and Examples items.  Improved the error
*        reports.
*     1992 September 22 (MJC):
*        Added Notes and a section on how to create the input file.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Used revised CON_DIPWR API.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'              ! Standard SAE constants
      INCLUDE 'DAT_PAR'              ! Data-system constants
      INCLUDE 'NDF_PAR'              ! NDF_ public constants
      INCLUDE 'CNF_PAR'              ! For CNF_PVAL function

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Constants:
      INTEGER   MAXBRK
      PARAMETER (MAXBRK = 1000)      ! Max. no. of breaks allowed in
                                     ! DIPSO data
      INTEGER   MAXSIZ
      PARAMETER (MAXSIZ = 28000)     ! Max. no. of data points allowed
                                     ! in DIPSO data

*  Local Variables:
      INTEGER   AXPTR                ! Pointer to axis data
      LOGICAL   AXSTAT               ! True if NDF has AXIS(1) component
      LOGICAL   BAD                  ! True if data array has bad pixels
      INTEGER   BREAK(MAXBRK)        ! Break array
      INTEGER   DATPTR               ! Pointer to NDF mapped array
      INTEGER   DIM(NDF__MXDIM)      ! Accommodates dimensions
      INTEGER   FD                   ! File descriptor
      CHARACTER FLXLOC*(DAT__SZLOC)  ! Locator to scratch space
      INTEGER   FLXPTR               ! Pointer to scratch space
      INTEGER   NDF                  ! Identifier for NDF
      INTEGER   NDIM                 ! No. of dimensions
      INTEGER   NPTS                 ! Number of mapped elements
      LOGICAL   STATE                ! Check for state of NDF components
      CHARACTER TITLE*80             ! Title
      INTEGER   UNIT                 ! Logical unit no. for Dipso file
      CHARACTER WAVLOC*(DAT__SZLOC)  ! Locator to scratch space
      INTEGER   WAVPTR               ! Pointer to scratch space

*.

*  Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the input NDF.
      CALL NDF_ASSOC ('IN', 'READ', NDF, STATUS)

*  Check the data array is one-dimensional.
      CALL NDF_DIM (NDF, NDF__MXDIM, DIM, NDIM, STATUS)
      IF (STATUS .NE. SAI__OK) GO TO 999
      IF (NDIM .NE. 1) THEN
         STATUS=SAI__ERROR
         CALL MSG_SETI ('NDIM', NDIM)
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP ('NDF2DIPSO_TOO_MANY_DIM',
     :     'NDF2DIPSO: Data array should be 1-dimensional. '/
     :     /'NDF ^NDF is ^NDIM-dimensional.', STATUS)
         GOTO 999
      END IF

*  Check the number of points does not exceed the DIPSO limit.
      IF (DIM(1) .GT. MAXSIZ) THEN
         STATUS=SAI__ERROR
         CALL MSG_SETI ('NPTS', DIM(1))
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_SETI ('MAXSIZ', MAXSIZ)
         CALL ERR_REP('NDF2DIPSO_TOO_MANY_ELEMENTS',
     :     'NDF2DIPSO: Number of elements (^NPTS) in NDF ^NDF exceeds '/
     :     /'DIPSO limit of ^MAXSIZ.', STATUS)
         GOTO 999
      END IF

*  Check the axis component exists.
      CALL NDF_ASTAT (NDF, 'Centre', 1, AXSTAT, STATUS)
      IF (.NOT.AXSTAT) THEN
         STATUS=SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP('NDF2DIPSO_NO_AXIS_DATA',
     :     'NDF2DIPSO: NDF ^NDF does not contain AXIS(1) structure.',
     :     STATUS)
         GOTO 999
      END IF

*  Map the input data array.
      CALL NDF_MAP (NDF, 'Data', '_REAL', 'READ', DATPTR, NPTS, STATUS)

*  Map the input axis array.
      CALL NDF_AMAP (NDF, 'Centre', 1, '_REAL', 'READ', AXPTR, NPTS,
     :               STATUS )

*  Get the title from NDF, if one is present.
      CALL NDF_STATE (NDF, 'TITLE', STATE, STATUS)
      IF (STATE) THEN
         CALL NDF_CGET (NDF, 'TITLE', TITLE, STATUS)
      ELSE
         TITLE = 'Data from NDF'
      END IF

*   Check to see if data array contains bad pixels.
      CALL NDF_BAD (NDF, 'DATA', .FALSE., BAD, STATUS)

      IF (STATUS .NE. SAI__OK) GOTO 999

*   Create a scratch area in which to put the new wavelength array.
      WAVLOC = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, WAVLOC, STATUS)
      CALL DAT_MAP(WAVLOC, '_REAL', 'WRITE', 1, DIM, WAVPTR, STATUS)

*   Create a scratch area in which to put the new flux array.
      FLXLOC = ' '
      CALL AIF_TEMP('_REAL', 1, DIM, FLXLOC, STATUS)
      CALL DAT_MAP(FLXLOC, '_REAL', 'WRITE', 1, DIM, FLXPTR, STATUS)

*   Report any errors getting workspace.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDFDIPSO_WORKSPACE.',
     :     'NDF2DIPSO: Error attempting to get workspace.', STATUS )
         GOTO 998
      END IF

*   Get name and logical unit number for the DIPSO file .
      CALL FIO_ASSOC ('OUT', 'WRITE', 'UNFORMATTED', 0, FD, STATUS)
      CALL FIO_UNIT (FD, UNIT, STATUS)

*   Call routine to write the data to a DIPSO file.
      CALL CON_DIPWR( UNIT, TITLE, NPTS, %VAL(CNF_PVAL(DATPTR)),
     :                %VAL(CNF_PVAL(AXPTR)),
     :                %VAL(CNF_PVAL(FLXPTR)), %VAL(CNF_PVAL(WAVPTR)),
     :                BAD, MAXBRK, BREAK, STATUS )

*   Cancel the ADAM parameter OUT and deactivate FIO.
      CALL FIO_CANCL ('OUT', STATUS)
      CALL FIO_DEACT (STATUS)

998   CONTINUE

*   Unmap and annul workspace for the scratch arrays.
      CALL AIF_ANTMP(WAVLOC, STATUS)
      CALL AIF_ANTMP(FLXLOC, STATUS)

999   CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)

      END

      SUBROUTINE COF_FTOPR(
     :   FUNIT, FILNAM, SAVEP, BLOCSZ, EXTN, STATUS )
*+
*  Name:
*     COF_FTOPR

*  Purpose:
*     Open a FITS file HDU for reading

*  Language:
*     Fortran 77

*  Invocation:
*     CALL COF_FTOPR( FUNIT, FILNAM, SAVEP, BLOCSZ, EXTN, STATUS )

*  Description:
*     Opens a FITS file on the given unit number and moves to any specified
*     header and data unit. The filename may have [extension-specifier]
*     appended to specify a particular extension in a multi-extension FITS
*     file.
*
*     Extension specifiers may take one of the following forms:
*     o An integer specifying the header and data unit (HDU) number.
*       0 is the primary HDU.
*     o keyword=value specifying a header keyword and value. The 'keyword='
*       part may be omitted, in which case EXTNAME is assumed.
*       Case is not significant in either 'keyword' or 'value'.
*
*     If and extension is specified or SAVEP is TRUE, the primary header
*     will be saved in dynamic memory to allow header inheritance.
*
*     If an extension is specified and found, EXTN is returned set to the
*     absolute HDU number.
*
*     If the routine fails, STATUS is set to SAI__ERROR and a suitable
*     message displayed. FITSIO message stack is cancelled often to avoid
*     confusion over the HDU number quoted.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        Logical unit number associated with a FITS file.
*     FILNAM = CHARACTER*(*) (Given)
*        Filename of the FITS file to be opened, optionally including
*        a FITS extension specifier.
*     SAVEP = LOGICAL (Given)
*        If the primary header is to be saved (EXTABLE or CONTAINER)
*     BLOCSZ = INTEGER (Returned)
*        FITS file blocksize or blocking factor
*     EXTN = INTEGER (Returned)
*        The FITS extension number found (0-n). -1 indicates no
*        extension was specified so the primary header unit was found.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Examples:
*     CALL COF_FTOPR( FUNIT, 'my_file.fit[1]', .FALSE., BLOCSZ, EXTN, STATUS )
*        Opens the FITS file 'my_file.fit' and moves to extension 1.
*        I.e. the one after the Primary Unit.
*        The primary headers are saved and
*        EXTN is returned = 1
*     [routine_example]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  References:
*     -  {reference}
*     [routine_references]...

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  External Routines Used:
*     CFITSIO
*        FTOPEN
*        FTMAHD
*     CHR
*        CHR_CTOI
*
*     {name_of_facility_or_package}:
*        {routine_used}...
*     [facility_or_package]...

*  Keywords:
*     {routine_keywords}...

*  Pitfalls:
*     -  HDU numbering from 0 for the Primary HDU is not the convention
*        used by the FITSIO library.
*     [pitfall_description]...

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     20-MAR-2000 (AJC):
*        Original version.
*     23-MAR-2000 (AJC):
*        Allow number or keyword value
*     10-MAY-2000 (AJC):
*        Use COF_CHKXT to check keyword=value specifiers
*     15-JUN-2000 (AJC):
*        Allow a null extension specifier
*     15-AUG-2000 (AJC):
*        Save primary header for inheritance
*        Added EXTABL argument
*     31-AUG-2000 (AJC):
*        Rename EXTABL to SAVEP
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER*(*) FILNAM
      LOGICAL SAVEP

*  Arguments Given and Returned:

*  Arguments Returned:
      INTEGER BLOCSZ
      INTEGER EXTN

*  Status:
      INTEGER STATUS            ! Global status

*  External References:

*  Local Constants:
      INTEGER   FITSOK           ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      INTEGER FLEN              ! Length of filename part of FILNAM
      INTEGER EXTLEN            ! Length of Extension in FILNAM
      INTEGER FSTAT             ! FITS status
      INTEGER HDUTYP            ! FITS HDU type
      LOGICAL LOOP              ! Loop control
      LOGICAL USE               ! If matched keywords
      CHARACTER*6 FTSUB         ! Current FITS subroutine name
      CHARACTER*(80) XTSPEC     ! Extension specifier
      CHARACTER*200 EBUFF      ! Buffer for Error messages

*  Local Data:

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the extension number and FITS status
      EXTN = -1
      FSTAT = 0

*  Get the parts of FSPEC
      CALL COF_EXTXT( FILNAM, FLEN, EXTLEN, STATUS )

*  and set XTSPEC to the specifier part
      IF( EXTLEN .GT. 2 ) THEN
         XTSPEC = FILNAM(FLEN+2:FLEN+EXTLEN-1)
      ELSE
         XTSPEC = ' '
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN
*  Open the FITS file with read access.
         CALL FTOPEN( FUNIT, FILNAM(1:FLEN), 0, BLOCSZ, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
         IF ( FSTAT .GT. FITSOK ) THEN
            FTSUB = 'FTOPEN'
            EBUFF = 'Error opening input FITS file ' //
     :              FILNAM(1:FLEN) // '.'

         ELSE IF ( ( XTSPEC .NE. ' ' ) .OR. SAVEP ) THEN
*  Read the primary header into memory if SAVEP is TRUE or a specific
*  extension was specifed
            CALL COF_SAVHD( FUNIT, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'FILE', FILNAM(:FLEN) )
               CALL ERR_REP( 'COF_FTOPR_SAV',
     :           'Error saving primary header of ^FILE', STATUS )

            ELSE IF ( XTSPEC .NE. ' ' ) THEN
*  An extension is specified - we are on the primary HDU
               EXTN = 0
*  Set error message buffer in case it's needed
               EBUFF = 'Error finding FITS extension '// FILNAM
*  Attempt to interpret the extension specifier as an extension number
               CALL CHR_CTOI( XTSPEC, EXTN, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
*  It is a number - attempt to move to that extension unless it's 0
                  IF ( EXTN .GT. 0 ) THEN
                     FTSUB = 'FTMAHD'
                     CALL FTMAHD( FUNIT, EXTN+1, HDUTYP, FSTAT )
                     CALL FTCMSG
                  END IF

               ELSE
*  Assume it's a 'keyword=value' form as it wasn't a number
*  Annul the error from CHR_CTOI
                  CALL ERR_ANNUL( STATUS )

*  Check each HDU in turn for the required keyword value(s)
                  LOOP = .TRUE.
                  DO WHILE ( LOOP .AND. ( STATUS .EQ. SAI__OK ) )
                     CALL COF_CHKXT( FUNIT, XTSPEC, USE, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        LOOP = .NOT. USE
                        IF ( LOOP ) THEN
*  Still trying - proceed to next HDU
                           EXTN = EXTN + 1
                           FTSUB = 'FTMRHD'
                           CALL FTMRHD( FUNIT, 1, HDUTYP, FSTAT )
                           IF ( FSTAT .GT. FITSOK ) THEN
                              STATUS = SAI__ERROR
                              LOOP = .FALSE.
                              CALL FTCMSG
                           END IF  ! failed moving to next HDU

                        END IF  ! loop continues

                     END IF  ! CHKXT OK

                  END DO  ! look in next HDU

               END IF  ! extension specifier is 'keyword=value' form

            END IF  ! Primary header saved OK

         END IF  ! file open and

      END IF ! FITS specifier parsed OK

      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_FTOPR_ERR', FTSUB,
     :     EBUFF, STATUS )
      END IF

      END

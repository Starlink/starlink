      SUBROUTINE SETBAD( STATUS )
*+
*  Name:
*     SETBAD

*  Purpose:
*     Sets new bad-pixel flag values for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETBAD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application sets new logical values for the bad-pixel flags
*     associated with an NDF's data and/or variance arrays.  It may
*     either be used to test whether bad pixels are actually present in
*     these arrays and to set their bad-pixel flags accordingly, or to
*     set explicit TRUE or FALSE values for these flags.

*  Usage:
*     setbad ndf [value]

*  ADAM Parameters:
*     DATA = _LOGICAL (Read)
*        This parameter controls whether the NDF's data array is
*        processed.  If a TRUE value is supplied (the default), then it
*        will be processed. Otherwise it will not be processed, so that
*        the variance array (if present) may be considered on its own.
*        The DATA and VARIANCE parameters should not both be set to
*        FALSE.  [TRUE]
*     MODIFY = _LOGICAL (Read)
*        If a TRUE value is supplied for this parameter (the default),
*        then the NDF's bad-pixel flags will be permanently modified if
*        necessary.  If a FALSE value is supplied, then no modifications
*        will be made.  This latter mode allows the routine to be used
*        to check for the presence of bad pixels without changing the
*        current state of an NDF's bad-pixel flags.  It also allows the
*        routine to be used on NDFs for which write access is not
*        available. [TRUE]
*     NDF = NDF (Read and Write)
*        The NDF in which bad pixels are to be checked for, and/or
*        whose bad-pixel flags are to be modified.  (Note that setting
*        the MODIFY parameter to FALSE makes it possible to check for
*        bad pixels without permanently modifying the NDF.)
*     VALUE = _LOGICAL (Read)
*        If a null (!) value is supplied for this parameter (the
*        default), then the routine will check to see whether any bad
*        pixels are present.  This will only involve testing the value
*        of each pixel if the bad-pixel flag value is initially TRUE,
*        in which case it will be reset to FALSE if no bad pixels are
*        found.  If the bad-pixel flag is initially FALSE, then it will
*        remain unchanged.
*
*        If a logical (TRUE or FALSE) value is supplied for this
*        parameter, then it indicates the new bad-pixel flag value
*        which is to be set.  Setting a TRUE value indicates to later
*        applications that there may be bad pixels present in the NDF,
*        for which checks must be made.  Conversely, setting a FALSE
*        value indicates that there are definitely no bad pixels
*        present, in which case later applications need not check for
*        them and should interpret the pixel values in the NDF
*        literally.
*
*        The VALUE parameter is not used (a null value is assumed) if
*        the MODIFY parameter is set to FALSE indicating that the NDF
*        is not to be permanently modified. [!]
*     VARIANCE = _LOGICAL (Read)
*        This parameter controls whether the NDF's variance array is
*        processed.  If a TRUE value is supplied (the default), then it
*        will be processed. Otherwise it will not be processed, so that
*        the data array may be considered on its own.  The DATA and
*        VARIANCE parameters should not both be set to FALSE.  [TRUE]

*  Examples:
*     setbad ngc1097
*        Checks the data and variance arrays (if present) in the NDF
*        called ngc1097 for the presence of bad pixels.  If the initial
*        bad-pixel flag values indicate that bad pixels may be present,
*        but none are found, then the bad-pixel flags will be reset to
*        FALSE.  The action taken will be reported.
*     setbad ndf=ngc1368 nomodify
*        Performs the same checks as described above, this time on the
*        NDF called ngc1368.  The presence or absence of bad pixels is
*        reported, but the NDF is not modified.
*     setbad myfile nodata
*        Checks the variance array (if present) in the NDF called
*        myfile for the presence of bad pixels, and modifies its
*        bad-pixel flag accordingly.  Specifying "nodata" inhibits
*        processing of the data array, whose bad-pixel flag is left
*        unchanged.
*     setbad halpha false
*        Sets the bad-pixel flag for the NDF called halpha to FALSE.
*        Any pixel values which might previously have been regarded as
*        bad will subsequently be interpreted literally as valid
*        pixels.
*     setbad hbeta true
*        Sets the bad-pixel flags for the NDF called hbeta to be TRUE.
*        If any pixels have the special "bad" value, then they will
*        subsequently be regarded as invalid pixels.  Note that if this
*        is followed by a further command such as "setbad hbeta", then
*        an actual check will be made to see whether any pixels have
*        this special value.  The bad-pixel flags will be returned to
*        FALSE if they do not.

*  Bad-Pixel Flag Values:
*     If a bad-pixel flag is TRUE, it indicates that the associated NDF
*     array may contain the special "bad" value and that affected
*     pixels are to be regarded as invalid.  Subsequent applications
*     will need to check for such pixels and, if found, take account of
*     them.
*
*     Conversely, if a bad-pixel flag value is FALSE, it indicates that
*     there are no bad pixels present.  In this case, any special "bad"
*     values appearing in the array are to be interpreted literally as
*     valid pixel values.

*  Related Applications:
*     KAPPA: NOMAGIC, SETMAGIC.

*  Quality Components:
*     Bad pixels may also be introduced into an NDF's data and variance
*     arrays implicitly through the presence of an associated NDF
*     quality component.  This application will not take account of such
*     a component, nor will it modify it.
*
*     However, if either of the NDF's data or variance arrays do not
*     contain any bad pixels themselves, a check will be made to see
*     whether a quality component is present.  If it is (and its
*     associated bad-bits mask is non-zero), then a warning message
*     will be issued indicating that bad pixels may be introduced via
*     this quality component.  If required, these bad pixels may be
*     eliminated either by setting the bad-bits mask to zero or by
*     erasing the quality component.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1991 (RFWS):
*        Original version.
*     26-MAR-1991 (RFWS):
*        Added the DATA and VARIANCE parameters.
*     10-APR-1991 (RFWS):
*        Improved the prologue.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      BYTE BB                    ! Quality bad-bits mask
      CHARACTER * ( 8 ) COMP( 2 ) ! List of NDF array components
      INTEGER ICOMP              ! Loop counter for NDF components
      INTEGER NCOMP              ! Number of NDF components to process
      INTEGER NDF                ! NDF identifier
      LOGICAL BAD                ! Bad pixels actually present?
      LOGICAL BAD0               ! Bad pixels may be present?
      LOGICAL CHANGE             ! Bad-pixel flag to be changed?
      LOGICAL CHECK              ! Need to check for bad pixels?
      LOGICAL DATA               ! Process the NDF's data array?
      LOGICAL ISFREE             ! Component free of bad pixels?
      LOGICAL MODIFY             ! NDF may be modified?
      LOGICAL THERE              ! Whether a component is defined
      LOGICAL VALUE              ! New bad-pixel flag value
      LOGICAL VAR                ! Process the NDF's variance array?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if the NDF's bad-pixel flags are to be permanently modified.
      CALL PAR_GET0L( 'MODIFY', MODIFY, STATUS )

*  Obtain the NDF with the appropriate access mode.
      IF ( MODIFY ) THEN
         CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )
      ELSE
         CALL LPG_ASSOC( 'NDF', 'READ', NDF, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If the NDF is modifiable, then determine if its bad-pixel flags are
*  to be set to an explicit value (as opposed to actually checking for
*  the presence of bad pixels).
      CHECK = .NOT. MODIFY
      IF ( MODIFY ) THEN
         CALL ERR_MARK
         CALL PAR_GET0L( 'VALUE', VALUE, STATUS )

*  Interpret a null response as indicating that a check for bad pixels
*  is required.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CHECK = .TRUE.
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Determine if the NDF's data array is to be processed. If so, then add
*  its name to the list of components to process.
      NCOMP = 0
      CALL PAR_GET0L( 'DATA', DATA, STATUS )
      IF ( DATA ) THEN
         NCOMP = NCOMP + 1
         COMP( NCOMP ) = 'data'
      END IF

*  Similarly, determine if the NDF's variance array is to be processed
*  and add its name to the list.
      CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )
      IF ( VAR ) THEN
         NCOMP = NCOMP + 1
         COMP( NCOMP ) = 'variance'
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Check that there is at least one potential NDF component to process
*  and report an error if there is not.
      IF ( NCOMP .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SETBAD_NOCOMP',
     :                 'The %DATA and %VARIANCE parameters should ' //
     :                 'not both be set to FALSE.',
     :                 STATUS )
         GO TO 99
      END IF

*  Initialise a flag indicating if either array component is free of bad
*  pixels.
      ISFREE = .FALSE.

*  Check for bad pixels:
*  ====================
*  If the NDF's array values are to be checked for bad pixels, then
*  display a heading, giving the NDF name.
      CALL MSG_BLANK ( STATUS )
      IF ( CHECK ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_OUT( 'CHECKING',
     :    '   Checking for bad pixels in ^NDF...', STATUS )

*  Disable automatic quality masking.
         CALL NDF_SQMF( .FALSE., NDF, STATUS )

*  Loop to process the data and/or variance components in turn, first
*  checking if each component is defined.
         DO 1 ICOMP = 1, NCOMP
            CALL NDF_STATE( NDF, COMP( ICOMP ), THERE, STATUS )

*  If so, then obtain the un-checked bad-pixel flag value.
            IF ( THERE ) THEN
               CALL NDF_BAD( NDF, COMP( ICOMP ), .FALSE., BAD0, STATUS )

*  Now see if there are actually any bad pixels present, this time
*  performing an explicit check if necessary. Note if any component is
*  free of bad pixels.
               CALL NDF_BAD( NDF, COMP( ICOMP ), .TRUE., BAD, STATUS )
               ISFREE = ISFREE .OR. ( .NOT. BAD )

*  If required, change the bad-pixel flag accordingly.
               CHANGE = MODIFY .AND. ( BAD .NEQV. BAD0 )
               IF ( CHANGE ) CALL NDF_SBAD( BAD, NDF, COMP( ICOMP ),
     :                                      STATUS )

*  Issue a message saying what has happened.
               CALL MSG_BLANK( STATUS )
               CALL MSG_SETC( 'COMP', COMP( ICOMP ) )
               IF ( BAD ) THEN
                  CALL MSG_OUT( 'ISBAD',
     :             '      Bad pixels found in the ^COMP array', STATUS )
               ELSE
                  CALL MSG_OUT( 'NOTBAD',
     :             '      No bad pixels found in the ^COMP array',
     :                          STATUS )
               END IF

*  Note if the bad pixel flag has been changed.
               IF ( CHANGE ) CALL MSG_OUT( 'CHANGED',
     :                       '            ...bad-pixel flag updated',
     :                                     STATUS )
            END IF
 1       CONTINUE

*  Set an explicit bad-pixel flag value:
*  ====================================
*  If the bad-pixel flagss are to be set to explicit values, then
*  display a heading, giving the NDF name. Note if the NDF will be free
*  of bad pixels.
      ELSE
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_OUT( 'SETTING',
     :    '   Setting bad-pixel flags for ^NDF...', STATUS )
         ISFREE = .NOT. VALUE

*  Loop to process the data and/or variance components in turn, first
*  checking if each component is defined.
         DO 2 ICOMP = 1, NCOMP
            CALL NDF_STATE( NDF, COMP( ICOMP ), THERE, STATUS )

*  If so, then set its bad-pixel flag.
            IF ( THERE ) THEN
               CALL NDF_SBAD( VALUE, NDF, COMP( ICOMP ), STATUS )

*  Output a message confirming the new value.
               CALL MSG_BLANK( STATUS )
               CALL MSG_SETC( 'COMP', COMP( ICOMP ) )
               CALL MSG_SETL( 'VALUE', VALUE )
               CALL MSG_OUT( 'SET',
     :         '      Bad-pixel flag for the ^COMP array set to ' //
     :         '^VALUE.', STATUS )

            END IF
 2       CONTINUE
      END IF
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  If any of the NDF's array components will be free of bad pixels as a
*  result of this routine's action, then check to see if a quality
*  component is present and whether its bad-bits mask is zero.
      IF ( ISFREE ) THEN
         CALL NDF_STATE( NDF, 'Quality', THERE, STATUS )
         CALL NDF_BB( NDF, BB, STATUS )

*  If the quality component may still introduce bad pixels, then issue a
*  warning to that effect.
         IF ( THERE .AND. ( BB .NE. ZEROUB ) ) THEN
            CALL MSG_OUT( 'WARNING',
     :      '   Warning: bad pixels may still be introduced ' //
     :      'via the quality component.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF
      END IF

*  Arrive here if an error occurs.
 99   CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETBAD_ERR',
     :    'SETBAD: Error setting new bad-pixel flag values for an NDF.',
     :    STATUS )
      END IF

      END

      SUBROUTINE NDF1_MAP( IACB, COMP, TYPE, CMPLX, MMOD, RPNTR, IPNTR,
     :                     STATUS )
*+
*  Name:
*     NDF1_MAP

*  Purpose:
*     Map an array component of an NDF specified by its ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_MAP( IACB, COMP, TYPE, CMPLX, MMOD, RPNTR, IPNTR,
*     STATUS )

*  Description:
*     The routine obtains mapped access to an array component of an NDF
*     specified by its ACB entry. A comma-separated list of component
*     names may also be supplied, in which case an array of pointers to
*     the components is returned (in the order specified by the list).

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component name(s); 'DATA', 'QUALITY' or
*        'VARIANCE' (or 'ERROR') (case insensitive).
*     TYPE = CHARACTER * ( * ) (Given)
*        Numeric data type to be used to access the data; an HDS
*        primitive numeric data type string (case insensitive).
*     CMPLX = LOGICAL (Given)
*        Whether access to complex data is required.
*     MMOD = CHARACTER * ( * ) (Given)
*        Mapping mode to be used to access the data (case insensitive).
*     RPNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped non-imaginary array component(s).
*     IPNTR( * ) = INTEGER (Returned)
*        Pointer(s) to the mapped imaginary array component(s) (not
*        used if CMPLX is .FALSE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check the mapping mode specification for validity.
*     -  Check that the requested mode of NDF access is available.
*     -  Initialise the component count.
*     -  See if the data and variance arrays, if mapped, may need to be
*     masked with the quality information. Masking may be necessary if
*     the quality masking flag is set and the access mode is READ or
*     UPDATE.
*     -  If masking may be necessary, then obtain the effective
*     bad-bits value for the quality component. If it is zero, then no
*     "bad" pixels can be generated, so masking is not necessary.
*     -  If masking may still be required, then see if a quality array
*     exists.  Masking cannot be performed if it does not.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each component from the list.
*     -  Find the final character of the next element in the list.
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn, taking the
*     appropriate action or reporting an error message if an
*     inappropriate component name has been given.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process it.
*     -  If no error has occurred, but the number of components
*     processed is zero, then report an error.
*     -  If no error has occurred, then see whether automatic quality
*     masking should be used to introduce "bad" pixels into the mapped
*     data and/or variance arrays.  Masking is not necessary unless the
*     data or variance arrays have been mapped.
*     -  If masking is required, then map the quality array for read
*     access. If the mapping operation fails, then add context
*     information to the error report.
*     -  If the data array was mapped, then put pointer(s) to its real
*     (and imaginary) component(s) into the start of the PTR array.
*     -  Similarly, add pointer(s) to the mapped variance array
*     component(s) to the end of the PTR array. The value of I then
*     records the number of mapped arrays to which quality masking must
*     be applied and PTR(1) to PTR(I) hold pointers to these arrays.
*     -  Apply the quality mask to the mapped data and/or variance
*     arrays.
*     -  Unmap the quality array.
*     -  If bad pixels have been introduced into the mapped data values,
*     then ensure that the bad pixel flag for these values is set to
*     .TRUE. and note it has been modified.
*     -  Similarly set the bad pixel flag for the mapped variance
*     values, if appropriate.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-OCT-1989 (RFWS):
*        Original version.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     19-DEC-1989 (RFWS):
*        Fixed error in argument list of NDF1_DMAP.
*     11-JAN-1990 (RFWS):
*        Changed error code.
*     15-JAN-1990 (RFWS):
*        Added support for comma-separated component lists, returning
*        arrays of pointers to the mapped data.
*     17-JAN-1990 (RFWS):
*        Added explicit error messages for invalid component names.
*     31-JAN-1990 (RFWS):
*        Added support for mapping of quality values.
*     7-FEB-1990 (RFWS):
*        Installed automatic quality masking of data and variance
*        arrays.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     16-MAR-1990 (RFWS):
*        Changed to reset the quality masking flag immediately
*        following the successful mapping of the quality component.
*     21-MAR-1990 (RFWS):
*        Changed handling of the bad pixel flags for mapped values to
*        take account of whether they have been modified.
*     3-APR-1990 (RFWS):
*        Changed to check if quality masking is required at the start
*        of the routine and to pass a flag to the data and quality
*        mapping routines to ensure that a modifiable version of the
*        mapped values is obtained if necessary.
*     16-NOV-1990 (RFWS):
*        Allow 'ERRORS' as a mis-spelling of 'ERROR'.
*     3-NOV-2010 (DSB):
*        Re-report ARY__CMPAC errors in a more NDF friendly way.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF_MXACB ) = LOGICAL (Write)
*           Whether the ACB_DMBAD value has been modified.
*        ACB_IDCB( NDF_MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_QMF( NDF__MXACB ) = LOGICAL (Read and Write)
*           Quality masking flag.
*        ACB_VMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped variance values.
*        ACB_VMBMD( NDF_MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX
      CHARACTER * ( * ) MMOD

*  Arguments Returned:
      INTEGER RPNTR( * )
      INTEGER IPNTR( * )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Constants:
      BYTE ZEROUB                ! Zero as an unsigned byte value
      PARAMETER ( ZEROUB = 0 )

*  Local Variables:
      BYTE BADBIT                ! Unsigned byte bad-bits mask value
      CHARACTER * ( NDF__SZIOP ) INOPT ! Initialisation option
      CHARACTER * ( NDF__SZMOD ) MODE ! Access mode
      INTEGER EL                 ! Number of elements to quality mask
      INTEGER F                  ! Position of first non-blank character
      INTEGER I                  ! Number of arrays for quality masking
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IDPTR              ! Index to data pointer in R/IPNTR
      INTEGER IVPTR              ! Index to variance pointer in R/IPNTR
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      INTEGER PTR( 4 )           ! Pointers to arrays to quality mask
      INTEGER QPNTR              ! Pointer to mapped quality array
      LOGICAL BAD                ! Quality masking gives bad pixels?
      LOGICAL MASK               ! Whether to apply quality masking

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the mapping mode specification for validity.
      CALL NDF1_VMMD( MMOD, MODE, INOPT, STATUS )

*  Check that the requested mode of NDF access is available.
      CALL NDF1_CHMOD( IACB, MODE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the component count.
         NCOMP = 0

*  Initialise indices which locate the mapped data and variance array
*  pointers in the returned RPNTR and IPNTR arrays.
         IDPTR = 0
         IVPTR = 0

*  See if the data and variance arrays, if mapped, may need to be
*  masked with the quality information. Masking may be necessary if the
*  quality masking flag is set and the access mode is READ or UPDATE.
         MASK = ACB_QMF( IACB ) .AND.
     :          ( ( MODE .EQ. 'READ' ) .OR. ( MODE .EQ. 'UPDATE' ) )

*  If masking may be necessary, then obtain the effective bad-bits
*  value for the quality component. If it is zero, then no "bad" pixels
*  can be generated, so masking is not necessary.
         IF ( MASK ) THEN
            CALL NDF1_GTBB( IACB, BADBIT, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               MASK = BADBIT .NE. ZEROUB
            END IF
         END IF

*  If masking may still be required, then see if a quality array
*  exists.  Masking cannot be performed if it does not.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( MASK ) THEN
               CALL NDF1_QSTA( IACB, MASK, STATUS )
            END IF
         END IF
      END IF

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
      IF ( STATUS .EQ. SAI__OK ) THEN
         I1 = 1
1        CONTINUE                ! Start of "DO WHILE" loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the component list
*  (the last character before a comma or end of string).
            I2 = INDEX( COMP( I1 : ), ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( COMP )
            ELSE
               I2 = I2 + I1 - 2
            END IF
            IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
               CALL CHR_FANDL( COMP( I1 : I2 ), F, L )
               IF ( L .GE. F ) THEN
                  F = F + I1 - 1
                  L = L + I1 - 1

*  Increment the component count.
                  NCOMP = NCOMP + 1

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component:
*  ==============
*  Report an error, as this component cannot be mapped.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_AXI',
     :               'An AXIS component cannot be mapped (possible ' //
     :               'programming error).', STATUS )

*  DATA component:
*  ==============
*  Map the data array and note which elements of the returned pointer
*  arrays point at it.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_DMAP( IACB, TYPE, CMPLX, MMOD, MASK,
     :                               RPNTR( NCOMP ), IPNTR( NCOMP ),
     :                               STATUS )
                     IDPTR = NCOMP

*  ERROR:
*  =====
*  Map the variance array with conversion from variance values to
*  standard deviations. Note which elements of the returned pointer
*  arrays point at it.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'ERRORS',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_VMAP( IACB, TYPE, CMPLX, MMOD, .TRUE.,
     :                               MASK, RPNTR( NCOMP ),
     :                               IPNTR( NCOMP ), STATUS )
                     IVPTR = NCOMP

*  EXTENSION:
*  =========
*  Report an error, as an extension cannot be mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_EXT',
     :               'An EXTENSION cannot be mapped (possible ' //
     :               'programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, as this component cannot be mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_HIS',
     :               'A HISTORY component cannot be mapped ' //
     :               '(possible programming error).', STATUS )

*  LABEL component:
*  ===============
*  Report an error, as this component cannot be mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_LAB',
     :               'A LABEL component cannot be mapped (possible ' //
     :               'programming error).', STATUS )

*  QUALITY component:
*  ==================
*  Map the quality array and note it has been mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                                  NDF__MINAB ) ) THEN
                     IF ( CMPLX ) THEN
                        STATUS = NDF__QMPIN
                        CALL ERR_REP( 'NDF1_MAP_QMAP',
     :                  'The QUALITY component of an NDF cannot be ' //
     :                  'mapped as complex values (possible ' //
     :                  'programming error).', STATUS )
                     ELSE
                        CALL NDF1_QMAP( IACB, TYPE, MMOD,
     :                                  RPNTR( NCOMP ), STATUS )

*  If the quality array has been mapped successfully, then reset the
*  quality masking flag and note that masking is not required in this
*  routine.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           ACB_QMF( IACB ) = .FALSE.
                           MASK = .FALSE.
                        END IF
                     END IF

*  TITLE component:
*  ===============
*  Report an error, as this component cannot be mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_TIT',
     :               'A TITLE component cannot be mapped (possible ' //
     :               'programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, as this component cannot be mapped.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF1_MAP_UNI',
     :               'A UNITS component cannot be mapped (possible ' //
     :               'programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Map the variance array and note which elements of the returned
*  pointer arrays point at it.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_VMAP( IACB, TYPE, CMPLX, MMOD, .FALSE.,
     :                               MASK, RPNTR( NCOMP ),
     :                               IPNTR( NCOMP ), STATUS )
                     IVPTR = NCOMP

*  If the array component name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF1_MAP_COMP',
     :                             'Invalid array component name ' //
     :                             '''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF

*  If an error occurred because WRITE or UPDATE mode was requested for a
*  compressed array (which are read-only), re-report it in a more
*  user-friendly way.
                  CALL NDF1_CMPAC( ACB_IDCB( IACB ), COMP( F : L ),
     :                             STATUS )
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the component list and return to process the next element.
            I1 = I2 + 2
            GO TO 1
         END IF

*  If no error has occurred, but no non-blank component names have been
*  processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
            STATUS = NDF__NOCMP
            CALL ERR_REP( 'NDF1_MAP_NONE',
     :                    'No array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If no error has occurred, then see whether automatic quality masking
*  should be used to introduce "bad" pixels into the mapped data and/or
*  variance arrays.  Masking is not necessary unless the data or
*  variance arrays have been mapped.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MASK = MASK .AND.
     :          ( ( IDPTR .NE. 0 ) .OR. ( IVPTR .NE. 0 ) )

*  If masking is required, then map the quality array for read access.
         IF ( MASK ) THEN
            CALL NDF1_QMAP( IACB, '_UBYTE', 'READ', QPNTR, STATUS )

*  If the mapping operation failed, then add context information to the
*  error report.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'NDF1_MAP_QMSK',
     :         'Unable to apply automatic quality masking to mapped ' //
     :         'NDF array values.', STATUS )

*  If the data array was mapped, then put pointer(s) to its real (and
*  imaginary) component(s) into the start of the PTR array.
            ELSE
               I = 0
               IF ( IDPTR .NE. 0 ) THEN
                  I = I + 1
                  PTR( I ) = RPNTR( IDPTR )
                  IF ( CMPLX ) THEN
                     I = I + 1
                     PTR( I ) = IPNTR( IDPTR )
                  END IF
               END IF

*  Similarly, add pointer(s) to the mapped variance array component(s)
*  to the end of the PTR array. The value of I then records the number
*  of mapped arrays to which quality masking must be applied and PTR(1)
*  to PTR(I) hold pointers to these arrays.
               IF ( IVPTR .NE. 0 ) THEN
                  I = I + 1
                  PTR( I ) = RPNTR( IVPTR )
                  IF ( CMPLX ) THEN
                     I = I + 1
                     PTR( I ) = IPNTR( IVPTR )
                  END IF
               END IF

*  Determine how many array elements there are to be masked from the
*  size of the NDF's data array. Then apply the quality mask to the
*  mapped data and/or variance arrays.
               CALL ARY_SIZE( ACB_DID( IACB ), EL, STATUS )
               CALL NDF1_QMA( EL, %VAL( CNF_PVAL( QPNTR ) ), BADBIT,
     :                        TYPE, I, PTR, BAD, STATUS )

*  If bad pixels were introduced into the mapped DATA component by the
*  quality masking process, then ensure that the bad pixel flag for the
*  mapped values is set to .TRUE. and note that it has been modified.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( BAD ) THEN
                     IF ( IDPTR .NE. 0 ) THEN
                        ACB_DMBAD( IACB ) = .TRUE.
                        ACB_DMBMD( IACB ) = .TRUE.
                     END IF

*  Similary set the bad pixel flag for the mapped variance values if
*  appropriate.
                     IF ( IVPTR .NE. 0 ) THEN
                        ACB_VMBAD( IACB ) = .TRUE.
                        ACB_VMBMD( IACB ) = .TRUE.
                     END IF
                  END IF
               END IF
            END IF

*  Unmap the quality array.
            CALL NDF1_QUMP( IACB, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_MAP', STATUS )

      END

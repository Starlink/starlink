*+ AXCONV - expands spaced axis arrays
      SUBROUTINE AXCONV( STATUS )
*+
*  Name:
*     AXCONV

*  Purpose:
*     Expands spaced axes in a file so that the file may subsequently
*     be used in applications built with the NDF routines (e.g. KAPPA)

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AXCONV( STATUS )

*  Description:
*     The routine expands 'spaced array' axes into 'simple' form.
*     The idea being that applications using the NDF routines aren't
*     currently capable of supporting spaced arrays.

*  ADAM Parameters:
*     INP = NDF (Update)
*        NDF to be modified.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     Richard Saxton   (LTVAD::RDS)

*  History:
*     22-JUN-1992
*        V1.6-1 Original version.
*      3 May 94 :
*        V1.7-0 Removed _FILL routine in favour of ASTLIB routine (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE                 ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'             ! Standard SAE constants
      INCLUDE 'DAT_PAR'             ! Standard SAE constants

*  Status:
      INTEGER STATUS                ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC    ! locator to the datafile
      CHARACTER*(DAT__SZLOC) DLOC   ! locator to the data array
      CHARACTER*(DAT__SZLOC) ALOC   ! locator to the axis structure
      CHARACTER*(DAT__SZLOC) CLOC   ! locator to an individual axis
      CHARACTER*(DAT__SZLOC) ADLOC  ! locator to the data_array in the axis
      CHARACTER*(DAT__SZLOC) DDLOC  ! locator to the data in the data_array
      CHARACTER*6 VARIANT           ! axis variant e.g. 'spaced', 'simple'

      LOGICAL VTHERE                ! Is the variant object present ?
      LOGICAL THERE                 ! Is the hds object present ?

      INTEGER NDIM                  ! Dimensionality of the data array
      INTEGER DIMS(DAT__MXDIM)      ! Dimensions of the data array
      INTEGER DPNTR                 ! Pointer to axis data array
      INTEGER LP

      REAL BASE                     ! value of the centre of the first pixel
      REAL SCALE                    ! width of each bin
*.
*  Version
      CHARACTER*20 VERSION
          PARAMETER (VERSION='AXCONV version 1.8-0')

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give version message
      CALL MSG_OUT('VERS', VERSION, STATUS)

*  Get name of NDF
      CALL USI_DASSOC('INP', 'UPDATE', LOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_OUT(' ', '** Error opening input file **', STATUS)
         GOTO 999
      ENDIF
*
*  Find the dimensionality of the data
      CALL DAT_FIND(LOC, 'DATA_ARRAY', DLOC, STATUS)
      CALL DAT_SHAPE(DLOC, DAT__MXDIM, DIMS, NDIM, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ', '** Error searching for data '/
     &               /'array **', STATUS)
         GOTO 999
      ENDIF
*
*  Annul the data array locator
      CALL DAT_ANNUL(DLOC, STATUS)
*
*  Get locator to the axis structure
      CALL DAT_FIND(LOC, 'AXIS', ALOC, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ', '** Error searching for axis '/
     &                                   /'structure **', STATUS)
         GOTO 999
      ENDIF
*
*  Loop over each axis and check if it is a spaced array
      DO LP=1,NDIM
*
*      Initialise values
         THERE = .FALSE.
         VARIANT = '      '
*
*      Get locator to this axis
         CALL DAT_CELL(ALOC, 1, LP, CLOC, STATUS)
*
*      Get locator to the axis data array
         CALL DAT_FIND(CLOC, 'DATA_ARRAY', ADLOC, STATUS)
*
*      Get the variant
         CALL DAT_THERE(ADLOC, 'VARIANT', VTHERE, STATUS)
*
*      Read the variant if available, otherwise look for a base
*      value, which is unique to the 'SPACED' array.
         IF (VTHERE) THEN
            CALL CMP_GET0C(ADLOC, 'VARIANT', VARIANT, STATUS)
         ELSE
            CALL DAT_THERE(ADLOC, 'BASE', THERE, STATUS)
*
            IF (THERE) VARIANT = 'SPACED'
         ENDIF
*
*      Check error status
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL MSG_SETI('AXNO', LP)
            CALL MSG_OUT(' ','** Error reading axis ^AXNO **', STATUS)
         ENDIF
*
*      Is it a 'spaced' array ?
         IF (INDEX(VARIANT, 'SPACED') .NE. 0) THEN
*
*         Get values for each part of the spaced array
            CALL CMP_GET0R(ADLOC, 'BASE', BASE, STATUS)
            CALL CMP_GET0R(ADLOC, 'SCALE', SCALE, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETI('AXNO', LP)
               CALL ERR_REP(' ','** Error reading base and scale '/
     &                           /'values from axis ^AXNO **', STATUS)
               GOTO 999
            ENDIF
*
*         Delete the base, scale and dimension objects
            CALL DAT_ERASE(ADLOC, 'BASE', STATUS)
            CALL DAT_ERASE(ADLOC, 'SCALE', STATUS)
*
            CALL DAT_THERE(ADLOC, 'DIMENSIONS', THERE, STATUS)
*
            IF (THERE) THEN
               CALL DAT_ERASE(ADLOC, 'DIMENSIONS', STATUS)
*
*         Asterix sometimes writes the wrong object
*         name in here, so check for this as well.
            ELSE
               CALL DAT_THERE(ADLOC, 'DIMENSION', THERE, STATUS)
*
               IF (THERE) CALL DAT_ERASE(ADLOC, 'DIMENSION', STATUS)
            ENDIF
*
*         Create a 'Simple' data array component
            CALL DAT_NEW1R(ADLOC, 'DATA', DIMS(LP), STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','** Error creating output axis '/
     &                     /'array **')
               GOTO 999
            ENDIF
*
*         Map the data array
            CALL DAT_FIND(ADLOC, 'DATA', DDLOC, STATUS)
            CALL DAT_MAPR(DDLOC, 'WRITE', 1, DIMS(LP), DPNTR, STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','** Error mapping output axis array **')
               GOTO 999
            ENDIF

*         Fill the data array
            CALL ARR_REG1R( BASE, SCALE, DIMS(LP), %VAL(DPNTR), STATUS )

*         Write the variant component
            IF (.NOT. VTHERE) THEN
               CALL DAT_NEW0C(ADLOC, 'VARIANT', 6, STATUS)
            ENDIF

            CALL CMP_PUT0C(ADLOC, 'VARIANT', 'SIMPLE', STATUS)

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','** Error writing VARIANT comp.**')
               GOTO 999
            ENDIF
*
            CALL MSG_SETI('AXNO', LP)
            CALL MSG_OUT(' ','Expanded axis ^AXNO',STATUS)
*
*         Unmap the data and annul locators
            CALL DAT_UNMAP(DDLOC, STATUS)
            CALL DAT_ANNUL(DDLOC, STATUS)
            CALL DAT_ANNUL(ADLOC, STATUS)
            CALL DAT_ANNUL(CLOC, STATUS)
*
         ELSE
            CALL MSG_SETI('AXNO', LP)
            CALL MSG_OUT(' ','NOT expanding axis ^AXNO',STATUS)
         ENDIF
*
      ENDDO
*
*  Annul the remaining locators
      CALL DAT_ANNUL(ALOC, STATUS)
      CALL DAT_ANNUL(LOC, STATUS)

*    If an error occurred, then report context information.
 999  CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END

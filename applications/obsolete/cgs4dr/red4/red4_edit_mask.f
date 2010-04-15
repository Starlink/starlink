*+  RED4_EDIT_MASK - Edit a bad pixel mask
      SUBROUTINE RED4_EDIT_MASK( STATUS )
*    Description :
*     This routine allows a bad pixel mask to be modified by
*     poking new values into it.
*    Invocation :
*     CALL RED4_EDIT_MASK( STATUS )
*    Parameters :
*     STATUS      = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     23-May-1990: Original version.                                (SMB)
*     24-May-1990: Bug fix. Mask name prefixed with "CGS4_MASKS:".  (SMB)
*      3-Sep-1990: Modified to make use of the FITS structure.      (SMB)
*      4-Sep-1990: Typing mistakes fixed.                           (SMB)
*     29-May-1991: MSG_SETI was being called twice for QVAL by
*                  mistake. Second call removed.                    (SMB)
*     18-Feb-1993: Conform to error strategy                        (PND)
*      9-Nov-1994: Attempt to make vaguely portable                 (AB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
      INCLUDE 'DSA_ERRORS'
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN            ! Character length determining function
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM             ! The maximum number of dimensions allowed
      PARAMETER ( MAXDIM = 3 )
*    Local variables :
      INTEGER
     :  NDIM,                    ! Number of dimensions in mask data array
     :  DIMS( MAXDIM ),          ! Dimensions of mask data array
     :  NELM,                    ! Total number of elements in mask data array
     :  ADDRESS,                 ! Address of mapped arrays
     :  MASK_SLOT,               ! Reference slot for mapped mask data
     :  MASK_PTR,                ! Address of mask data.
     :  IPOS,                    ! Column number to be poked.
     :  JPOS,                    ! Row number to be poked.
     :  QVAL,                    ! New data quality value
     :  OQVAL,                   ! Old data quality value
     :  CPOS,                    ! Position in character string
     :  CLEN                     ! Non-blank length of character string
      LOGICAL
     :  LOOP,                    ! TRUE if this action is to loop
     :  AGAIN                    ! Determines if another value is to be changed
      CHARACTER*80
     :  MASK,                    ! Name of mask to be edited
     :  BUFFER                   ! Character buffer
      CHARACTER*20 LPREFIX       ! Prefix to apply
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the name of the mask, and determine if this action
*   is to loop internally.
      CALL PAR_GET0C( 'MASK', MASK, STATUS )
      CLEN = CHR_LEN( MASK )
      CPOS = INDEX( MASK, 'CGS4_MASKS:')
      IF (CPOS .EQ. 0) CPOS = INDEX( MASK, 'CGS4_MASKS/')
      IF ( CPOS .EQ. 0 ) THEN
         CALL RED4_GET_PREFIX ('MASK', LPREFIX, STATUS)
         MASK  = LPREFIX(:CHR_LEN(LPREFIX)) // MASK(1:CLEN)
      END IF
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. ADAM__OK ) THEN

*      Open DSA
         CALL DSA_OPEN( STATUS )

*      Open the mask structure. (N.B. It is a peculiarity of DSA
*      that a structure can be opened for input and then modified).
         CALL DSA_NAMED_INPUT( 'MASK', MASK, STATUS )

*      Obtain the size of the data array in the mask structure.
         CALL DSA_DATA_SIZE( 'MASK', MAXDIM, NDIM, DIMS,
     :     NELM, STATUS )

*      Map the data array in the MASK structure as a byte array.
         CALL DSA_MAP_DATA( 'MASK', 'UPDATE', 'BYTE', ADDRESS,
     :     MASK_SLOT, STATUS )
         MASK_PTR = ADDRESS

*      Check this has worked.
         IF ( STATUS .EQ. ADAM__OK ) THEN

*         Loop until told to stop, or until an error occurs.
*         (N.B. This loop will only execute once if LOOP=FALSE).
            AGAIN = .TRUE.
            DO WHILE ( (AGAIN) .AND. (STATUS .EQ. ADAM__OK ) )

*            Obtain the column and row of the pixel to be modified
*            and the value to be poked.
               CALL PAR_GET0I( 'IPOS', IPOS, STATUS )
               CALL PAR_GET0I( 'JPOS', JPOS, STATUS )
               CALL PAR_GET0I( 'QVAL', QVAL, STATUS )

*            Check this has worked.
               IF ( STATUS .EQ. ADAM__OK ) THEN

*               Check that IPOS and JPOS are within the bounds
*               of the array.
                  IF ( (IPOS .GE. 1) .AND. (IPOS .LE. DIMS(1) ) .AND.
     :                 (JPOS .GE. 1) .AND. (JPOS .LE. DIMS(2) ) ) THEN

*                  Poke the required element with the required value
*                  and report this.
                     CALL RED4_QPOKE( DIMS(1), DIMS(2),
     :                 IPOS, JPOS, QVAL, %val(MASK_PTR),
     :                 OQVAL, STATUS )

                     CALL MSG_SETI( 'IPOS', IPOS )
                     CALL MSG_SETI( 'JPOS', JPOS )
                     CALL MSG_SETI( 'QVAL', QVAL )
                     CALL MSG_SETI( 'OQVAL', OQVAL )
                     CALL MSG_OUT( ' ', 'MASK(^IPOS,^JPOS): '/
     :                 /'^OQVAL replaced with ^QVAL', STATUS )

*                  Record this edit in the FITS structure of the mask.
                     CPOS = 0
                     CALL CHR_PUTC( 'MASK(', BUFFER, CPOS )
                     CALL CHR_PUTI( IPOS, BUFFER, CPOS )
                     CALL CHR_PUTC( ',', BUFFER, CPOS )
                     CALL CHR_PUTI( JPOS, BUFFER, CPOS )
                     CALL CHR_PUTC( '): ', BUFFER, CPOS )
                     CALL CHR_PUTI( OQVAL, BUFFER, CPOS )
                     CALL CHR_PUTC( ' replaced with ', BUFFER, CPOS )
                     CALL CHR_PUTI( QVAL, BUFFER, CPOS )
                     CALL DSA_PUT_FITS_C( 'MASK', 'COMMENT',
     :                 BUFFER(1:CPOS), ' ', STATUS )
                  ELSE

*                  The element number is outside the bounds of the array.
*                  Issue a warning but do not set a bad status (so that
*                  the loop is not terminated).
                     CALL MSG_OUT( ' ', 'WARNING - Specified '/
     :                 /'element outside array bounds - No '/
     :                 /'change made.', STATUS )
                  END IF

*               Determine if the loop is to be executed again.
                  IF ( LOOP ) THEN

*                  Cancel the AGAIN parameter, to ensure it is prompted
*                  for, and then prompt for it.
                     CALL PAR_CANCL( 'AGAIN', STATUS )
                     CALL PAR_GET0L( 'AGAIN', AGAIN, STATUS )

*                  Check this parameter has been obtained ok.
                     IF ( STATUS .EQ. ADAM__OK ) THEN

*                     If the loop is to be executed again, cancel
*                     the IPOS, JPOS and QVAL parameters, so they
*                     may be prompted for again.
                        IF ( AGAIN ) THEN

                           CALL PAR_CANCL( 'IPOS', STATUS )
                           CALL PAR_CANCL( 'JPOS', STATUS )
                           CALL PAR_CANCL( 'QVAL', STATUS )

*                        Report if something has gone wrong.
                           IF ( STATUS .NE. ADAM__OK ) THEN

                              STATUS = SAI__ERROR
                              CALL ERR_REP( ' ', 'RED4_EDIT_MASK: '/
     :                          /'Failed to cancel %IPOS, %JPOS and %QVAL '/
     :                          /'parameters', STATUS )
                           END IF
                        END IF
                     ELSE

                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'RED4_EDIT_MASK: '/
     :                    /'Failed to get %AGAIN '/
     :                    /'parameter', STATUS )
                     END IF
                  ELSE

*                  LOOP=.FALSE., so the loop is not to be executed again.
                     AGAIN = .FALSE.
                  END IF
               ELSE

                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'RED4_EDIT_MASK: '/
     :              /'Failed to get %IPOS and '/
     :              /'%JPOS parameters', STATUS )
               END IF

*         Loop again, provided AGAIN=.TRUE. and STATUS=ADAM__OK.
            END DO
         ELSE

             STATUS = SAI__ERROR
             CALL ERR_REP( ' ', 'RED4_EDIT_MASK: '/
     :        /'Failed to open and map data structures', STATUS )
         END IF

*      Close DSA. It will create the new mask structure and will
*      also unmap all the arrays mapped above.
*      (The routine will attempt to work even if the status on entry
*      is bad).
         CALL DSA_CLOSE( STATUS )
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'RED4_EDIT_MASK: '/
     :     /'Failed to get %MASK and %LOOP '/
     :     /'parameters', STATUS )
      END IF
      END

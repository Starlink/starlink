      SUBROUTINE DEFEXT( STATUS )
*+
*  Name:
*     SUBROUTINE DEFEXT

*  Purpose:
*     Read the general extraction parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DEFEXT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The general parameters that are used for "integral" extraction
*     techniques are read.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC)
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Local Variables:
      INTEGER ACTVAL        ! Parameter value count.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   GSAMP.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'GSAMP\\', .FALSE., 1, GSAMP, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'GSAMP\\', STATUS )
            GO TO 9999

         ELSE IF ( GSAMP .LE. 0.5 ) THEN
            CALL ERRPAR( 'GSAMP\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 100
         END IF

         CALL CNPAR( 'GSAMP\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'GSAMP\\', STATUS )
            GO TO 9999
         END IF
      END DO
 100  CONTINUE

*   CUTWV - whether cutoff wavelengths used to define extraction grid.
      CALL RDPARL( 'CUTWV\\', .FALSE., 1, CUTWV, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'CUTWV\\', STATUS )
         GO TO 9999
      END IF

*   CENTM - whether pre-existing centroid template is used.
      CALL RDPARL( 'CENTM\\', .FALSE., 1, CENTM, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'CENTM\\', STATUS )
         GO TO 9999
      END IF

*   CENSH - whether data just shift pre-existing template shape.
      CALL RDPARL( 'CENSH\\', .FALSE., 1, CENSH, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'CENSH\\', STATUS )
         GO TO 9999
      END IF

*   CENSV - whether data defined centroids update template.
      CALL RDPARL( 'CENSV\\', .FALSE., 1, CENSV, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'CENSV\\', STATUS )
         GO TO 9999
      END IF

*   CENIT.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'CENIT\\', .FALSE., 1, CENIT, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CENIT\\', STATUS )
            GO TO 9999

         ELSE IF ( CENIT.LT.0 .OR. CENIT.GT.10 ) THEN
            CALL ERRPAR( 'CENIT\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 200
         END IF

         CALL CNPAR( 'CENIT\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'CENIT\\', STATUS )
            GO TO 9999
         END IF
      END DO
 200  CONTINUE

*   CENAV.
      DO WHILE ( .TRUE. )
         CALL RDPARF('CENAV\\', .FALSE., 1, CENAV, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CENAV\\', STATUS )
            GO TO 9999

         ELSE IF ( CENAV .LE. 0.5 ) THEN
            CALL ERRPAR( 'CENAV\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 300
         END IF

         CALL CNPAR( 'CENAV\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'CENAV\\', STATUS )
            GO TO 9999
         END IF
      END DO
 300  CONTINUE

*   CENSD.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'CENSD\\', .FALSE., 1, CENSD, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CENSD\\', STATUS )
            GO TO 9999

         ELSE IF ( CENSD .LT. 1.0 ) THEN
            CALL ERRPAR( 'CENSD\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 400
         END IF

         CALL CNPAR( 'CENSD\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'CENSD\\', STATUS )
            GO TO 9999
         END IF
      END DO
 400  CONTINUE

*   BKGIT.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'BKGIT\\', .FALSE., 1, BKGIT, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'BKGIT\\', STATUS )
            GO TO 9999

         ELSE IF ( BKGIT.LT.0 .OR. BKGIT.GT.10 ) THEN
            CALL ERRPAR( 'BKGIT\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 500
         END IF

         CALL CNPAR( 'BKGIT\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'BKGIT\\', STATUS )
            GO TO 9999
         END IF
      END DO
 500  CONTINUE

*   BKGAV.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'BKGAV\\', .FALSE., 1, BKGAV, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'BKGAV\\', STATUS )
            GO TO 9999

         ELSE IF ( BKGAV .LE. 0.5 ) THEN
            CALL ERRPAR( 'BKGAV\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 600
         END IF

         CALL CNPAR( 'BKGAV\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'BKGAV\\', STATUS )
            GO TO 9999
         END IF
      END DO
 600  CONTINUE

*   BKGSD.
      IF ( BKGIT .GT. 0 ) THEN
         DO WHILE ( .TRUE. )
            CALL RDPARF( 'BKGSD\\', .FALSE., 1, BKGSD, ACTVAL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'BKGSD\\', STATUS )
               GO TO 9999

            ELSE IF ( BKGSD .LT. 1.0 ) THEN
               CALL ERRPAR( 'BKGSD\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               GO TO 700
            END IF

            CALL CNPAR( 'BKGSD\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'BKGSD\\', STATUS )
               GO TO 9999
            END IF
         END DO
      END IF
 700  CONTINUE

*   EXTENDED.
      CALL RDPARL( 'EXTENDED\\', .FALSE., 1, EXTND, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'EXTENDED\\', STATUS )
         GO TO 9999
      END IF

*   CONTINUUM.
      IF ( EXTND ) THEN
         CALL RDPARL( 'CONTINUUM\\', .FALSE., 1, CONTN, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CONTINUUM\\', STATUS )
            GO TO 9999
         END IF

      ELSE
         CONTN = .TRUE.
      END IF

*   AUTOSLIT - whether slit is determined automatically.
      CALL RDPARL( 'AUTOSLIT\\', .FALSE., 1, AUSLIT, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'AUTOSLIT\\', STATUS )
         GO TO 9999
      END IF

*   Only read GSLIT, BDIST and BSLIT for AUTOSLIT=F.
      IF ( .NOT. AUSLIT ) THEN

*      GSLIT.
         DO WHILE ( .TRUE. )
            CALL RDPARF( 'GSLIT\\', .FALSE., 2, GSLIT, ACTVAL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'GSLIT\\', STATUS )
               GO TO 9999

            ELSE IF ( ACTVAL .EQ. 1 ) THEN
               IF ( ABS( GSLIT( 1 ) ) .LT. 0.1 ) THEN
                  CALL ERRPAR( 'GSLIT\\' )
                  CALL ERROUT( ': out of range\\', STATUS )

               ELSE
                  GSLIT( 1 ) = -ABS( GSLIT( 1 ) )
                  GSLIT( 2 ) = ABS( GSLIT( 1 ) )
                  GO TO 800
               END IF

            ELSE IF ( GSLIT( 2 ) - GSLIT( 1 ) .LT. 0.2 ) THEN
               CALL ERRPAR( 'GSLIT\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               GO TO 800
            END IF

            CALL CNPAR( 'GSLIT\\', STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PCANER( 'GSLIT\\', STATUS )
               GO TO 9999
            END IF
         END DO
 800     CONTINUE

*    BSLIT - only for LORES, or for HIRES with Extended and no Continuum.
         IF ( STR_SIMLR( 'LORES\\', RESOL ) .OR.
     :                  (EXTND .AND. .NOT.CONTN) ) THEN
            DO WHILE ( .TRUE. )
               CALL RDPARF( 'BSLIT\\', .FALSE., 2, BSLIT, ACTVAL,
     :                      STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'BSLIT\\', STATUS )
                  GO TO 9999

               ELSE IF ( ACTVAL .EQ. 1) THEN
                  IF ( ABS( BSLIT( 1 ) ) .LT. 0.5 ) THEN
                     CALL ERRPAR( 'BSLIT\\' )
                     CALL ERROUT( ': out of range\\', STATUS )

                  ELSE
                     BSLIT( 1 ) = ABS( BSLIT( 1 ) )
                     BSLIT( 2 ) = ABS( BSLIT( 1 ) )
                     GO TO 900
                  END IF

               ELSE IF ( ABS( BSLIT( 1 ) ).LT.0.5 .OR.
     :                   ABS( BSLIT( 2 ) ).LT.0.5 ) THEN
                  CALL ERRPAR( 'BSLIT\\' )
                  CALL ERROUT( ': out of range\\', STATUS )

               ELSE
                  BSLIT( 1 ) = ABS( BSLIT( 1 ) )
                  BSLIT( 2 ) = ABS( BSLIT( 2 ) )
                  GO TO 900
               END IF

               CALL CNPAR( 'BSLIT\\', STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PCANER( 'BSLIT\\', STATUS )
                  GO TO 9999
               END IF
            END DO

         ELSE
            BSLIT( 1 ) = 0.5
            BSLIT( 2 ) = 0.5
         END IF
 900     CONTINUE

*    BDIST - only for LORES, or for HIRES with Extended and no Continuum.
         IF ( STR_SIMLR( 'LORES\\', RESOL ) .OR.
     :                  (EXTND .AND. .NOT.CONTN) ) THEN
            DO WHILE ( .TRUE. )
               CALL RDPARF( 'BDIST\\', .FALSE., 2, BDIST, ACTVAL,
     :                      STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PARFER( 'BDIST\\', STATUS )
                  GO TO 9999

               ELSE IF ( ACTVAL .EQ. 1 ) THEN
                  IF ( ABS( BDIST( 1 ) ) .LT. 0.5 ) THEN
                     CALL ERRPAR( 'BDIST\\' )
                     CALL ERROUT( ': out of range\\', STATUS )

                  ELSE
                     BDIST( 1 ) = -ABS( BDIST( 1 ) )
                     BDIST( 2 ) = ABS( BDIST( 1 ) )
                     GO TO 1000
                  END IF

               ELSE IF ( ABS( BDIST( 1 ) ).LT.0.5 .OR.
     :                   ABS( BDIST( 2 ) ).LT.0.5 ) THEN
                  CALL ERRPAR( 'BDIST\\' )
                  CALL ERROUT( ': out of range\\', STATUS )

               ELSE
                  GO TO 1000
               END IF

               CALL CNPAR( 'BDIST\\', STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL PCANER( 'BDIST\\', STATUS )
                  GO TO 9999
               END IF
            END DO
         END IF
      END IF
 1000 CONTINUE

 9999 CONTINUE
      END

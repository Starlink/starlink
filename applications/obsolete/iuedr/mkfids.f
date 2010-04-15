      SUBROUTINE MKFIDS(NPRINT, NEW, NAXIS1, NAXIS2, DATA, QUAL, STATUS)

*+
*
*   Name:
*      SUBROUTINE MKFIDS
*
*   Description:
*      The stacked data quality is modified to account for fiducial
*      marks.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Since the data quality may have been previously through this
*      route, the IUEPIXRES data quality bit is set zero (first)
*      for ALL pixels in the subset.
*      The NEW parameter, if set TRUE, can save time by avoiding this
*      reset.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPRINT                     ! print level

      LOGICAL NEW                        ! whether dataquality is new

      INTEGER NAXIS1                     ! size of axis 1 (sample)
      INTEGER NAXIS2                     ! size of axis 2 (line)

*   Import-Export:
      INTEGER*2 DATA(NAXIS1, NAXIS2)     ! image

      BYTE QUAL(NAXIS1, NAXIS2)          ! quality

*   Export:
      INTEGER STATUS                     ! status return

*   CMDATA:
      INCLUDE 'CMDATA'

*   CMFIDS:
      INCLUDE 'CMFIDS'

*   Local variables:
      INTEGER DQ                         ! workable data quality value
      INTEGER IL                         ! loop index
      INTEGER ILF                        !
      INTEGER ILL                        !
      INTEGER IS                         ! loop index
      INTEGER ISF                        !
      INTEGER ISL                        !
      INTEGER IX                         ! loop index
      INTEGER IY                         ! loop index
      INTEGER N                          ! total number of pixels affected

*   Print message
      IF (NPRINT.GT.0) THEN

         CALL LINE_WCONT(
     :       '%p Changing Image Data Quality due to Fiducials.\\'
     :               )
         CALL PRTBUF( STATUS )

      END IF

*   Reset if not new
      IF (.NOT.NEW) THEN

         IF (NPRINT.GT.0) THEN

            CALL LINE_WCONT('%p Will reset data quality first.\\')
            CALL PRTBUF( STATUS )

         END IF

         DO 50 IL = LMIN, LMAX

            DO 20 IS = SMIN(IL), SMAX(IL)

               IF (QUAL(IS, IL).NE.0) THEN

                  CALL DQ_UTOI(QUAL(IS, IL), DQ)
                  CALL DQ_WRPK(0, 8, 1, DQ)
                  CALL DQ_ITOU(DQ, QUAL(IS, IL))

               END IF

 20         CONTINUE

 50      CONTINUE

      END IF

*   Reset data quality in whole image
      IF (NOFIDS) THEN

         IF (NPRINT.GT.0) THEN

            CALL LINE_WCONT(
     :         '%p No pixels marked as affected by fiducials.\\'
     :                  )
            CALL PRTBUF( STATUS )

         END IF

         RETURN

      END IF

      N = 0

      DO 200 IY = 1, NFIDY

         DO 100 IX = 1, NFIDX

            ILF = MAX(NINT(REAL(FIDL(IX, IY) - FIDHW)), LMIN)
            ILL = MIN(NINT(REAL(FIDL(IX, IY) + FIDHW)), LMAX)

            IF (ILF.LE.ILL) THEN

               DO 60 IL = ILF, ILL

                  ISF = MAX(NINT(REAL(FIDS(IX, IY) - FIDHW)), LMIN)
                  ISL = MIN(NINT(REAL(FIDS(IX, IY) + FIDHW)), LMAX)

                  IF (ISF.LE.ISL) THEN

                     DO 52 IS = ISF, ISL

                        N = N + 1
                        CALL DQ_UTOI(QUAL(IS, IL), DQ)
                        CALL DQ_WRPK(1, 8, 1, DQ)
                        CALL DQ_ITOU(DQ, QUAL(IS, IL))

 52                  CONTINUE

                  END IF

 60            CONTINUE

            END IF

 100     CONTINUE

 200  CONTINUE

*   Print statistics
      IF (NPRINT.GT.0) THEN

         CALL LINE_WRITI(
     :         '%p Total of %i pixels affected by fiducials.\\'
     :                   , N)
         CALL PRTBUF( STATUS )

      END IF

      END

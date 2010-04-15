C DEC/CMS REPLACEMENT HISTORY, Element RED4_NORMALISE_FIT.FOR
C *1     2-JUN-1991 14:45:56 AB "Insert .FORs"
C DEC/CMS REPLACEMENT HISTORY, Element RED4_NORMALISE_FIT.FOR
*+ RED4_NORMALISE_SMOOTH - Normalise 2-D spectrum by smoothing
      SUBROUTINE RED4_NORMALISE_SMOOTH( NPIX, NLINE, BOXSIZE, DATA,
     :  VARIANCE, QUALITY, SPECTRUM, SUM, SQUAL, SMOOTHED, SMQUAL,
     :  STATUS )
*    Description :
*     This normalises a 2-D spectrum (usually a flat-field) and removes
*     any large-scale changes with wavelength by applying the following
*     procedure :-
*
*     1. The 2-D spectrum is collapsed in Y to an average 1-D spectrum.
*     2. The spectrum is heavily smoothed using a moving box filter.
*     3. Each row of the 2-D spectrum is divided by the smoothed spectrum.
*     4. Errors are also divided by this spectrum.
*
*     The routine will also zero any area of the data which is assigned
*     a bad quality.
*
*     It is assumed that wavelength increases monotonically with column
*     number on the 2-D array.
*    Invocation :
*      CALL RED4_NORMALISE_SMOOTH( NPIX, NLINE, BOXSIZE, DATA,
*     :  VARIANCE, QUALITY, SPECTRUM, SUM, SQUAL, SMOOTHED, SMQUAL,
*    :   STATUS )
*    Parameters :
*     NPIX                = INTEGER( READ )
*           x-dimension of data
*     NLINE               = INTEGER( READ )
*           y-dimension of data
*     BOXSIZE             = INTEGER( READ )
*           Smooth box size. This should be an odd number in the
*           range 1 - NPIX/2.
*     DATA( NPIX, NLINE ) = REAL( UPDATE )
*           2-D (flat field) data to be normalised.
*           This array is overwritten in situ.
*     VARIANCE( NPIX, NLINE ) = REAL( UPDATE )
*           The variance of the 2-D data (to be processed
*           simultateously).
*     QUALITY( NPIX, NLINE )  = BYTE( UPDATE )
*           The quality array associated with the input data.
*           (0 is assumed to mean "good").
*     SPECTRUM( NPIX )    = REAL( WRITE )
*           Work area to hold extracted spectrum
*     SUM( NPIX )         = INTEGER( WRITE )
*           Work area to hold the number of items summed.
*     SQUAL( NPIX )       = BYTE( WRITE )
*           Work area to hold spectrum quality.
*     SMOOTHED( NPIX )    = REAL( WRITE )
*           Work area to hold smoothed spectrum
*     SMQUAL( NPIX )      = BYTE( WRITE )
*           Work area to hold smoothed spectrum quality.
*     STATUS              = INTEGER( UPDATE )
*           Global status
*    Method :
*    Deficiencies :
*     The routine may return an array with an average value
*     significantly greater than 1.0 if many if the data values
*     end up being below <fracmin> of the smoothed spectrum.
*    Bugs :
*    Authors :
*     S.M.Beard        (REVAD::SMB)
*     P.N.Daly         (JACH::PND)
*    History :
*     31-Jul-1991: Original version, converted from
*                  RED4_NORMALISE_FIT                    (SMB)
*      1-Aug-1991: Made to report of the BOXSIZE has
*                  been altered.                         (SMB)
*      2-Aug-1991: Typing mistake fixed.                 (SMB)
*      6-Aug-1991: Despite the quality checking, it was
*                  still possible to get a divide by zero
*                  if the actual data happened to be zero
*                  down several adjacent columns. Trap
*                  for divide by zero included.          (SMB)
*     22-Feb-1993: Conform to error strategy             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import:
      INTEGER
     :  NPIX,
     :  NLINE,
     :  BOXSIZE
*    Import-export:
      REAL
     :  DATA( NPIX, NLINE ),
     :  VARIANCE( NPIX, NLINE )
      BYTE
     :  QUALITY( NPIX, NLINE )
*    Export:
      REAL
     :  SPECTRUM( NPIX ),
     :  SMOOTHED( NPIX )
      INTEGER
     :  SUM( NPIX )
      BYTE
     :  SQUAL( NPIX ),
     :  SMQUAL( NPIX )
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
      REAL FRACMIN       ! Fraction of fit below which data are ignored.
      PARAMETER ( FRACMIN = 0.05 )
      BYTE GOOD          ! Good quality
      PARAMETER ( GOOD = 0 )
      BYTE BAD           ! Bad quality
      PARAMETER ( BAD = 1 )
*    Local variables :
      INTEGER
     :  I,               ! Loop counter
     :  J,               ! Loop counter
     :  OBOXSIZE         ! Old box size.
      REAL
     :  ERROR            ! Error value (derived from variance)
*    Global variables :
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialise the spectrum and sum arrays to zero and the
*   spectrum quality array to "bad".
      DO I = 1, NPIX

         SPECTRUM(I) = 0.0
         SUM(I) = 0
         SQUAL(I) = BAD
      END DO

*   Sum the columns of the 2-D data to obtain a 1-D spectrum,
*   ignoring bad quality data and remembering the number of
*   points summed in each column.
      DO J = 1, NLINE
         DO I = 1, NPIX

            IF ( QUALITY(I,J) .EQ. GOOD ) THEN

               SPECTRUM(I) = SPECTRUM(I) + DATA(I,J)
               SUM(I) = SUM(I) + 1
            END IF
         END DO
      END DO

*   Divide the spectrum by the number of lines summed to obtain an
*   average spectrum. (If no points have contributed, the value
*   of spectrum is left at zero and the quality is flagged as "bad").
      DO I = 1, NPIX

         IF ( SUM(I) .GT. 0 ) THEN
            SPECTRUM(I) = SPECTRUM(I) / REAL( SUM(I) )
            SQUAL(I) = GOOD
         END IF
      END DO

*   Remember the original box size.
      OBOXSIZE = BOXSIZE

*   Smooth the spectrum array with a moving box filter.
      CALL GEN_BSMOTHQ( NPIX, SPECTRUM, SQUAL, BOXSIZE, SMOOTHED,
     :  SMQUAL, .TRUE., .FALSE., 0.0,  STATUS )

*   Report if a different box size to the one requested has been used.
*   (The GEN routine may alter the box size of it is too large, or if
*   it is not an odd number).
      IF ( BOXSIZE .NE. OBOXSIZE ) THEN

         CALL MSG_SETI( 'OBOXSIZE', OBOXSIZE )
         CALL MSG_SETI( 'BOXSIZE', BOXSIZE )
         CALL MSG_OUT( ' ', '**** Original box size of ^OBOXSIZE '/
     :     /'was unsuitable - altered to ^BOXSIZE.', STATUS )
      END IF

*   Now divide every row of the input data by this smooth function
*   to obtain normalised data corrected for wavelength variations.
*   Process the variances to ensure they are scaled with the data.
*   Only "good" data values which are greater than a fraction <fracmin>
*   of the average value are used, the rest are set to zero and
*   their quality set to "bad"
      DO J = 1,NLINE
         DO I = 1,NPIX

            IF ( QUALITY(I,J) .EQ. GOOD ) THEN
               IF ( ( SMQUAL(I) .EQ. GOOD ) .AND.
     :              ( SMOOTHED(I) .NE. 0.0 ) .AND.
     :              ( DATA(I,J) .GE. (FRACMIN * SMOOTHED(I)) ) ) THEN

                  DATA(I,J) = DATA(I,J) / SMOOTHED(I)

                  IF ( VARIANCE(I,J) .GT. 0.0 ) THEN

                     ERROR = SQRT( VARIANCE(I,J) )
                     ERROR = ERROR / SMOOTHED(I)
                     VARIANCE(I,J) = ERROR * ERROR
                  ELSE

                     VARIANCE(I,J) = 0.0
                  END IF
               ELSE

                  DATA(I,J) = 0.0
                  VARIANCE(I,J) = 0.0
                  QUALITY(I,J) = BAD
               END IF
            ELSE

*            Set all the bad quality data to zero. This is done
*            because it is assumed the data being normalised is
*            a flat-field, and it will be easier to display the
*            flat-field when the bad areas are uniformly set to
*            zero. This is not compulsory.
               DATA(I,J) = 0.0
               VARIANCE(I,J) = 0.0
            END IF
         ENDDO
      ENDDO

      END

************************************************************************

      SUBROUTINE EXTR (XCEN, YCEN, DCEN, IMAGE, NX, NY, SEE,
     :                 CLIP, PADU, SATURE, SHAPE, OPTNRM,
     :                 COMPAN, XCOMP, YCOMP, FLUX, ERROR,
     :                 XFIT, YFIT, XERR, YERR, PEAK, BESTN,
     :                 SKY, SIGMA, VSKY, CODE, STATUS)

*+
*  Name :
*     EXTR
*
*  Purpose :
*     This subroutine calculated the flux optimally
*
*  Language :
*     FORTRAN
*
*  Invocation :
*
*      CALL EXTR (XCEN, YCEN, DCEN, IMAGE, NX, NY, SEE,
*     :           CLIP, PADU, SATURE, SHAPE, OPTNRM,
*     :           COMPAN, XCOMP, YCOMP, FLUX, ERROR,
*     :           XFIT, YFIT, XERR, YERR, PEAK, BESTN,
*     :           SKY, SIGMA, VSKY, CODE, STATUS)
*
*  Description :
*     {routine_description}...
*
*  Arguments :
*
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     TN: Tim Naylor (Keele University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     ??-???-1997 (TN)
*        Original code written by Tim in Fortran 90
*     6-JAN-1998 (AA)
*        Hack and slash to Fortran 77
*        Added NX and NY to calling arguments
*     11-JAN-1999 (AA)
*        Addded SKY, SIGMA and VSKY to calling arguments
*     20-JAN-1999
*        Added CODE to passed arguements
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given :

      INTEGER NX, NY
      LOGICAL COMPAN

      REAL XCEN, YCEN, DCEN, SEE,CLIP, PADU, SATURE,
     :     OPTNRM, XCOMP, YCOMP, SKY, SIGMA, VSKY

      REAL SHAPE(3)
      REAL IMAGE(NX, NY)

*  Arguments Returned :

      REAL  FLUX, ERROR, XFIT, YFIT, XERR, YERR,
     :      PEAK, BESTN
      CHARACTER * ( 2 ) CODE

*  Status :

      INTEGER STATUS

*  Local Variables :

      REAL APAR(6)
      REAL ECEN(2)

      INTEGER I


*.

*   Check status on entry - return if not o.k.
*      WRITE(*,*) ' DEBUG --- --- Entering EXTR()'

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Do initial check to see if the candidate PSF is on the frame

      IF( (XCEN .LT. 0.0) .OR. (REAL(XCEN) .GT. NX) .OR.
     :    (YCEN .LT. 0.0) .OR. (REAL(YCEN) .GT. NY) ) THEN
            STATUS  =  SAI__ERROR
            CALL ERR_REP( 'ERR_EXTR_NOSTAR',
     :        'EXTR: Star not on frame', STATUS )
      ENDIF

*   Set up the parameters for the fit

*   Right now APAR is defined as APAR(6) and is used this way
*   in FPEAK(), SUMFLX(), DOSUM() and GFIT(). Next step will be
*   to dynamincally allocate this so we can account for a companion
*   star as follows
*
*     IF(COMPAN)
*         APAR(7)=IMAGE(NINT(XCOMP),NIN(YCOMP)-SKY
*         APAR(8)=XCOMP
*         APAR(9)=YCOMP
*     ELSE
*
*   Right now I don't do that, instead we only do the following...

      DO I=1,3
          APAR(I)=SHAPE(I)
      END DO

*   Set the position

      APAR(5)=XCEN
      APAR(6)=YCEN

*   If the position is free set normalisation to 1, and hunt for
*   best counts

      IF(DCEN .GT. 0.0 ) THEN
          APAR(4)=1.0
*          WRITE(*,*) ' DEBUG --- --- calling FPEAK()'
	  CALL FPEAK(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR, PADU,
     :           NX, NY, CODE, STATUS)


*   Set normalisation to the peak counts.

          IF( (IMAGE(NINT(APAR(5)),NINT(APAR(6))) - SKY )
     :	      .GT. SQRT(VSKY) ) THEN

             APAR(4) = IMAGE(NINT(APAR(5)),NINT(APAR(6)))-SKY
          ELSE
             APAR(4) = SQRT(VSKY)
          ENDIF

*   Reset position
          APAR(5)=XCEN
	  APAR(6)=YCEN

      ELSE
          IF( (IMAGE(NINT(APAR(5)),NINT(APAR(6))) - SKY )
     :	      .GT. SQRT(VSKY) ) THEN

             APAR(4) = IMAGE(NINT(APAR(5)),NINT(APAR(6)))-SKY
          ELSE
             APAR(4) = SQRT(VSKY)
          ENDIF
      ENDIF

      IF(DCEN .GT. 0.0 ) THEN

*          WRITE(*,*) ' DEBUG --- --- 1st call to GFIT()'
          CALL GFIT(IMAGE, .TRUE., 0.0, .FALSE., SKY, VSKY,
     :              PADU, SATURE, APAR, ECEN, NX, NY, CODE, STATUS)
*          WRITE(*,*) ' DEBUG --- --- 2nd call to GFIT()'
          CALL GFIT(IMAGE, .TRUE., DCEN, .FALSE., SKY, VSKY,
     :              PADU, SATURE, APAR, ECEN, NX, NY, CODE, STATUS)

*   Update position

          XFIT = APAR(5)
	  YFIT = APAR(6)
	  XERR = ECEN(1)
	  YERR = ECEN(2)
      ELSE
          XFIT = XCEN
	  YFIT = YCEN
      ENDIF
      PEAK = APAR(4)

*   And here is the one we've all been waiting for, the very final
*   call to do the optimal photometry

      APAR(4) = OPTNRM
*      WRITE(*,*) ' DEBUG --- --- calling SUMFLX()'
      CALL SUMFLX(IMAGE, CLIP, SKY, SIGMA, VSKY, APAR,
     :            PADU, SATURE, NX, NY, ERROR, BESTN, FLUX,
     :            CODE, STATUS)


*   End of routine

  99  CONTINUE
*      WRITE(*,*) ' DEBUG --- --- Leaving EXTR()'

      END

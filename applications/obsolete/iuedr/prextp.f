      SUBROUTINE PREXTP( STATUS )
*+
*  Name:
*     SUBROUTINE PREXTP

*  Purpose:
*     Print extraction parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREXTP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     This does NOT check NOEXTP, since it only prints the subset
*     of parameters that are independent of specific APERTURE/ORDER.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Status:
      INTEGER STATUS        ! Global status.

*  External References:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMSIST'

*.

*   Sample rate.
      CALL LINE_WRITF( '%p Sample width %.2f pixels\\', GSAMP )
      CALL LINE_WRITF( ' (%.2f that of IUESIPS#1).\\', GSAMP / 1.414 )
      CALL PRTBUF( STATUS )

*   Background averaging.
      CALL LINE_WRITF( '%p Background folding FWHM %.1f pixels,\\',
     :                 BKGAV )
      IF ( BKGIT .GT. 0 ) THEN
         CALL LINE_WRITI( ' evaluated %i times,\\', BKGIT + 1 )

      ELSE
         CALL LINE_WCONT( ' evaluated once,\\' )
      END IF
      CALL PRTBUF( STATUS )

      IF ( BKGIT .GT. 0 ) THEN
         CALL LINE_WRITF(
     :   '%p with pixels outside %.1f s.d. rejected.\\', BKGSD )
         CALL PRTBUF( STATUS )

      ELSE
         CALL LINE_WCONT( '%p with no pixel rejection.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Templates.
      IF ( CENTM ) THEN
         CALL LINE_WCONT(
     :        '%p Will try to base templates on pre-existing\\' )
         CALL LINE_WCONT( ' shapes.\\' )

      ELSE
         CALL LINE_WCONT(
     :        '%p Will base initial templates on dispersion\\' )
         CALL LINE_WCONT( ' constants.\\' )
      END IF
      CALL PRTBUF( STATUS )

      IF ( CENSV ) THEN
         CALL LINE_WCONT( '%p Template will be saved in dataset.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Centroid tracking.
      CALL LINE_WRITF('%p Centroid folding FWHM %.1f pixels,\\', CENAV)
      IF ( CENIT .GT. 0 ) THEN
         CALL LINE_WRITI( ' evaluated %i times,\\', CENIT + 1 )

      ELSE
         CALL LINE_WCONT( ' evaluated once,\\' )
      END IF
      CALL PRTBUF( STATUS )

      IF ( CENIT .GT. 0 ) THEN
         CALL LINE_WRITF(
     :      '%p with signal above %.1f s.d. used for tracking.\\',
     :      CENSD )
         CALL PRTBUF( STATUS )

      ELSE
         CALL LINE_WCONT( '%p without auto-tracking.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Centroid shift.
      IF ( CENSH ) THEN
         CALL LINE_WCONT(
     :        '%p Centroids will produce linear shift of template.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   Extended and continuum switches (only significant for HIRES).
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         IF ( EXTND ) THEN
            CALL LINE_WCONT( '%p Spatially extended Object\\' )
            IF ( CONTN ) THEN
               CALL LINE_WCONT( ' with continuum.\\' )

            ELSE
               CALL LINE_WCONT( ' without continuum.\\' )
            END IF

         ELSE
            CALL LINE_WCONT( '%p Point source Object.\\' )
         END IF
         CALL PRTBUF( STATUS )
      END IF

*   Autoslit.
      IF ( AUSLIT ) THEN
         CALL LINE_WCONT( '%p Slit determined automatically.\\' )

      ELSE
         CALL LINE_WCONT(
     :        '%p Slit determined by command parameters.\\' )
      END IF
      CALL PRTBUF( STATUS )

      END

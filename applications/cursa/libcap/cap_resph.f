      SUBROUTINE CAP_RESPH (ROWS, CI, ENAME, CINCL, CCATMG, CINSMG,
     :  CAIRMS, INSCON, ZEROP, ATMOS, CALCMG, RESID, STATUS)
*+
*  Name:
*     CAP_RESPH
*  Purpose:
*     List the residuals from fitting instrumental magnitudes.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RESPH (ROWS, CI, ENAME, CINCL, CCATMG, CINSMG, CAIRMS,
*       INSCON, ZEROP, ATMOS; CALCMG, RESID; STATUS)
*  Description:
*     List the residuals from fitting instrumental magnitudes for a
*     set of standard stars.  The standard deviation of the residuals
*     is also computed and displayed.
*  Arguments:
*     ROWS  =  INTEGER (Given)
*        The number of rows (ie. standard stars) in the list.
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     ENAME(ROWS)  =  INTEGER (Given)
*        Identifier to the column to contain the star names.  If star
*        names are not required it is set to the null identifier.
*     CINCL(ROWS)  =  LOGICAL (Given)
*        Flags indicating for each row (ie. star) whether they are
*        included in the fit.  The coding is as follows:
*        .TRUE.  - included in fit,
*        .FALSE. - excluded from fit.
*     CCATMG(ROWS)  =  DOUBLE PRECISION (Given)
*        Catalogue magnitudes for the standard stars.
*     CINSMG(ROWS)  =  DOUBLE PRECISION (Given)
*        Instrumental magnitudes for the standard stars.
*     CAIRMS(ROWS)  =  DOUBLE PRECISION (Given)
*        Air mass for the standard stars.
*     INSCON  =  DOUBLE PRECISION (Given)
*        Arbitrary constant applied to the instrumental magnitudes.
*     ZEROP  =  DOUBLE PRECISION (Given)
*        Zero point of the transformation.
*     ATMOS  =  DOUBLE PRECISION (Given)
*        Atmospheric extinction of the transformation.
*     CALCMG(ROWS)  =  DOUBLE PRECISION (Work)
*        Work array used to hold the magnitudes calculated for the
*        standard stars.
*     RESID(ROWS)  =  DOUBLE PRECISION (Work)
*        Work array used to hold the residuals calculated for the
*        standard stars.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Compute the calculated magnitudes, residuals and find the largest
*     absolute residuals for the standard stars.  Also compute the
*     sum of the squares of the residuals.
*     Compute the standard deviation for (a) the fitted stars and (b)
*     all stars.
*     Write the title of the table.
*     For each star
*       If required then
*         Read the star name.
*       end if
*       Calculate the residual bar.
*       Assemble the output buffer.
*       Write out the buffer.
*     end for
*     Write out the standard deviations.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     3/10/97 (ACD): Original version.
*     6/10/97 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parameteric constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  ENAME,
     :  ROWS
      LOGICAL
     :  CINCL(ROWS)
      DOUBLE PRECISION
     :  CCATMG(ROWS),
     :  CINSMG(ROWS),
     :  CAIRMS(ROWS),
     :  INSCON,
     :  ZEROP,
     :  ATMOS
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  CALCMG(ROWS),
     :  RESID(ROWS)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Constants:
      INTEGER NFIT          ! Number of parameters to fit.
      PARAMETER (NFIT = 2)
*  Local Variables:
      INTEGER
     :  CURROW,     ! Current row.
     :  NUMFIT,     ! Number of stars fitted.
     :  LOOP,       ! Loop index.
     :  LSTAT,      ! Local Fortran I/O status.
     :  RESLEN,     ! Length of residual bar.
     :  BUFLEN      ! Length of BUFFER (excl. trail. blanks).
      DOUBLE PRECISION
     :  MAXRSD,     ! Absolute maximum residual.
     :  INCR,       ! Increment for residual bar.
     :  FSUMSQ,     ! Sum of square of residuals (fitted stars).
     :  ASUMSQ,     !  "  "    "    "     "      (all      "  ).
     :  FSTDV,      ! Standard deviation (fitted stars).
     :  ASTDV       !    "        "      (all      "  ).
      CHARACTER
     :  BUFFER*75,  ! Output buffer.
     :  FITSYM*1,   ! Symbol indicating whether star was fitted.
     :  AST*1,      ! Asterisk (symbol for residuals of fitted stars).
     :  DASH*1,     ! Dash     (  "     "     "      "  omitted  "  ).
     :  RESBAR*11,  ! Residual bar.
     :  FNAME*15,   ! Star name for the current row.
     :  STDBUF*10   ! Buffer for displaying the standard deviation.
      LOGICAL
     :  NNAME       ! Null value flag for the current star name.
*  Local Data:
      DATA AST/'*'/, DASH/'-'/
      SAVE AST, DASH
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Compute the calculated magnitudes, residuals and find the largest
*       absolute residual for the standard stars which were fitted.
*       Also compute the sum of the squares of the residuals for (a)
*       the fitted stars and (b) all stars.

         MAXRSD = 0.0D0

         NUMFIT = 0

         FSUMSQ = 0.0D0
         ASUMSQ = 0.0D0

         DO CURROW = 1, ROWS
            CALCMG(CURROW) =
     :        CINSMG(CURROW) - INSCON + ZEROP - (ATMOS * CAIRMS(CURROW))

            RESID(CURROW) = CCATMG(CURROW) - CALCMG(CURROW)

            ASUMSQ = ASUMSQ + (RESID(CURROW) * RESID(CURROW))

            IF (CINCL(CURROW)) THEN
               NUMFIT = NUMFIT + 1

               IF (ABS(RESID(CURROW)) .GT. MAXRSD) THEN
                  MAXRSD = ABS(RESID(CURROW))
               END IF

               FSUMSQ = FSUMSQ + (RESID(CURROW) * RESID(CURROW))
            END IF
         END DO

         INCR = MAXRSD / 1.0D1

*
*       Compute the standard deviation for the fitted stars.

         IF (NUMFIT .GE. 2) THEN
            FSTDV = SQRT(FSUMSQ / DBLE(NUMFIT - 1) )
         ELSE
            FSTDV = 0.0D0
         END IF

*
*       Compute the standard deviation for all the stars.

         IF (ROWS .GE. 2) THEN
            ASTDV = SQRT(ASUMSQ / DBLE(ROWS - 1) )
         ELSE
            ASTDV = 0.0D0
         END IF

*
*       Write the title.  Note that the title varies depending on
*       whether or not star names are being listed.

         IF (ENAME .NE. CAT__NOID) THEN
            WRITE(BUFFER, 2000, IOSTAT=LSTAT)
 2000       FORMAT(T1, 'Seq.', T7, 'Star',
     :        T20, 'Fit', T24, 'Air', T33, 'Cat.',
     :        T44, 'Instrumental Mag.')
            CALL MSG_OUT (' ', BUFFER, STATUS)

            WRITE(BUFFER, 2001, IOSTAT=LSTAT)
 2001       FORMAT(T1, 'no.', T24, 'mass', T33, 'Mag.',
     :        T41, 'calc.',  T48, 'observe', T56, 'residual')
            CALL MSG_OUT (' ', BUFFER, STATUS)
         ELSE
            WRITE(BUFFER, 2002, IOSTAT=LSTAT)
 2002       FORMAT(T1, 'Seq.', T5, 'Fit', T9, 'Air', T18, 'Cat.',
     :        T29, 'Instrumental Mag.')
            CALL MSG_OUT (' ', BUFFER, STATUS)

            WRITE(BUFFER, 2003, IOSTAT=LSTAT)
 2003       FORMAT(T1, 'no.', T9, 'mass', T18, 'Mag.',
     :        T26, 'calc.',  T33, 'observe', T41, 'residual')
            CALL MSG_OUT (' ', BUFFER, STATUS)
         END IF

*
*       Output a line for each star.

         DO CURROW = 1, ROWS

*
*          If required then read the star name.

            IF (ENAME .NE. CAT__NOID) THEN
               CALL CAT_RGET (CI, CURROW, STATUS)

               CALL CAT_EGT0C (ENAME, FNAME, NNAME, STATUS)
               IF (NNAME) THEN
                  WRITE(FNAME, 2004, IOSTAT=LSTAT) CURROW
 2004             FORMAT('Star', I4)
               END IF
            END IF

*
*          Calculate the residual bar.

            RESLEN = INT(ABS(RESID(CURROW) ) / INCR)

C           print 3003, reslen
C3003       format(1x, 'reslen: ', i5 )

            RESLEN = MIN(RESLEN, 10)

            RESBAR = ' '

            DO LOOP = 1, RESLEN
               IF (CINCL(CURROW)) THEN
                  RESBAR(LOOP : LOOP) = AST
               ELSE
                  RESBAR(LOOP : LOOP) = DASH
               END IF
            END DO

            IF (ABS(RESID(CURROW)) .GT. MAXRSD  .AND.
     :          .NOT. CINCL(CURROW) ) THEN
               RESBAR(11 : 11) = '>'
            END IF

*
*          Set the symbol showing whether or not the star was included
*          in the fit.

            IF (CINCL(CURROW)) THEN
               FITSYM = 'Y'
            ELSE
               FITSYM = ' '
            END IF

*
*          Assemble the output buffer.  Note that the format differs
*          slightly depending on whether or not a star name is to be
*          listed.

            IF (ENAME .NE. CAT__NOID) THEN
               WRITE(BUFFER, 2005, IOSTAT=LSTAT) CURROW, FNAME,
     :           FITSYM, CAIRMS(CURROW), CCATMG(CURROW),
     :           CALCMG(CURROW), CINSMG(CURROW), RESID(CURROW),
     :           RESBAR
 2005          FORMAT(T1, I3, T6, A15, T21, A1,
     :           T22,  F6.2,   T30, F7.3,
     :           T39, F7.3,   T47, F7.3,   T55, F7.3,
     :           T63, ':', A11 )
            ELSE
               WRITE(BUFFER, 2006, IOSTAT=LSTAT) CURROW, FITSYM,
     :           CAIRMS(CURROW), CCATMG(CURROW),
     :           CALCMG(CURROW), CINSMG(CURROW), RESID(CURROW),
     :           RESBAR
 2006          FORMAT(T1, I3, T6, A1,
     :           T7,  F6.2,   T15, F7.3,
     :           T24, F7.3,   T32, F7.3,   T40, F7.3,
     :           T48, ':', A11 )
            END IF

*
*          Write out the buffer.

            CALL MSG_OUT (' ', BUFFER, STATUS)

         END DO

*
*       Write out the standard deviations.

         CALL MSG_OUT (' ', ' ', STATUS)
         CALL MSG_OUT (' ', 'Standard deviation of the residuals:',
     :     STATUS)

*       ... just the fitted stars.

         BUFFER = ' '
         BUFLEN = 3

         IF (NUMFIT .GE. 2) THEN
            WRITE(STDBUF, '(0PF10.3)', IOSTAT=LSTAT) FSTDV
            CALL CHR_LDBLK (STDBUF)

            CALL CHR_PUTC ('Fitted stars: ', BUFFER, BUFLEN)

            BUFLEN = 18
            CALL CHR_PUTC (STDBUF, BUFFER, BUFLEN)

            BUFLEN = 30
            CALL CHR_PUTC ('(', BUFFER, BUFLEN)
            CALL CHR_PUTI (NUMFIT, BUFFER, BUFLEN)
            CALL CHR_PUTC (' points)', BUFFER, BUFLEN)

         ELSE
            CALL CHR_PUTC ('Too few points to compute the standard '/
     :        /'deviation for the fitted stars.', BUFFER, BUFLEN)

         END IF

         CALL MSG_OUT (' ', BUFFER(1 : BUFLEN), STATUS)

*       ... all stars.

         BUFFER = ' '
         BUFLEN = 3

         IF (ROWS .GE. 2) THEN
            WRITE(STDBUF, '(0PF10.3)', IOSTAT=LSTAT) ASTDV
            CALL CHR_LDBLK (STDBUF)

            CALL CHR_PUTC ('All stars: ', BUFFER, BUFLEN)

            BUFLEN = 18
            CALL CHR_PUTC (STDBUF, BUFFER, BUFLEN)

            BUFLEN = 30
            CALL CHR_PUTC ('(', BUFFER, BUFLEN)
            CALL CHR_PUTI (ROWS, BUFFER, BUFLEN)
            CALL CHR_PUTC (' points)', BUFFER, BUFLEN)

         ELSE
            CALL CHR_PUTC ('Too few points to compute the standard '/
     :        /'deviation for all the stars.', BUFFER, BUFLEN)

         END IF

         CALL MSG_OUT (' ', BUFFER(1 : BUFLEN), STATUS)

         CALL MSG_OUT (' ', ' ', STATUS)

      END IF

      END

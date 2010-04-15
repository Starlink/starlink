C+
      SUBROUTINE ECH_ARMENU(ZVALS,ARC1,ARC2,ARC3,ARCS,NLARCS,NX,NC,
     :                     SIGMA,NLMAX,CHANS,WAVES,WEIGHTS,CLASS,NLID,
     :                     FITTED,NCOEFF,PARMS,WORK,WORK2,REPEAT,COEFFS)
C
C     E C H _ A R M E N U
C
C     ECHARC utility.  Handles the fit, refit, edit, menu section of the
C     program, giving the user the option to fit the identified lines with
C     various orders of polynomial, to edit lines (to a limited extent),
C     and so forth.
C
C     Parameters (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ZVALS     (Real array ZVALS(NX)) The arc spectrum.
C     (>) ARC1      (Real array ARC1(NLARCS)) Holds the wavelengths
C                   for the first arc type.  Terminates with 0.
C     (>) ARC2      (Real array ARC2(NLARCS)) The wavelengths for the
C                   second arc type.
C     (>) ARC3      (Real array ARC3(NLARCS)) The wavelengths for the
C                   third arc type.
C     (>) ARCS      (Character) The arc types used.  This is the
C                   ARCTYPE parameter for the main ARC routine.
C     (>) NLARCS    (Integer) The dimension of the ARCn arrays.
C     (>) NX        (Integer) The number of data values
C     (>) NC        (Integer) The maximum number of polynomial
C                   coefficients.
C     (>) SIGMA     (Real) The current sigma value.
C     (>) NLMAX     (Integer) Maximum possible number of arc lines
C     (!) CHANS     (Real array CHANS(NLMAX)) The centers of the
C                   identified lines, in pixel numbers.
C     (!) WAVES     (Real array WAVES(NLMAX)) The wavelengths of the
C                   identified lines.
C     (!) WEIGHTS   (Real array WEIGHTS(NLMAX)) The weights for the
C                   identified arc lines.
C     (!) CLASS     (Integer array CLASS(NLMAX)) The class codes for
C                   the identified arc lines.
C     (!) NLID      (Integer) The number of identified lines.
C     (!) FITTED    (Logical) Set true if a fit is performed, otherwise
C                   left unchanged.
C     (!) NCOEFF    (Integer) Passed as the initial number of parameters
C                   used for the fit, returned as the final number used.
C     (!) PARMS     (Real array PARMS(2)) The parameters for the automatic
C                   arc fit.
C     (W) WORK      Real array WORK(NX)) Used to hold the wavelengths
C                   for each pixel.
C     (W) WORK2     (Real array WORK2(NX)) Used to hold the fit-linear
C                   values for each pixel.
C     (!) REPEAT    (Logical) Set false if the program is to be exited
C                   and left unchanged otherwise.
C     (<) COEFFS    (Double precision array COEFFS(NC)) The
C                   coefficients of the final fit.
C
C     Originally ARMENU:                      KS / AAO 30th Sept 1985
C
C        Modified:
C
C     30th Jun 1986  KS / AAO  Now tests for no lines identified
C                    before call to AREDIT (which crashes if NLID=0).
C
C     Stolen & Modified --> ECH_ARMENU:      JKM / CIT 11th Dec. 1986
C
C     26th Jul 1993  HME / UoE, Starlink.  Disuse PAR_RDUSER, PAR_Q*.
C                    Use PAR_ABORT.
C     9th  Jun 1999  TCDA / RAL, Starlink. Removed unsupported
C                    keywords from OPEN statements.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FITTED, REPEAT
      INTEGER NLARCS, NX, NC, NLMAX, NLID, NCOEFF, CLASS(NLMAX)
      REAL    ZVALS(NX), ARC1(NLARCS), ARC2(NLARCS)
      REAL    ARC3(NLARCS), SIGMA, WORK(NX), WORK2(NX)
      REAL    PARMS(2), CHANS(NLMAX), WAVES(NLMAX)
      REAL    WEIGHTS(NLMAX)
      DOUBLE PRECISION COEFFS(NC)
      CHARACTER*(*) ARCS
C
C     Functions used
C
      LOGICAL PAR_ABORT
      INTEGER ICH_FOLD, ICH_KEY, ICH_LEN
C
C     Local variables
C
      LOGICAL CARRYON
      INTEGER I, IGNORE, INVOKE, NEXT, OPTION, STATUS, TEMPLU, IGNOR2
      REAL VALUE
      CHARACTER REPLY*16
C
C     Possible option codes (note, values depend on order in ICH_KEY
C     argument string)
C
      INTEGER FIT, ORD, DISP, HELP, EDIT, RESLCT, CONT, PRNT, QUEST
      INTEGER AUTO, XAUTO, MODIFY
      PARAMETER (FIT=1, ORD=2, DISP=3, HELP=4, EDIT=5, RESLCT=6,
     :      CONT=7, PRNT=8, QUEST=9, AUTO=10, XAUTO=11, MODIFY=12)
C
C     Set for first time through loop
C
      OPTION=FIT
      CARRYON=.TRUE.
      DO WHILE (CARRYON)
C
C        The following structure is a CASE construct switching
C        on the value of OPTION.  The first time through, a fit will
C        always be performed, while on subsequent times it will be
C        the option selected by the user.  (The way the ORDER option
C        then forces the FIT option is not too clean, perhaps..)
C
         IF (OPTION.EQ.ORD) THEN
C
C           'ORDER' change order of fit, then force new fit.
C
            CALL PAR_CNPAR('ORDERFIT')
            CALL PAR_RDVAL('ORDERFIT',0.,FLOAT(NC),FLOAT(NCOEFF-1),
     :         ' ',VALUE)
            IF (PAR_ABORT()) RETURN
            NCOEFF=VALUE+1
            OPTION=FIT
         END IF
C
         IF (OPTION.EQ.FIT) THEN
C
C           'FIT' Perform a fit and list results.
C
C           First sort lines into pixel order
C
            CALL ARC_ARSORT(CHANS,WAVES,WEIGHTS,CLASS,NLID)
C
C           Then check that we have some lines identified.
C
            IF (NLID.LE.0) THEN
               CALL PAR_WRUSER(
     :           'No arc lines identified in this order.',STATUS)
            ELSE IF (NLID.EQ.1) THEN
               CALL PAR_WRUSER(
     :           'Only one arc line identified in this order.'
     :                                                   ,STATUS)
            ELSE
C
C              We have a set of lines and wavelengths, perform a fit.
C
               CALL ARC_ARFIT(CHANS,WAVES,WEIGHTS,CLASS,NLID,NX,NC,
     :                                   .TRUE.,MIN(NLID,NCOEFF),COEFFS)
               FITTED=.TRUE.
            END IF
C
         ELSE IF (OPTION.EQ.DISP) THEN
C
C           'DISPERSION' show dispersion curve plotted as deviation
C           from a linear fit.
C
            IF (NLID.LT.2) THEN
               CALL PAR_WRUSER('Not enough lines for a linear fit.',
     :                                                         STATUS)
            ELSE
               CALL ARC_ARDISC(CHANS,WAVES,WEIGHTS,CLASS,NLID,COEFFS,
     :                                        NCOEFF,NX,WORK,WORK2)
            END IF
C
         ELSE IF (OPTION.EQ.EDIT) THEN
C
C           'EDIT' edit lines in a simple way without using graphics.
C
            IF (NLID.LE.0) THEN
               CALL PAR_WRUSER('No lines identified',STATUS)
            ELSE
               CALL ARC_AREDIT(ARC1,ARC2,ARC3,ARCS,NLARCS,
     :                            CHANS,WAVES,WEIGHTS,CLASS,NLID)
               IF (PAR_ABORT()) RETURN
            END IF
C
         ELSE IF ((OPTION.EQ.HELP).OR.(OPTION.EQ.QUEST)) THEN
C
C           'HELP' or '?' output a little help.
C
            CALL FIG_HELP('echarcmenu',STATUS)
            IF (STATUS.NE.0) THEN
               CALL PAR_WRUSER('Unable to open menu help file',STATUS)
            END IF
C
         ELSE IF (OPTION.EQ.PRNT) THEN
C
C           'PRINT' Print out the results of the fit.
C
C           CALL LIB$GET_LUN(TEMPLU)
            IGNOR2=0
            CALL DSA_GET_LU(TEMPLU,IGNOR2)
            OPEN (UNIT=TEMPLU,FILE='arctemp.ech',STATUS='NEW',
     :                    IOSTAT=STATUS)
            IF (STATUS.EQ.0) THEN
               CALL ARC_ARSORT(CHANS,WAVES,WEIGHTS,CLASS,NLID)
               CALL ARC_ARLIST(TEMPLU,CHANS,WAVES,CLASS,NLID,COEFFS,
     :                                        NCOEFF,FITTED,SIGMA)
               CLOSE (UNIT=TEMPLU,IOSTAT=IGNORE)
               CALL PAR_WRUSER('Results written to temporary list file'
     :            // ' arctemp.lis',STATUS)
            ELSE
               CALL PAR_WRUSER('Unable to create temporary list file',
     :                                                     STATUS)
            END IF
C           CALL LIB$FREE_LUN(TEMPLU)
            IGNOR2=0
            CALL DSA_FREE_LU(TEMPLU,IGNOR2)
C
         ELSE IF (OPTION.EQ.CONT) THEN
C
C           'CONTINUE' get out of this section, with the main loop
C           repeat flag reset, to force an exit from the subroutine.
C
            CARRYON=.FALSE.
            REPEAT=.FALSE.
C
         ELSE IF (OPTION.EQ.RESLCT) THEN
C
C           'RESELECT'. Get out of this section, but with the main
C           loop repeat flag still set.  This forces a return to
C           the interactive line selection.
C
            CARRYON=.FALSE.
C
         ELSE IF (OPTION.EQ.AUTO) THEN
C
C           'AUTO' Perform an automatic search for further arc lines
C
            CALL ARC_ARAUTO(ZVALS,NX,SIGMA,ARC1,ARC2,ARC3,NLARCS,NLMAX,
     :              MIN(NLID,NCOEFF),COEFFS,PARMS,NLID,CHANS,WAVES,
     :                                               WEIGHTS,CLASS)

C
         ELSE IF (OPTION.EQ.XAUTO) THEN
C
C           'XAUTO' Delete all the lines found automatically.
C
            I=1
            DO WHILE(I.LE.NLID)
               IF (CLASS(I).EQ.1) THEN
                  CALL ARC_ARDELE(I,CHANS,WAVES,WEIGHTS,CLASS,NLID)
               ELSE
                  I=I+1
               END IF
            END DO
            CALL PAR_WRUSER(
     :          'All lines found by ''AUTO'' removed from list',STATUS)
C
         ELSE IF (OPTION.EQ.MODIFY) THEN
C
C            'MODIFY' Modify the parameters for the automatic arc line
C            search.
C
             CALL ARC_ARPARM(PARMS)
C
         END IF
C
C        End of the CASE construct.
C
C        Now get new command, unless we're on the way out anyway.
C
         IF (CARRYON) THEN
            CALL PAR_WRUSER(' ',STATUS)
            CALL PAR_CNPAR('CMD')
            CALL PAR_RDCHAR('CMD',' ',REPLY)
            IF (PAR_ABORT()) RETURN
            INVOKE=ICH_FOLD(REPLY)
            OPTION=ICH_KEY(REPLY,1,',; ','FIT:O_FIT:DISP:'//
     :      'HELP:EDIT:RESELECT:CONTINUE:PRINT:?:AUTO:XAUTO:MODIFY:',
     :                                                  'Abbr.',NEXT)
            IF ((OPTION.EQ.0).AND.(REPLY.NE.' ')) THEN
               CALL PAR_WRUSER(REPLY(:ICH_LEN(REPLY))//
     :              ' is not a valid response.  Try ''H'' for help',
     :                                                       STATUS)
            END IF
         END IF
C
      END DO
C
      RETURN
      END

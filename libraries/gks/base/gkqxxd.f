C# IL>=b, OL>=0
      SUBROUTINE GKQXXD (IWK,IENT,IDNR,MNID,MNRD,MLDR,INP,IER,
     :                      IOUT1,IOUT2,IOUT3,EAREA,NID,IDAT,NRD,RX,RY,
     :                      LDR,DATREC,I1,I2,I3,R1,R2,R3)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Feeder routine for inquire input device state or default device
*     data.
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     10/02/84  JL    Remove call to GKPRLG to calling routines
*     17/04/84  RSK   Add check for logical input device number
*                     less than or equal to 0 (IER = 140)
*     10/05/84  NGB   initialise returned counts to zero
*     22/01/87  JCS   IS conversion. Error number changes.
*     27/09/90  KEVP  Moved check for error 2002 here and implemented
*                     in accordence the GKS FORTRAN BINDING (C41).
*
*  ARGUMENTS
*  ---------
*     INP IWK     workstation identifier or workstation type
*     INP IENT    entrypoint code
*     INP IDNR    device number
*     INP MNID    dimension of integer array
*     INP MNRF    dimension of real array
*     INP MLDR    dimension of data record
*     INP INP     type of returned values or list element requested
*     OUT IER     error indicator
*     OUT IOUT1   operating mode or null
*     OUT IOUT2   echo switch or number of available prompt echo types
*     OUT IOUT3   prompt and echo type
*     OUT EAREA   echo area
*     OUT NID     length of integer array
*     OUT IDAT    integer array
*     OUT NRD     length of real array
*     OUT RX      real array
*     OUT RY      real array
*     OUT LDR     length of data record
*     OUT DATREC  data record
*     OUT I1,I2,I3 single integers
*     OUT R1,R2,R3 single reals
*
      INTEGER IWK, IENT, IDNR, MNID, MNRD, MLDR, INP, IER,
     :        IOUT1, IOUT2, IOUT3, NID, IDAT(*), NRD, LDR, I1, I2, I3
      REAL EAREA(4), RX(*), RY(*), R1, R2, R3
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER LSTR
*
*  ERRORS
*  ------
*
*      140  Specified input device is not present on workstation
*     2001  Specified array size too small
*     2002  List element (PET) not available
*     2003  character string not of length 80
*
*---------------------------------------------------------------------


      IF (IDNR.LE.0) THEN
        IER = 140
      ELSE
        IF (MNID.GT.0 .AND. MNRD.GT.0 .AND. MLDR.GT.0) THEN
          LSTR = LEN(DATREC(1))
          IF (LSTR.EQ.80) THEN
            KWI1 = IDNR
            KWI2 = INP
* preset return counts to zero as they are checked later,
* but may not be set by the workstation
            KNIR = 0
            KNRR = 0
            KNCR = 0

            CALL GKSONW(IWK,IENT,MNID,IDAT,MNRD,RX,RY,MLDR,DATREC)
            IER = KERROR
            IF (KERROR.EQ.0) THEN
              IOUT1 = KWI1
              IOUT2 = KWI2
              IOUT3 = KWI3
              EAREA(1) = QWR1
              EAREA(2) = QWR2
              EAREA(3) = QWR3
              EAREA(4) = QWR4
              NID = KNIR
              NRD = KNRR
              LDR = KNCR
              I1 = KWI4
              I2 = KWI5
              I3 = KWI6
              R1 = QWR5
              R2 = QWR6
              R3 = QWR7
              IF (NID.GT.MNID .OR. NRD.GT.MNRD .OR. LDR.GT.MLDR) THEN
                IER = 2001
              ELSEIF(IENT.GE.KQDLC .AND. IOUT3.EQ.KNIL .AND. INP.NE.0)
     :        THEN
                IER = 2002
              ENDIF
            ENDIF
          ELSE
          IER = 2003
          ENDIF
        ELSE
        IER = 2001
        ENDIF
      ENDIF


      END

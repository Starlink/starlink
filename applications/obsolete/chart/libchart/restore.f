      SUBROUTINE RESTORE(IEND,ICHAN, STATUS )
*+
*   Reads the intermediate file and stores the values read in the
*   COMMON blocks held in MAIN.CBL
*
*   Gets
*   ----
*      ICHAN   - the channel number that the intermediate file is read f
*
*   Returns
*   -------
*      IEND    - = 0 if a good read and reaturn
*                = 1 if end of file detected
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*   Written by K F Hartley at RGO on 11-1-83
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      READ (ICHAN,ERR=800,END=800) CATRUN,GRID,CHOOSE,SQUARE,COLOUR,
     :      QEBOX,IDENTS
      READ (ICHAN) IND,IPRINT,ICAT1,NCH,JCAT,MAXNUM,NUM
      READ (ICHAN) SCALE,SIZE,ERRB,FAINT,EQUIN,EQUOUT
      READ (ICHAN) EPOCH,A,D,AP,DP,AO,DO,DEFAULT,NONS
      READ (ICHAN) BATCH,END,ERR,SUPP,NUMSUPP
      READ (ICHAN) IDCHAR,IDFLD,EPROM,ERDIAM
      READ (ICHAN) (QEBC(I),I=1,8)
      DO I=1,50
         READ (ICHAN) YESCAT(I),NOCAT(I)
      END DO
      DO I=1,200
         READ (ICHAN) OWNOBJ(1,I),OWNOBJ(2,I)
      END DO
      DO I=1,NUM
         READ (ICHAN) (NSTAR(K,I),K=1,4),IP(I),DIAM(I),STAR(1,I),
     :             STAR(2,I),(ID(J,I),J=1,IDBYTE),PM(1,I),PM(2,I)
      END DO
      DO I=1,NUM
         READ (ICHAN) CSTAR(I),DESCR(I),NAME(I)
      END DO
*
*   Set parameter for good read and return
*
      IEND=0
      GO TO 899
*
*   Come here if end-of-file detected (or any error condition)
*
  800 CONTINUE
      IEND=1
*
*   The unique exit from this subroutine.
*
  899 CONTINUE
      END

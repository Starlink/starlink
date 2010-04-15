      SUBROUTINE STORE(ICHAN, STATUS )
*+
*   Writes the common block which contains all the useful data
*   to a file for use by other programs.
*
* ****** N.B. This is only an interim solution. ******
*
*   Written by K F Hartley at RGO on 12-1-83
*
*   Gets
*   ----
*      ICHAN  - The Channel Number the data is written to.
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'MAIN'

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Firstly write all the parameters in COMMON/CONTROL/
*
      WRITE (ICHAN) CATRUN,GRID,CHOOSE,SQUARE,COLOUR,QEBOX,IDENTS
      WRITE (ICHAN) IND,IPRINT,ICAT1,NCH,JCAT,MAXNUM,NUM
      WRITE (ICHAN) SCALE,SIZE,ERRB,FAINT,EQUIN,EQUOUT
      WRITE (ICHAN) EPOCH,A,D,AP,DP,AO,DO,DEFAULT,NONS
      WRITE (ICHAN) BATCH,END,ERR,SUPP,NUMSUPP
      WRITE (ICHAN) IDCHAR,IDFLD,EPROM,ERDIAM
      WRITE (ICHAN) (QEBC(I),I=1,8)
      DO I=1,50
         WRITE (ICHAN) YESCAT(I),NOCAT(I)
      END DO
      DO I=1,200
         WRITE (ICHAN) OWNOBJ(1,I),OWNOBJ(2,I)
      END DO
      DO I=1,NUM
         WRITE (ICHAN) (NSTAR(K,I),K=1,4),IP(I),DIAM(I),STAR(1,I),
     :             STAR(2,I),(ID(J,I),J=1,IDBYTE),PM(1,I),PM(2,I)
      END DO
*
*   and then all those in COMMON/SPECS/
*
      DO I=1,NUM
         WRITE (ICHAN) CSTAR(I),DESCR(I),NAME(I)
      END DO
      END

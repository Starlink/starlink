*  History:
*     20 Dec 1993 (hme):
*        Use RAM rather than NSPEC in WFILE, since it is now at the
*        start of the common block.
*     13 Aug 1994 (hme):
*        Change name from TOMAP to ADDMAP and simplify a little. Use
*        MV4_ routines to write spectrum and map header. Write index at
*        this level instead of leaving it to the caller.
*     21 Sep 2000 (ajc):
*        Unused ISTAT, IST2, J, INDEX2, NBYTES_INDEX
*-----------------------------------------------------------------------

      SUBROUTINE ADDMAP2 (IDUP, XCELL, YCELL,
     &                  DATA, LDATA, INDEX, IFAIL)

C   Routine to put spectral data in map file and update index table
C   appropriately.

      IMPLICIT   NONE

*     Formal parameters

      INTEGER   IDUP          ! If non-zero, Ok to replace existing entry
      REAL      XCELL         ! X-offset in cells
      REAL      YCELL         ! Y-offset in cells
      REAL      DATA(*)       ! Data to be filed in cube
      INTEGER   LDATA         ! Length of data array
      INTEGER   INDEX(*)      ! Index array to show which spectra present
      INTEGER   IFAIL         ! Error return

*     Include files:

      INCLUDE  'FLAGCOMM'
      INCLUDE  'CUBE'
      INCLUDE  'MAPHD'
      INCLUDE  'PROTOTYPE'
      INCLUDE  'CNF_PAR'

*     Local variables:

      INTEGER   NSP
      INTEGER   IIN,    JIN
      INTEGER   INS
      INTEGER   IROFF2  ! Data x-location relative to map-centre (half-pixels)
      INTEGER   IDOFF2  ! Data y-location relative to map-centre (half-pixels)

*  Ok, go...

      IFAIL = 0

*     Test that spectrum is compatible with map

      IF (LDATA .NE. NPTS1) THEN
        IFAIL = 87
        RETURN
      END IF

*     Calculate position in grid with NE corner first,
*     indexed in E-W rows, with declination decreasing.

      IROFF2 = NINT (2.0*XCELL)
      IDOFF2 = NINT (2.0*YCELL)

*     Check to see if data is already
*     present for this sky position.

        IIN = MSTEP+1-(IROFF2+MSTEP+1)/2
        JIN = NSTEP+1-(IDOFF2+NSTEP+1)/2
        INS = MSTEP*(JIN-1)+IIN

*       Check bounds
        IF ((IIN.LT.1.OR.IIN.GT.MSTEP)
     &      .OR. (JIN.LT.1.OR.JIN.GT.NSTEP)) THEN
          IFAIL = 56
          RETURN
        ENDIF

*       Check for duplicates
        IF (INDEX(INS).GT.0)   THEN
          IF (IDUP.EQ.0)   THEN
            IFAIL = 58
            RETURN
          ELSE
            NSP = INDEX(INS)
          END IF

*       If not a duplicate find a free slot in the file.
*       Not trivial; need to look for a number .le. the maximum number
*       in the file which does *not* exist in any cell of the INDEX
*       array --- this is a spectrum position which is not in use.

        ELSE
          CALL INVERT_INDEX (INDEX, MSTEP*NSTEP, 
     :                       %VAL(CNF_PVAL(INVINDEX_ADDRESS)))
          CALL SEARCH_ARRAY (%VAL(CNF_PVAL(INVINDEX_ADDRESS)), 
     :                       NSP, IFAIL)
        END IF


C     Check that the value we now have for NSP is reasonable

      IF (NSP.GT.NSPEC+1)   THEN
        IFAIL = 57
        RETURN
      ENDIF

C     First update cube

      CALL XCOPY (4*LDATA, DATA,
     :     %VAL(CNF_PVAL(CUBE_ADDRESS)+ 4*LDATA*(INS-1) ))

C     Then update the index and write it to file

      INDEX(INS) = NSP
      CALL MV4_INDXWR( INDEX )

C     Now copy new data from cube to file

      CALL MV4_SPECWR( IIN, JIN, INDEX, %VAL(CNF_PVAL(CUBE_ADDRESS)) )

C     If increasing file length modify header and write back

      IF (NSP.EQ.NSPEC+1)  THEN
        NSPEC = NSPEC+1
*       CALL WFILE (ICHAN, 0, RAM, 0, 64)
        CALL MV4_HEADWR( )
      END IF

*     Normal and error return

   99 CONTINUE
      RETURN

      END

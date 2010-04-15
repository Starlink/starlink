      SUBROUTINE GEN_FICOPY(LUTO,LUFROM,IST,IEND,ITO,IWORK,IDIM,
     +                                               NOK,IPU,LU)
C
C     G E N _ F I C O P Y
C
C     Copies data from one file to another.
C     FICOPY is designed as a service routine for the Spica
C     function 'STORE', although it may have other uses.
C
C     Parameters -  (">" input, "W" workspace, "<" output)
C
C     (>) LUTO   (Integer) Logical unit to which data is copied.
C     (>) LUFROM (Integer) Logical unit from which data is read.
C     (>) IST    (Integer) First record number of data to be copied.
C                (on Lufrom, starting from 1).
C     (>) IEND   (Integer) Last record number of data to be copied.
C                (on Lufrom) IEND must be > IST.
C     (>) ITO    (Integer) First record number to which data is to be
C                copied. (On Luto).
C     (W) IWORK  (Integer*2 array IWORK(IDIM)) Array to be used as
C                workspace.  Must be at least 256 bytes long - ie one
C                file record.
C     (>) IDIM   (Integer) Number of halfwords in IWORK.  Ignored by
C                this version of FICOPY.
C     (<) NOK    (Integer) Returned as 0 if no I/O errors, as
C                a VMS Fortran I/O error code otherwise.
C     (<) IPU    (Integer) Set to 0 for a read error, to 1 for a
C                write error.
C     (<) LU     (Integer) Returned as the logical unit number on which
C                the error occurred, if there was one.
C
C     Note - this is a VAX version of a routine that was very
C            efficient on the Interdata where it made use of
C            contiguous files.  On the VAX it will be very
C            slow because of the RMS overheads.
C
C            LUTO and LUFROM must represent open, direct access
C            files, with a 256 byte recordlength.
C
C            IF LUTO=LUFROM then the order in which data is
C            copied is backwards, in order not to destroy some
C            of the data, should there be an overlap of records.
C
C     This is a renamed version of the Spica routine FICOPY, with
C     the code returned in NOK changed to a Fortran error code,
C     rather than just 1 or 0.
C
C                                        KS / AAO 22nd Nov 1984
C+
C
      INTEGER IWORK(64)
C
C     Check whether forward copy will overwrite data
C
      IF ((LUTO.EQ.LUFROM).AND.(ITO.GE.IST)) THEN
C
C        Copy backwards
C
         ITP=ITO+IEND-IST
         DO IFROM=IEND,IST,-1
            READ (LUFROM,REC=IFROM,ERR=370,IOSTAT=NOK) IWORK
            WRITE(LUTO,REC=ITP,ERR=380,IOSTAT=NOK) IWORK
            ITP=ITP-1
            END DO
C
      ELSE
C
C        Copy forwards
C
         ITP=ITO
         DO IFROM=IST,IEND
            READ (LUFROM,REC=IFROM,ERR=370,IOSTAT=NOK) IWORK
            WRITE(LUTO,REC=ITP,ERR=380,IOSTAT=NOK) IWORK
            ITP=ITP+1
            END DO
C
      END IF
      NOK=0
      GO TO 400
C
C     Error conditions
C
  370 LU=LUFROM
      IPU=0
      GO TO 400
  380 LU=LUTO
      IPU=1
C
C     Return
C
  400 CONTINUE
C
      END


*+ASR_EXPATT         Include file defines Exposure aspect record
*-

      STRUCTURE /ATTSTRUC/
         DOUBLE PRECISION REC_START_MJD
         DOUBLE PRECISION REC_END_MJD
         INTEGER END_SCLOCK
         INTEGER START_SCLOCK
         INTEGER REF_SCLOCK
         REAL FOV2SKY(3,3)
         REAL SCAN_RATE_AZ
         REAL SCAN_RATE_EL
         REAL DURN_DAYS		! Record duration, days
	 integer c_rec		! pointer to current record
	 integer iatt		! FITS i/o unit #
      END STRUCTURE


C--------------------------------------------------------------------------

      SUBROUTINE SWAP_ARR(NBYTES,IDATA,NDAT)

C   Routine to reverse order of data in array of length NDAT where
C   the data are NBYTES bytes long each

      BYTE IDATA(1),TEMP
      ND2=NDAT/2

C   Iterate over data elements

      DO J=1,ND2
        IOFF=(J-1)*NBYTES
        J1=NDAT+1-J
        IOFF1=(J1-1)*NBYTES
C   Swap each byte of DATA(J) with those of DATA(J1)
        DO IB=1,NBYTES
          TEMP=IDATA(IOFF+IB)
          IDATA(IOFF+IB)=IDATA(IOFF1+IB)
          IDATA(IOFF1+IB)=TEMP
        END DO
      END DO
      RETURN
      END




C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0112(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM


      PARAMETER       (BNAMES = 'UBCOUNT'
     &,                OPM    = 1
     &,                INMIN  = 0
     &,                INS    = 0
     &,                OUTS   = 1
     &,                IPS    = 45
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 2
     &,                BPS    = 2
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),II
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1),
     &                 Data(999999,10)
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
         rp(1) = bp(1)
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF


      Out(1) = rp(1)
      rp(1) = rp(1) + bp(2)

      RETURN
      END



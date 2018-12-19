
      SUBROUTINE UB0093(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBFAILDETECTADD'
     &,          OPM   = 1
     &,          INMIN = 2
     &,          INS   = 2
     &,          OUTS  = 1
     &,          IPS   = 12
     &,          RPS   = 10
     &,          DPS   = 0
     &,          BPMIN = 1
     &,          BPS   = 1
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)






C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF


C---- Standard call ----------------------------------------------------
      IF ((IN(1).GE.IN(2)*BP(1)).and.(IN(1).LE.(IN(2)*(2-BP(1))))
     &.and.(in(1).GT.0).and.(in(2).gt.0))THEN
      OUT(1) = 1

      ELSE
      OUT(1) = 0

      END IF


      RETURN

      END
C-----------------------------------------------------------------------

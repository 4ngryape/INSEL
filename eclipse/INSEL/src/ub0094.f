
      SUBROUTINE UB0094(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBFAILDETECT'
     &,          OPM   = 1
     &,          INMIN = 6
     &,          INS   = 6
     &,          OUTS  = 2
     &,          IPS   = 12
     &,          RPS   = 10
     &,          DPS   = 0
     &,          BPMIN = 4
     &,          BPS   = 4
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      REAL             Qsim,Qreal
      INTEGER          t,dt,set






C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
         set = 0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF

      t = IN(5)
      dt = t-RP(2)
      RP(2) = t


      IF (IP(11) .EQ. 0) THEN
      !     Initialising
         RP(2)  = t
         IP(11) = 1
         dt = 0

      ELSE





C---- Standard call ----------------------------------------------------

      Qreal = (IN(2) - IN(1)) * IN(4) * BP(1) * dt
      Qsim = (IN(3) - IN(1)) * IN(4) * BP(1) * dt


      RP(3) = Qreal + RP(3)
      RP(4) = Qsim + RP(4)

      OUT(1) = RP(3)/60000/60
      OUT(2) = RP(4)/60000/60

      END IF

      RETURN
      END
C-----------------------------------------------------------------------

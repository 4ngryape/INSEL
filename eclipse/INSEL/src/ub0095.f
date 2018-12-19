
      SUBROUTINE UB0095(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBDELAY'
     &,          OPM   = 1
     &,          INMIN = 2
     &,          INS   = 2
     &,          OUTS  = 1
     &,          IPS   = 12
     &,          RPS   = 300
     &,          DPS   = 0
     &,          BPMIN = 0
     &,          BPS   = 0
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)


      DOUBLE PRECISION             x, y, z



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

c---- Standard call ----------------------------------------------------
       x = in(1)
       y = in(2)
       Z = 0
       do while (nint ( x ) .ne. nint ( y ))
            Z = Z + 0.001

            IF (Z .GE. 60) THEN
            OUT(1)= 4
c            OUT(2)= bp(10)
            EXIT
            ELSE

      OUT(1) = Z

      END IF
      END DO

      END

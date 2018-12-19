
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0110(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBHPCURVE'
     &,                OPM    = 1
     &,                INMIN  = 4
     &,                INS    = 8
     &,                OUTS   = 7
     &,                IPS    = 15
     &,                RPS    = 10
     &,                DPS    = 0
     &,                BPMIN  = 2
     &,                BPS    = 2
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      REAL             Tc_out, Tc_in, Te_out, Te_in, Pc, mdotc, cpc,
     &                 Pe, mdote, cpe, COP, Pel, T_HP_Vorlauf
C-----------------------------------------------------------------------
       IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,
     &         OPM,INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call

         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------

      Tc_in = in(1)
      mdotc = in(2)
      Te_in = in(3)
      mdote = in(4)
      Pc = in(5)
      Pe = in(6)
      Pel = in(7)
      T_HP_Vorlauf = in(8)
      cpe = bp(1)
      cpc = bp(2)



      Tc_out = T_HP_Vorlauf !+ Pc / (mdotc*cpc)
      mdotc = Pc / ((T_HP_Vorlauf - Tc_in)*cpc)
      Te_out = Te_in - Pe / (mdote*cpe)

      COP = Pc / Pel

      IF (in(1) .gt. 0) then

      Out(1) = Tc_out
      Out(2) = Te_out
      Out(3)= IN(5)
      Out(4)= IN(6)
      Out(5)= IN(7)
      Out(6)= COP
      Out(7)= mdotc

      ELSE
      Out(1) = 0
      Out(2) = 0
      Out(3)= 0
      Out(4)= 0
      Out(5)= 0
      Out(6)= 0
      Out(7)= 0


      END IF

      RETURN
      END
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C #Begin
C #Block PID
C #Description
C    The PID block simulates a PID controller.
C #Layout
C    #Inputs      3
C    #Outputs     2
C    #Parameters  3
C    #Strings     0
C    #Group       S
C #Details
C    #Inputs
C       #IN(1) Setpoint value
C       #IN(2) Controlled variable
C       #IN(3) Freigabe
C       #IN(4) Time
C    #Outputs
C       #OUT(1) Control signal                                $u$
C       #OUT(2) Tracking error                                $e$
C       #OUT(3) Control signal without anti-windup            $v$
C       #OUT(4) Proportional action                           $P$
C       #OUT(5) Integral action                               $I$
C       #OUT(6) Derivative action                             $D$
C    #Parameters
C       #BP(1) Proportionnal gain K
C       #BP(2) Integral time Ti
C       #BP(3) Derivative time Td
C       #BP(4) Minimum value for the control signal           $u_min$
C       #BP(5) Maximum value for the control signal           $u_max$
C       #BP(6) Threshold for the control signal               $u_thr$
C    #Strings
C       #None
C #Internals
C    #Integers
C       #IP(1)  Return code
C       #IP(2)  Call mode
C               \begin{detaillist}
C                  \item[-1] Identification call
C                  \item[0]  Standard call
C                  \item[1]  Constructor call
C                  \item[2]  Destructor call
C               \end{detaillist}
C       #IP(3)  Operation mode
C       #IP(4)  User defined block number
C       #IP(5)  Number of current block inputs
C       #IP(6)  Jump parameter
C       #IP(7)  Debug level
C       #IP(8..10)  Reserved
C    #Reals
C       #None
C    #Doubles
C       #None
C #Dependencies
C    #Subroutine ID
C #End
C     Authors: Dirk Pietruschka
C              Antoine Dalibard
C-----------------------------------------------------------------------
C      INCLUDE   'ub0111.h'
      SUBROUTINE ub0111(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*80     BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBPID'
     &,                OPM    = 1
     &,                INMIN  = 4
     &,                INS    = 6
     &,                OUTS   = 6
     &,                IPS    = 11
     &,                RPS    = 10
     &,                DPS    = 0
     &,                BPMIN  = 6
     &,                BPS    = 6
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*80     SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      REAL             y_set,y,RTIME,K,Ti,Td,e,RDT,P,I,D,u_min,u_max,u
      REAL             u_THR, v
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,
     &         OPM,INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(11) = 0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
C     Set inputs and parameters
      y_set = IN(1)
	   y     = IN(2)
      RTIME = IN(4)
      K     = BP(1)
      Ti    = BP(2)
      Td    = BP(3)
      u_min = BP(4)
      u_max = BP(5)
      u_thr = BP(6)
C     First call
      IF (IP(11) .EQ. 0) THEN
        OUT(1)= 0.0
        OUT(2)= 0.0
        OUT(3)= 0.0
        OUT(4)= 0.0
        OUT(5)= 0.0
        OUT(6)= 0.0
        RDT   = 0.0
        RP(1) = RTIME
        RP(2) = RDT
        RP(3) = y_set
        RP(4) = y
        RP(5) = OUT(1)
        RP(6) = OUT(2)
        RP(7) = OUT(3)
        RP(8) = OUT(4)
        RP(9) = OUT(5)
        RP(10)= OUT(6)
        IP(11)= 1
        RETURN
      END IF
C     Others calls
      IF (IP(11) .EQ. 1) THEN
	   RDT = RTIME - RP(1)
        RP(1) = RTIME
        RP(2) = RDT
      END IF
      IF(IN(3) .EQ. 0)THEN
        OUT(1) = 0
        RP(9)  = 0
	   RETURN
      END IF     
      e = y_set - y
C     Set proportionnal term
      P = K * e
C     Set integral term (with 'crisis' error funktion for anti wind up)
      IF (Ti .GT. 0.0) THEN
        I = RP(9) + (K * RDT * e / Ti)!*EXP(-(100*(RP(3)-RP(4)))**2.0)

C     Set anti-windup ilyes
      IF (I .GT. u_max .OR. I .LT. u_min) THEN
      I =  RP(9)!0.0
      ELSE
        RP(9) = I
      END IF
      ELSE
        I = 0.0
      END IF
C     Set derivative term
      IF (Td .GT. 0.0) THEN
        D = K * Td * (e - (RP(3) - RP(4))) / RDT
	   ELSE
	   D = 0.0
	   END IF
C     Set unsaturated control signal
      v = P + I + D
C     Set saturated control signal
      IF (ABS(v) .LT. u_thr) THEN
	   u = 0.0
	   ELSE IF (v .LE. u_min) THEN
	   u = u_min
	   ELSE IF (v .GE. u_max) THEN
	   u = u_max
	   ELSE
	   u = v
	   END IF
      OUT(1) = u
      OUT(2) = e
	   OUT(3) = v
	   OUT(4) = P
	   OUT(5) = I
	   OUT(6) = D
      RP(3)  = y_set
      RP(4)  = y
      RP(5)  = OUT(1)
	   RP(6)  = OUT(2)
      RP(7)  = OUT(3)
      RP(8)  = OUT(4)
      RP(9)  = OUT(5)
      RP(10) = OUT(6)
      RETURN
      END
C-----------------------------------------------------------------------

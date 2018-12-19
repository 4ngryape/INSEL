C-----------------------------------------------------------------------
C #Begin
C #Block 2pcon2
C #Description
C    Einfacher Zweipunktregler
C #Layout
C    #Inputs      3
C    #Outputs     1
C    #Parameters  2
C    #Strings     0
C    #Group       3
C #Details
C    #Inputs
C       #IN(1) control_temperature [Celsius]
C       #IN(2) setpoint_temperature [Celsius]
C        #IN(3) Massenstrom [kG/s]
C    #Outputs
C       #OUT(1) mdot [kG/s]
C    #Parameters
C       #bp(1) deltaT [Kelvin]
C       #bp(2) mdotmin [kG/s]
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
C #Authors
C    Ruben Pesch
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0053(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UB2PCOOLCON2'
     &,                OPM    = 1
     &,                INMIN  = 3
     &,                INS    = 3
     &,                OUTS   = 1
     &,                IPS    = 10
     &,                RPS    = 0
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
C---- Standard call -------------------------------------------------------
      IF (IN(1) .GT. in(2)) THEN
         OUT(1) = IN(3)
      ELSE IF (IN(1) .LT. (in(2)-bp(1))) THEN
         OUT(1) = bp(2)
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------


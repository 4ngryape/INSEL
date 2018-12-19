C-----------------------------------------------------------------------
C #Begin
C #Block UB2PTCTRL
C #Description  2 punkt Regler
C    
C #Layout
C  #Inputs      2
C  #Outputs     1
C  #Parameters  3
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) T hot
C     #IN(2) T cold
C  #Outputs
C     #OUT(1) control
C  #Parameters
C     #BP(1) deltaT on
C     #BP(2) deltaT off
C     #BP(3) T max
C  #Strings
C     #None
C #Internals
C  #Integers
C     #IP(1)  Return code
C     #IP(2)  Call mode
C             \begin{detaillist}
C                 \item[-1] Identification call
C                 \item[0]  Standard call
C                 \item[1]  Constructor call
C                 \item[2]  Destructor call
C             \end{detaillist}
C     #IP(3)  Operation mode
C     #IP(4)  User defined block number
C     #IP(5)  Number of current block inputs
C     #IP(6)  Jump parameter
C     #IP(7) Debug level
C     #IP(8..10) Reserved
C   #Reals
C      #None
C   #Doubles
C      #None
C #Dependencies
C   #Subroutine ID
C #Authors
C   INSEL Block Wizard
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0016(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     & OPM, GROUP
      PARAMETER (BNAMES = 'UB2PTCRL'
     &,          OPM   = 1
     &,          INMIN = 2
     &,          INS   = 2
     &,          OUTS  = 1
     &,          IPS   = 12
     &,          RPS   = 0
     &,          DPS   = 0
     &,          BPMIN = 3
     &,          BPS   = 3
     &,          GROUP = 3
     &,          SPMIN = 0
     &,          SPS   = 0)
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
      IF ((IN(1)-IN(2)) .LT. BP(2)) THEN
               OUT(1) = 0.0
 !
      END IF
      IF ((IN(1)-IN(2)) .GE. BP(1)) THEN
               OUT(1) = 1.0
      END IF
      IF ((IN(1).GT. BP(3)) .OR. (IN(2).GT. BP(3))) THEN
               OUT(1) = 0.0
      END IF
      RETURN
      END

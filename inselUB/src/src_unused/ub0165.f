C-----------------------------------------------------------------------
C #Begin
C #Block UBTENT
C #Description
C    
C #Layout
C  #Inputs      0
C  #Outputs     0
C  #Parameters  0
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #None
C  #Outputs
C     #None
C  #Parameters
C     #None
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
      SUBROUTINE UB0165(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBTENT'
     &,          OPM   = 1
     &,          INMIN = 0
     &,          INS   = 0
     &,          OUTS  = 0
     &,          IPS   = 10
     &,          RPS   = 0
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
      RETURN
      END
C-----------------------------------------------------------------------

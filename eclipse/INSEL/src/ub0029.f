C-----------------------------------------------------------------------
C #Begin
C #Block UBNIRADIA
C #Description
C    Calculates the distribution of solar radiation on the surface of internal walls
C #Layout
C  #Inputs      1
C  #Outputs     1
C  #Parameters  2
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) Total solar irradiation though all windows / W
C  #Outputs
C     #OUT(1) Specific solar radiation heat flux to surface 1 / Wm-2
C     #OUT(2) Specific solar radiation heat flux to surface 2 / Wm-2
C     #OUT(3) Specific solar radiation heat flux to surface 3 / Wm-2
C     #OUT(4) Specific solar radiation heat flux to surface 4 / Wm-2
C     #OUT(5) Specific solar radiation heat flux to surface 5 / Wm-2
C     #OUT(6) Specific solar radiation heat flux to surface 6 / Wm-2
C     #OUT(7) Specific solar radiation heat flux to surface 7 / Wm-2
C     #OUT(8) Specific solar radiation heat flux to surface 8 / Wm-2
C     #OUT(9) Specific solar radiation heat flux to surface 9 / Wm-2
C     #OUT(10) Specific solar radiation heat flux to surface 10 / Wm-2
C     #OUT(11) Specific solar radiation heat flux to surface 11 / Wm-2
C     #OUT(12) Specific solar radiation heat flux to surface 12 / Wm-2
C     #OUT(13) Specific solar radiation heat flux to surface 13 / Wm-2
C     #OUT(14) Specific solar radiation heat flux to surface 14 / Wm-2
C     #OUT(15) Specific solar radiation heat flux to surface 15 / Wm-2
C     #OUT(16) Specific solar radiation heat flux to surface 16 / Wm-2
C     #OUT(17) Specific solar radiation heat flux to surface 17 / Wm-2
C     #OUT(18) Specific solar radiation heat flux to surface 18 / Wm-2
C     #OUT(19) Specific solar radiation heat flux to surface 19 / Wm-2
C     #OUT(20) Specific solar radiation heat flux to surface 20 / Wm-2
C  #Parameters
C     #BP(1) Nw
C     #BP(2) l   A (m2)   l   alpha   l
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
      SUBROUTINE UB0029(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBNIRADIA'
     &,          OPM   = 1
     &,          INMIN = 1
     &,          INS   = 1
     &,          OUTS  = 20
     &,          IPS   = 11
     &,          RPS   = 1
     &,          DPS   = 0
     &,          BPMIN = 3
     &,          BPS   = 41
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER          NWALLS,NWMAX,I
      PARAMETER        (NWMAX = 20)
      REAL             A(NWMAX),ALPHA(NWMAX), SUM, GSolar, SUM2
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(11) = ANINT(BP(1))
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
      NWALLS = IP(11)
      SUM = 0
      DO I = 1,NWALLS
         A(I)     = BP(2*I)
         ALPHA(I) = BP(2*I+1)
         SUM      = SUM + A(I) * ALPHA(I)
      END DO
      GSolar = IN(1)
      SUM2 = 0
      DO I = 1, NWALLS
        If (SUM.eq.0) then
          out(i)=0
        else
          OUT(I) = GSolar * ALPHA(I) / SUM
          SUM2 = SUM2 + OUT(I)*A(I)
        end if
      END DO
         OUT(NWALLS+1) = SUM2
      RETURN
      END
C-----------------------------------------------------------------------

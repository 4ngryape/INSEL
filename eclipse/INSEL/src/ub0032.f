C-----------------------------------------------------------------------
C #Begin
C #Block UBROOMLIGHT
C #Description
C    Q convect is an input
C #Layout
C  #Inputs      8
C  #Outputs     3
C  #Parameters  8
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) Time (s)
C     #IN(2) Heating/cooling power on air temp node3 (W)
C     #IN(3) Convective part of internal gains (W)
C     #IN(4) External air temperature (\degC)
C     #IN(5) Air exchange rate (only infiltration) (h-1)
C     #IN(6) Setpoint max. room temperature (\degC)
C     #IN(7) Setpoint min. room temperature (\degC)
C     #IN(8) Convective gains from walls (W)
C  #Outputs
C     #OUT(1) Room air temperature (°C)
C     #OUT(2) Cooling load (W)
C     #OUT(3) Heating load (W)
C  #Parameters
C     #BP(1) Vair (m3)
C     #BP(2) Tair0 (°C)
C     #BP(3) CS on(0)/off(1)
C     #BP(4) Pcool (W)
C     #BP(5) HS on(0)/off(1)
C     #BP(6) Pheat (W)
C     #BP(7) Nw
C     #BP(8) Ai & HCi
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
      SUBROUTINE UB0032(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBROOMLIGHT'
     &,          OPM   = 1
     &,          INMIN = 8
     &,          INS   = 8
     &,          OUTS  = 3
     &,          IPS   = 12
     &,          RPS   = 1
     &,          DPS   = 0
     &,          BPMIN = 8
     &,          BPS   = 47
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER  NWALLS,NWMAX,I
      PARAMETER (NWMAX = 20)
      REAL     DRUCK  /1.0E5/
      REAL     V,TOLD,DELTAT,QGAIN, VdotAIR, Tamb, QC_MAX, QH_MAX
      REAL     A(NWMAX),TSURF(NWMAX), SUM, QVENT, H(NWMAX), SUM2
      COMMON  /CBS0002/ NWALLS, A, TSURF, V, TOLD, DELTAT, QGAIN,
     &                  VdotAIR, Tamb, QCOOL, QHEAT, H
      REAL     TROOM, TR_MAX, TR_MIN
      REAL     EROOM, QCOOL, QHEAT, QCONV
      REAL     AIRRHO,AIRCP,ETOT
      REAL     TBridges
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(11) = 0
            IP(12) = ANINT(BP(7))
            OUT(1) = 22.0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
      !RETURN
      NWALLS   = IP(12)
      !print*,NWALLS
      IF (IP(11) .EQ. 0) THEN
         IP(11) = 1
         RP(1)  = IN(1)
         OUT(1) = BP(2)
         DO I = 1,NWALLS
            !OUT(I+3) = 0.0 ! THE BUG!!! HOPEFULLY!
         END DO
         RETURN
      END IF

      TBridges=BP(34)          !W/K         !!!!!!!!!!!!!!!!!TO BE FIXED!!!!!!!!!!!!!!

      DELTAT = IN(1) - RP(1)
      RP(1)  = IN(1)
      IF (DELTAT .LE. 0.0) THEN
C        RUECKSETALGORITHMUS schreiben!!
         DELTAT = 0.0
      END IF

C     Load COMMON block
      DO I = 1,NWALLS
         A(I) = BP(6+2*I)
         H(I) = BP(7+2*I)
C         TSURF(I) = IN(7+I)
      END DO
      V        = BP(1)
      TOLD     = OUT(1)
      QGAIN    = IN(2) + IN(3)
      Tamb     = IN(4)
      VdotAIR  = IN(5)*V/3600 + TBridges/AIRRHO(Told,DRUCK)/AIRCP(Told)
      TR_MAX   = IN(6)
      TR_MIN   = IN(7)
      QCONV    = IN(8)
      QCOOL    = 0
      QHEAT    = 0
      QH_MAX   = BP(6)
      QC_MAX   = BP(4)
C      SUM1 = 0.0
      SUM2= 0.0
      DO I = 1,NWALLS
C         SUM1 = SUM1 + H(I)* A(I) * TSURF(I)
         SUM2 = SUM2 + H(I)* A(I)
      END DO
      TROOM=(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/DELTAT*Told+QGAIN+QHEAT+
     & QCOOL+QCONV+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)*
     & (Tamb-Told/2)+SUM2*Told)/(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/
     & DELTAT+SUM2+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)/2)

      IF(TROOM .GT. TR_MAX .AND. BP(3) .EQ. 0)THEN
C     Calculate required cooling power
         TROOM = TR_MAX
         SUM = QCONV
         DO I = 1,NWALLS
            SUM = SUM + H(I) * A(I) * (Told - TROOM)
         END DO
         QVENT  = AIRRHO(TROOM,DRUCK) * VdotAIR * AIRCP(TROOM)
     &          * (Tamb-((TROOM+TOLD)/2.0))
         QCOOL  = AIRRHO(TROOM,DRUCK) * V * AIRCP(TROOM)
     &          * (TROOM - TOLD) / DELTAT - SUM - QGAIN - QVENT
         IF(-QCOOL .GT. QC_MAX)THEN
           QCOOL  = - QC_MAX
           TROOM=(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/DELTAT*Told+QGAIN+
     & QHEAT+QCOOL+QCONV+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)*
     & (Tamb-Told/2)+SUM2*Told)/(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/DELTAT
     & +SUM2+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)/2)
         END IF
      END IF
      IF(TROOM .LT. TR_MIN .AND. BP(5) .EQ. 0)THEN
C     Calculate required heating power
         TROOM = TR_MIN
         SUM = QCONV
         DO I = 1,NWALLS
            SUM = SUM + H(I) * A(I) * (Told - TROOM)
         END DO
         QVENT  = AIRRHO(TROOM,DRUCK) * VdotAIR * AIRCP(TROOM)
     &          * (Tamb-((TROOM+TOLD)/2.0))
         QHEAT  = AIRRHO(TROOM,DRUCK) * V * AIRCP(TROOM)
     &          * (TROOM - TOLD) / DELTAT - SUM - QGAIN - QVENT
         IF(QHEAT .GT. QH_MAX)THEN
           QHEAT  = QH_MAX
           TROOM=(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/DELTAT*Told+QGAIN+
     & QHEAT+QCOOL+QCONV+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)*
     & (Tamb-Told/2)+SUM2*Told)/(AIRRHO(Told,DRUCK)*V*AIRCP(Told)/DELTAT
     & +SUM2+AIRRHO(Told,DRUCK)*VdotAIR*AIRCP(Told)/2)
         END IF
      END IF
      OUT(1) = TROOM
      OUT(2) = QCOOL
      OUT(3) = QHEAT
      ETOT   = 0.0
c      DO I = 1,NWALLS
c         OUT(I+3) = H(I)* (TROOM - TSURF(I))
c         ETOT = ETOT + OUT(I+3) * A(I)
c      END DO
      EROOM = AIRRHO(TOLD,DRUCK) * V * AIRCP(TOLD) * TROOM
     &      - AIRRHO(TOLD,DRUCK) * V * AIRCP(TOLD) * TOLD
c      OUT(24) = EROOM / 3600.0      ! in Wh
      ETOT  = ETOT + EROOM / 3600.0
      RETURN
      END
C-----------------------------------------------------------------------

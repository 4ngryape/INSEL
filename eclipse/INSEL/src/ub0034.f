C-----------------------------------------------------------------------
C #Begin
C #Block UBINTRAD
C #Description
C    
C #Layout
C  #Inputs      4
C  #Outputs     4
C  #Parameters  3
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) Temperature (°C) wall 1
C     #IN(2) Temperature (°C) wall 2
C     #IN(3) Temperature (°C) wall 3
C     #IN(4) Temperature (°C) wall 4
C     #IN(5) Temperature (°C) wall 5
C     #IN(6) Temperature (°C) wall 6
C     #IN(7) Temperature (°C) wall 7
C     #IN(8) Temperature (°C) wall 8
C     #IN(9) Temperature (°C) wall 9
C     #IN(10) Temperature (°C) wall 10
C     #IN(11) Temperature (°C) wall 11
C     #in(19) Temperature (°C) wall 12
C     #IN(13) Temperature (°C) wall 13
C     #IN(14) Temperature (°C) wall 14
C     #IN(15) Temperature (°C) wall 15
C     #IN(16) Temperature (°C) wall 16
C     #IN(17) Temperature (°C) wall 17
C     #IN(18) Temperature (°C) wall 18
C     #IN(19) Temperature (°C) wall 19
C     #IN(20) Temperature (°C) wall 20
C  #Outputs
C     #OUT(1) Radiative heat flux (W/m^2) from wall 1
C     #OUT(2) Radiative heat flux (W/m^2) from wall 2
C     #OUT(3) Radiative heat flux (W/m^2) from wall 3
C     #OUT(4) Radiative heat flux (W/m^2) from wall 4
C     #OUT(5) Radiative heat flux (W/m^2) from wall 5
C     #OUT(6) Radiative heat flux (W/m^2) from wall 6
C     #OUT(7) Radiative heat flux (W/m^2) from wall 7
C     #OUT(8) Radiative heat flux (W/m^2) from wall 8
C     #OUT(9) Radiative heat flux (W/m^2) from wall 9
C     #OUT(10) Radiative heat flux (W/m^2) from wall 10
C     #OUT(11) Radiative heat flux (W/m^2) from wall 11
C     #OUT(12) Radiative heat flux (W/m^2) from wall 12
C     #OUT(13) Radiative heat flux (W/m^2) from wall 13
C     #OUT(14) Radiative heat flux (W/m^2) from wall 14
C     #OUT(15) Radiative heat flux (W/m^2) from wall 15
C     #OUT(16) Radiative heat flux (W/m^2) from wall 16
C     #OUT(17) Radiative heat flux (W/m^2) from wall 17
C     #OUT(18) Radiative heat flux (W/m^2) from wall 18
C     #OUT(19) Radiative heat flux (W/m^2) from wall 19
C     #OUT(20) Radiative heat flux (W/m^2) from wall 20
C  #Parameters
C     #BP(1) Ns
C     #BP(2) Area (m2)  /  Emission coef
C     #BP(3) Fij
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
      SUBROUTINE UB0034(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBINTRAD'
     &,          OPM   = 1
     &,          INMIN = 4
     &,          INS   = 20
     &,          OUTS  = 20
     &,          IPS   = 10
     &,          RPS   = 0
     &,          DPS   = 0
     &,          BPMIN = 25
     &,          BPS   = 441
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)

      INTEGER          Nwalls,I,J,K
      REAL             PHI(20,20),QRAD(20),E(20),A(20)
      REAL             QRADM,SIGMA,SUME,SUMEPS
      REAL             EPSILON(20),SUMAB
      REAL             Fij(20,20)

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
      Nwalls = BP(1)
      do i=1,Nwalls
        A(i)=BP(2*(i-1)+2)
        epsilon(i)=BP(2*(i-1)+3)
      end do
      do i=1,Nwalls
        do j=1,Nwalls
          k=Nwalls*2+1+j+(i-1)*Nwalls
          Fij(i,j)=BP(k)
          PHI(i,j)=Fij(i,j)*A(i)
        end do
      end do

C     Calculate the mean radiative flux from all walls:
      SIGMA  = 0.0000000567
      SUME   = 0.0
      SUMEPS = 0.0

      DO I = 1,Nwalls
         E(I)   = SIGMA  * EPSILON(I) * (IN(I)+273.15)**4
         SUME   = SUME   + E(I)       * A(I)
         SUMEPS = SUMEPS + EPSILON(I) * A(I)
      END DO

C     Mittlere Flaechenhelligkeit des Raumes: QRADM
      if(SUMEPS.gt.0) then
      QRADM = SUME / SUMEPS
      else
      QRADM=0
      end if

C     Calculate sum of radiative fluxes from other walls onto wall i
C     using view factors and mean radiative flux
      DO I = 1,Nwalls
         SUMAB = 0.0
         DO J = 1,Nwalls
            SUMAB = SUMAB + PHI(I,J) * (E(J) + (1-EPSILON(J)) * QRADM)
         END DO
         QRAD(I) = E(I) - EPSILON(I) * SUMAB/ A(I)
      END DO
      DO I = 1,Nwalls
         OUT(I) = -QRAD(I)
      END DO

      RETURN
      END
C-----------------------------------------------------------------------

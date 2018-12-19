C-----------------------------------------------------------------------
C #Begin
C #Block UBWALLI2015
C #Description
C    
C #Layout
C  #Inputs      9
C  #Outputs     4
C  #Parameters  8
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) Time / s
C     #IN(2) Interior shortwave irradiance gains on wall / W\,m$^{-2}$
C     #IN(3) Longwave heat flux from the other surfaces / W\,m$^{-2}$
C     #IN(4) Radiative part of internal gains inside / W\,m$^{-2}$
C     #IN(5) Exterior shortwave irradiance gains on the wall / W\,m$^{-2}$
C     #IN(6) Longwave heat flux from exterior surfaces and sky / W\,m$^{-2}$
C     #IN(7) Radiative part of exterior heat sources / W\,m$^{-2}$
C     #IN(8) Interior Temperature / °C
C     #IN(9) Exterior Temperature / °C
C     #IN(10) Any other input 1
C     #IN(11) Any other input 2
C     #in(19) Any other input 3
C  #Outputs
C     #OUT(1) Interior surface temperature facing room at $t + \Delta  t$
C     #OUT(2) Exterior surface temperature at $t + \Delta  t$
C     #OUT(3) Interior conv flux [W/m$^2$]
C     #OUT(4) Exterior conv flux [W/m$^2$]
C     #OUT(5) Any other output 1
C     #OUT(6) Any other output 2
C     #OUT(7) Any other output 3
C  #Parameters
C     #BP(1) h_ci (W/m2)
C     #BP(2) h_ri (W/m2)
C     #BP(3) T_i (°C)
C     #BP(4) h_co (W/m2)
C     #BP(5) h_ro (W/m2)
C     #BP(6) T_o (°C)
C     #BP(7) N_l
C     #BP(8) e /m + U /Wm-1K-1 + ro /kgm-3 + Cp /Jkg-1K-1
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
      SUBROUTINE UB0036(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBWALLI2015'
     &,          OPM   = 1
     &,          INMIN = 9
     &,          INS   = 12
     &,          OUTS  = 7
     &,          IPS   = 12
     &,          RPS   = 300
     &,          DPS   = 0
     &,          BPMIN = 11
     &,          BPS   = 50
     &,          SPMIN = 0
     &,          SPS   = 0
     &,          GROUP = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER          NLAYMAX,NDMAX
      PARAMETER       (NLAYMAX = 10, NDMAX = 100)
      REAL             DELTATMAX
      PARAMETER       (DELTATMAX = 1.0)
      LOGICAL          EXTERIOR
      INTEGER          NLAY,NLAYi, I,J,K,ILEFT,IRIGHT,JLEFT,JRIGHT
      INTEGER          JL,JR,IR, T, TCOUNT
      INTEGER          NDIV(NLAYMAX)
      REAL             SUMSL,U, TIME,HCI
      REAL             LAMBDA(NLAYMAX), RHO(NLAYMAX), CAP(NLAYMAX)
      REAL             THICK(NLAYMAX)
      REAL             DELTAX(NLAYMAX),TEMP(NLAYMAX,NDMAX)
      REAL             TOLD(NLAYMAX,NDMAX)
      REAL             A(NLAYMAX),DEL(NLAYMAX),A1,TROOM
      REAL             C3,C4,DELTAT
      REAL             C1L,C2L,C1R,C2R,QRIGHT
      REAL             TAU(NLAYMAX)

      INTEGER  INTA1
      DOUBLE PRECISION ETOT,EOLD
      REAL              QLEFT,GSOLAR,HCO,TA,HRO,TSKY,DT,
     &                  QCOND,TLAYER2,ELOSS, CONVI, CONVO

      REAL     HRI

      INTEGER  icont
      REAL     THICK_a, LAMBDA_a, RHO_a, CAP_a, XX, AA, BB, TEXT
      logical  SS

C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(12) = ANINT(BP(7))
            IF (IP(12) .LT. 1) THEN
C              At least one layer is required
               IP(1) = 305320
               CALL MSG(IP,RP,SP)
            END IF
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
C      IF (ABS(IP(3)) .EQ. 2) THEN
C         EXTERIOR = .TRUE.
C      ELSE
         EXTERIOR = .FALSE.
C      END IF
      NLAYi = IP(12)
      HCI  = BP(1)
      HRI  = BP(2)
      HCO  = BP(4)
      HRO  = BP(5)
      if(EXTERIOR) HCO = HCO + 3.68*IN(8)         !http://editorial.cda.ulpgc.es/ftp/ambiente/antesol/TESIS/Cap4.pdf -> he = 9.42 + 3.68•v
C     First call:
C     Calculate initial temperature profile for first timestep here
C     under steady state conditions using the external temperature and
C     the initial room temperature given as a parameter.
      IF (IP(11) .EQ. 0) THEN
         IP(11) = 1
C        Absolute time is memorised in rp(1), unit is "seconds"
         TIME  = IN(1)
         RP(1) = TIME
C        Determine U-value from thickness bp(8+..) divided by
C        conductivity bp(8+nlay+..), U value calculated from interior
C        surface temperature, i.e. without 1/hci
         SUMSL = 1.0 / HCI
         DO I = 1,NLAYi
            THICK(I)  = BP(8+4*(I-1))
            LAMBDA(I) = BP(9+4*(I-1))
            SUMSL = THICK(I) / LAMBDA(I) + SUMSL
         END DO
         U = 1.0 / (SUMSL + 1.0 / HCO)
         RP(3) = U
C        Initial temperature profile for t=0,
C        Tsurf is calculated from room temperature starting value
C        at parameter BP(3) using heat transfer coefficient given in parameters.
C        BP(6) is the initial value for the outside temperature.
         TROOM = BP(3)
C        Heat transfer resistance inside
         SUMSL = 1.0 / HCI
C        Surface temperature RP(4+nlay)
         RP(4+NLAYi) = TROOM - SUMSL * U * (TROOM - BP(6))       !Initial temperature of first layer
         EOLD = 0.0
         DO I = 1,NLAYi
            RHO(I)    = BP(10+4*(I-1))
            CAP(I)    = BP(11+4*(I-1))
            SUMSL        = (THICK(I) / LAMBDA(I)) + SUMSL
            RP(4+NLAYi+I) = TROOM - SUMSL * U * (TROOM - BP(6))       !Initial temp of the following layers
            EOLD = EOLD + RHO(I) * CAP(I) * THICK(I) * RP(4+NLAYi+I)  !Inc internal energy
         END DO

C        Tsurf is calculated from room temperature starting value
C        at parameter BP(3) or BP(6) using heat transfer coefficient given in parameters.
         CONVI = 0.
         CONVO = 0.

         OUT(1) = RP(4+NLAYi)                                         !Temp of interior surface
         OUT(2) = RP(4+2*NLAYi)                                       !Temp of exterior surface
         OUT(3) = CONVI
         OUT(4) = CONVO

         OUT(5) = RP(3)                                               !U-value

         RETURN
      END IF
C     Second call of block to calculate time step, division of material
C     layers and steady state temperature distribution at each material
C     division
      IF (IP(11) .EQ. 1) THEN
         IP(11) = 2
         U      = RP(3)
         TROOM  = BP(3)
         TIME   = IN(1)
         DT     = TIME - RP(1)                                        !This is why we need a second call to establish the layers division
         SS=.false.
         IF (DT .LT. 0.0) DT = 0.0

C        Calculation of thermal diffusivity a=lambda/rho/c and
C        del=a*delta t/delta x**2
C        delta x needs to be chosen such that the stability criteria
C        del<0.5 is maintained
C        With deltax=thick/ndiv follows: ndiv<thick*sqrt(0.5/(a*deltat))
         THICK(1)  = BP(8)
         LAMBDA(1) = BP(9)
         RHO(1)    = BP(10)
         CAP(1)    = BP(11)
         TAU(1) = (RHO(1) * CAP(1) * THICK(1)**2)/LAMBDA(1)
         A(1)  = LAMBDA(1) / (RHO(1) * CAP(1))
         icont=1
         DO I = 2,NLAYi
            if(TAU(icont).le.DT) then
                THICK_a=THICK(icont)
                LAMBDA_a=LAMBDA(icont)
                RHO_a=RHO(icont)
                CAP_a=CAP(icont)
                THICK(icont)=BP(8+4*(I-1))+THICK_a
                LAMBDA(icont)=THICK(icont)/(BP(8+4*(I-1))/BP(9+4*(I-1))+
     &             THICK_a/LAMBDA_a)
                RHO(icont)=(BP(10+4*(I-1))*BP(8+4*(I-1))+
     &            RHO_a*THICK_a)/THICK(icont)
                XX=RHO_a*THICK_a/(RHO_a*THICK_a+
     &             BP(10+4*(I-1))*BP(8+4*(I-1)))
                CAP(icont)=BP(11+4*(I-1))*(1-XX)+CAP_a*XX
C               Thermal diffusivity
                A(icont)  = LAMBDA(icont) / (RHO(icont) * CAP(icont))
                TAU(icont) = THICK(icont)**2/A(icont)
            else
                icont=icont+1
                THICK(icont)  = BP(8+4*(I-1))
                LAMBDA(icont) = BP(9+4*(I-1))
                RHO(icont)    = BP(10+4*(I-1))
                CAP(icont)    = BP(11+4*(I-1))
C               Thermal diffusivity
                A(icont)  = LAMBDA(icont) / (RHO(icont) * CAP(icont))
                TAU(icont) = THICK(icont)**2/A(icont)
            end if
         END DO
         !Finally, if the last layer is still too thin
         if(TAU(icont).le.DT) then
          if(icont.ge.2) then                       !add it to the previous
            THICK_a=THICK(icont)
            LAMBDA_a=LAMBDA(icont)
            RHO_a=RHO(icont)
            CAP_a=CAP(icont)
            THICK(icont)=THICK_a+THICK(icont-1)
            LAMBDA(icont)=THICK(icont)/(THICK_a/LAMBDA_a+
     &         THICK(icont-1)/LAMBDA(icont-1))
            RHO(icont)=(RHO_a*THICK_a+
     &         RHO(icont-1)*THICK(icont-1))/THICK(icont)
            XX=RHO(icont-1)*THICK(icont-1)/(RHO(icont-1)*THICK(icont-1)+
     &         RHO_a*THICK_a)
            CAP(icont)=CAP_a*(1-XX)+CAP(icont-1)*XX
C           Thermal diffusivity
            A(icont)  = LAMBDA(icont) / (RHO(icont) * CAP(icont))
            TAU(icont) = THICK(icont)**2/A(icont)
            icont=icont-1
          else                                       !if there is no previous -> SS
            SS=.true.
            C1L = RHO(1) * CAP(1) * THICK(1) / 2.0
            C2L = LAMBDA(1) / THICK(1)
            RP(194) = C1L
            RP(195) = C2L
          end if
         end if
         NLAY=icont
         SUMSL=0.0
         RP(4+NLAY) = TROOM - SUMSL * U * (TROOM - BP(6))       !Initial temp of the following layers
         DO I = 1,NLAY
            SUMSL        = (THICK(I) / LAMBDA(I)) + SUMSL
            RP(4+NLAY+I) = TROOM - SUMSL * U * (TROOM - BP(6))       !Initial temp of the following layers
            EOLD = EOLD + RHO(I) * CAP(I) * THICK(I) * RP(4+NLAY+I)  !Inc internal energy
         END DO
        if(.not.ss) then
         DO I = 1,NLAY
            A1    = THICK(I) * SQRT(0.5 / (A(I) * DT))
C           Number of divisions of each layer
            INTA1   = INT(A1)
            NDIV(I) = MAX(1,INTA1)
            IF (I .GT. NDMAX) THEN
               !print*,"WARNING NDIV restricted to NDMAX = 100"
               NDIV(I) = NDMAX
            END IF
C           Thickness of each sublayer
            DELTAX(I) = THICK(I) / NDIV(I)
            IF (DELTAX(I) .GT. 0.02) THEN
               NDIV(I)   = INT(THICK(I)  / 0.02) + 1
               DELTAX(I) = THICK(I) / NDIV(I)
            END IF
C           Stability criterion
            DEL(I)    = A(I) * DT / DELTAX(I)**2
C           Control of maximum step width and decrease of time step dt
C           If number of divisions cannot be reduced any further
            DO WHILE (DEL(I) .GE. 0.5)
               DT     = DT / 2.0
               DEL(I) = A(I) * DT / DELTAX(I)**2
               print*, del(i),dt,"del dt second call"
            END DO
         END DO
C        The stability criteria needs to be checked again for surface
C        and boundary nodes.
C        The node adjacent to the surface layer (1,1) is either (1,2)
C        or if the layer has only one division: (2,1)
C        Adjacent layer number even if the stability criteria is obeyed
         IF (NDIV(1) .EQ. 1) THEN
            ILEFT = 2
            JLEFT = 1
         ELSE
            ILEFT = 1
            JLEFT = 2
         END IF
C        First, "left" surface node facing room air
         C1L = RHO(1) * CAP(1) * DELTAX(1) / 2.0
         C2L = LAMBDA(1) / DELTAX(1)
         C3  = C2L + HCI
         C4  = C1L / C3
         DO WHILE (C4 .LT. DT)
            DT = DT / 2.0
         END DO
C        Last, "right" surface node facing exterior air (node nlay+1,1)
C        the first node adjacent to the surface is
C        iright = nlay, jright = ndiv(nlay)
         IRIGHT = NLAY
         JRIGHT = NDIV(NLAY)
         C1R    = RHO(NLAY) * CAP(NLAY) * DELTAX(NLAY) / 2.0
         C2R    = LAMBDA(NLAY) / DELTAX(NLAY)
         C3     = C2R + HCO
         C4     = C1R / C3
         DO WHILE (C4 .LT. DT)
            DT = DT / 2.0
         END DO
C        Store del(i) with actual time step dt
         DO I = 1,NLAY
            DEL(I)  = A(I) * DT / DELTAX(I)**2
            RP(3+I) = NDIV(I)
         END DO
C        Calculation of steady state temperature field at each
C        subdivision this field is stored in the rp variables for the
C        instationary calculations the surface temperature RP(4+nlay) is
C        UNCHANGED FROM THE FIRST CALL
         SUMSL = 1.0 / HCI + 1.0 / C2L
C        Counter for stored temperature field
         K         = 4+NLAY+1
         TOLD(1,1) = RP(4+NLAY)
         TOLD(ILEFT,JLEFT) = TROOM - SUMSL * U * (TROOM - BP(6))
         RP(4+NLAY+1) = TOLD(ILEFT,JLEFT)
         DO I = ILEFT,IRIGHT
            IF (I .EQ. ILEFT)THEN
               JL = JLEFT + 1
               JR = NDIV(I)
            ELSE IF (I .EQ. IRIGHT) THEN
               JL = 2
               JR = JRIGHT
            ELSE
               JL = 2
               JR = NDIV(I)
            END IF
            DO J = JL,JR
               SUMSL     = SUMSL + DELTAX(I) / LAMBDA(I)
               TOLD(I,J) = TROOM - SUMSL * U * (TROOM - BP(6))
               K         = K + 1
               RP(K)     = TOLD(I,J)
            END DO
            IF (I .EQ. IRIGHT) THEN
               SUMSL = SUMSL + 1 / C2R
C              Outside surface
               TOLD(NLAY+1,1) = TROOM - SUMSL * U * (TROOM - BP(6))
               K = K + 1
C              Outside layer is node(nlay+1,1)
               J = 1
               RP(K) = TOLD(NLAY+1,1)
            ELSE
               SUMSL = SUMSL + DELTAX(I) / LAMBDA(I)
C              Boundary layer
               TOLD(I+1,1) = TROOM - SUMSL * U * (TROOM - BP(6))
               K           = K + 1
C              Outside layer is node(nlay+1,1)
               J = 1
               RP(K) = TOLD(I+1,1)
            END IF
         END DO
         RP(190) = ILEFT
         RP(191) = IRIGHT
         RP(192) = JLEFT
         RP(193) = JRIGHT
         RP(194) = C1L
         RP(195) = C2L
         RP(196) = C1R
         RP(197) = C2R
         RP(220) = DT
         RP(180) = NLAY
         DO I = 1,NLAY
           RP(221+4*(I-1)) = THICK(I)
           RP(222+4*(I-1)) = LAMBDA(I)
           RP(223+4*(I-1)) = RHO(I)
           RP(224+4*(I-1)) = CAP(I)
         END DO
        end if
      END IF
C-----------------------------------------------------------------------
C     2nd, 3rd call and all subsequent calls to calculate instationary
C     temperature field
      if(SS) then
        C2L = RP(195)
        AA = HCI + HRI
        BB = HCO + HRO
        if (EXTERIOR) then
          TEXT = (HCO*IN(6) + HRO*IN(7)) / BB
          TEMP(1,1) = ((1/BB+1/C2L)*IN(9)+(1/AA)*TEXT
     &        + 1/AA*(1/BB+1/C2L)*(IN(2)+IN(3)+IN(4))
     &        + 1/AA/BB*(IN(5)))
     &        / (1/AA + 1/C2L + 1/BB)
          TEMP(NLAY+1,1) = ((1/AA+1/C2L)*TEXT+(1/BB)*IN(9)
     &        + 1/BB*(1/AA+1/C2L)*(IN(5))
     &        + 1/AA/BB*(IN(2)+IN(3)+IN(4)))
     &        / (1/AA + 1/C2L + 1/BB)
          CONVO = HCO * (IN(7)-TEMP(NLAY+1,1))
          CONVI = HCI * (IN(9)-TEMP(1,1))
        else
          TEMP(1,1) = ((1/BB+1/C2L)*IN(8)+(1/AA)*IN(9)
     &        + 1/AA*(1/BB+1/C2L)*(IN(2)+IN(3)+IN(4))
     &        + 1/AA/BB*(IN(5)+IN(6)+IN(7)))
     &        / (1/AA + 1/C2L + 1/BB)
          TEMP(NLAY+1,1) = ((1/AA+1/C2L)*IN(9)+(1/BB)*IN(8)
     &        + 1/BB*(1/AA+1/C2L)*(IN(5)+IN(6)+IN(7))
     &        + 1/AA/BB*(IN(2)+IN(3)+IN(4)))
     &        / (1/AA + 1/C2L + 1/BB)
          CONVO = HCO * (IN(9)-TEMP(NLAY+1,1))
          CONVI = HCI * (IN(8)-TEMP(1,1))
        end if
        OUT(1)=TEMP(1,1)
        OUT(2)=TEMP(NLAY+1,1)
        OUT(3)=CONVI
        OUT(4)=CONVO
      else
      NLAY = RP(180)
      DO I = 1,NLAY
         THICK(I)  = RP(221+4*(I-1))
         LAMBDA(I) = RP(222+4*(I-1))
         RHO(I)    = RP(223+4*(I-1))
         CAP(I)    = RP(224+4*(I-1))
         NDIV(I)   = RP(3+I)
         DELTAX(I) = THICK(I) / NDIV(I)
      END DO
      EOLD = 0.0
      K = 4 + NLAY
      DO I = 1,NLAY
         DO J = 1,NDIV(I)
            IF (J .EQ. 1) THEN
               EOLD = EOLD + RHO(I) * CAP(I) * DELTAX(I) * RP(K) / 2.0
            ELSE IF (J .EQ. NDIV(I)) THEN
               EOLD = EOLD + RHO(I) * CAP(I) * DELTAX(I) * RP(K+1) / 2.0
     &              + RHO(I) * CAP(I) * DELTAX(I) * RP(K)
            ELSE
               EOLD = EOLD + RHO(I) * CAP(I) * DELTAX(I) * RP(K)
            END IF
            K    = K + 1
         END DO
      END DO
      TIME   = IN(1)
      DELTAT = TIME - RP(1)
      RP(1)  = TIME
      IF (DELTAT .LE. 0.0) THEN
C        RUECKSETZUNG programmieren!!   Programar reajuste
      END IF
      DT     = RP(220)
C     Use temporary variables within the program: old timestep TOLD
      ILEFT  =  RP(190)
      IRIGHT =  RP(191)
      JLEFT  =  RP(192)
      JRIGHT =  RP(193)
      C1L    =  RP(194)
      C2L    =  RP(195)
      C1R    =  RP(196)
      C2R    =  RP(197)

      TCOUNT = INT(DELTAT/DT)
      DO T = 1,TCOUNT                                                 !Contador de pasos de tiempo internos por cada paso de tiempo oficial de cálculo del modelo completo
         K = 4 + NLAY
C        Surface temperature inside
         TOLD(1,1) = RP(4+NLAY)
         DO I = ILEFT,IRIGHT
            IF (I .EQ. ILEFT)THEN
               JL = JLEFT
               JR = NDIV(I)
            ELSE IF (I .EQ. IRIGHT) THEN
               JL = 2
               JR = JRIGHT
            ELSE
               JL = 2
               JR = NDIV(I)
            END IF
C           Transfer stored RP temperature field to internally
C           used TOLD field (checked!)
            DO J = JL,JR
               K         = K + 1
               TOLD(I,J) = RP(K)
            END DO
            K = K + 1
            IF (I .EQ. IRIGHT) THEN
               TOLD(NLAY+1,1) = RP(K)
            ELSE
               TOLD(I+1,1) = RP(K)
            END IF
         END DO
C        Surface temperature facing room TEMP(1,1):
C        surface temperature new = surface temperature old
C           + conductive heat flux + solar gains (IN2)
C           + convective heat flux (IN3) + radiative heat flux (IN4)
C           + radiative part internal gains (IN5)
C        (only half the thickness of the first material layer is
C        considered for surface heat capacity),the layer next to the
C        surface is called RP(4+nlay+1) and is the old temperature
         QCOND   = C2L * (RP(4+NLAY+1) - TOLD(1,1))
         TLAYER2 = RP(4+NLAY+1)
         TEMP(1,1) = (TOLD(1,1) + DT / C1L
     &          * (C2L * TLAYER2 + IN(2) + HCI*IN(8) + IN(3) + IN(4)
     &          + HRI * TOLD(1,1)))
     &          / (1.0 + DT / C1L * (C2L + HCI + HRI))                !Temp of interior surface (no afected by thickness)
         if (EXTERIOR) then
           CONVI = HCI*(IN(9)-TEMP(1,1))
         else
           CONVI = HCI*(IN(8)-TEMP(1,1))
         end if
         IF (ILEFT .LE. NLAY) THEN
C           Temperature (ILEFT, JLEFT) erster Knoten nach Innenknoten:
C           Left of node (ILEFT,JLEFT) is node 1,1
            A1 = C1L + RHO(ILEFT) * CAP(ILEFT) * DELTAX(ILEFT) / 2.0
!            QLEFT  = C2L * (TOLD(1,1) - TOLD(ILEFT,JLEFT))
            QLEFT  = C2L * (TEMP(1,1) - TOLD(ILEFT,JLEFT))
            QRIGHT = LAMBDA(ILEFT) / DELTAX(ILEFT)
     &             * (RP(4+NLAY+2) - TOLD(ILEFT,JLEFT))
            TEMP(ILEFT,JLEFT) = TOLD(ILEFT,JLEFT)
     &                        + DT * (QLEFT + QRIGHT) / A1
         END IF
C        Temperature (IRIGHT,JRIGHT) Letzter Knoten vor aussen
         IF (IRIGHT .EQ. 1 .AND. JRIGHT .EQ. 1)THEN
C           There is only one layer with no subdivision
            CONTINUE
         ELSE IF (JRIGHT .EQ. 1) THEN
            QLEFT = LAMBDA(IRIGHT-1) / DELTAX(IRIGHT-1) *
     &         (TOLD(IRIGHT-1,NDIV(IRIGHT-1)) - TOLD(IRIGHT,JRIGHT))
            QRIGHT = C2R * (TOLD(NLAY+1,1) - TOLD(IRIGHT,JRIGHT))
            A1 = C1R
     &         + RHO(IRIGHT-1) * CAP(IRIGHT-1) * DELTAX(IRIGHT-1) / 2.0
            TEMP(IRIGHT,JRIGHT) = TOLD(IRIGHT,JRIGHT) + DT / A1
     &         * (QLEFT + QRIGHT)
         ELSE
            QLEFT  = LAMBDA(IRIGHT) / DELTAX(IRIGHT)
     &             * (TOLD(IRIGHT,JRIGHT-1)- TOLD(IRIGHT,JRIGHT))
            QRIGHT = C2R * (TOLD(NLAY+1,1) - TOLD(IRIGHT,JRIGHT))
            A1     = C1R
     &             + RHO(IRIGHT) * CAP(IRIGHT) * DELTAX(IRIGHT) / 2.0
            TEMP(IRIGHT,JRIGHT) = TOLD(IRIGHT,JRIGHT) + DT / A1
     &         * (QLEFT + QRIGHT)
         END IF
C        Standard layers within material
         DO I = ILEFT,IRIGHT
            A(I)   = LAMBDA(I) / (RHO(I) * CAP(I))
            DEL(I) = A(I)   * DT / DELTAX(I)**2
            A1     = RHO(I) * CAP(I) * DELTAX(I)
            IF (I .EQ. ILEFT) THEN
               JL = JLEFT + 1
               JR = NDIV(I) - 1
            ELSE IF (I .EQ. IRIGHT) THEN
               JL = 2
               JR = JRIGHT - 1
            ELSE
               JL = 2
               JR = NDIV(I) - 1
            END IF
            DO J = JL,JR
C              JL == 2
               QLEFT  = LAMBDA(I) / DELTAX(I)
     &                * (TOLD(I,J-1) - TOLD(I,J))
               QRIGHT = LAMBDA(I) / DELTAX(I)
     &                * (TOLD(I,J+1) - TOLD(I,J))
               TEMP(I,J) = TOLD(I,J) + DEL(I)
     &                * (TOLD(I,J+1) + TOLD(I,J-1) - 2.0 * TOLD(I,J))
            END DO
C           Last layer before material changes
C           First layer facing room is treated first:
            IF (I .EQ. ILEFT) THEN
               IF (NDIV(I) .GT. 2) THEN
                  TEMP(I,NDIV(I)) = TOLD(I,NDIV(I)) + DEL(I)
     &            * (TOLD(I+1,1) + TOLD(I,NDIV(I)-1)
     &              - 2.0 * TOLD(I,NDIV(I)))
               END IF
            END IF
C           All other cases up to last layer
            IF (I .NE. IRIGHT .AND. I .NE. ILEFT) THEN
               IF (NDIV(I) .GT. 1) THEN
                  TEMP(I,NDIV(I)) = TOLD(I,NDIV(I)) + DEL(I)
     &            * (TOLD(I+1,1) + TOLD(I,NDIV(I)-1)
     &              - 2.0 * TOLD(I,NDIV(I)))
               END IF
            END IF
         END DO
C        Boundary layer temperatures within the wall
C        (NODeS Ileft+1,1..right,1)
         IF (JRIGHT .EQ. 1)THEN
            IR = IRIGHT - 1
         ELSE
            IR = IRIGHT
         END IF
         DO I = ILEFT+1,IR
            QLEFT = LAMBDA(I-1) / DELTAX(I-1) *
     &            (TOLD(I-1,NDIV(I-1)) - TOLD(I,1))
            A1 = (RHO(I-1) * CAP(I-1) * DELTAX(I-1)
     &         + RHO(I) * CAP(I) * DELTAX(I))/2
            IF (NDIV(I) .EQ. 1) THEN
               TEMP(I,1) = TOLD(I,1) + DT/A1 * (QLEFT
     &          + LAMBDA(I)/DELTAX(I)*(TOLD(I+1,1)-TOLD(I,1)))
            ELSE
               TEMP(I,1) = TOLD(I,1) + DT/A1 * (QLEFT
     &         + LAMBDA(I)/DELTAX(I)*(TOLD(I,2)-TOLD(I,1)))
            END IF
         END DO
C        External surface temperature  (NODE nlay+1,1)
C        external solar irradiance IN(5), radiative
C        heat gains (in6), radiative gains from heat source IN7
         QLEFT = C2R * (TOLD(IRIGHT,JRIGHT) - TOLD(NLAY+1,1))
         IF (EXTERIOR) THEN
            TSKY    = IN(7)
            GSOLAR  = IN(5)
            TA      = IN(6)
            TEMP(NLAY+1,1) = (C1R * TOLD(NLAY+1,1) / DT + QLEFT + GSOLAR
     &             + HCO * TA + HRO * TSKY)
     &             / (C1R / DT + HRO + HCO)
            CONVO = 0.
         ELSE
            TEMP(NLAY+1,1) = (TOLD(NLAY+1,1) + DT / C1R
     &             * (QLEFT + IN(5) + HCO*IN(9) + IN(6) + IN(7)
     &             + HRO*TOLD(NLAY+1,1)))
     &             / (1 + DT / C1R * (HRO + HCO))
            ELOSS = 0.0
            CONVO = HCO*(IN(9)-TEMP(NLAY+1,1))
         END IF

C        Store new temperature field in RP field
         K = 4 + NLAY
         RP(4+NLAY) = TEMP(1,1)
         DO I = ILEFT,IRIGHT        !room surface temperature
            IF (I .EQ. ILEFT) THEN  !1st layer next to room surface
               JL = JLEFT
               JR = NDIV(I)
            ELSE IF (I .EQ. IRIGHT) THEN  !last layer before outside surface
               JL = 2
               JR = JRIGHT
            ELSE
               JL = 2                  !all layers between 1st and last
               JR = NDIV(I)
            END IF
            DO J = JL,JR
               K = K + 1
               RP(K) = TEMP(I,J)
            END DO
            K = K + 1
            IF (I .EQ. IRIGHT) THEN
               RP(K) = TEMP(NLAY+1,1)
            ELSE
               RP(K) = TEMP(I+1,1)
            END IF
         END DO
      END DO

      OUT(1) = TEMP(1,1)
      OUT(2) = TEMP(NLAY+1,1)
      OUT(3) = CONVI
      OUT(4) = CONVO

      ETOT = 0.0
      K = 4 + NLAY
      DO I = 1,NLAY
         DO J = 1,NDIV(I)
            IF (J .EQ. 1) THEN
               ETOT = ETOT + RHO(I) * CAP(I) * DELTAX(I) * RP(K) / 2.0
            ELSE IF (J .EQ. NDIV(I)) THEN
               ETOT = ETOT + RHO(I) * CAP(I) * DELTAX(I) * RP(K+1) / 2.0
     &              + RHO(I) * CAP(I) * DELTAX(I) * RP(K)
            ELSE
               ETOT = ETOT + RHO(I) * CAP(I) * DELTAX(I) * RP(K)
            END IF
            K = K + 1
         END DO
      END DO
      end if

      RETURN
      END
C-----------------------------------------------------------------------

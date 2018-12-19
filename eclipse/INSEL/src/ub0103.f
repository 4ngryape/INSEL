C-----------------------------------------------------------------------
C #Begin
C #Block WALL, WALLX
C #Description WALL
C    The WALL block solves the one-dimensional heat transfer
C    equation for a interior wall consisting of several layers with
C    varying thickness and heat conductivity.
C    Shortwave irradiance,
C    convective heat flux from the room air and longwave radiation flux
C    from the other surfaces are given as boundary conditions.
C #Description WALLX
C    The WALLX block solves the one-dimensional heat transfer
C    equation for a external wall consisting of several layers with
C    varying thickness and heat conductivity. As boundary conditions,
C    the exterior temperature and solar irradiance is given for the
C    outside wall surface, on the inside shortwave irradiance,
C    convective heat flux from the room air and longwave radiation flux
C    from the other surfaces are given as boundary conditions.
C #Layout
C    #Inputs       9 $\ldots$ [10]
C    #Outputs      4
C    #Parameters   11 $\ldots$ [47]
C    #Strings      0
C    #Group        3
C #Details
C    #Inputs WALL
C       #IN(1)  Time / s
C       #IN(2)  Interior shortwave irradiance gains on wall / W\,m$^{-2}$
C       #IN(3)  Convective heat flux from the room air / W\,m$^{-2}$
C       #IN(4)  Longwave heat flux from the other surfaces / W\,m$^{-2}$
C       #IN(5)  Radiative part of internal gains inside / W\,m$^{-2}$
C       #IN(6)  Exterior shortwave irradiance gains on the wall / W\,m$^{-2}$
C       #IN(7)  Convective heat flux from exterior air / W\,m$^{-2}$
C       #IN(8)  Longwave heat flux from exterior surfaces
C               and sky / W\,m$^{-2}$
C       #IN(9)  Radiative part of exterior heat sources / W\,m$^{-2}$
C       #IN(10) Distance $x$ from inner surface / m
C    #Inputs WALLX
C       #IN(1)  Time / s
C       #IN(2)  Interior shortwave irradiance gains on wall / W\,m$^{-2}$
C       #IN(3)  Convective heat flux from the room air / W\,m$^{-2}$
C       #IN(4)  Longwave heat flux from the other surfaces / W\,m$^{-2}$
C       #IN(5)  Radiative part of internal gains inside / W\,m$^{-2}$
C       #IN(6)  Exterior shortwave irradiance gains on the wall / W\,m$^{-2}$
C       #IN(7)  Ambient temperature / \degC
C       #IN(8)  Sky temperature / \degC
C       #IN(9)  Wind speed / m\,s$^{-1}$
C       #IN(10) Distance $x$ from inner surface / m
C    #Outputs
C       #OUT(1) Interior surface temperature facing room at
C               $t + \Delta  t$
C       #OUT(2) Exterior surface temperature at
C               $t + \Delta t$
C       #OUT(3) Temperature at distance $x$ from inner surface
C       #OUT(4) $U$-value / W\,m$^{-2}$\,K$^{-1}$
C    #Parameters
C       #BP(1)   Interior convective heat transfer coefficient
C                $h_{\mrm c,i}$ / W\,m$^{-2}$\,K$^{-1}$
C       #BP(2)   Interior radiative heat transfer coefficient
C                $h_{\mrm r,i}$ / W\,m$^{-2}$\,K$^{-1}$
C       #BP(3)   Initial value interior air temperature
C                $T_{\mrm i}$ / \degC
C       #BP(4)   Exterior convective heat transfer coefficient
C                $h_{\mrm c,o}$ / W\,m$^{-2}$\,K$^{-1}$
C       #BP(5)   Exterior radiative heat transfer coefficient
C                $h_{\mrm r,o}$ / W\,m$^{-2}$\,K$^{-1}$
C       #BP(6)   Initial value exterior air temperature
C                $T_{\mrm o}$ / \degC
C       #BP(7)   Number of different material layers of wall
C                $1 \le N_{\mrm l} \le 10$
C       #BP(8)   Thickness of material layer one / m
C       #BP(9)   Heat conductivity of layer one / W\,m$^{-1}$\,K$^{-1}$
C       #BP(10)   Density of material layer one / kg\,m$^{-3}$
C       #BP(11)   Heat capacity of material layer one
C                / J\,kg$^{-1}$\,K$^{-1}$
C       #BP(12) Thickness of further material layer
C       #BP(13) Heat conductivity of further layer
C       #BP(14) Density of further material layer
C       #BP(15) Heat capacity of further material layer
C       #BP($\ldots$) etc.
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
C       #IP(11) Counter for the number of calls
C       #IP(12) INTEGER of BP(7)
C    #Reals
C       #RP(1)        Absolute time
C       #RP(2)      DT time step resulting from layer division
C       #RP(3)      U value
C       #RP(4..4+nlay-1) Number of divisions of each layer
C       #RP(4+nlay...)     Temperature field of all nodes
C
C       #RP(190-193->weg)  Layer counter boundaries
C       #RP(194..197->weg) Combined material values
C       #RP(210...210+NLAY->weg)  a * deltat / deltax2
C    #Doubles
C       #None
C #Dependencies
C    #None
C #Authors
C    Ursula Eicker
C    Juergen Schumacher
C #End
C Remarks
C    Organisation of layer numbering:
C    The inside surface temperature facing the room is number T(1,1)
C    and is stored as RP(4+nlay), RP(1) being reserved for the time.
C    A layer "i" can be subdivided in a number of divisions ndiv(i),
C    so that the stability criterion is always fulfilled.
C    The boundary layer between two layers starts with the number of
C    the second layer, for example "i+1" and counter 1..NDIV(i+1)
C    Example: wall with two layers with 2 and 3 subdivisions:
C    T(1,1) Surface temperature facing room
C    T(1,2) First material node in layer 1
C    T(2,1) Boundary node between layer 1 and 2
C    T(2,2) Second node in layer 2
C    T(2,3) Third node in layer 3
C    T(3,1) External surface temperature facing outside
C    The last layer is always T(NLAY+1,1)
C-----------------------------------------------------------------------
      SUBROUTINE UB0103(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBWALLX2'
     &,                OPM    = 2
     &,                INMIN  = 9
     &,                INS    = 12
     &,                OUTS   = 4
     &,                IPS    = 12
     &,                RPS    = 250
     &,                DPS    = 0
     &,                BPMIN  = 13
     &,                BPS    = 47
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER          NLAYMAX,NDMAX
      PARAMETER       (NLAYMAX = 10, NDMAX = 100)
      REAL             DELTATMAX
CCCCC Eventuell als BP herausfuehren....................................
      PARAMETER       (DELTATMAX = 1.0)
      LOGICAL          HALFDT,EXTERIOR
      INTEGER          NLAY, I,J,K,ILEFT,IRIGHT,JLEFT,JRIGHT
      INTEGER          JL,JR,IR, T, TCOUNT,ILAY,JLAY
      INTEGER          NDIV(NLAYMAX)
      REAL             SUMSL,U, TIME,HCI
      REAL             LAMBDA(NLAYMAX), RHO(NLAYMAX), CAP(NLAYMAX)
      REAL             THICK(NLAYMAX), LAMBDAS, RHOS, CAPS
      REAL             DELTAX(NLAYMAX),TEMP(NLAYMAX,NDMAX)
      REAL             TOLD(NLAYMAX,NDMAX),TEMPX
      REAL             A(NLAYMAX),DEL(NLAYMAX),A1,TROOM
      REAL             C3,C4,DELTAT, T_EXT
      REAL             C1L,C2L,C1R,C2R,QRIGHT
      REAL             X,XLAY,XDIV,DTNEW!,HPLUS,C3PLUS,C4PLUS
      REAL             SIGMA /5.67E-8/, EPSILON/0.9/
      REAL             HCOWIND,HCOFREE
      REAL             TAU, TRE
      !REAL             DXMIN,NDIVTEST

      INTEGER  INTA1
      DOUBLE PRECISION ETOT,EOLD
      REAL              QLEFT,GSOLAR,HCO,TA,HRO,TSKY,DT,
     &                  QCOND,TLAYER2,ELOSS

      REAL     HRI
C-----------------------------------------------------------------------

      IP(3)=2 ! set external wall
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            !HCA = 7.7 ! INSEL 7
            !HCI = 7.7
            !HRA = 0
            !HRI = 0
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
      IF (ABS(IP(3)) .EQ. 2) THEN
         EXTERIOR = .TRUE.
         !HCA      = 25.0 ! Normwert
         !HCA      = 7.7 ! INSEL 7.0.4
      ELSE
         EXTERIOR = .FALSE.
         !HCA      = 7.7  ! Normwert
      END IF
      NLAY = IP(12)
      HCI  = BP(1)
      HRI  = BP(2)
      HCO  = BP(4)
      HRO  = BP(5)
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
         DO I = 1,NLAY
            THICK(I)  = BP(8+4*(I-1))
            LAMBDA(I) = BP(9+4*(I-1))
         END DO
         SUMSL = 1.0 / HCI
         DO I = 1,NLAY
            SUMSL = THICK(I) / LAMBDA(I) + SUMSL
         END DO
         U = 1.0 / (SUMSL + 1.0 / HCO)
         RP(3) = U
C        Initial temperature profile for t=0,
C        Tsurf is calculated from room temperature starting value
C        at parameter BP(3) using standard heat transfer coefficient
C        of 7.7 W/m2K.
C        BP(6) is the initial value for the outside temperature.
         TROOM = BP(3)
C        Heat transfer resistance inside
         SUMSL = 1.0 / HCI
C        Surface temperature RP(4+nlay)
         RP(4+NLAY) = TROOM - SUMSL * U * (TROOM - BP(6))
         THICK(1)  = BP(8)
         RHO(1)    = BP(9)
         CAP(1)    = BP(10)
         EOLD = 0.0
         DO I = 1,NLAY
            THICK(I)  = BP(8+4*(I-1))
            LAMBDA(I) = BP(9+4*(I-1))
            RHO(I)    = BP(10+4*(I-1))
            CAP(I)    = BP(11+4*(I-1))
            SUMSL        = (THICK(I) / LAMBDA(I)) + SUMSL
            RP(4+NLAY+I) = TROOM - SUMSL * U * (TROOM - BP(6))
            EOLD = EOLD + RHO(I) * CAP(I) * THICK(I) * RP(4+NLAY+I)
         END DO
         !print*,"EOLD(0) = ",eold/3600.0
         OUT(1) = RP(4+NLAY)
         OUT(2) = RP(4+2*NLAY)
         IF (IP(5) .EQ. 10) THEN
            X    = IN(10)
            ILAY = 0
            XLAY = 0.0
            DO I = 1,NLAY
               XLAY = XLAY + THICK(I)
               IF (XLAY .GE. X) THEN
                  XLAY = XLAY - THICK(I)
                  ILAY = I
                  GO TO 10
               END IF
            END DO
10          CONTINUE
            TEMPX = RP(4+NLAY+ILAY-1)+
     &                (RP(4+NLAY+ILAY)-RP(4+NLAY+ILAY-1))/
     &                THICK(ILAY)*(X-XLAY)
            OUT(3) = TEMPX
         END IF
         OUT(4) = RP(3)
         !OUT(6) = IN(2) + IN(3) + IN(4) + IN(5) + IN(6)
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
         DT     = TIME - RP(1)
         IF (DT .LT. 0.0) DT = 0.0
         !print*,"second call DT",DT
C        Calculation of thermal diffusivity a=lambda/rho/c and
C        del=a*delta t/delta x**2
C        delta x needs to be chosen such that the stability criteria
C        del<0.5 is maintained
C        With deltax=thick/ndiv follows: ndiv<thick*sqrt(0.5/(a*deltat))
         DO I = 1,NLAY
            THICK(I)  = BP(8+4*(I-1))
            LAMBDA(I) = BP(9+4*(I-1))
            RHO(I)    = BP(10+4*(I-1))
            CAP(I)    = BP(11+4*(I-1))
C           Thermal diffusivity
            A(I)  = LAMBDA(I) / (RHO(I) * CAP(I))
            A1    = THICK(I) * SQRT(0.5 / (A(I) * DT))
            !DXMIN = SQRT(2.0 * A(I) * DT)
            !print*,ip(4),I,A1,dxmin
            !IF (THICK(I) .LE. DXMIN) THEN
            !   NDIVTEST = 1
            !ELSE
            !   NDIVTEST = INT(THICK(I) / DXMIN)
            !END IF
C           Number of divisions of each layer
            INTA1   = INT(A1)
            NDIV(I) = MAX(1,INTA1)
            !NDIV(I)   = NDIVTEST
            !print*,i,NDIV(I)
            !pause
            IF (I .GT. NDMAX) THEN
               !print*,"WARNING NDIV restricted to NDMAX = 100"
               NDIV(I) = NDMAX
            END IF
C           Thickness of each sublayer
            DELTAX(I) = THICK(I) / NDIV(I)
            !print*,I,DELTAX(I),THICK(I),NDIV(I)
            IF (DELTAX(I) .GT. 0.02) THEN
               NDIV(I)   = INT(THICK(I)  / 0.02) + 1
               DELTAX(I) = THICK(I) / NDIV(I)
            END IF
            !print*,ip(4)," DELTAX(I) = ",DELTAX(I),NDIV(I),DXMIN
C           Stability criterion
            DEL(I)    = A(I) * DT / DELTAX(I)**2
C           Control of maximum step width and decrease of time step dt
C           If number of divisions cannot be reduced any further
            DO WHILE (DEL(I) .GE. 0.5)
               DT     = DT / 2.0
               DEL(I) = A(I) * DT / DELTAX(I)**2
               print*, del(1),dt,"del dt second call"
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
      END IF
C-----------------------------------------------------------------------
C     2nd, 3rd call and all subsequent calls to calculate instationary
C     temperature field
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
      !print*,"EOLD(2) ",eold / 3600.0
      TIME   = IN(1)
      DELTAT = TIME - RP(1)
      RP(1)  = TIME
      IF (DELTAT .LE. 0.0) THEN
C        RUECKSETZUNG programmieren!!
      END IF
      DTNEW  = RP(220)
C     Use temporary variables within the program: old timestep TOLD
      ILEFT  =  RP(190)
      IRIGHT =  RP(191)
      JLEFT  =  RP(192)
      JRIGHT =  RP(193)
      C1L    =  RP(194)
      C2L    =  RP(195)
      C1R    =  RP(196)
      C2R    =  RP(197)
      DO I = 1,NLAY
         THICK(I)  = BP(8+4*(I-1))
         LAMBDA(I) = BP(9+4*(I-1))
         RHO(I)    = BP(10+4*(I-1))
         CAP(I)    = BP(11+4*(I-1))
         NDIV(I)   = RP(3+I)
         DELTAX(I) = THICK(I) / NDIV(I)
         TAU = CAP(I) * RHO(I) * THICK(I)**2
     &       / (2.0 * LAMBDA(I) * NDIV(I)**2)
         !print*,ip(4),i,ndiv(i),tau,dtnew
      END DO
C-----LOOP if surface temperature changes by more than .. kelvin-------
!11111 CONTINUE
      HALFDT = .FALSE.
      TCOUNT = INT(DELTAT/DTNEW)
      DT     = DTNEW

!      print *,"RP220,DT,TCOUNT,DT*TCOUNT/3600 ",
!     &          RP(220),DT,TCOUNT,DT*TCOUNT/3600,TIME
!      GECHECKT: STIMMT!
      DO T = 1,TCOUNT
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
     &          * (C2L * TLAYER2 + IN(2) + IN(3) + IN(4) + IN(5)
     &          + (HCI + HRI) * TOLD(1,1)))
     &          / (1.0 + DT / C1L * (C2L + HCI + HRI))
         IF (ABS(TEMP(1,1) - TOLD(1,1)) .GT. DELTATMAX) THEN
!           DTNEW  = DTNEW / 2.0
            HALFDT = .TRUE.
!            EXIT
         END IF
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
C        external solar irradiance IN(6), convective gains (IN7), radiative
C        heat gains (in8), radiative gains from heat sourceIN9
         QLEFT = C2R * (TOLD(IRIGHT,JRIGHT) - TOLD(NLAY+1,1))
         IF (EXTERIOR) THEN
            TSKY    = IN(8)
            TRE = ((0.2*(TSKY**4))+(0.41*(IN(7)**4))
     &      +(0.39*(IN(7)**4)))**(1.0/4.0)
C           Outside heat transfer coefficient
            HCOWIND = 4 + 4 * IN(9)
            HCOFREE = 1.78 * ABS(TOLD(NLAY+1,1) - IN(7))**(1.0/3.0)
            HCO     = (HCOWIND**3 + HCOFREE**3)**(1.0/3.0)
C           HCO = IN(10)
            HRO     = SIGMA * EPSILON
     &              * ABS((TRE+273.15)**4 - (TOLD(NLAY+1,1)+273.15)**4)
            GSOLAR  = IN(6)
            TA      = IN(7)
            TEMP(NLAY+1,1) = (C1R * TOLD(NLAY+1,1) / DT + QLEFT + GSOLAR
     &             + HCO * TA + HRO * TRE)
     &             / (C1R / DT + HRO + HCO)
         ELSE
            TEMP(NLAY+1,1) = TOLD(NLAY+1,1) + DT / C1R
     &                     * (QLEFT + IN(6) + IN(7) + TRE + IN(9))
            ELOSS = 0.0
         END IF
         IF (ABS(TEMP(NLAY+1,1) - TOLD(NLAY+1,1)) .GT. DELTATMAX) THEN
!           DTNEW = DTNEW / 2.0
            HALFDT = .TRUE.
!           EXIT
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
C-----LOOP if surface temperature changes by more than .. kelvin-------
      IF (HALFDT) THEN
         !print*,"WARNING HALFDT nicht aktiviert"
         !PRINT *,T
         !GO TO 11111
      END IF
C-----LOOP if surface temperature changes by more than .. kelvin-------
      OUT(1) = TEMP(1,1)
      OUT(2) = TEMP(NLAY+1,1)
      IF (IP(5) .EQ. 10) THEN
         X    = IN(10)
         ILAY = 0
         XLAY = 0.0
         DO I = 1,NLAY
            XLAY = XLAY + THICK(I)
            IF (XLAY .GE. (X-1.E-5)) THEN
               XLAY = XLAY - THICK(I)
               ILAY = I
               EXIT
            END IF
         END DO
         XDIV = XLAY
         DO I = 1,NDIV(ILAY)
            XDIV = XDIV + DELTAX(ILAY)
            IF (XDIV .GE. (X-1.E-5)) THEN
               XDIV = XDIV - DELTAX(ILAY)
               JLAY = I
               EXIT
            END IF
         END DO
         IF (NDIV(ILAY) .GT. 1.AND. NDIV(ILAY) .GT. JLAY) THEN
            TEMPX = TEMP(ILAY,JLAY) +
     &             (TEMP(ILAY,JLAY+1) - TEMP(ILAY,JLAY)) /
     &             DELTAX(ILAY) * (X - XDIV)
         ELSE
            TEMPX = TEMP(ILAY,JLAY) +
     &             (TEMP(ILAY+1,1) - TEMP(ILAY,JLAY)) /
     &             DELTAX(ILAY) * (X - XDIV)
         END IF
         OUT(3) = TEMPX
      END IF
      !OUT(5) = ELOSS
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
      !OUT(6) = (ETOT - EOLD) / 3600.0 - ELOSS
      !do i = 1,13
      !   out(100+i) = rp(3+nlay+i)
      !end do
      RETURN
      END

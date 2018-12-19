C-----------------------------------------------------------------------
C #Begin
C #Block TNKST2
C #Description
C    The TNKST2 block simulates a stratified storage tank which can be
C    used either as hot-water storage tank (case 1) or as cold-water
C    storage tank (case 2). The original model (TANKST) has been
C    modified to allow the operation of the Tank with a second heat/cold
C    source.
C #Layout
C    #Inputs      11
C    #Outputs     $N+1$
C    #Parameters  8
C    #Strings     0
C    #Group       D
C #Details
C    #Inputs
C       #IN(1)   Supply temperature heat source 1 $T_{\mrm h,s}$/ \degC:
C                Case 1 (Hot-water storage tank): From collector/boiler
C                Case 2 (Cold-water storage tank): From load
C       #IN(2)   Heat source 1 mass flow rate $\dot{m}_{\mrm h}$
C                / kg\,s$^{-1}$
C       #IN(3)   Supply temperature heat source 2 $T_{\mrm h,s}$/ \degC:
C                Case 1 (Hot-water storage tank): From collector/boiler
C                Case 2 (Cold-water storage tank): From load
C       #IN(4)   Heat source 2 mass flow rate $\dot{m}_{\mrm h}$
C                / kg\,s$^{-1}$
C       #IN(5)   Return temperature cold source 1 $T_{\mrm l,r}$/ \degC:
C                Case 1 (Hot-water storage tank): From load
C                Case 2 (Cold-water storage tank): From chiller
C       #IN(6)   Cold source 1 mass flow rate $\dot{m}_{\mrm l}$
C                / kg\,s$^{-1}$
C       #IN(7)   Return temperature cold source 2 $T_{\mrm l,r}$/ \degC:
C                Case 1 (Hot-water storage tank): From load
C                Case 2 (Cold-water storage tank): From chiller
C       #IN(8)   Cold source 2 mass flow rate $\dot{m}_{\mrm l}$
C                / kg\,s$^{-1}$
C       #IN(9)   Environment temperature $T_{\mrm e}$ / \degC
C       #IN(10)  Time / s
C       #IN(11)  srcs
C    #Outputs
C       #OUT(1)  Temperature of node 1 (top) $T_1$ / \degC
C       #OUT(2)  Temperature of node 2 $T_2$ / \degC
C       #OUT(N)  Temperature of node $N$ (bottom) $T_N$ / \degC
C       #OUT(N+1)  Energy content of the storage $Q$ / J
C    #Parameters
C       #BP(1)   Tank volume $V$ / m$^3$
C       #BP(2)   Number of temperature nodes $N \le 200$
C       #BP(3)   Tank diameter $d$ / m
C       #BP(4)   Specfic heat of fluid
C                $c_{\mrm p}$ / J\,kg$^{-1}$\,K$^{-1}$
C       #BP(5)   Fluid density $\rho$ / kg\,m$^{-3}$
C       #BP(6)   Overall heat-loss coefficient
C                $U$ / W\,m$^{-2}$\,K$^{-1}$
C       #BP(7)   Effective heat conductivity $\lambda_{\mrm eff}$
C                / W\,m$^{-1}$\,K$^{-1}$
C       #BP(8)   Initial tank temperature $T_0$ / \degC
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
C       #IP(11) First call switch
C       #IP(12) Integer of BP(2)
C    #Reals
C       #RP(1-200) Temperatur der Nodes = TNode(1-200)
C       #RP(201) Zeit des letzten Aufrufs = TLA
C    #Doubles
C       #None
C #Dependencies
C    #Intrinsic function ABS
C    #Intrinsic function MAX
C    #Subroutine ID
C #Authors
C    Ursula Eicker
C    Carsten Hoyer (Basic version: TCSTOR)
C    Dirk Pietruschka
C    Juergen Schumacher
C    Ruben Pesch
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0005(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'UBTNKST4'
     &,                OPM    = 1
     &,                INMIN  = 10
     &,                INS    = 10
     &,                OUTS   = 201
     &,                IPS    = 12
     &,                RPS    = 201
     &,                DPS    = 0
     &,                BPMIN  = 12
     &,                BPS    = 12
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 5)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER          TCI1, TCI2, TCI(2), MC1, MC2, MC(2), TLI1, TLI2,
     &                 TLI(2), ML1, ML2, ML(2), TA, T, V,
     &                 N, D, CP, RHO, U, LAEFF, SRCS
      PARAMETER        (TCI1 = 1, TCI2 = 5, MC1 = 2, MC2 = 6,
     &                  TLI1 = 3, TLI2 = 7, ML1 = 4, ML2 = 8,
     &                  TA = 9, T = 10, V = 1, D = 3, CP = 4, RHO = 5,
     &                  U = 6, LAEFF = 7, N = 2)
      REAL             TLA, HLSWITCH
      REAL             TNODE(200), TNODET(200)
      INTEGER          I,IS,J,K,MTS, nnode1, nnode2, nnode3, nnode4
      REAL             TS
      REAL             Set
      REAL             NMASS
      REAL             ASNODE
      REAL             ANODE,HNODE
      REAL             PI /3.141592654/
      REAL             DELTAT
      REAL             TM
      REAL             Qverlust(200),Qv
      REAL             NETM,MDOTC,MDOTL,DELTAM
      REAL             QSV,QSN,QSNIS,QIN,QOUT,QS,QS1,QS2
      REAL             MASSIN
      INTEGER          NTS
      INTEGER          DELTAIK, DELTAIL, ZAEHLIK, ZAEHLIL
      INTEGER          DELTADOWN, DELTAUP
      INTEGER          INLOAD, INCOL
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(11)  = 0
            IP(12)  = ANINT(BP(2))
C           Set nodes to initial temperature

            nnode1 = AINT(BP(2)/4)
            nnode2 = AINT(BP(2)/4*2)
            nnode3 = AINT(BP(2)/4*3)
            nnode4 = AINT(BP(2))

            DO K = 1,nnode1
               RP(K) = BP(8)
            END DO
            DO K = nnode1, nnode2
               RP(K) = BP(9)
            END DO
            DO K = nnode2, nnode3
               RP(K) = BP(10)
            END DO
            DO K = nnode3, nnode4
               RP(K) = BP(11)
            END DO


            DO K = 1,nnode1
               OUT(K) = BP(8)
            END DO
            DO K = nnode1, nnode2
               OUT(K) = BP(9)
            END DO
            DO K = nnode2, nnode3
               OUT(K) = BP(10)
            END DO
            DO K = nnode3, nnode4
               OUT(K) = BP(11)
            END DO


C           Initial heat content
         OUT(IP(12)+1)=BP(1)*BP(4)*BP(5)*(BP(8)) !+BP(9)+BP(10)+BP(11))/4)
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
C     Time of last call
      IF (IP(11) .EQ. 0) THEN
         IP(11) = 1
         TLA    = IN(T)
      ELSE
         TLA = RP(201)
      END IF
      RP(201) = IN(T)
C     Time since last call
      TS = IN(T) - TLA
      IF (TS .LE. 0.0) THEN
C        Reset temperature data
         DO K = 1,IP(12)
            TNODE(K)  = RP(K)
            TNODET(K) = RP(K)
         END DO
      ELSE
C        New time step
         DO K = 1,IP(12)
            TNODE(K)  = OUT(K)
            TNODET(K) = OUT(K)
         END DO
         DO K = 1,IP(12)
            RP(K) = TNODE(K)
         END DO
      END IF
      TCI(1) = TCI1
      TCI(2) = TCI2
      MC(1)  = MC1
      MC(2)  = MC2
      TLI(1) = TLI1
      TLI(2) = TLI2
      ML(1)  = ML1
      ML(2)  = ML2
C     Mass of one layer
      NMASS = BP(V) / BP(N) * BP(RHO)
C     Side area of one node
      ASNODE = BP(V) / (BP(N) * 0.25 * BP(D))
C     Top area of one node
      ANODE =  PI * BP(D) * BP(D) / 4.0
      HNODE = BP(V) / ANODE / BP(N)
      SRCS = 2
      if (in(MC2) .eq. 0.0) then
         if (in(ML2) .eq. 0.0) then
            SRCS = 1
         end if
      end if
      DO IS = 1,SRCS
         hlswitch = 1.0
         if (SRCS .eq. 2) then
            if (IS .eq. 1) then
               hlswitch = 0.0
            end if
         end if
C        Net mass flow between layers
         NETM = IN(MC(IS)) - IN(ML(IS))
C        Number of internal time steps
         NTS = 1
         MASSIN = MAX(ABS(IN(MC(IS))*TS),ABS(IN(ML(IS))*TS))

         IF (ABS(MASSIN) .GT. NMASS) THEN
            NTS = INT(ABS(MASSIN)/NMASS) + 1
            TS  = TS / NTS
         END IF
         MTS = 1
         IF (TS .GT. 360.0) THEN
C           Restrict time step to about 360 seconds maximum
            MTS = ANINT(TS / 360.0)
            TS  = TS / MTS
         END IF
         QIN  = 0.0
         QOUT = 0.0
         DO I = 1,MTS
            DO J = 1,NTS
               QSV = 0
               DO K = 1,IP(12)
                  QSV = QSV + NMASS * BP(CP) * TNODET(K)
               END DO
               QS1 = IN(MC(IS)) * BP(CP)* (IN(TCI(IS)) - TNODET(10))* TS
               QS2 = IN(ML(IS)) * BP(CP)* (IN(TLI(IS)) - TNODET(1)) * TS
               QS  = QSV + QS1 + QS2
               ZAEHLIK = 0
               ZAEHLIL = 0
               INCOL   = 0
               INLOAD  = 0
               DO K = 1,IP(12)
                  IF (IN(TCI(IS)) .GE. TNODE(K) .AND. ZAEHLIK .EQ.0)THEN
                     INCOL   = K
                     ZAEHLIK = 1
                  END IF
                  IF (IN(TLI(IS)) .GE. TNODE(K) .AND. ZAEHLIL .EQ.0)THEN
                     INLOAD  = K
                     ZAEHLIL = 1
                  END IF
               END DO
               IF (INCOL  .EQ. 0) INCOL  = IP(12)
               IF (INLOAD .EQ. 0) INLOAD = IP(12)
               DO K = 1,IP(12)
                  MDOTC     = 0.0
                  MDOTL     = 0.0
                  DELTAUP   = 0
                  DELTADOWN = 0
                  DELTAIL   = 0
                  DELTAIK   = 0
                  IF (INCOL .NE. K .AND. INLOAD .NE. K) THEN
C                    Layer without direct input
                     IF (INCOL  .LT. K) MDOTC = IN(MC(IS))
                     IF (INLOAD .GT. K) MDOTL = IN(ML(IS))
                     DELTAM = MDOTC - MDOTL
                     IF (DELTAM .GT. 0.0) DELTADOWN = 1
                     IF (DELTAM .LT. 0.0) DELTAUP   = 1
                  END IF
                  IF (INCOL .EQ. K .AND. INLOAD .NE. K) THEN
C                    Layer with collector input
                     DELTAIK = 1
                     IF (INLOAD .GT. K) MDOTL = IN(ML(IS))
                     DELTAM = IN(MC(IS)) - MDOTL
                     IF (DELTAM .LT. 0.0) DELTAUP = 1
                  END IF
                  IF (INCOL .NE. K .AND. INLOAD .EQ. K) THEN
C                    Layer with load input
                     DELTAIL = 1
                     IF (INCOL .LT. K) MDOTC = IN(MC(IS))
                     DELTAM = MDOTC - IN(ML(IS))
                     IF (DELTAM .GT. 0.0) DELTADOWN = 1
                  END IF
                  IF (INCOL .EQ. K .AND. INLOAD .EQ. K) THEN
C                    Layer with collector and load input
                     DELTAIK = 1
                     DELTAIL = 1
                     DELTAM  = 0.0
                  END IF
                  NETM = DELTAM
                  TM   = TNODE(K)
                  IF (K .GT. 1 .AND. K .LT. IP(12)) THEN

                  IF (TS .LT. 1) THEN
                  Set = 0
                  Else
                  Set = 1
                  End IF

                  Qverlust(K) = BP(U)*ASNODE*(IN(TA)-TM)*Set

                     DELTAT = (
     &                  BP(U) * ASNODE * (IN(TA) - TM) / BP(CP)
     &                  * hlswitch
     &                  + DELTAIK   * IN(MC(IS))  * (IN(TCI(IS)) - TM)
     &                  + DELTAUP   * NETM    * (TM - TNODE(K+1))
     &                  + DELTADOWN * NETM    * (TNODE(K-1) - TM)
     &                  - DELTAIL   * IN(ML(IS))  * (TM - IN(TLI(IS)))
     &                  - ANODE * BP(LAEFF) / HNODE / BP(CP)
     &                          * (TM - TNODE(K-1))* hlswitch
     &                  - ANODE * BP(LAEFF) / HNODE / BP(CP)
     &                          * (TM - TNODE(K+1))* hlswitch
     &                  ) * TS / NMASS
                  ELSE IF (K .EQ. 1) THEN

                IF (TS .LT. 1) THEN
                Set = 0
                Else
                Set = 1
                End IF

                Qverlust(K) = BP(U)*(ASNODE+ANODE)*(IN(TA)-TM)*Set

                     DELTAT = (
     &                  BP(U) * (ASNODE + ANODE) * (IN(TA) - TM) /BP(CP)
     &                  * hlswitch
     &                  + DELTAIK   * IN(MC(IS))  * (IN(TCI(IS)) - TM)
     &                  + DELTAUP   * NETM    * (TM - TNODE(2))
     &                  - DELTAIL   * IN(ML(IS))  * (TM - IN(TLI(IS)))
     &                  - ANODE     * BP(LAEFF) / HNODE
     &                     / BP(CP) * (TM - TNODE(2))* hlswitch
     &                  ) * TS / NMASS

                  ELSE

               IF (TS .LT. 1) THEN
               Set = 0
               Else
               Set = 1
               End IF
               Qverlust(K) = BP(U)*(ASNODE+ANODE)*(IN(TA)-TM)*Set

                     DELTAT = (
     &                  BP(U) * (ASNODE + ANODE) * (IN(TA) - TM) /BP(CP)
     &                  * hlswitch
     &                  + DELTAIK   * IN(MC(IS))  * (IN(TCI(IS)) - TM)
     &                  + DELTADOWN * NETM    * (TNODE(K-1) - TM)
     &                  - DELTAIL   * IN(ML(IS))  * (TM - IN(TLI(IS)))
     &                  - ANODE     * BP(LAEFF) / HNODE
     &                     / BP(CP) * (TM - TNODE(K-1))* hlswitch
     &                  ) * TS / NMASS
                  END IF
                  TNODET(K) = TNODE(K) + DELTAT
               END DO
               DO K = 1,IP(12)
                  TNODE(K) = TNODET(K)
               END DO
               QSN = 0
               DO K = 1,IP(12)
                  QSN = QSN + NMASS * BP(CP) * TNODET(K) ! J
               END DO
               IF (IS .EQ. 1) THEN
                  QSNIS = 0
               END IF
               QSNIS = QSNIS + QSN
C              check Bilanz
C              print*,"ST ",IP(12),QS,QSN,QS-QSN
            END DO
         END DO
      END DO
      DO K = 1,IP(12)
         OUT(K) = TNODET(K)
         Qv = (Qv + Qverlust(K))*hlswitch
      END DO
      OUT(IP(12)+1) = QSNIS
      OUT(IP(12)+2) = (Qv*(-1))
      Qv = 0

      RETURN
      END
C-----------------------------------------------------------------------

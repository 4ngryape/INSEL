C-----------------------------------------------------------------------
C #Begin
C #Block UBPIPE1
C #Description
C    fully mixed pipe model
C    end T calculated by mixing medium masses of input and initial T
C    heat losses are considered
C    models acts as PT1 Term (one storage unit)
C #Layout
C  #Inputs      4
C  #Outputs     2
C  #Parameters  6
C  #Strings     0
C  #Group       S
C #Details
C  #Inputs
C     #IN(1) time in sec
C     #IN(2) mdot in kg/s
C     #IN(3) Tinput in °C
C     #IN(4) Tamb in °C
C  #Outputs
C     #OUT(1) Toutput in °C
C     #OUT(2) Qloss in W
C  #Parameters
C     #BP(1) L
C     #BP(2) di
C     #BP(3) UA
C     #BP(4) T0
C     #BP(5) rho
C     #BP(6) cp
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
C   Ilyes Ben Hassine
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0019 (IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        GROUP,OPM
      PARAMETER (BNAMES = 'UBPIPE2'
     &,          OPM   = 1
     &,          INMIN = 4
     &,          INS   = 4
     &,          OUTS  = 2
     &,          IPS   = 12
     &,          RPS   = 999
     &,          DPS   = 0
     &,          BPMIN = 6
     &,          BPS   = 6
     &,          SPMIN = 0
     &,          GROUP = 3
     &,          SPS   = 0)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
     &,t,mdot,Tin,Tamb,di,UA,T0,rho,cp,Tout,Qloss
     &,Tres,m,dt,m_in,m_0,Pi,surf,Qlossold,mrohr,cprohr,lambdafluid
     &,Afluid, Qgain, lambdarohr, Arohr
      INTEGER J, L
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
                  !    assign all parameters and inputs
      t                = IN(1)
      mdot             = IN(2)
      Tin              = IN(3)
      Tamb             = IN(4)

      L                = BP(1)
      di               = BP(2)
      UA               = BP(3)
      T0               = BP(4)
      rho              = BP(5)
      cp               = BP(6)
      Qlossold         = 0
      Pi               = 3.14159265359
      m                = rho*pi*di*di/4
      mrohr            = 2
      cprohr           = 385
      lambdafluid      = 0.650
      Afluid           = 0.0035
      lambdarohr       = 350
      Arohr            = 0.0035

C
      surf             = Pi*di
C

      J = 1

      IF (IP(11) .EQ. 0) THEN
      !     Initialising
         RP(1)  = t
         IP(11) = 1
         DO J = 1, L
            RP(j+1) = T0
         IF (J .EQ. l) EXIT
         END DO
         OUT(1) = T0
         OUT(2) = 0.0
      END IF

      IF (IP(11) .GT. 0) THEN
       dt              = t-RP(1)
         DO J = 1, L
            IF ((J .EQ. 1) .and. (mdot .LE. 0.002)) THEN

               ! standby conditions
               Tin   = in(3)

               Qloss = UA*surf*(RP(j+1)-Tamb)
          Qgain = (lambdafluid*Afluid+lambdarohr*Arohr)*(Tin-RP(j+1))
               Tout  = RP(j+1)-(Qloss*dt-Qgain*dt)/
     &         (m*cp+mrohr*cprohr)
               Qlossold = Qloss + Qlossold
               RP(j+1)         = Tout
               Tin             = Tout

            ELSE IF ((J .GT. 1) .and. (mdot .LE. 0.002)) THEN
               ! standby conditions


               Qloss = UA*surf*(RP(j+1)-Tamb)
          Qgain = (lambdafluid*Afluid+lambdarohr*Arohr)*(Tin-RP(j+1))
               Tout  = RP(j+1)-(Qloss*dt-Qgain*dt)/
     &         (m*cp+mrohr*cprohr)
               Qlossold = Qloss + Qlossold
               RP(j+1)         = Tout
               Tin             = Tout


            END IF

            IF ((J .EQ. 1) .and. (mdot .gt. 0.002)) THEN


               m_in            = mdot*dt
               m_in            = AMIN1(m_in,m)
               m_0             = m-m_in
               Tres            = (m_in*Tin+m_0*RP(j+1))/m
               Qloss           = UA*surf*(Tres-Tamb)
               Tout            = Tres-(Qloss*dt)/
     &                           (m*cp+mrohr*cprohr)

               RP(j+1)         = Tout
               Qlossold        = Qloss + Qlossold
            END IF

            IF ((J .GT. 1) .and. (mdot .gt. 0.002)) THEN


               m_in            = mdot*dt
               m_in            = AMIN1(m_in,m)
               m_0             = m-m_in
               Tres            = (m_in*Tin+m_0*RP(j+1))/m
               Qloss           = UA*surf*(Tres-Tamb)
               Tout            = Tres-(Qloss*dt)/(m*cp+mrohr*cprohr)

               RP(j+1)         = Tout
               Tin             = Tout
               Qlossold        = Qloss + Qlossold
            END IF

      IF (J .EQ. l) EXIT
      END DO

      RP(1)              = t
      OUT(1)=Tout
      OUT(2)=Qlossold

      END IF

      RETURN
      END
C-----------------------------------------------------------------------


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
      SUBROUTINE UB0015(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT NONE
      CHARACTER*1024 BNAMES
      INTEGER INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &        group,OPM
      PARAMETER (BNAMES = 'UBPIPE1'
     &,          OPM   = 1
     &,          INMIN = 4
     &,          INS   = 4
     &,          OUTS  = 2
     &,          IPS   = 12
     &,          RPS   = 4
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
     &,t,mdot,Tin,Tamb,L,di,UA,T0,rho,cp,Tout,Qloss
     &,Tres,m,dt,m_in,m_0,Pi,surf       
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
      t       = IN(1)
      mdot             = IN(2)
      Tin                  = IN(3)
      Tamb             = IN(4)
                  
      L       = BP(1)
      di       = BP(2)
      UA       = BP(3)
      T0         = BP(4)
      rho       = BP(5)
      cp         = BP(6)
                  
      Pi          =3.14159265359                                 
      m           =rho*L*Pi*di*di/4         !mass of fluid within pipe
      surf       =L*Pi*(di) !+0.002
                  
      IF (IP(11) .EQ. 0) THEN
      !     Initialising                
      IP(11)    =1
                  
      RP(1)  =t                                  !store time
      RP(2)  =T0                                 !store current T
                  
      OUT(1)=T0
      OUT(2)=0.0
                  
      ELSE
                  
        IF (mdot .LT. 0.002) THEN
         ! standby conditions
         dt                  =t-RP(1)
         Qloss           =UA*surf*(RP(2)-Tamb)             
         Tout             =RP(2)-(Qloss*dt)/(m*cp)

        ELSE
         dt                  =t-RP(1)
         m_in            =mdot*dt
         m_in            =AMIN1(m_in,m)
         m_0             =m-m_in
         Tres              =(m_in*Tin+m_0*RP(2))/m
         Qloss           =UA*surf*(Tres-Tamb)               
         Tout             =Tres-(Qloss*dt)/(m*cp)                           
        END IF
                  
      RP(1)              =t
      RP(2)              =Tout
                  
      OUT(1)=Tout
      OUT(2)=Qloss             
                               
      END IF

      RETURN
      END
C-----------------------------------------------------------------------



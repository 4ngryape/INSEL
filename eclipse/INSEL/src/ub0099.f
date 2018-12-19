C-----------------------------------------------------------------------
C #Begin
C #Block FBH
C #Description
C    Fuﬂbodenheizung gem. DIN 126-2
C #Layout
C    #Inputs      3
C    #Outputs     2
C    #Parameters  12
C    #Strings     0
C    #Group       3
C #Details
C    #Inputs
C   #IN(1) T_Vorlauf [Celsius]
C	#IN(2) T_Rauminnen [Celcius]
C   #IN(3) Massenstrom [kg/s]
C   #IN(4) T_Ruecklauf Error [Celcius]
C    #Outputs
C       #OUT(1) T_Ruecklauf [Celcius]
C        #OUT(1) Heizleistung [Watt]
C    #Parameters
C       #BP(1) aT [-]
C       #BP(2) aU [-]
C       #BP(3) aD [-]
C       #BP(4) mT [-]
C       #BP(5) mU [-]
C       #BP(6) mD [-]
C       #BP(7) Bx [-]
C       #BP(8) LambdaE [-]
C       #BP(9)) Rtb [-]
C       #bp(10) Heizleistung Error [Watt]
C       #bp(11) Iterationsschitt [-]
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
C    #Reals
C       #None
C    #Doubles
C       #None
C #Dependencies
C    #Subroutine ID
C #Authors
C    Marcus Brennenstuhl
C    Ruben Pesch
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0099(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      double precision x, y, z
      PARAMETER       (BNAMES = 'UBFBH'
     &,                OPM    = 1
     &,                INMIN  = 4
     &,                INS    = 4
     &,                OUTS   = 2
     &,                IPS    = 10
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 11
     &,                BPS    = 11
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1), T_V,
     &                 T_I, M_DOT, delta
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
c---- Standard call ----------------------------------------------------
       T_V = in(1)
       T_I = in(2)
       M_DOT = in(3)
       Z = RP(1)
       x = 1
       y = 2

       do while (nint (x) .ne. nint (y))

       IF (X .EQ. 1) THEN

                x = M_DOT*(T_V-Z)*4190 !gem. DIN 1264-2
                y = ((1/10.8+0.045/1)/(1/10.8+0.045/BP(8)+
     &          BP(9))*BP(7)*BP(1)**BP(4)*BP(2)**BP(5)*BP(3)**BP(6))*
     &          (T_V-Z)/DLOG((T_V-T_I)/(Z-T_I))
                delta = abs(x - y)

                Z = Z - BP(11)
                x = M_DOT*(T_V-Z)*4190 !gem. DIN 1264-2
                y = ((1/10.8+0.045/1)/(1/10.8+0.045/BP(8)+
     &          BP(9))*BP(7)*BP(1)**BP(4)*BP(2)**BP(5)*BP(3)**BP(6))*
     &          (T_V-Z)/DLOG((T_V-T_I)/(Z-T_I))
       END IF
       IF ((abs(x + y)) .LT. delta) THEN
                Z = Z - BP(11)
                    IF (Z .LT. 0) THEN
                    OUT(1)= in(4)
                    OUT(2)= bp(10)
                    RP(1) = 0
                    EXIT
                    END IF
                x = M_DOT*(T_V-Z)*4190 !gem. DIN 1264-2
                y = ((1/10.8+0.045/1)/(1/10.8+0.045/BP(8)+
     &          BP(9))*BP(7)*BP(1)**BP(4)*BP(2)**BP(5)*BP(3)**BP(6))*
     &          (T_V-Z)/DLOG((T_V-T_I)/(Z-T_I))

       ELSE
               Z = Z + BP(11)
                    IF (Z .GT. 60) THEN
                    OUT(1)= in(4)
                    OUT(2)= bp(10)
                    RP(1) = 0
                    EXIT
                    END IF
                x = M_DOT*(T_V-Z)*4190 !gem. DIN 1264-2
                y = ((1/10.8+0.045/1)/(1/10.8+0.045/BP(8)+
     &          BP(9))*BP(7)*BP(1)**BP(4)*BP(2)**BP(5)*BP(3)**BP(6))*
     &          (T_V-Z)/DLOG((T_V-T_I)/(Z-T_I))

      END IF
      END DO


      OUT(1)= z
      OUT(2)= x

      RP(1) = Z
      END
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C #Begin
C #Block POLYG2
C #Description
C    The POLYG2 block interpolates on a two dimensional polygon of
C    points as defined by parameters.
C #Layout
C    #Inputs      2
C    #Outputs     1
C    #Parameters  $8 \ldots [1502]$
C    #Strings     0
C    #Group       S
C #Details
C    #Inputs
C       #IN(1)  Any signal $\alpha$
C       #IN(2)  Any signal $\beta$
C    #Outputs
C       #OUT(1) Interpolated value $\gamma$
C    #Parameters
C       #BP(1)  Interpolation mode $m$, determines which of the
C               parameters defining the polygon is interpreted as
C               dependant variable $\gamma$
C               \begin{detaillist}
C                  \item[0] $\gamma = y(p,x) = y(\alpha,\beta)$
C                  \item[1] $\gamma = p(x,y) = p(\alpha,\beta)$
C                  \item[2] $\gamma = x(p,y) = x(\alpha,\beta)$
C               \end{detaillist}
C       #BP(2)  Number $n$ of given data triples, $2 \leq n \leq 200$
C       #BP(3)  Parameter $p_1$ of $1^{\rm st}$ point
C       #BP(4)  $x$-coordinate $x_1$ of $1^{\rm st}$ point
C       #BP(5)  y-coordinate $y_1$ of $1^{\rm st}$ point
C       #BP(6)  Parameter $p_2$ of $2^{\rm nd}$ point
C       #BP(7)  $x$-coordinate $x_2$ of $2^{\rm nd}$ point
C       #BP(8)  $y$-coordinate $y_2$ of $2^{\rm nd}$ point
C       #BP(...)
C       #BP(3$n$) Parameter $p_n$ of $n^{\rm th}$ point
C       #BP(3$n$+1) $x$-coordinate  $x_n$ of $n^{\rm th}$ point
C       #BP(3$n$+2) $y$-coordinate $y_n$ of $n^{\rm th}$ point
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
C       #IP(11) Interpolation mode
C    #Reals
C       #Later
C    #Doubles
C       #Later
C #Dependencies
C    #Intrinsic function ABS
C    #Subroutine ID
C    #Subroutine MSG
C #Authors
C    Juergen Schumacher
C #End
C-----------------------------------------------------------------------
      SUBROUTINE FB0049(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM
      PARAMETER       (BNAMES = 'POLYG2'
     &,                OPM    = 1
     &,                INMIN  = 2
     &,                INS    = 2
     &,                OUTS   = 1
     &,                IPS    = 15
     &,                RPS    = 10
     &,                DPS    = 0
     &,                BPMIN  = 8
     &,                BPS    = 1502
     &,                SPMIN  = 0
     &,                SPS    = 0
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1)
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1)
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1)
      INTEGER          I
      REAL             R1,R2
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1) THEN
C           Constructor call
            IP(11) = 1
            IF (IP(3) .LT. 0) IP(11) = IP(11) - 1 ! Simulink case
            IF (IP(11) .EQ. 0) IP(11) = 3
            IF (IP(11) .LT. 1 .OR. IP(11) .GT. 3) THEN
C              Invalid interpolation mode
               IP(1) = 305113
               CALL MSG(IP,RP,SP)
            END IF
            IP(12) = 48 !ANINT(BP(2))
            IF (IP(12) .GT. (BPS - 2) / 3) THEN
C              Too many base points
               IP(1) = 305114
               CALL MSG(IP,RP,SP)
            END IF
C           Make sure the p axis is increasing
            DO I = 1,IP(12)-1
               IF (BP(3*I) .GT. BP(3*(I+1))) THEN
C                 Non increasing p values
                  IP(1) = 305157
                  CALL MSG(IP,RP,SP)
                  EXIT
               END IF
            END DO
            IF ((IP(11) .EQ. 1) .OR. (IP(11) .EQ. 3)) THEN
C              Make sure the x axis is increasing for equal p's
               DO I = 1,IP(12)-1
                  IF ((BP(3*I + 1) .GT. BP(3*(I+1) + 1)) .AND.
     &                (BP(3*I) .EQ. BP(3*(I+1)))) THEN
C                    Non increasing x values
                     IP(1) = 305115
                     CALL MSG(IP,RP,SP)
                     EXIT
                  END IF
               END DO
            END IF
            IF (IP(11) .EQ. 2) THEN
C              Make sure the y axis is increasing for equal p's
               DO I = 1,IP(12)-1
                  IF ((BP(3*I + 2) .GT. BP(3*(I+1) +2) ) .AND.
     &                (BP(3*I) .EQ. BP(3*(I+1)))) THEN
C                    Non increasing y values
                     IP(1) = 305158
                     CALL MSG(IP,RP,SP)
                     EXIT
                  END IF
               END DO
            END IF
C           Make sure the rest of the parameters is zero
C            DO I = 3*IP(12)+3,IBPS
            DO I = 3*IP(12)+3,BPS
               IF (ABS(BP(I)) .GT. 1.E-35) THEN
C                 Number of data points inconsistent
                  IP(1) = 305116
                  CALL MSG(IP,RP,SP)
                  EXIT
               END IF
            END DO
         ELSE
C           Destructor call
            IF (IP(15) .GT. 0) THEN
C              Display number of calls with invalid input
               IP(9)  = IP(15)
               IP(1) = 205048
               CALL MSG(IP,RP,SP)
            END IF
         END IF
         RETURN
      END IF
C---- Standard call ----------------------------------------------------
      GO TO (1,2,3) IP(11)
1     CONTINUE
      IP(13) = 3
      RP(1)  = BP(3)
100   CONTINUE
      IF (BP(IP(13)+3) .EQ. RP(1)) THEN
         IF (IN(1) .LE. BP(IP(13)+4)) THEN
            RP(3) = BP(IP(13)+1)
            RP(4) = BP(IP(13)+2)
            RP(5) = BP(IP(13)+4)
            RP(6) = BP(IP(13)+5)
         ELSE
            IP(13) = IP(13) + 3
            GO TO 100
         END IF
      ELSE
         RP(3) = BP(IP(13)-2)
         RP(4) = BP(IP(13)-1)
         RP(5) = BP(IP(13)+1)
         RP(6) = BP(IP(13)+2)
      END IF
      R1 = RP(4) + (RP(6)-RP(4)) * (IN(1)-RP(3)) / (RP(5)-RP(3))
101   CONTINUE
      IF (BP(IP(13)) .EQ. RP(1)) THEN
         IP(13) = IP(13) + 3
         GO TO 101
      ELSE
         IF (IP(13) .LE. 3*IP(12)) THEN
            RP(2) = BP(IP(13))
         ELSE
            RP(2) = BP(IP(13))
            IF (IP(15) .EQ. 0) THEN
C              Input out of range
               IP(15) = 1
               IP(1) = 305117
               CALL MSG(IP,RP,SP)
            ELSE
               IP(15) = IP(15) + 1
            END IF
            RETURN
         END IF
      END IF
110   CONTINUE
      IF (BP(IP(13)+3) .EQ. RP(2)) THEN
         IF (IN(1) .LE. BP(IP(13)+4)) THEN
            RP(7)  = BP(IP(13)+1)
            RP(8)  = BP(IP(13)+2)
            RP(9)  = BP(IP(13)+4)
            RP(10) = BP(IP(13)+5)
         ELSE
            IP(13) = IP(13) + 3
            GO TO 110
         END IF
      ELSE
         RP(7)  = BP(IP(13)-2)
         RP(8)  = BP(IP(13)-1)
         RP(9)  = BP(IP(13)+1)
         RP(10) = BP(IP(13)+2)
      END IF
      R2 = RP(8) + (RP(10)-RP(8)) * (IN(1)-RP(7)) / (RP(9)-RP(7))
      IF ((IN(2) .GE. R1 .AND. IN(2) .LE. R2) .OR.
     &    (IN(2) .LE. R1 .AND. IN(2) .GE. R2)) THEN
         OUT(1) = RP(1) + (RP(2)-RP(1)) * (IN(2)-R1) / (R2-R1)
      ELSE
         RP(1) = RP(2)
         RP(3) = RP(7)
         RP(4) = RP(8)
         RP(5) = RP(9)
         RP(6) = RP(10)
         R1    = R2
         GO TO 101
      END IF
      RETURN
2     CONTINUE
C     x=f(p,y)
C     at first, find the I with P(I+1) > P(I) and IN(1) < P(I+1)
      IP(13) = 3   ! pointer to start of P(i1)
      IP(14) = 3   ! pointer to start of P(i2) with IN(1) < P(i2)
      RP(1)  = BP(3)
      RP(2)  = BP(3) ! current Pi
      DO I = 6,3*IP(12),3
         IF (BP(I) .NE. RP(2)) THEN
            IP(13) = IP(14)
            RP(1)  = RP(2)
            IP(14)  = I ! here starts P2
            RP(2)  = BP(I)
            IF (IN(1) .LE. BP(I)) EXIT
         END IF
      END DO
      IF (IP(13).EQ.IP(14)) GOTO 210 ! only one p-curve given
      DO I = IP(13)+2,IP(14)-4,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. IP(14)-4) THEN
            RP(3) = BP(I)
            RP(4) = BP(I-1)
            RP(5) = BP(I+3)
            RP(6) = BP(I+2)
            EXIT
         END IF
      END DO
      DO I = IP(14)+2,3*IP(12)-1,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. 3*IP(12)-1 .OR.
     &       RP(2) .NE. BP(I+4) ) THEN
            RP(7)  = BP(I)
            RP(8)  = BP(I-1)
            RP(9)  = BP(I+3)
            RP(10) = BP(I+2)
            EXIT
         END IF
      END DO
      R1 = RP(4) + (RP(6)-RP(4))  * (IN(2)-RP(3)) / (RP(5)-RP(3))
      R2 = RP(8) + (RP(10)-RP(8)) * (IN(2)-RP(7)) / (RP(9)-RP(7))
      OUT(1) = R1 + (R2-R1) * (IN(1)-RP(1)) / (RP(2)-RP(1))
      RETURN
210   CONTINUE
C     only one p-curve given
      DO I = IP(13)+2,3*IP(12)-1,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. 3*IP(12)-1) THEN
            RP(3) = BP(I)
            RP(4) = BP(I-1)
            RP(5) = BP(I+3)
            RP(6) = BP(I+2)
            EXIT
         END IF
      END DO
      R1 = RP(4) + (RP(6)-RP(4))  * (IN(2)-RP(3)) / (RP(5)-RP(3))
      OUT(1) = R1
      RETURN
3     CONTINUE
C     y=f(p,x)
C     at first, find the I with P(I+1) > P(I) and IN(1) < P(I+1)
      IP(13) = 3   ! pointer to start of P(i1)
      IP(14) = 3   ! pointer to start of P(i2) with IN(1) < P(i2)
      RP(1) = BP(3)
      RP(2) = BP(3)! current Pi
      DO I = 6,3*IP(12),3
         IF (BP(I) .NE. RP(2)) THEN
            IP(13) = IP(14)
            RP(1)  = RP(2)
            IP(14) = I ! here starts P2
            RP(2)  = BP(I)
            IF (IN(1) .LE. BP(I)) EXIT
         END IF
      END DO
      IF (IP(13).EQ.IP(14)) GOTO 310 ! only one p-curve given
      DO I = IP(13)+1,IP(14)-5,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. IP(14)-5) THEN
            RP(3) = BP(I)
            RP(4) = BP(I+1)
            RP(5) = BP(I+3)
            RP(6) = BP(I+4)
            EXIT
         END IF
      END DO
      DO I = IP(14)+1,3*IP(12)-2,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. 3*IP(12)-2 .OR.
     &       RP(2) .NE. BP(I+5) ) THEN
            RP(7)  = BP(I)
            RP(8)  = BP(I+1)
            RP(9)  = BP(I+3)
            RP(10) = BP(I+4)
            EXIT
         END IF
      END DO
      R1 = RP(4) + (RP(6)-RP(4))  * (IN(2)-RP(3)) / (RP(5)-RP(3))
      R2 = RP(8) + (RP(10)-RP(8)) * (IN(2)-RP(7)) / (RP(9)-RP(7))
      OUT(1) = R1 + (R2-R1) * (IN(1)-RP(1)) / (RP(2)-RP(1))
      RETURN
310   CONTINUE
      DO I = IP(13)+1,3*IP(12)-2,3
         IF (IN(2) .LE. BP(I+3) .OR. I .EQ. 3*IP(12)-2) THEN
            RP(3) = BP(I)
            RP(4) = BP(I+1)
            RP(5) = BP(I+3)
            RP(6) = BP(I+4)
            EXIT
         END IF
      END DO
      R1 = RP(4) + (RP(6)-RP(4))  * (IN(2)-RP(3)) / (RP(5)-RP(3))
      OUT(1) = R1
      RETURN
      END
C-----------------------------------------------------------------------

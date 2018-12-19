
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0104(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      REAL set, set2, u
      INTEGER x, I, io, J, error, RECSIZE, ios
      CHARACTER dir
      LOGICAL ex, existfile, od





      PARAMETER       (BNAMES = 'UBPAUSE'
     &,                OPM    = 1
     &,                INMIN  = 1
     &,                INS    = 4
     &,                OUTS   = 1
     &,                IPS    = 45
     &,                RPS    = 1
     &,                DPS    = 0
     &,                BPMIN  = 0
     &,                BPS    = 2
     &,                SPMIN  = 1
     &,                SPS    = 2
     &,                GROUP  = 3)
      CHARACTER*1024   SP(SPS+1),STEXT
      DOUBLE PRECISION DP(DPS+1)
      INTEGER          IP(IPS+1),II
      REAL             IN(INS+1),OUT(OUTS+1),RP(RPS+1),BP(BPS+1),
     &                 Data(999999,10)
C-----------------------------------------------------------------------
      IF (IP(2) .NE. 0) THEN
         IF (IP(2) .EQ. -1) THEN
C           Identification call
            CALL ID(IN,OUT,IP,RP,DP,BP,SP,BNAMES,OPM,
     &         INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,GROUP)
         ELSE IF (IP(2) .EQ. 1 ) THEN
C           Constructor call
            x = 0
         ELSE
C           Destructor call
         END IF
         RETURN
      END IF




         I = 1
    1   CONTINUE




!            INQUIRE(File=sp(1), RECL = RECSIZE, Exist=existfile)
!            PRINT *, RECSIZE
!            IF (existfile .EQV. .TRUE.) THEN

            OPEN(30,FILE=sp(1), STATUS='old', IOSTAT=IO,
     &      ACTION='READ', access='direct', RECL=12,FORM='FORMATTED')

            IF (IO .EQ. 0) THEN

            READ (30,'(1F12.4)',REC=I) Data(I,0)

            ELSE
            CLOSE(30)
            GOTO 1
            CLOSE(30)
            END IF

!
            Data(I,0) = 0

    2       CONTINUE


            OPEN(30,FILE=sp(1), STATUS='old', IOSTAT=IO,
     &      ACTION='WRITE', access='direct', RECL=12,FORM='FORMATTED')


            IF (IO .EQ. 0) THEN
            WRITE (30,'(1F12.4)',REC=I)Data(I,0)
            CLOSE(30)

            ELSE
            CLOSE(30)
            GOTO 2
            END IF

            DO WHILE (Data(I,0) .LT. 1)

            !call sleep(1)
            call SYSTEM("ping 10.0.0.0 -n 1 -w 1 > nul")

!            do j = 1, 2000000
!            call random_number(u)
!            end do

    3       CONTINUE


            OPEN(30,FILE=sp(1), STATUS='OLD', IOSTAT=IO,
     &      ACTION='READ', access='direct', RECL=12,FORM='FORMATTED')

            IF (IO .EQ. 0) THEN

            READ (30,'(1F12.4)',REC=I) Data(I,0)
            CLOSE(30)

            ELSE
            CLOSE(30)
            GOTO 3
            END IF

            END DO



      RETURN
      END



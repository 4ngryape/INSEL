
C #Authors
C    Marcus Brennenstuhl
C #End
C-----------------------------------------------------------------------
      SUBROUTINE UB0108(IN,OUT,IP,RP,DP,BP,SP)
      IMPLICIT         NONE
      CHARACTER*1024   BNAMES
      INTEGER          INMIN,INS,OUTS,IPS,RPS,DPS,BPMIN,BPS,SPMIN,SPS,
     &                 GROUP,OPM

      REAL set, set2, set0
      INTEGER x, I, io, J, error
      LOGICAL ex



      PARAMETER       (BNAMES = 'UBPAUSE2'
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


         set0 = 0
         I = 1

         IF (in(1) .LT. rp(1)) THEN

         set0 = 1

         END IF

         IF (in(1) .GT. bp(2) .and. set0 .eq. 1) THEN

            set = 1
            set2 = 1

            ELSE IF (in(1) .LE. bp(2) .AND. set2 .EQ. 1) THEN
            set = 0
            set2 = 0

            ELSE IF (in(1) .LE. bp(2) .AND. set2 .EQ. 0) THEN
            set = 1
            set0 = 0

         END IF



        IF (set .EQ. 0) THEN


            OPEN(30,FILE=sp(1), STATUS='old', IOSTAT=IO,
     &      ACTION='READ', access='direct', RECL=12,FORM='FORMATTED')
            READ (30,'(1F12.4)',REC=I) Data(I,0)
            CLOSE(30)

            Data(I,0) = 0

            OPEN(30,FILE=sp(1), STATUS='old', IOSTAT=IO,
     &      ACTION='WRITE', access='direct', RECL=12,FORM='FORMATTED')
            WRITE (30,'(1F12.4)',REC=I)Data(I,0)
            CLOSE(30)


            DO WHILE (Data(I,0) .LT. 1)


            OPEN(30,FILE=sp(1), STATUS='OLD', IOSTAT=IO,
     &      ACTION='READ', access='direct', RECL=12,FORM='FORMATTED')
            READ (30,'(1F12.4)',REC=I) Data(I,0)
            CLOSE(30)

            call sleep(1)

            END DO

        END IF


      rp(1) = in(1)

      RETURN
      END



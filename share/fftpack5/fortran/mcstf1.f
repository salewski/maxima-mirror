CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   FFTPACK 5.0 
C
C   Authors:  Paul N. Swarztrauber and Richard A. Valent
C
C   $Id$
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MCSTF1(LOT,JUMP,N,INC,X,WSAVE,DSUM,WORK,IER)
      REAL       X(INC,*)       ,WSAVE(*)
      DOUBLE PRECISION           DSUM(*)
      IER = 0
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      LJ = (LOT-1)*JUMP+1
      IF (N-2) 200,101,102
  101 DO 111 M=1,LJ,JUMP
         X1H = X(M,1)+X(M,2)
         X(M,2) = .5*(X(M,1)-X(M,2))
         X(M,1) = .5*X1H
  111 CONTINUE
      GO TO 200
  102 IF (N .GT. 3) GO TO 103
      DO 112 M=1,LJ,JUMP
         X1P3 = X(M,1)+X(M,3)
         TX2 = X(M,2)+X(M,2)
         X(M,2) = .5*(X(M,1)-X(M,3))
         X(M,1) = .25*(X1P3+TX2)
         X(M,3) = .25*(X1P3-TX2)
  112 CONTINUE
      GO TO 200
  103 M1 = 0
      DO 113 M=1,LJ,JUMP
         M1 = M1+1
         DSUM(M1) = X(M,1)-X(M,N)
         X(M,1) = X(M,1)+X(M,N)
  113 CONTINUE
      DO 104 K=2,NS2
         M1 = 0
         DO 114 M=1,LJ,JUMP
         M1 = M1+1
         KC = NP1-K
         T1 = X(M,K)+X(M,KC)
         T2 = X(M,K)-X(M,KC)
         DSUM(M1) = DSUM(M1)+WSAVE(KC)*T2
         T2 = WSAVE(K)*T2
         X(M,K) = T1-T2
         X(M,KC) = T1+T2
  114    CONTINUE
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .EQ. 0) GO TO 124
         DO 123 M=1,LJ,JUMP
         X(M,NS2+1) = X(M,NS2+1)+X(M,NS2+1)
  123    CONTINUE
 124  CONTINUE
      LENX = (LOT-1)*JUMP + INC*(NM1-1)  + 1
      LNSV = NM1 + INT(LOG(REAL(NM1))/LOG(2.)) + 4
      LNWK = LOT*NM1
C
      CALL RFFTMF(LOT,JUMP,NM1,INC,X,LENX,WSAVE(N+1),LNSV,WORK,
     1            LNWK,IER1)
      IF (IER1 .NE. 0) THEN
        IER = 20
        CALL XERFFT ('MCSTF1',-5)
        GO TO 200
      ENDIF
C
      SNM1 = 1./FLOAT(NM1)
      DO 10 M=1,LOT
      DSUM(M) = SNM1*DSUM(M)
   10 CONTINUE
      IF(MOD(NM1,2) .NE. 0) GO TO 30
      DO 20 M=1,LJ,JUMP
      X(M,NM1) = X(M,NM1)+X(M,NM1)
   20 CONTINUE
 30   DO 105 I=3,N,2
         M1 = 0
         DO 115 M=1,LJ,JUMP
            M1 = M1+1
            XI = .5*X(M,I)
            X(M,I) = .5*X(M,I-1)
            X(M,I-1) = DSUM(M1)
            DSUM(M1) = DSUM(M1)+XI
  115 CONTINUE
  105 CONTINUE
      IF (MODN .NE. 0) GO TO 117
      M1 = 0
      DO 116 M=1,LJ,JUMP
         M1 = M1+1
         X(M,N) = DSUM(M1)
  116 CONTINUE
 117  DO 118 M=1,LJ,JUMP
      X(M,1) = .5*X(M,1)
      X(M,N) = .5*X(M,N)
 118  CONTINUE
C
 200  CONTINUE
      RETURN
      END

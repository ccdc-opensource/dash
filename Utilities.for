      SUBROUTINE SORT_REAL(VAL,IP,N)
C
C
CX
CC 16C
CH Sorts pointers to a real array using Heapsort.
C
CA On entry VAL is an array of N real numbers.
CA On exit IP is an array of N pointers to VAL in ascending order
C
C
C
      DIMENSION VAL(N),IP(N)
C
C EXTRA PART (WHICH MAY BE REMOVED AGAIN) - SET UP POINTERS:
      DO 16 I=1,N
  16  IP(I)=I
      IF (N .LT. 2) GO TO 100
C  INITIALISE
      L=N/2+1
      IR=N
    1 L=L-1
      K=IP(L)
    3 J=L
      GOTO 4
C
C  SIFTING LOOP
    5 IF (VAL(IP(J)).LT.VAL(IP(J+1))) J=J+1
    7 IP(I)=IP(J)
    4 I=J
      J=J+J
      IF (J-IR) 5,7,8
C
C  FLOYDS MODIFICATION
   10 IP(J)=IP(I)
    8 J=I
      I=I/2
      IF (I) 6,6,9
    9 IF (J.GT.L .AND. VAL(K).GT.VAL(IP(I))) GO TO 10
   6  IP(J)=K
C
C  END OF A SIFT
    2 IF (L.GT.1) GO TO 1
      K=IP(IR)
      IP(IR)=IP(1)
      IR=IR-1
      IF (IR.GT.1) GO TO 3
      IP(1)=K
 100  RETURN
      END
C
C
C
C
C
C LEVEL 3      SUBROUTINE InverseMatrix(A,B,N)
      SUBROUTINE InverseMatrix(A,B,N)
C
C
CX
CC 12C
CH Inverts matrix A into matrix B.
CA On entry A is a square NxN real matrix
CA On exit  B is its inverse
C
      DIMENSION II(100),IL(100),IG(100),A(N,N),B(N,N)
C
      DO J=1,N
        DO I=1,N
          B(I,J)=A(I,J)
        END DO
      END DO
C      CALL GMEQ(A,B,N,N)
      D=1.
      IS=N-1
      DO 10 K=1,N
      IL(K)=0
   10 IG(K)=K
C
      DO 150 K=1,N
      R=0.
      DO 40 I=1,N
      IF (IL(I) .NE. 0) GO TO 40
      W=B(I,K)
      X=ABS(W)
      IF (R .GT. X) GO TO 40
      R=X
      P=W
      KF=I
   40 CONTINUE
      II(K)=KF
      IL(KF)=KF
      D=D*P
      IF (D .EQ. 0.) GOTO 999
C
      DO 80 I=1,N
      IF (I .EQ. KF) THEN
      B(I,K)=1./P
      ELSE
      B(I,K)=-B(I,K)/P
      ENDIF
   80 CONTINUE
C
      DO 140 J=1,N
      IF (J .EQ. K) GO TO 140
      W=B(KF,J)
      IF (W .EQ. 0.) GO TO 140
      DO 130 I=1,N
      IF (I .EQ. KF) THEN
      B(I,J)=W/P
      ELSE
      B(I,J)=B(I,J)+W*B(I,K)
      ENDIF
  130 CONTINUE
  140 CONTINUE
C
  150 CONTINUE
C.....
C
      DO 190 K=1,IS
      KF=II(K)
      KL=IL(KF)
      KG=IG(K)
      IF(KF .EQ. KG) GO TO 190
      DO 170 I=1,N
      R=B(I,KF)
      B(I,KF)=B(I,KG)
  170 B(I,KG)=R
      DO 180 J=1,N
      R=B(K,J)
      B(K,J)=B(KL,J)
  180 B(KL,J)=R
      IL(KF)=K
      IL(KG)=KL
      IG(KL)=IG(K)
      IG(K)=KF
      D=-D
  190 CONTINUE
  999 RETURN
      END
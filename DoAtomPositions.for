CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE DO_ATOM_POS(TRANS,ROTA,POS,NATOMS)
C     -------------------------------------------------------
C
      REAL*8 TRANS(3),ROTA(3,3),POSIN(3),POS(3,NATOMS)
C
      DO J=1,NATOMS
        DO I=1,3
          POSIN(I)=POS(I,J)
        END DO
        CALL ROTCAR(POSIN,POS(1,J),ROTA)
        DO I=1,3
          POS(I,J)=POS(I,J)+TRANS(I)
        END DO
      END DO
C
      END
C
C
C----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
C
	SUBROUTINE ROTCAR(XORTO,XORTN,ROTA)
	REAL*8 XORTO(3),XORTN(3),ROTA(3,3)
C
        DO I=1,3
          XORTN(I)=0.
          DO J=1,3
            XORTN(I)=XORTN(I)+ROTA(I,J)*XORTO(J)
          END DO
        END DO
C
        RETURN
	END	
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

!*==DO_ATOM_POS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!*****************************************************************************
!
      SUBROUTINE DO_ATOM_POS(TRANS,ROTA,POS,NATOMS)

      REAL*8 TRANS(3), ROTA(3,3), POSIN(3), POS(3,*)

      DO J = 1, NATOMS
        DO I = 1, 3
          POSIN(I) = POS(I,J)
        ENDDO
        CALL ROTCAR(POSIN,POS(1,J),ROTA)
        DO I = 1, 3
          POS(I,J) = POS(I,J) + TRANS(I)
        ENDDO
      ENDDO

      END SUBROUTINE DO_ATOM_POS
!
!*****************************************************************************
!
      SUBROUTINE ROTCAR(XORTO,XORTN,ROTA)

      REAL*8 XORTO(3), XORTN(3), ROTA(3,3)

      DO I = 1, 3
        XORTN(I) = 0.
        DO J = 1, 3
          XORTN(I) = XORTN(I) + ROTA(I,J)*XORTO(J)
        ENDDO
      ENDDO

      END SUBROUTINE ROTCAR
!
!*****************************************************************************
!

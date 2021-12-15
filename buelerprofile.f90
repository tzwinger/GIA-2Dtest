!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
! ******************************************************************************
FUNCTION Bueler_old ( Model, nodenumber, xcoord) RESULT(elevation)
  USE Types
  USE DefUtils
  IMPLICIT NONE
  TYPE(Model_t) :: Model
  INTEGER :: nodenumber
  REAL(KIND=dp) :: xcoord, elevation

  REAL(KIND=dp), PARAMETER :: L = 361.25d03, H0 = 2.06d03, n = 3.0_dp, minh=100.0_dp
  REAL(KIND=dp) :: T1, T2, H, R

  R = xcoord
  T1 = ( H0/((n - 1.0_dp)**(n/(2.0_dp*n + 2.0_dp))) )
  T2 = (n + 1.0_dp)*(R/L) - n*((R/L)**((n + 1.0_dp)/n)) + &
       n*((1.0_dp - (R/L))**((n + 1.0_dp)/n) ) - 1.0
  H =  T1 * ( T2**(n/(2.0*n + 2.0)) )
  elevation = MAX(H,minh)
END FUNCTION Bueler_old



FUNCTION Bueler_t ( Model, nodenumber, xcoord) RESULT(elevation)
  USE Types
  USE DefUtils
  IMPLICIT NONE
  TYPE(Model_t) :: Model
  INTEGER :: nodenumber
  REAL(KIND=dp) :: xcoord, elevation

  INTEGER :: i,nodesinelement
  REAL(KIND=dp), PARAMETER :: n = 3.0_dp
  REAL(KIND=dp) :: L = 361.25d03, H0 = 2.06d03, minh, T1, T2, H, R,&
       offsetvelo, offsetdistance, offsettime
  REAL(KIND=dp), ALLOCATABLE :: auxReal(:)
  TYPE(ValueList_t), POINTER :: BC
  LOGICAL :: GotIt, OffsetActive=.FALSE.

  SAVE OffsetActive

  BC => GetBC(Model % CurrentElement)
  IF (.NOT.ASSOCIATED(BC)) THEN
    CALL FATAL("Bueler_t","No boundary condition found for element")
  ELSE
    nodesinelement = GetElementNOFNodes()
    ALLOCATE (auxReal(nodesinelement))
    DO i=1, nodesinelement
      IF (NodeNumber== Model % CurrentElement % NodeIndexes( i )) EXIT 
    END DO
    ! Get the external (probably water) pressure
    ! Use the convention Pext > 0 => Compression
    !auxReal(1:nodesinelement) = GetReal( BC, 'External Pressure', GotIt )
    R = xcoord
    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Reference Length', GotIt )
    IF (.NOT.GotIt) CALL FATAL("Bueler_t",">Bueler Reference Length< not found")
    L = auxReal(i)

    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Reference Height', GotIt )
    IF (.NOT.GotIt) CALL FATAL("Bueler_t",">Bueler Reference Height< not found")
    H0 = auxReal(i)
    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Minimum Height', GotIt )
    IF (.NOT.GotIt) THEN
      CALL INFO("Bueler_t",">Bueler Minimum Height< not found. Defaulting to 10",Level=9)
      minh=10.0_dp
    ELSE
      minh = auxReal(i)
    END IF

    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Offset Distance', GotIt )
    IF (.NOT.GotIt) THEN
      CALL INFO("Bueler_t"," >Bueler Offset Distance< not found. Defaulting to 0",Level=9)
      offsetdistance = 0.0_dp
    ELSE
      offsetdistance = auxReal(i)
    END IF

    IF ((L >= xcoord) .AND. (H0 > minh)) THEN
      offsettime = GetConstReal( BC, 'Bueler Offset Time', GotIt )
      IF (.NOT.GotIt) THEN
        CALL INFO("Bueler_t",">Bueler Offset Time< not found. Defaulting to 0",Level=9)
        OffsetActive = .FALSE.
      ELSE
        WRITE (Message, *) "Bueler_t"," > Bueler Offset Time< set to",offsettime
        CALL INFO("Bueler_t",Message,Level=9)
        OffsetActive = .TRUE.
      END IF

      !   offsetdistance = offsetvelo * GetTimestepSize()
      !   PRINT *,offsetdistance
      IF (OffsetActive) THEN
        IF(GetTime() <= offsettime) THEN
          elevation = minh
        ELSE
          OffsetActive = .FALSE.
        END IF
      END IF
      IF (.NOT.OffsetActive) THEN
        T1 = ( H0/((n - 1.0_dp)**(n/(2.0_dp*n + 2.0_dp))) )
        T2 = (n + 1.0_dp)*(R/L) - n*((R/L)**((n + 1.0_dp)/n)) + &
             n*((1.0_dp - (R/L))**((n + 1.0_dp)/n) ) - 1.0_dp
        H =  T1 * ( T2**(n/(2.0_dp*n + 2.0_dp)) )
        IF (H > H0) THEN
          PRINT *, "H",H,">",H0,"dz",offsetdistance,R,L,n
          H = H0
        END IF
        elevation = MAX(H,minh) + offsetdistance
      END IF
    ELSE
      elevation = minh + offsetdistance
    END IF

    DEALLOCATE(auxReal)
  END IF
END FUNCTION Bueler_t

FUNCTION Bueler_smb ( Model, nodenumber, xcoord) RESULT(smb)
  USE Types
  USE DefUtils
  IMPLICIT NONE
  TYPE(Model_t) :: Model
  INTEGER :: nodenumber
  REAL(KIND=dp) :: xcoord, smb

  INTEGER :: i,nodesinelement
  REAL(KIND=dp), PARAMETER :: n = 3.0_dp
  REAL(KIND=dp) :: L = 361.25d03, H0 = 2.06d03, minh, T1, T2, H, R, A0, &
       offsetvelo, offsetdistance, offsettime, aux1, aux2, alpha, auxn, relx
  REAL(KIND=dp), ALLOCATABLE :: auxReal(:)
  TYPE(ValueList_t), POINTER :: BC
  LOGICAL :: GotIt, OffsetActive=.FALSE.

  SAVE OffsetActive

  BC => GetBC(Model % CurrentElement)
  IF (.NOT.ASSOCIATED(BC)) THEN
    CALL FATAL("Bueler_smb","No boundary condition found for element")
  ELSE
    nodesinelement = GetElementNOFNodes()
    ALLOCATE (auxReal(nodesinelement))
    DO i=1, nodesinelement
      IF (NodeNumber== Model % CurrentElement % NodeIndexes( i )) EXIT 
    END DO
    ! Get the external (probably water) pressure
    ! Use the convention Pext > 0 => Compression
    !auxReal(1:nodesinelement) = GetReal( BC, 'External Pressure', GotIt )
    R = xcoord
    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Reference Length', GotIt )
    IF (.NOT.GotIt) CALL FATAL("Bueler_smb",">Bueler Reference Length< not found")
    L = auxReal(i)

    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Reference Height', GotIt )
    IF (.NOT.GotIt) CALL FATAL("Bueler_smb",">Bueler Reference Height< not found")
    H0 = auxReal(i)
    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Minimum Height', GotIt )
    IF (.NOT.GotIt) THEN
      CALL INFO("Bueler_smb",">Bueler Minimum Height< not found. Defaulting to 10",Level=9)
      minh=10.0_dp
    ELSE
      minh = auxReal(i)
    END IF

    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Offset Distance', GotIt )
    IF (.NOT.GotIt) THEN
      CALL INFO("Bueler_smb",">Bueler Offset Distance< not found. Defaulting to 0",Level=9)
      offsetdistance = 0.0_dp
    ELSE
      offsetdistance = auxReal(i)
    END IF
    auxReal(1:nodesinelement) = GetReal( BC, 'Bueler Arrhenius Factor' , GotIt )
    IF (.NOT.GotIt) THEN
      CALL FATAL("Bueler_smb",">Bueler Arrhenius Factor< not found")
    ELSE
      A0 = 2.0_dp*auxReal(i) * ((917.0_dp*9.81_dp)**n) / (n +2) 
    END IF
    DEALLOCATE(auxReal)
  END IF
  
  aux1 = (2.0_dp*L*(n - 1.0_dp)/n)**n
  alpha = (H0**(2.0_dp*n + 2.0_dp)) * A0/aux1
  relx = xcoord/L
  IF (relx <= 0.0) THEN
    smb = 0.0_dp
  ELSE
    IF (relx >= 0.99_dp) relx = 0.99
    auxn = 1.0_dp/n
    aux1 = ((relx**auxn) + ((1.0_dp - relx)**auxn) - 1.0_dp)**(n - 1.0_dp)
    auxn = (1.0_dp/n) - 1.0_dp
    aux2 = (relx**auxn) - (1.0_dp - relx)**auxn
    smb = (alpha/L) * aux1 * aux2!/(365.25 * 24.0 * 3600.0)
    !PRINT *,  relx, smb, aux1 * aux2, alpha
    !PRINT *, alpha, aux1, aux2
    !PRINT *,"SMB:",relx, smb
  END IF
END FUNCTION Bueler_smb
  

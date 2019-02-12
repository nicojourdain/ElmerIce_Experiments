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
! *
! *  Author:
! *  Email:
! *  Web:     http://elmerice.elmerfem.org
! *
! *  Original Date: 
! *****************************************************************************
SUBROUTINE FrontThickness_mask( Model,Solver,dt,TransientSimulation )
!------------------------------------------------------------------------------
!******************************************************************************
!
!  ARGUMENTS:
!
!  TYPE(Model_t) :: Model,
!     INPUT: All model information (mesh, materials, BCs, etc...)
!
!  TYPE(Solver_t) :: Solver
!     INPUT: Linear & nonlinear equation solver options
!
!  REAL(KIND=dp) :: dt,
!     INPUT: Timestep size for time dependent simulations
!
!  LOGICAL :: TransientSimulation
!     INPUT: Steady state or transient simulation
!
!******************************************************************************
  USE DefUtils

  IMPLICIT NONE
  !------------------------------------------------------------------------------
  TYPE(Solver_t) :: Solver
  TYPE(Model_t) :: Model

  REAL(KIND=dp) :: dt
  LOGICAL :: TransientSimulation

  !------------------------------------------------------------------------------
  ! Local variables
  !------------------------------------------------------------------------------

  TYPE(Mesh_t),POINTER :: Mesh
  TYPE(Element_t),POINTER :: Element
  TYPE(ValueList_t), POINTER :: Material, SolverParams
  TYPE(Variable_t), POINTER :: PointerToVariable=>NULL(), HVar=>NULL()
  TYPE(Nodes_t), SAVE :: Nodes

  LOGICAL :: AllocationsDone = .FALSE., GotIt, stat,UnFoundFatal=.TRUE.
  LOGICAL :: FirstTime = .True., NewTime
  INTEGER :: DIM, node, Nmax
  INTEGER, POINTER :: Permutation(:), HPerm(:)

  REAL(KIND=dp), POINTER :: VariableValues(:), H(:)
  REAL(KIND=dp) :: xcf, hmin

  LOGICAL :: Parallel

  CHARACTER(LEN=MAX_NAME_LEN) :: SolverName = 'FrontThickness_mask'

  SAVE AllocationsDone, DIM, SolverName
  !------------------------------------------------------------------------------

  Mesh => Solver % Mesh

  PointerToVariable => Solver % Variable
  Permutation  => PointerToVariable % Perm
  VariableValues => PointerToVariable % Values

  CALL INFO(SolverName, 'Computing Real Calving front mask', level=3)

  print *, '11111111'

  Parallel = .FALSE.
  IF ( ASSOCIATED( Solver % Matrix % ParMatrix ) ) THEN
    IF ( Solver %  Matrix % ParMatrix % ParEnv % PEs > 1 )  THEN
      Parallel = .TRUE.
    END IF
  END IF

  print *, '222222222222222222'

  !--------------------------------------------------------------
  ! Allocate some permanent storage:
  !--------------------------------------------------------------

  hmin = 11.0_dp
  xcf = 635000.0_dp

  HVar => VariableGet( Model % Mesh % Variables, 'H')
  IF (.NOT.ASSOCIATED(HVar)) THEN
     Message='H not found'
     CALL FATAL(SolverName,Message)
  END IF

  HPerm => HVar % Perm
  H => HVar % Values

  Nmax = Solver % Mesh % NumberOfNodes

  DO node=1,Nmax
    print *, H(HPerm(node))
    ! cond for mask=0
    IF ( H(HPerm(node)) .LE. hmin .OR. Mesh % Nodes % x(node) .GE. xcf) THEN
      VariableValues(Permutation(node)) = 0.0_dp
    ELSE
      VariableValues(Permutation(node)) = 1.0_dp
    ENDIF
  ENDDO


  IF ( ParEnv % PEs>1 ) CALL ParallelSumVector( Solver % Matrix, VariableValues, 1 )

  CALL INFO( SolverName , 'Done')

END SUBROUTINE FrontThickness_mask
!------------------------------------------------------------------------------


SUBROUTINE MISMIP_Melt_Param_Zambiant( Model,Solver,dt,Transient )
!------------------------------------------------------------------------------
USE CoordinateSystems
USE MeshUtils
USE DefUtils

IMPLICIT NONE
!------------------------------------------------------------------------------
TYPE(Model_t)  :: Model
TYPE(Solver_t), TARGET :: Solver
LOGICAL ::  Transient
REAL(KIND=dp) :: dt

!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
TYPE(Mesh_t),POINTER :: Mesh
TYPE(Solver_t),POINTER :: PSolver
TYPE(Variable_t),POINTER :: MeltVar=>NULL(), GMVar=>NULL(), DepthVar=>NULL()
TYPE(Variable_t),POINTER :: isfslopeVar=>NULL(), distGLVar=>NULL(), distIFVar=>NULL()
TYPE(Variable_t),POINTER :: TimeVar=>NULL()
TYPE(Nodes_t) :: ElementNodes
TYPE(GaussIntegrationPoints_t) :: IntegStuff
TYPE(Element_t),POINTER ::  Element

REAL(kind=dp),allocatable :: VisitedNode(:),db(:),Basis(:),dBasisdx(:,:)
REAL(kind=dp),allocatable :: xgl(:),ygl(:),dgl(:),xglloc(:),yglloc(:),dglloc(:),xgltot(:),ygltot(:),dgltot(:)
REAL(kind=dp) :: u,v,w,SqrtElementMetric,s

INTEGER , POINTER :: MeltPerm(:), GMPerm(:), DepthPerm(:),NodeIndexes(:)
INTEGER , POINTER :: isfslopePerm(:), distGLPerm(:), distIFPerm(:)
REAL(KIND=dp) , POINTER :: Melt(:),GM(:),Depth(:),isfslope(:)
REAL(KIND=dp) , POINTER ::DATAPointer(:,:), distGL(:), distIF(:)

LOGICAL,SAVE :: Initialized = .FALSE.
LOGICAL,SAVE :: ExtrudedMesh=.False.
LOGICAL :: Found, Got, stat, Parallel, llGL, cond1, cond2, cond3, condgm

CHARACTER(len=MAX_NAME_LEN) :: para_name
CHARACTER(len = 200) :: meltValue
CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='MELT_MISMIP'

INTEGER :: Nmax, ncid, node, ncidDraft, e, t, n, i, j, EXPnumber, knd, kk, nD, ii, LazerType
INTEGER :: varXid, varYid, varid, dimid1, dimid2, dimid3, lenX, lenY, lenTime, status1, res, ierr
INTEGER :: ngl, nglloc, ngltot, cpt, nn, debilos, dir, nbdir, cpttmp, jj, hwidth, kkstart, halfw, halfpt, kk1, cptglob
INTEGER :: status(MPI_STATUS_SIZE)

INTEGER, DIMENSION(:), ALLOCATABLE :: inds

REAL(KIND=dp) :: x_NC_Init, x_NC_Fin, y_NC_Init, y_NC_Fin, x_NC_Res, y_NC_Res, localInteg, Integ, Integ_Reduced,   &
&                z1_0, z2_0, z1, z2, zzz, Tsrf0, Tbot0, Tsrf, Tbot, T0, Ssrf0, Sbot0, Ssrf, Sbot, S0, dz1, dz2,    &
&                dTsrf, dTbot, Tf, xP , yP, xPn, yPn, meltInt, time00, sealevel, lbd1, lbd2, lbd3, meltfac, K, gT, &
&                zGL, dn, wn, div, tana, x0, E0, Cd, GamT, GefT, GTS, MM, M_hat, X_hat, ll, alpha, AvgTmTf, localunity,  &
&                Area, Area_Reduced, Za, rhostar, CC, beta, g1, g2, rr, xbox, Tstar, tmp1, S_stratif, timsc, qqq,  &
&                mskcrit, time, epsz, sn, xgltmp, ygltmp, epsy, disty, refx, refy, zGLmin, yGLmin, xGLmin, radius, &
&                angle, angle1, angle2, xp2, yp2, xp3, yp3, xp12, yp12, xp23, yp23, xp31, yp31, zGLtmp, ltmp, det, &
&                zGLloc, minDist, dist, xGLfin, yGLfin, alphax, alphay, locslope, locslope1, locslope2, angle4, &
&                closestgl, lgl

REAL(KIND=dp), DIMENSION(12) :: pp

REAL(KIND=dp), DIMENSION(:), ALLOCATABLE ::  Zbox, Abox, Tbox, Sbox, Mbox, sinus, xgltot2, ygltot2, dgltot2, dgltotsmooth

!------------------------------------------------------------------------------
! 1- Read constants and parameters of the simulation :
!------------------------------------------------------------------------------

epsz = 10.0_dp

!- Simulation parameters (idealized ocean forcing) :

EXPnumber=GetInteger( Model % Simulation, 'Experiment Number',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Experiment Number> not found')

time00=GetCReal( Model % Simulation, 'Experiment initial time',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Experiment initial time> not found')

para_name=GetString( Model % Simulation, 'Melt Parameterization',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Melt Parameterization> not found')

llGL=GetLogical( Model % Simulation, 'Grounding Line Melt',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Grounding Line Melt> not found')

!- General :

sealevel = GetCReal( Model % Constants, 'Sea Level', Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Sea Level> not found')

meltfac=GetCReal( Model % Constants, 'Melt factor',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Melt factor> not found')

lbd1 = GetCReal( Model % Constants, 'Liquidus slope', Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Liquidus slope> not found')

lbd2 = GetCReal( Model % Constants, 'Liquidus intercept', Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Liquidus intercept> not found')

lbd3 = GetCReal( Model % Constants, 'Liquidus pressure coeff', Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Liquidus pressure coeff> not found')

Za=GetCReal( Model % Constants, 'Depth Ambiant PICO',Found )
IF (.NOT.Found) &
&  CALL FATAL(SolverName,'<Depth Ambiant PICO> not found')

!- Parameters, according to chosen parameterization:

SELECT CASE (para_name)

CASE('bg') !- e.g. Beckmann and Goose 2002

  gT=GetCReal( Model % Constants, 'Multiplying Factor BG',Found )
  IF (.NOT.Found) &
  &  CALL FATAL(SolverName,'<Multiplying Factor BG> not found')

CASE('pdc','pdcstar') !- Pollard & DeConto 2012

  gT=GetCReal( Model % Constants, 'Multiplying Factor PDC',Found )
  IF (.NOT.Found) &
  &  CALL FATAL(SolverName,'<Multiplying Factor PDC> not found')

CASE('lazer') !- Lazeroms et al. 2017

  pp = (/  1.371330075095435e-1, &  ! p0
        &  5.527656234709359e1,  &  ! p1
        & -8.951812433987858e2,  &  ! p2
        &  8.927093637594877e3,  &  ! p3
        & -5.563863123811898e4,  &  ! p4 
        &  2.218596970948727e5,  &  ! p5
        & -5.820015295669482e5,  &  ! p6
        &  1.015475347943186e6,  &  ! p7
        & -1.166290429178556e6,  &  ! p8
        &  8.466870335320488e5,  &  ! p9
        & -3.520598035764990e5,  &  ! p10
        &  6.387953795485420e4   /) ! p11

   x0   = 0.56    ! Dimensionless transition melting/freezing
   Cd   = 2.5e-3  ! Drag coefficent

   epsy = 5000.0_dp

   if ( llGL ) CALL INFO(SolverName, &
   & 'WARNING : no melt at the grounding line with this parameterization',Level=1)

   E0=GetCReal( Model % Constants, 'Entrainment Coeff LAZER',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Entrainment Coeff LAZER> not found')

   GamT=GetCReal( Model % Constants, 'Heat Exchange Coeff LAZER',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Heat Exchange Coeff LAZER> not found')

   GefT=GetCReal( Model % Constants, 'Effective Exchange Coeff LAZER',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Effective Exchange Coeff LAZER> not found')

   K=GetCReal( Model % Constants, 'K Coeff LAZER',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<K Coeff LAZER> not found')

   ! 3 different flavours for the plume model emulator from Lazeroms, 2018
   ! 1 -> 'simple' -> Zgl and Alpha are found between draft point and deepest grounding line
   ! 2 -> 'appenB' -> Appendix B from Lazeroms, 2017, TCD (not existing in final TC paper)
   ! 3 -> 'Jenkin' -> find the closest GL point, take the deepest GL counterclockwise
   LazerType=GetInteger( Model % Constants, 'Type LAZER',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Type LAZER> not found')

CASE('pico') !- Reese et al. 2017

   nD=GetInteger( Model % Constants, 'Number Boxes PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Number Boxes PICO> not found')

   CC=GetCReal( Model % Constants, 'Circulation Parameter PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Circulation Parameter PICO> not found')

   gT=GetCReal( Model % Constants, 'Effective Exchange Velocity PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Effective Exchange Velocity PICO> not found')

   alpha=GetCReal( Model % Constants, 'Thermal expansion coeff PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Thermal expansion coeff PICO> not found')

   beta=GetCReal( Model % Constants, 'Salinity contraction coeff PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<Salinity contraction coeff PICO> not found')

   rhostar=GetCReal( Model % Constants, 'EOS ref Density PICO',Found )
   IF (.NOT.Found) &
   &  CALL FATAL(SolverName,'<EOS ref Density PICO> not found')

CASE DEFAULT

  CALL FATAL(SolverName,'Unknonw Melt Parameterization')

END SELECT

!------------------------------------------------------------------------------
! 2- Define temperature and salinity profiles 
!------------------------------------------------------------------------------

SELECT CASE (EXPnumber)

CASE(0)

  !-- ISOMIP EXP3
  z1_0  =   0.0 ! initial depth of upper thermocline
  z2_0  = 720.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.9 ! initial surface temperature
  Tbot0 =   1.0 ! initial bottom temperature
  Ssrf0 =  33.8 ! initial surface salinity 
  Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =   1.e9! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(3)

  !-- ISOMIP EXP3
  z1_0  =   0.0 ! initial depth of upper thermocline
  z2_0  = 720.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.9 ! initial surface temperature
  Tbot0 =   1.0 ! initial bottom temperature
  Ssrf0 =  33.8 ! initial surface salinity 
  Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =   1.e9! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(4)

  !-- ISOMIP EXP4
  z1_0  =   0.0 ! initial depth of upper thermocline
  z2_0  = 720.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.9 ! initial surface temperature
  Tbot0 =  -1.9 ! initial bottom temperature
  Ssrf0 =  33.8 ! initial surface salinity 
  Sbot0 =  34.55! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =   1.e9! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(10)

  !-- Uniform warming :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   1.0 ! warming  (over time scale timsc)
  dTbot =   1.0 !    "            "
  timsc = 100.0 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(11)

  !-- Deep warming :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   1.0 !    "            "
  timsc = 100.0 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(12)

  !-- From cold to warm cavity :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.5 ! initial surface temperature
  Tbot0 =  -1.5 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.0 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.9 ! warming  (over time scale timsc)
  dTbot =   2.7 !    "            "
  timsc = 100.0 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(13)

  !-- From cold to warm cavity :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =-200.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =-200.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc = 100.0 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)
       
CASE(20)

  !-- Uniform warming :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =  1.e9 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(21)

  !-- Deep warming :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.5 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =  1.e9 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(22)

  !-- From cold to warm cavity :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.5 ! initial surface temperature
  Tbot0 =  -1.5 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.0 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =  1.e9 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)

CASE(23)

  !-- From cold to warm cavity :
  z1_0  = 300.0 ! initial depth of upper thermocline
  z2_0  = 700.0 ! initial depth of lower thermocline
  Tsrf0 =  -1.0 ! initial surface temperature
  Tbot0 =   1.2 ! initial bottom temperature
  Ssrf0 =  34.0 ! initial surface salinity 
  Sbot0 =  34.7 ! initial bottom salinity (an increase of 0.2psu/700m is added below)
  dz1   =   0.0 ! vertical displacement of the upper thermocline  (over time scale timsc)
  dz2   =   0.0 ! vertical displacement of the lower thermocline  (over time scale timsc)
  dTsrf =   0.0 ! warming  (over time scale timsc)
  dTbot =   0.0 !    "            "
  timsc =  1.e9 ! time scale for changes dz1, dz2, dTsrf, dTbot (in years)
CASE DEFAULT
        
  CALL FATAL(SolverName,'Unknown Experiment number')
        
END SELECT

!- Background additional salinity stratification added to all profiles :
S_stratif = 0.2 / 700. ! psu/m

if (EXPnumber .eq. 0) then
  S_stratif = 0.0_dp
endif

!- Time varying variables :
TimeVar => VariableGet( Model % Variables,'Time')
time = TimeVar % Values(1)
z1 = z1_0 + (time-time00) * dz1 / timsc
z2 = z2_0 + (time-time00) * dz2 / timsc
Tsrf = Tsrf0 + (time-time00) * dTsrf / timsc
Tbot = Tbot0 + (time-time00) * dTbot / timsc
Ssrf = Ssrf0  ! no trend on salinity except if z1 and z2 vary
if ( EXPnumber == 12 ) then
 Sbot = Sbot0 + (time-time00) * 0.5 / timsc ! trend only in this case
else
 Sbot = Sbot0  ! no trend on salinity except if z1 and z2 vary
endif

!------------------------------------------------------------------------------
! 3- Calculate melt rates at each floating  node
!------------------------------------------------------------------------------

Nmax=Solver % Mesh % NumberOfNodes

ALLOCATE(VisitedNode(Nmax),  &
        Basis(Model % MaxElementNodes),  &
        dBasisdx(Model % MaxElementNodes,3))

Mesh => Model % Mesh

!Nmax = Solver % Mesh % NumberOfNodes

Parallel = .FALSE.
IF ( ASSOCIATED( Solver % Matrix % ParMatrix ) ) THEN
     IF ( Solver %  Matrix % ParMatrix % ParEnv % PEs > 1 )  THEN
        Parallel = .TRUE.
     END IF
END IF

MeltVar => VariableGet( Model % Mesh % Variables, 'Melt')
IF (.NOT.ASSOCIATED(MeltVar)) &
&    CALL FATAL(SolverName,'Melt not found')

GMVar => VariableGet( Model % Mesh % Variables, 'GroundedMask')
IF (.NOT.ASSOCIATED(GMVar)) &
&    CALL FATAL(SolverName,'GroundedMask not found')

DepthVar => VariableGet( Model % Mesh % Variables, 'Zb')
IF (.NOT.ASSOCIATED(DepthVar)) &
&    CALL FATAL(SolverName,'Zb not found')

MeltPerm => MeltVar % Perm
Melt => MeltVar % Values

GMPerm => GMVar % Perm
GM => GMVar % Values

DepthPerm => DepthVar % Perm
Depth => DepthVar % Values
Depth = sealevel - Depth     ! Depth < 0 under sea level

if ( llGL ) then
  mskcrit =  0.5 ! Melt is at the Grounding Line and floating points
else
  mskcrit = -0.5 ! No melt at the Grounding Line, only floating points
endif

SELECT CASE (para_name)

CASE('bg')

   DO node=1,Nmax 
     zzz=Depth(DepthPerm(node))
     if ( GM(GMPerm(node)) .lt. mskcrit .and. zzz .ge. epsz ) then
        ! Temperature and salinity at depth Za
        if ( Za .le. z1 ) then
          T0 = Tsrf
          S0 = Ssrf + Za * S_stratif
        elseif ( Za .ge. z2 ) then
          T0 = Tbot
          S0 = Sbot + Za * S_stratif
        else
          T0 = (Tbot-Tsrf)*Za/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
          S0 = (Sbot-Ssrf)*Za/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + Za*S_stratif
        endif
        Tf = lbd1*S0 + lbd2 + lbd3*zzz  ! Sea water freezing temperature
        ! Melt in m/yr (meters of ice per year), positive if ice ablation
        Melt(MeltPerm(node)) = - gT * meltfac * (T0-Tf)  ! 3 Equations with uniform velocity

     else
        Melt(MeltPerm(node)) = 0.0_dp
     end if
   ENDDO

CASE('pdc')

   DO node=1,Nmax 
     zzz=Depth(DepthPerm(node))
     if ( GM(GMPerm(node)) .lt. mskcrit .and. zzz .ge. epsz ) then
        ! Temperature and salinity at depth Za
        if ( Za .le. z1 ) then
          T0 = Tsrf
          S0 = Ssrf + Za * S_stratif
        elseif ( Za .ge. z2 ) then
          T0 = Tbot
          S0 = Sbot + Za * S_stratif
        else
          T0 = (Tbot-Tsrf)*Za/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
          S0 = (Sbot-Ssrf)*Za/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + Za*S_stratif
        endif
        Tf = lbd1*S0 + lbd2 + lbd3*zzz  ! Sea water freezing temperature
        ! Melt in m/yr (meters of ice per year), positive if ice ablation
        Melt(MeltPerm(node)) = - gT * meltfac * abs(T0-Tf) * (T0-Tf)  ! Pollard & DeConto 2012
     else
        Melt(MeltPerm(node)) = 0.0_dp
     end if
   ENDDO

CASE('pdcstar')

   !- we first store T0-Tf in the melt variable :
   DO node=1,Nmax 
     zzz=Depth(DepthPerm(node))
     if ( GM(GMPerm(node)) .lt. mskcrit .and. zzz .ge. epsz ) then
        ! Temperature and salinity at depth Za
        if ( Za .le. z1 ) then
          T0 = Tsrf
          S0 = Ssrf + Za * S_stratif
        elseif ( Za .ge. z2 ) then
          T0 = Tbot
          S0 = Sbot + Za * S_stratif
        else
          T0 = (Tbot-Tsrf)*Za/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
          S0 = (Sbot-Ssrf)*Za/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + Za*S_stratif
        endif
        Tf = lbd1*S0 + lbd2 + lbd3*zzz  ! Sea water freezing temperature
        ! NB: we first store (T0-Tf) in the melt variable :
        Melt(MeltPerm(node)) = T0-Tf
     else
        Melt(MeltPerm(node)) = 0.0_dp
     end if
   ENDDO

   !- we now calculate the average thermal forcing :
   Integ = 0.0_dp
   Area  = 0.0_dp
   ! Integrate melt rate over the Elmer grid for conservative interpolation purpose
   DO e=1,Solver % NumberOfActiveElements
      Element => GetActiveElement(e)
      CALL GetElementNodes( ElementNodes )
      n = GetElementNOFNodes()
      NodeIndexes => Element % NodeIndexes
      VisitedNode(NodeIndexes(1:n))=VisitedNode(NodeIndexes(1:n))+1.0_dp
      ! leave the loop if grounded point in the element
      IF ( ANY( GM(GMPerm(NodeIndexes(:))) .GE. mskcrit )  .OR. ANY(Depth(DepthPerm(NodeIndexes(:))) .LT. epsz ) ) CYCLE
      localInteg = 0.0_dp
      localunity = 0.0_dp
      IntegStuff = GaussPoints( Element )
      DO t=1,IntegStuff % n
         U = IntegStuff % u(t)
         V = IntegStuff % v(t)
         W = IntegStuff % w(t)
         stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
              Basis,dBasisdx )
         s = SqrtElementMetric * IntegStuff % s(t)
         localInteg = localInteg + s * SUM(Basis(:) * Melt(MeltPerm(NodeIndexes(:))))
         localunity = localunity + s * SUM(Basis(:))
      END DO
      Integ = Integ + localInteg
      Area  = Area  + localunity
   END DO
   IF (Parallel) THEN
    CALL MPI_ALLREDUCE(Integ,Integ_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    CALL MPI_ALLREDUCE(Area,Area_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
   ELSE
    Integ_Reduced = Integ
    Area_Reduced = Area
   END IF
   AvgTmTf = Integ_Reduced / Area_Reduced

   !- Now we calculate the melt rate :
   DO node=1,Nmax 
     zzz=Depth(DepthPerm(node))
     if ( GM(GMPerm(node)) .lt. mskcrit .and. zzz .ge. epsz ) then
        ! Temperature and salinity at depth Za 
        if ( Za .le. z1 ) then
          T0 = Tsrf
          S0 = Ssrf + Za * S_stratif
        elseif ( Za .ge. z2 ) then
          T0 = Tbot
          S0 = Sbot + Za * S_stratif
        else
          T0 = (Tbot-Tsrf)*Za/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
          S0 = (Sbot-Ssrf)*Za/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + Za*S_stratif
        endif
        Tf = lbd1*S0 + lbd2 + lbd3*zzz  ! Sea water freezing temperature
        ! Melt in m/yr (meters of ice per year), positive if ice ablation
        Melt(MeltPerm(node)) = - gT * meltfac * AvgTmTf * (T0-Tf)
     else
        Melt(MeltPerm(node)) = 0.0_dp
     end if
   ENDDO

CASE('lazer')

   ! 1 -> simple law
   ! 2 -> TC lazeroms law
   ! 3 -> Appendix B TCD Lazeroms law
   ! 4 -> Jenkins law
   ! new method to implement, instead of taking the GL, take 2 kms from the GL
   ! it should remove small ice rises

   if (LazerType .eq. 2 .or. LazerType .eq. 3 .or. LazerType .eq. 4) then
      isfslopeVar => VariableGet( Model % Mesh % Variables, 'isfslope')
      IF (.NOT.ASSOCIATED(isfslopeVar)) &
      &    CALL FATAL(SolverName,'isfslope not found')

      isfslopePerm => isfslopeVar % Perm
      isfslope => isfslopeVar % Values
   endif

   ! FIRST part, exhanging information between partitions
   ! Number of GL nodes, their position and depth
   ! for any LazerType
   if (Parallel) then
      ! count the number of GL nodes in this partition 
      ngl=0
      DO node=1,Nmax
        IF (GM(GMPerm(node)) .EQ. 0) THEN
          ngl = ngl+1
        ENDIF
      ENDDO

      debilos=-9999
      ! if nodes in GL, fill up xgl,ygl and dgl
      if (ngl .gt. 0) then
        allocate(xgl(ngl),ygl(ngl),dgl(ngl))
        cpt=1
        DO node=1,Nmax
          IF (GM(GMPerm(node)) .EQ. 0) THEN
          ! REALLY BAD SOLUTION TO REMOVE ICE RISES, GET A BETTER PLAN FOR REAL LIFE
            IF (LazerType .EQ. 4 .AND. Mesh % Nodes % x(node) .GT. 506000.0_dp) THEN
              xgl(cpt) = 506000.0_dp
            ELSE
              xgl(cpt) = Mesh % Nodes % x(node)
            ENDIF
            ygl(cpt) = Mesh % Nodes % y(node)
            dgl(cpt) = Depth(DepthPerm(node))
            cpt=cpt+1
          ENDIF
        ENDDO
      ELSE
        ngl=debilos
        ALLOCATE(xgl(1),ygl(1),dgl(1))
        xgl(1)=debilos
        ygl(1)=debilos
        dgl(1)=debilos
      ENDIF

      ! first, send and receive the number of GL nodes between all partitions
      do i=1,ParEnv % PEs
         ! need to send info to all partitions but this one
         if ( ParEnv % MyPE .ne. (i-1) ) then
            call MPI_SEND(ngl,1,MPI_Integer,i-1,1004,ELMER_COMM_WORLD,ierr)
         endif
      enddo

      ngltot=0
      do i=1,ParEnv % PEs
         ! need to send info to all partitions but this one
         if ( ParEnv % MyPE .ne. (i-1) ) then
            call MPI_RECV(nglloc,1,MPI_Integer,i-1,1004,ELMER_COMM_WORLD,status,ierr)
            if (nglloc .ne. debilos) then
               ngltot=ngltot+nglloc
            endif
         else
            if (ngl .ne. debilos) then
               ngltot=ngltot+ngl
            endif
         endif
      enddo

      ! ngltot should always be positive whenever you have a grounding line
      ! means ngltot > 0
      allocate(xgltot(ngltot),ygltot(ngltot),dgltot(ngltot))

      do i=1,ParEnv % PEs
         if ( ParEnv % MyPE .ne. (i-1) ) then
            call MPI_SEND(ngl,1,MPI_Integer,i-1,1006,ELMER_COMM_WORLD,ierr)
            if (ngl .ne. debilos) then
               call MPI_SEND(xgl,ngl,MPI_DOUBLE_PRECISION,i-1,1001,ELMER_COMM_WORLD,ierr)
               call MPI_SEND(ygl,ngl,MPI_DOUBLE_PRECISION,i-1,1002,ELMER_COMM_WORLD,ierr)
               call MPI_SEND(dgl,ngl,MPI_DOUBLE_PRECISION,i-1,1003,ELMER_COMM_WORLD,ierr)
            else
               call MPI_SEND(xgl,1,MPI_DOUBLE_PRECISION,i-1,1001,ELMER_COMM_WORLD,ierr)
               call MPI_SEND(ygl,1,MPI_DOUBLE_PRECISION,i-1,1002,ELMER_COMM_WORLD,ierr)
               call MPI_SEND(dgl,1,MPI_DOUBLE_PRECISION,i-1,1003,ELMER_COMM_WORLD,ierr)
            endif
         endif
      enddo

      cpt=1
      do i=1,ParEnv % PEs
         if ( ParEnv % MyPE .ne. (i-1) ) then

            call MPI_RECV(nglloc,1,MPI_Integer,i-1,1006,ELMER_COMM_WORLD,status,ierr)

            if (nglloc .ne. debilos) then

               allocate(xglloc(nglloc),yglloc(nglloc),dglloc(nglloc))
               call MPI_RECV(xglloc,nglloc,MPI_DOUBLE_PRECISION,i-1,1001,ELMER_COMM_WORLD,status,ierr)
               call MPI_RECV(yglloc,nglloc,MPI_DOUBLE_PRECISION,i-1,1002,ELMER_COMM_WORLD,status,ierr)
               call MPI_RECV(dglloc,nglloc,MPI_DOUBLE_PRECISION,i-1,1003,ELMER_COMM_WORLD,status,ierr)

               xgltot(cpt:cpt+nglloc-1)=xglloc
               ygltot(cpt:cpt+nglloc-1)=yglloc
               dgltot(cpt:cpt+nglloc-1)=dglloc

               cpt=cpt+nglloc

               deallocate(xglloc,yglloc,dglloc)
            else
               allocate(xglloc(1),yglloc(1),dglloc(1))
               call MPI_RECV(xglloc,1,MPI_DOUBLE_PRECISION,i-1,1001,ELMER_COMM_WORLD,status,ierr)
               call MPI_RECV(yglloc,1,MPI_DOUBLE_PRECISION,i-1,1002,ELMER_COMM_WORLD,status,ierr)
               call MPI_RECV(dglloc,1,MPI_DOUBLE_PRECISION,i-1,1003,ELMER_COMM_WORLD,status,ierr)
               deallocate(xglloc,yglloc,dglloc)
            endif
         else
            if (ngl .ne. debilos) then
               xgltot(cpt:cpt+ngl-1)=xgl
               ygltot(cpt:cpt+ngl-1)=ygl
               dgltot(cpt:cpt+ngl-1)=dgl
               cpt=cpt+ngl
            endif
         endif
      enddo

   else !if not parallel
      WRITE(Message,'(A)') 'Lazeroms not coded in serial simulations'
      WRITE(Message,'(A)') 'PLease try in parallel'
      CALL FATAL(SolverName,Message)
   endif

   ! For simple law, get a group of points around the central flowline
   ! that should correspond to the deepest points at the GL
   if (LazerType .eq. 1) then
     zGLmin=0.0_dp
     xGLmin=0.0_dp
     yGLmin=40000.0_dp ! CHANGE EVENTUALLY
     !yGLmin=(minval(Mesh % Nodes % y)+maxval(Mesh % Nodes % y))/2.0_dp
     cpt=0
     DO ii=1,ngltot
       disty=abs(ygltot(ii)-yGLmin)
       if (disty.le.epsy) then
         zGLmin=zGLmin+dgltot(ii)
         xGLmin=xGLmin+xgltot(ii)
         cpt=cpt+1
       endif
     ENDDO
     zGLmin=zGLmin/cpt
     xGLmin=xGLmin/cpt
   endif

   if (LazerType .eq. 4) then

     refy = 40000.0_dp
     refx = 1000000.0_dp

     ALLOCATE(sinus(ngltot),inds(ngltot),xgltot2(ngltot),ygltot2(ngltot),dgltot2(ngltot),dgltotsmooth(ngltot))
     DO ii=1,ngltot
       inds(ii) = ii
     ENDDO

     sinus=(ygltot-refy)/(xgltot-refx) ! value is increased as y gets lower

     ! order this vector using Elmer routines
     CALL SortD(ngltot,sinus,inds)

     DO ii=1,ngltot
       xgltot2(ii) = xgltot(inds(ii))
       ygltot2(ii) = ygltot(inds(ii))
       dgltot2(ii) = dgltot(inds(ii))
     ENDDO

     !do ii=1,ngltot
     !  print *, 'sinus', sinus(ii)
     !  print *, 'inds', inds(ii)
     !  print *, 'xgltot', xgltot2(ii)
     !  print *, 'dgltot', dgltot2(ii)
     !enddo
     
     ! smooth the angles' vector
     hwidth = 4
     DO ii=1,ngltot
       IF (ii .LT. hwidth .or. ii .GT. ngltot-hwidth) then
         dgltotsmooth(ii)=dgltot2(ii)
       else
         dgltotsmooth(ii)=sum(dgltot2(ii-hwidth:ii+hwidth))/(2.*hwidth+1.)
       endif
     enddo
     dgltot2 = dgltotsmooth
   ENDIF

   DO node=1,Nmax
     zzz=Depth(DepthPerm(node))
     ! NB: this param does not allow melt at the GL
     if ( GM(GMPerm(node)) .lt. -0.5 .and. zzz .ge. epsz ) then    
       ! Effective slope angle calculated through Solver Compute2DNodalGradient
       ! tan(alpha) = sqrt( (dZb/dx)^2 + (dZb/dy)^2 ) (Lazeroms et al. 2017, Appendix B) :
       ! maximum local slope only accounted for in the AppenB type
       if (LazerType .eq. 3 .OR. LazerType .EQ. 4) then
         alpha = atan( sqrt( isfslope(2*(isfslopePerm(node)-1)+1)**2 + isfslope(2*(isfslopePerm(node)-1)+2)**2 ) )
       endif

       ! Calculate the effective grounding line depth (Lazeroms et al. 2017, Appendix B) :
       xP  =  Mesh % Nodes % x(node)
       yP  =  Mesh % Nodes % y(node)

       if (LazerType .eq. 1) then
         if (zGLmin .gt. zzz) then
           zGL=zGLmin
           alpha = atan(abs(zGL-zzz)/sqrt((xGLmin-xP)**2+(yGLmin-yP)**2))
         else
           zGL=0.0_dp
           alpha=0.0_dp
         endif
       endif

       IF (LazerType .EQ. 2) THEN
         zGL = 0.0_dp
         sn = 0.0_dp
         cptglob = 0
         ! the two local slopes along x and y
         alphax = isfslope(2*(isfslopePerm(node)-1)+1)
         alphay = isfslope(2*(isfslopePerm(node)-1)+2)
         ! HARD CODING, TO BE CHANGED
         ! define a radius, bigger than the domain size
         radius = 1000000.0_dp
         nbdir = 64
         angle = 360.0_dp / nbdir * PI / 180.0_dp

         DO ii=1,nbdir

           ! calculate the positions of the two other points in every triangle
           angle1 = (ii-1) * angle
           angle2 = ii * angle

           locslope1 = ( COS(ATAN(alphax)) * SIN(ATAN(alphay)) * SIN(angle1) + COS(ATAN(alphay)) * SIN(ATAN(alphax)) * COS(angle1) ) / ( COS(ATAN(alphax)) * COS(ATAN(alphay)) )
           locslope2 = ( COS(ATAN(alphax)) * SIN(ATAN(alphay)) * SIN(angle2) + COS(ATAN(alphay)) * SIN(ATAN(alphax)) * COS(angle2) ) / ( COS(ATAN(alphax)) * COS(ATAN(alphay)) )

           locslope = (locslope1 + locslope2) / 2.0_dp
           if (locslope .GE. 0.0_dp) CYCLE

           xP2 = xP + radius * COS(angle1)
           yP2 = yP + radius * SIN(angle1)
           xP3 = xP + radius * COS(angle2)
           yP3 = yP + radius * SIN(angle2)

           xP12 = xP - xP2
           yP12 = yP - yP2
           xP23 = xP2 - xP3
           yP23 = yP2 - yP3
           xP31 = xP3 - xP
           yP31 = yP3 - yP

           ! now check if GL points lie inside the triangles
           cpttmp = 0
           zGLtmp = 0.0_dp
           !closestgl = 1000000.0_dp
           !lgl = 0.0_dp
           DO jj=1,ngltot
             cond1 = SIGN(1.0_dp,determinant(xP31,yP31,xP23,yP23)) * SIGN(1.0_dp,determinant(xP3-xgltot(jj),yP3-ygltot(jj),xP23,yP23)) .GE. 0
             cond2 = SIGN(1.0_dp,determinant(xP12,yP12,xP31,yP31)) * SIGN(1.0_dp,determinant(xP-xgltot(jj),yP-ygltot(jj),xP31,yP31)) .GE. 0
             cond3 = SIGN(1.0_dp,determinant(xP23,yP23,xP12,yP12)) * SIGN(1.0_dp,determinant(xP2-xgltot(jj),yP2-ygltot(jj),xP12,yP12)) .GE. 0
             IF (cond1 .AND. cond2 .AND. cond3) THEN
             !  IF (SQRT((xP-xgltot(jj))**2+(yP-ygltot(jj))**2) .LT. closestgl) THEN
             !    closestgl = dgltot(jj)
             !    lgl = SQRT((xP-xgltot(jj))**2+(yP-ygltot(jj))**2)
             !  ENDIF
             !ENDIF
               zGLtmp = zGLtmp + dgltot(jj)
               cpttmp = cpttmp + 1
             ENDIF
           ENDDO

           IF (cpttmp .EQ. 0) CYCLE
           !if (closestgl .eq. 1000000.d0) CYCLE

           zGLloc = zGLtmp / cpttmp
           !zGLloc = closestgl

           if (zGLloc .GT. zzz) THEN
             zGL = zGL + zGLloc
             sn = sn + abs(locslope)
             cptglob = cptglob + 1
           ENDIF

         ENDDO

         IF (cptglob .GT. 0) THEN
           zGL = zGL / cptglob
           alpha = ATAN(sn/cptglob)
         ELSE
           zGL = zzz
           alpha = 0.0_dp
         ENDIF

       ENDIF

       IF (LazerType .EQ. 3) THEN
         zGL = 0.0_dp
         div = 0.0_dp
         ! define a radius, bigger than the domain size
         ! HARD CODING, TO BE CHANGED
         radius = 1000000.0_dp
         nbdir = 64
         angle = 360.0_dp / nbdir * PI / 180.0_dp

         DO ii=1,nbdir

           ! calculate the positions of the two other points in every triangle
           angle1 = (ii-1) * angle
           angle2 = ii * angle
           xP2 = xP + radius * COS(angle1)
           yP2 = yP + radius * SIN(angle1)
           xP3 = xP + radius * COS(angle2)
           yP3 = yP + radius * SIN(angle2)

           xP12 = xP - xP2
           yP12 = yP - yP2
           xP23 = xP2 - xP3
           yP23 = yP2 - yP3
           xP31 = xP3 - xP
           yP31 = yP3 - yP

           ! now check if GL points lie inside the triangles
           cpttmp = 0
           zGLtmp = 0.0_dp
           ltmp = 0.0_dp
           DO jj=1,ngltot
             cond1 = SIGN(1.0_dp,determinant(xP31,yP31,xP23,yP23)) * SIGN(1.0_dp,determinant(xP3-xgltot(jj),yP3-ygltot(jj),xP23,yP23)) .GE. 0
             cond2 = SIGN(1.0_dp,determinant(xP12,yP12,xP31,yP31)) * SIGN(1.0_dp,determinant(xP-xgltot(jj),yP-ygltot(jj),xP31,yP31)) .GE. 0
             cond3 = SIGN(1.0_dp,determinant(xP23,yP23,xP12,yP12)) * SIGN(1.0_dp,determinant(xP2-xgltot(jj),yP2-ygltot(jj),xP12,yP12)) .GE. 0
             IF (cond1 .AND. cond2 .AND. cond3) THEN
               zGLtmp = zGLtmp + dgltot(jj)
               ltmp = ltmp + SQRT((xP-xgltot(jj))**2+(yP-ygltot(jj))**2)
               cpttmp = cpttmp + 1
             ENDIF
           ENDDO

           IF (cpttmp .EQ. 0) CYCLE

           zGLloc = zGLtmp / cpttmp

           IF (zGLloc .GT. zzz) THEN
             ltmp = ltmp / cpttmp
             wn = (zGLloc - zzz) / ltmp
             zGL = zGL + wn * zGLloc
             div = div + wn
           ENDIF

         ENDDO

         IF (div .GT. 0.0_dp) THEN
           zGL = zGL / div
         ELSE
           zGL = zzz
           alpha = 0.0_dp
         ENDIF
       ENDIF

       IF (LazerType .EQ. 4) THEN
         ! find the closest GL point
         mindist = 1000000.0e9
         kkstart = -9999
         DO kk=1,ngltot
           dist = SQRT((xP-xgltot2(kk))**2+(yP-ygltot2(kk))**2)
           IF (dist .eq. 0.0) CYCLE
           IF (dgltot2(kk) .GT. zzz .AND. dist .LT. mindist) THEN
             mindist = dist
             kkstart = kk
           ENDIF
         ENDDO

         IF (kkstart .LE. 0) THEN
           zGL = zzz
           alpha = 0.0
         ELSE
           ! go find the GL points from where will start the plume
           halfw = 2
           DO kk=kkstart,ngltot-halfw
             IF (dgltot2(kk) .GE. dgltot2(kk+1) .AND. dgltot2(kk) .GE. dgltot2(kk+2)) EXIT
           ENDDO

           ! take 5 GL points to build zGL and alpha
           halfpt = 2
           zGL = 0.0
           xGLfin = 0.0
           yGLfin = 0.0

           IF (kk-halfpt .LE. 0 .OR. kk+halfpt .GE. ngltot+1) THEN
             zGL=dgltot2(kk)
             xGLfin=xgltot2(kk)
             yGLfin=ygltot2(kk)
           ELSE
             DO kk1=kk-halfpt,kk+halfpt
               zGL = zGl + dgltot2(kk1)
               xGLfin = xGLfin + xgltot2(kk1)
               yGL = yGL + ygltot2(kk1)
             ENDDO
             zGL = zGL / (2.*halfpt+1.)
             xGLfin = xGLfin / (2.*halfpt+1.)
             yGL = yGL / (2.*halfpt+1.)

           ENDIF

         ENDIF

         !still need to check if zGL>zzz
         IF (zGL .GE. zzz) THEN
           dist = sqrt((xP-xGLfin)**2+(yP-yGLfin)**2)
           alpha = atan(abs(zzz-zGL)/dist)
         ELSE
           zGL=zzz
           alpha=0.0
         ENDIF

       ENDIF

       ! Ambient temperature and salinity at the effective grounding line depth :
       if ( zGL .le. z1 ) then
          T0 = Tsrf
          S0 = Ssrf + zGL * S_stratif
       elseif ( zGL .ge. z2 ) then
          T0 = Tbot
          S0 = Sbot + zGL * S_stratif
       else
          T0 = (Tbot-Tsrf)*zGL/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
          S0 = (Sbot-Ssrf)*zGL/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + zGL*S_stratif
       endif
       ! Sea water freezing temperature at the effective grounding line depth :
       Tf = lbd1*S0 + lbd2 + lbd3*zGL  
       ! Effective heat exchange coefficient :
       GTS = GamT * ( 0.545 + 3.5e-5 * (T0-Tf)/lbd3 * E0*sin(alpha)/(GefT+E0*sin(alpha)) )

       ! Melt scale :
       MM = 10. * (T0-Tf)**2 * sqrt( sin(alpha)/(Cd+E0*sin(alpha)) ) * sqrt( GTS/(GTS+E0*sin(alpha)) ) * E0*sin(alpha)/(GTS+E0*sin(alpha))
       ! Length scale :
       ll = (T0-Tf)/lbd3 * (x0*GTS+E0*sin(alpha)) / (x0*(GTS+E0*sin(alpha)))
       ! Dimensionless coordinate :
       X_hat = MAX( 0.0_dp, MIN( 1.0_dp, ( zzz - zGL ) / ll ) )
       ! Dimensionless melt curve :
       M_hat = 0.0_dp
       do kk=1,12
         M_hat = M_hat + pp(kk) * X_hat**(kk-1)
       enddo

       ! Melt rate in m/yr:
       Melt(MeltPerm(node)) = - K * MM * M_hat
     else
       Melt(MeltPerm(node)) = 0.0_dp
     endif

   ENDDO

   !if (ngl .gt. 0) then
   !   deallocate(xgl,ygl,dgl)
   !endif
   !deallocate(xgltot,ygltot,dgltot)

CASE('pico')

   distGLVar => VariableGet( Model % Mesh % Variables, 'distGL')
   IF (.NOT.ASSOCIATED(distGLVar)) &
   &    CALL FATAL(SolverName,'distGL not found')

   distIFVar => VariableGet( Model % Mesh % Variables, 'distIF')
   IF (.NOT.ASSOCIATED(distIFVar)) &
   &    CALL FATAL(SolverName,'distIF not found')

   distGLPerm => distGLVar % Perm
   distGL => distGLVar % Values

   distIFPerm => distIFVar % Perm
   distIF => distIFVar % Values

   ALLOCATE( Zbox(nD), Abox(nD), Tbox(nD), Sbox(nD), Mbox(nD) )

   !- Ambiant temperature and salinity (at depth Za)
   if ( Za .le. z1 ) then
     T0 = Tsrf
     S0 = Ssrf + Za * S_stratif
   elseif ( Za .ge. z2 ) then
     T0 = Tbot
     S0 = Sbot + Za * S_stratif
   else
     T0 = (Tbot-Tsrf)*Za/(z2-z1) + Tsrf - (Tbot-Tsrf)*z1/(z2-z1)
     S0 = (Sbot-Ssrf)*Za/(z2-z1) + Ssrf - (Sbot-Ssrf)*z1/(z2-z1) + Za*S_stratif
   endif

   !- Calculate total area and mean depth of each box :
   Abox(:)=0.0_dp
   Zbox(:)=0.0_dp
   DO e=1,Solver % NumberOfActiveElements
      Element => GetActiveElement(e)
      CALL GetElementNodes( ElementNodes )
      n = GetElementNOFNodes()
      NodeIndexes => Element % NodeIndexes
      VisitedNode(NodeIndexes(1:n))=VisitedNode(NodeIndexes(1:n))+1.0_dp
      ! leave the loop if grounded point in the element
      IF ( ANY( GM(GMPerm(NodeIndexes(:))) .GE. mskcrit ) ) CYCLE
      rr = SUM(distGL(distGLPerm(NodeIndexes(:)))    &
      &             / ( distGL(distGLPerm(NodeIndexes(:))) + distIF(distIFPerm(NodeIndexes(:))) )) &
      &    / MAX(1,SIZE(distGL(distGLPerm(NodeIndexes(:)))))
      localInteg = 0.0_dp
      localunity = 0.0_dp
      IntegStuff = GaussPoints( Element )
      DO t=1,IntegStuff % n
         U = IntegStuff % u(t)
         V = IntegStuff % v(t)
         W = IntegStuff % w(t)
         stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
              Basis,dBasisdx )
         s = SqrtElementMetric * IntegStuff % s(t)
         localInteg = localInteg + s * SUM(Basis(:)*Depth(DepthPerm(NodeIndexes(:))))
         localunity = localunity + s * SUM(Basis(:))
      END DO
      DO kk=1,nD
        IF ( rr .ge. 1.0-sqrt(1.0*(nD-kk+1)/nD) .and. rr .le. 1.0-sqrt(1.0*(nD-kk)/nD) ) THEN
          Zbox(kk) = Zbox(kk) + localInteg
          Abox(kk) = Abox(kk) + localunity
        ENDIF
      ENDDO
   END DO
   DO kk=1,nD
     IF (Parallel) THEN
       CALL MPI_ALLREDUCE(Zbox(kk),Integ_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
       CALL MPI_ALLREDUCE(Abox(kk),Area_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
       Zbox(kk) = Integ_Reduced
       Abox(kk) = Area_Reduced
     END IF
   ENDDO

   Zbox(:)=Zbox(:)/Abox(:) ! mean depth of each box

   !- Temerature and salinity in Box #1 :
   Tstar = lbd1*S0 + lbd2 + lbd3*Zbox(1) - T0
   g1 = Abox(1) * gT 
   tmp1 = g1 / (CC*rhostar*(beta*S0*meltfac-alpha))
   xbox = - 0.5*tmp1 + sqrt( (0.5*tmp1)**2 - tmp1*Tstar )
   Tbox(1) = T0 - xbox
   Sbox(1) = S0 - xbox*S0*meltfac
   qqq = CC*rhostar*(beta*(S0-Sbox(1))-alpha*(T0-Tbox(1)))
   Mbox(1) = - gT * meltfac * ( lbd1*Sbox(1) + lbd2 + lbd3*Zbox(1) - Tbox(1) )

   !- Temperature and salinity in possible other boxes :
   DO kk=2,nD
     Tstar = lbd1*Sbox(kk-1) + lbd2 + lbd3*Zbox(kk) - Tbox(kk-1)
     g1  = Abox(kk) * gT
     g2  = g1 * meltfac
     xbox = - g1 * Tstar / ( qqq + g1 - g2*lbd1*Sbox(kk-1) )
     Tbox(kk) = Tbox(kk-1) - xbox
     Sbox(kk) = Sbox(kk-1) - xbox*Sbox(kk-1)*meltfac
     Mbox(kk) = - gT * meltfac * ( lbd1*Sbox(kk) + lbd2 + lbd3*Zbox(kk) - Tbox(kk) )
   ENDDO

   !- Attribute melt at each node to a box value :
   DO node=1,Nmax
     zzz=Depth(DepthPerm(node))
     if ( GM(GMPerm(node)) .lt. mskcrit .and. zzz .ge. epsz ) then
       rr = distGL(distGLPerm(node)) / ( distGL(distGLPerm(node)) + distIF(distIFPerm(node)) )
       do kk=1,nD
         if ( rr .ge. 1.0-sqrt(1.0*(nD-kk+1)/nD) .and. rr .le. 1.0-sqrt(1.0*(nD-kk)/nD) ) then
           Melt(MeltPerm(node)) = - Mbox(kk)

           IF (zzz + Melt(MeltPerm(node)) * dt .LE. epsz) THEN
             Melt(MeltPerm(node)) = 0.0_dp
           ENDIF

         endif

       enddo
     else
       Melt(MeltPerm(node)) = 0.0_dp
     endif
   ENDDO

END SELECT

!---------------------------------------------

Integ = 0.0_dp  
! Integrate melt rate over the Elmer grid for conservative interpolation purpose
DO e=1,Solver % NumberOfActiveElements
   Element => GetActiveElement(e)
   CALL GetElementNodes( ElementNodes )
   n = GetElementNOFNodes()
   NodeIndexes => Element % NodeIndexes
   VisitedNode(NodeIndexes(1:n))=VisitedNode(NodeIndexes(1:n))+1.0_dp
   localInteg = 0.0_dp
   IntegStuff = GaussPoints( Element )
   DO t=1,IntegStuff % n
      U = IntegStuff % u(t)
      V = IntegStuff % v(t)
      W = IntegStuff % w(t)
      stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
           Basis,dBasisdx )
      s = SqrtElementMetric * IntegStuff % s(t)
      localInteg = localInteg + s * SUM(Basis(:) * Melt(MeltPerm(NodeIndexes(:))))
   END DO
   Integ = Integ + localInteg
END DO

!Parallel = .FALSE.
!IF ( ASSOCIATED( Solver % Matrix % ParMatrix ) ) THEN
!     IF ( Solver %  Matrix % ParMatrix % ParEnv % PEs > 1 )  THEN
!        Parallel = .TRUE.
!     END IF
!END IF

IF (Parallel) THEN
    CALL MPI_ALLREDUCE(Integ,Integ_Reduced,1,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
ELSE
    Integ_Reduced = Integ
ENDIF

IF (Solver % Matrix % ParMatrix % ParEnv % MyPE == 0) then
   WRITE(meltValue,'(F20.2)') Integ_Reduced 
   Message='TOTAL_MELT_RATE: '//meltValue
   CALL INFO(SolverName,Message,Level=1)
END IF

!!!
CONTAINS

  FUNCTION determinant(x1,y1,x2,y2) RESULT(det)
    REAL(KIND=dp), INTENT(IN) :: x1, y1, x2, y2
    REAL(KIND=dp) :: det

    det=x1*y2-x2*y1

  END FUNCTION determinant

END SUBROUTINE MISMIP_Melt_Param_Zambiant





#!/bin/bash

export LANG=C

###################################################
## User's choices :
###################################################

NUM_PARTITIONS_ELMER=24
NUM_NODES_ELMER=1

NRUN_MAX=11  # maximum number of consecutive Elmer/Ice runs

#ELMER_MESH_NAME=MISMIP_REGULAR

TIME_STEP_ELMER=0.0833333333         # ELMER time step (in yr)
INTERVALS_ELMER=120                  # duration of ELMER run (in nb time steps)
FREQ_OUTPUT_ELMER=12                 # frequency for ELMER outputs
                                     #   NB1: should be ${INTERVALS_ELMER}
                                     #        or ${INTERVALS_ELMER} times an integer
                                     #   NB2: there is also another output at ELMER's initial step

## Elmer/Ice restart used as initial state for the coupled simultion
#  NB: restart file is expected to be in ${PATH_RESTART}/${CASE_RESTART}/Results/${RUN_RESTART} 
#                           with Mesh in ${PATH_RESTART}/${CASE_RESTART}/Mesh/${RUN_RESTART} 
PATH_RESTART=${STOREDIR}/output_MISMIP+
CASE_RESTART=Test500m_Schoof_SSAStar
RUN_RESTART=Run0

MELT_PARAM=$5          ## Melt parameterization. Choice in:
                       ##  bg      >> Constant exchange velocity (e.g. Beckmann and Goose 2002)
                       ##  pdc     >> Exchange velocity % Tf (Pollard and DeConto 2012)
                       ##  pdcstar >> Exchange velocity % Tf averaged over the ice draft
                       ##  lazer   >> see Lazeroms et al. 2017
                       ##  pico    >> see Reese et al. 2017
BOXES=10                #for PICO only

MELT_COEF=$2           ## Will multiply melt, to be given as a second argument

ZAMBIANT=$3            ## For Zambiant, to be given as the third argument

FORCING_EXP_ID=$4      ## ='EXP3' for ocean relaxation towards warm conditions 
                       ## ='EXP4' for ocean relaxation towards cold conditions
                       ## NB: should always be "EXP" + a number

PREFIX_ELMER='Ice1r'   ## ='Ice1r' for retreat and warm ocean forcing (FORCING_EXP_ID=EXP3)
                       ## ='Ice1a' for readvance and cold ocean forcing (FORCING_EXP_ID=EXP4)

WORKDIR_ELMER=${SCRATCHDIR}/ELMERICE/PARAMSZa
STORE_ELMER=${STOREDIR}/ELMERICE/PARAMSZa

###################################################
## End of User's choices
###################################################

#Do nothing and leave the script if no argument is supplied
if [ $# -lt 5 ]
then
        echo "You didn't supply enough arguments >>>>>>>>>  STOP !!"
	echo "1st argument: Case name                              " 
        echo "2nd argument: melting coef                           "
        echo "3rd argument: Zambiant                               "
        echo "4rt argument: 'EXP??'                                "
        echo "5th argument: 'param' (bg, pdc,pdcstar,pico)         "
        exit 1
fi

CHECK_CASE_1=`echo $1 | grep "${MELT_PARAM}" |wc -l`
CHECK_CASE_2=`echo $1 | awk '{print tolower($0)}' | grep "${MELT_PARAM}" |wc -l`
if [ $CHECK_CASE_1 -eq 0 ] && [ $CHECK_CASE_2 -eq 0 ]; then
  echo 'Argument ($CASE) should include the Melt parameterization choice >>>>>>>>> STOP !!'
  exit
fi

CHECK_CASE_1=`echo $1 | grep "${FORCING_EXP_ID}" |wc -l`
CHECK_CASE_2=`echo $1 | awk '{print tolower($0)}' | grep "${FORCING_EXP_ID}" |wc -l`
if [ $CHECK_CASE_1 -eq 0 ] && [ $CHECK_CASE_2 -eq 0 ]; then
  echo 'Argument ($CASE) should include the Experience Name >>>>>>>>> STOP !!'
  exit
fi

CHECK_CASE_1=`echo $1 | grep "${MELT_COEF}" |wc -l`
CHECK_CASE_2=`echo $1 | awk '{print tolower($0)}' | grep "${FORCING_EXP_ID}" |wc -l`
if [ $CHECK_CASE_1 -eq 0 ] && [ $CHECK_CASE_2 -eq 0 ]; then
  echo 'Argument ($CASE) should include the melting coefficient >>>>>>>>> STOP !!'
  exit
fi

echo "Creating Run $1"
echo "  > experiment is ${FORCING_EXP_ID} / ${PREFIX_ELMER}"
echo "  > melt param is $MELT_PARAM"
echo "  > melt coefficient is $MELT_COEF"

#Create folders
HOMEDIR_MISMIP=$PWD/RUNS/runs/$1
mkdir -p $HOMEDIR_MISMIP
mkdir -p $WORKDIR_ELMER/runs/$1
mkdir -p $WORKDIR_ELMER/runs/$1/Results
mkdir -p $WORKDIR_ELMER/runs/$1/Work
mkdir -p $WORKDIR_ELMER/Executables

ELMER_WORK_PATH=$WORKDIR_ELMER/runs/$1/Work

#Files in HOMEDIR

#ln -sf $From_VTK_TO_NETCDF_PATH $HOMEDIR_MISMIP/fromVTKtoElmer

cat Makefile_G | sed -e "s#<ExecutablePath>#$WORKDIR_ELMER/Executables/#g" > $ELMER_WORK_PATH/Makefile

cat Scripts/scriptIce1rExecute_Za.sh | sed -e "s#<run>#$1#g" \
                 -e "s#<NRUN_MAX>#$NRUN_MAX#g" \
                 -e "s#<HOMEDIR_MISMIP>#$HOMEDIR_MISMIP#g" \
                 -e "s#<OUTPUT_FREQ_ELMER>#$FREQ_OUTPUT_ELMER#g" \
                 -e "s#<INTERVALS_ELMER>#$INTERVALS_ELMER#g" \
                 -e "s#<TIME_STEP_ELMER>#$TIME_STEP_ELMER#g" \
                 -e "s#<numParts>#$NUM_PARTITIONS_ELMER#g" \
                 -e "s#<numNodes>#$NUM_NODES_ELMER#g" \
                 -e "s#<WORKDIR_ELMER>#$WORKDIR_ELMER#g" \
                 -e "s#<Executables>#$WORKDIR_ELMER/Executables/#g" \
                 -e "s#<MeltPar>#$MELT_PARAM#g" \
                 -e "s#<MeltCoef>#$MELT_COEF#g" \
                 -e "s#<Boxes>#$BOXES#g" \
                 -e "s#<Zambiant>#$ZAMBIANT#g" \
                 -e "s#<EXP>#$FORCING_EXP_ID#g" \
                 -e "s#<MeshNamePath>#$WORKDIR_ELMER/runs/$1#g" \
                 -e "s#<STORE_ELMER>#$STORE_ELMER#g" > $ELMER_WORK_PATH/scriptIce1rExecute_Za.sh
chmod a+rx $ELMER_WORK_PATH/scriptIce1rExecute_Za.sh

cat Scripts/scriptInitDomain.sh | sed -e "s#<run>#$1#g" \
                 -e "s#<caseTest>#$CASE_RESTART#g" \
                 -e "s#<path_restart>#$PATH_RESTART#g" \
                 -e "s#<HOMEDIR_MISMIP>#$HOMEDIR_MISMIP#g" \
                 -e "s#<RunRestart>#$RUN_RESTART#g" \
                 -e "s#<numParts>#$NUM_PARTITIONS_ELMER#g" \
                 -e "s#<numNodes>#$NUM_NODES_ELMER#g" \
                 -e "s#<WORKDIR_ELMER>#$WORKDIR_ELMER#g" \
                 -e "s#<Executables>#$WORKDIR_ELMER/Executables/#g" \
                 -e "s#<MeshNamePath>#$WORKDIR_ELMER/runs/$1#g" > $ELMER_WORK_PATH/scriptInitDomain.sh
chmod a+rx $ELMER_WORK_PATH/scriptInitDomain.sh

#cat Scripts/write_coupling_run_info.sh | sed -e "s#<HOMEDIR_MISMIP>#$HOMEDIR_MISMIP#g" > $HOMEDIR_MISMIP/write_coupling_run_info.sh
#chmod a+rx $HOMEDIR_MISMIP/write_coupling_run_info.sh

#cat Scripts/script_Exec_MISMIP.sh | sed -e "s#<run>#$1#g" \
#		 -e "s#<NEMO_RUN>#$WORKDIR_NEMO/run/$1#g" \
#                 -e "s#<ELMER_RUN>#$ELMER_WORK_PATH#g" > $HOMEDIR_MISMIP/script_Exec_MISMIP.sh
#chmod a+rx $HOMEDIR_MISMIP/script_Exec_MISMIP.sh

cat Scripts/script_RUN_MISMIP.sh | sed -e "s#<run>#$1#g" \
                 -e "s#<ELMER_RUN>#$ELMER_WORK_PATH#g" > $HOMEDIR_MISMIP/script_RUN_MISMIP.sh
chmod a+rx $HOMEDIR_MISMIP/script_RUN_MISMIP.sh

#cat Scripts/script_SpinUp_MISMIP.sh | sed -e "s#<run>#$1#g" \
#                 -e "s#<ELMER_RUN>#$ELMER_WORK_PATH#g" > $HOMEDIR_MISMIP/script_SpinUp_MISMIP.sh
#chmod a+rx $HOMEDIR_MISMIP/script_SpinUp_MISMIP.sh

#cat Scripts/script_Start_From_Restart.sh | sed -e "s#<run>#$1#g" \
#                 -e "s#<NEMO_RUN>#$WORKDIR_NEMO/run/$1#g" \
#                 -e "s#<RESTART_FILE>#$RST_FILE#g" \
#                 -e "s#<ELMER_RUN>#$ELMER_WORK_PATH#g" > $HOMEDIR_MISMIP/script_Start_From_Restart.sh
#chmod a+rx $HOMEDIR_MISMIP/script_Start_From_Restart.sh

cp Scripts/read_write_Elmer_run_info.sh $HOMEDIR_MISMIP/read_write_Elmer_run_info.sh
chmod a+rx $HOMEDIR_MISMIP/read_write_Elmer_run_info.sh

ln -sf $ELMER_WORK_PATH $HOMEDIR_MISMIP/WORK_ELMER

#SetFiles

#COMPILING ELMER SOLVERS



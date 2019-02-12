#!/bin/bash


#### JOB NAME
#SBATCH -J elmersolo_<jobName>

#### RESSOURCES: Here 10 nodes (i.e. 240 cores) (each node as 24 cores)
#SBATCH --nodes=<nodes>
#SBATCH --ntasks=<tasks>
#SBATCH --ntasks-per-node=24
#SBATCH --cpus-per-task=1
#SBATCH --constraint=BDW28
##SBATCH --constraint=HSW24
#SBATCH --exclusive

####  TIME
#SBATCH --time=<time>

##### OUTPUT FILES
#SBATCH --output MyJOB_SRUN.%j.output
#SBATCH --error MyJOB_SRUN.%j.error

######
export OMP_NUM_THREADS=1

#####
echo "Starting at `date`"
echo "Running on hosts: $SLURM_NODELIST"
echo "Running on $SLURM_NNODES nodes."
echo "Running on $SLURM_NPROCS processors."
echo "Current working directory is `pwd`"

#### RUN ELMER


WORKPATH=`pwd`

module list

#source $HOMEDIR/scriptModulesELMER.sh

#module list
time
srun --mpi=pmi2 -K1 --resv-ports -n $SLURM_NTASKS ElmerSolver_mpi
time

MISMIP_HOMEDIR=<MISMIP_HOMEDIR>

mv <RUN_ELMER_PATH>/runs/<RUN>/Mesh/*vtu <RUN_ELMER_PATH>/runs/<RUN>/Results/<jobName>

#######archiving
#care, we want to keep the results of previous Elmer runs for restart
number=<NUMBER>

cd ../
if [ $number -eq 1 ]
then
    echo 'Creating archive with Run0'
    tar cvf Results_<RUN>.tar Results/Run0
    rm -rf Results/Run0
fi
if [ $number -gt 1 ]
then
    echo 'Updating archive'
    tar uvf Results_<RUN>.tar Results/Ice1r$((number - 1))
    rm -rf Results/Ice1r$((number - 1))
fi
if [ $number -eq <NRUNMAX> ]
then
    echo 'Updating archive for the last time'
    tar uvf Results_<RUN>.tar Results/Ice1r$((number))
    rm -rf Results/Ice1r$((number))
fi
cd $WORKPATH
####### end of archiving

echo "RUNfile::" $Melt_Rate_File
RUNFILE="$(ls -t $WORKPATH/*.output | head -1)"
$MISMIP_HOMEDIR/read_write_Elmer_run_info.sh $RUNFILE
STATUS_RUN=`cat status_run.log`
if [ $STATUS_RUN -eq 1 ]; then
  echo 'STATUS_RUN = 1  => continue'
else
  echo 'STATUS_RUN = 0  => Stopping !!!!!!!!!!'
  exit
fi

#echo $1 >> Run_ELMER.db

#$MISMIP_HOMEDIR/write_coupling_run_info.sh 0 0 $NRUN $NRUN $RUNFILE
#stat=$?
#
#echo HOLAAA $stat
#
#if [ ! $stat == 0 ];
#then
#    echo 'ERROR IN ELMER SIMULATION ---> EXIT'
#    exit
#fi

LAST_ELMER_OUTPUT="$(ls -t <RUN_ELMER_PATH>/runs/<RUN>/Results/<jobName>/*pvtu | head -1)"

echo 'hola'
echo $LAST_ELMER_OUTPUT

cd $WORKPATH
echo $WORKPATH

./scriptIce1rExecute_Za.sh $1

time

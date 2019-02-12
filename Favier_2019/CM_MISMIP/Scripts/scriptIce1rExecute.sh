time

run=<run>
numParts=<numParts>

number=$1

mesh=Mesh
nameRestart=Ice1r$number

if [ $number -gt 1 ]; then
   nameRestart=Ice1r$((number - 1 ))
else
   nameRestart=Run0
fi

WorkPath=<WORKDIR_ELMER>

NRUN_MAX=<NRUN_MAX>
STORE_ELMER=<STORE_ELMER>
if [ $number -gt ${NRUN_MAX} ]; then
   echo '~!@#$%^&* Number of runs > $NRUN_MAX >>>>>>>>>>>> stop !!!!'
   echo 'Keeping only tar results'
   cd $WorkPath/runs/$run
   #tar cvzf Results_$run.tar.gz Results
   rm -r Results
   #mv Results_$run.tar.gz $STORE_ELMER/
   exit
fi

name=Ice1r$number

HomePath=<HOMEDIR_MISMIP>

FORCING_EXP_ID=<EXP>
FORCING_EXP_NB=`echo $FORCING_EXP_ID | sed -e "s/EXP//g"`

RUN_ELMER=$WorkPath

ExecPath=<Executables>
ResultsPath=../Results/${name}
OutputPath=../Results/${name}
MeshPath=<MeshNamePath>
mkdir -p $MeshPath/Results/$name

rm $MeshPath/RESTART
ln -sf $WorkPath/runs/$run/Results/$nameRestart $MeshPath/RESTART
Restart=../RESTART/$nameRestart.result
RestartPosition=0

sifName=$name.sif
ScketchPath=$HomePath/../../../Templates/Sif/
sketch=$ScketchPath/sketchIce1r_SSAStar_Melt_Param.sif

outIntervals=<OUTPUT_FREQ_ELMER>
Intervals=<INTERVALS_ELMER>
TimeStep=<TIME_STEP_ELMER>

# To skip unused Solvers :
MELT_PARAM="<MeltPar>"
if [ $MELT_PARAM == "pico" ]; then
  EXEC_DIST="Always"
else
  EXEC_DIST="never"
fi
if [ $MELT_PARAM == "lazer" ]; then
  EXEC_SLOPE="Always"
  EXEC_DIST="Always"
else
  EXEC_SLOPE="never"
fi 

MELT_COEF=<MeltCoef>
NB_BOXES=<Boxes>
LAZER_TYPE=<LazerType>

cat $sketch | sed -e "s#<ResultsPath>#$ResultsPath#g" \
                  -e "s#<MeshPath>#$MeshPath#g" \
                  -e "s#<Restart>#$Restart#g" \
                  -e "s#<ExecPath>#$ExecPath#g" \
                  -e "s#<RestartPosition>#$RestartPosition#g" \
                  -e "s#<outIntervals>#$outIntervals#g" \
                  -e "s#<Intervals>#$Intervals#g" \
                  -e "s#<TimeStep>#$TimeStep#g" \
                  -e "s#<ExpNb>#$FORCING_EXP_NB#g" \
                  -e "s#<name>#$name#g" \
                  -e "s#<ExeSlp>#$EXEC_SLOPE#g" \
                  -e "s#<ExeDst>#$EXEC_DIST#g" \
                  -e "s#<parmlt>#$MELT_PARAM#g" \
                  -e "s#<coefmlt>#$MELT_COEF#g" \
                  -e "s#<nb_boxes>#$NB_BOXES#g" \
                  -e "s#<lazert>#$LAZER_TYPE#g" \
                  -e "s#<mesh>#$mesh#g" > $sifName

echo $sifName >> toto
mv toto ELMERSOLVER_STARTINFO

nodes=1
tasks=$numParts
timeJob=00:29:00  # SBATCH walltime for executon of Elmer/Ice
jobName=$name
slurmSketch=$HomePath/../../../Templates/Slurm/launchSck_SoloElmer.slurm 
slurmFile=launchExec.slurm

cat $slurmSketch | sed -e "s#<jobName>#$jobName#g" \
                       -e "s#<nodes>#$nodes#g" \
                       -e "s#<RUN>#$run#g" \
                       -e "s#<MISMIP_HOMEDIR>#$HomePath#g" \
                       -e "s#<RUN_ELMER_PATH>#$RUN_ELMER#g" \
                       -e "s#<tasks>#$tasks#g" \
                       -e "s#<time>#$timeJob#g" \
                       -e "s#<NUMBER>#$number#g" \
                       -e "s#<NRUNMAX>#$NRUN_MAX#g" > $slurmFile

#jobidComp=$2

#sbatch --dependency=afterany:$jobidComp $slurmFile $(( number +1 )) $CALL_NEMO $PATH_MELT_FILE 0 "DUMMY_RST_FILE" $number
sbatch $slurmFile $(( number +1 ))

time

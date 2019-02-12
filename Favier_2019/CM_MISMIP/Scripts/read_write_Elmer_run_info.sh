#!/bin/bash

OUTPUT_FILE=$1
NRUN=$2

if grep -q 'Elmer Solver: ALL DONE' $OUTPUT_FILE; then
   STATUS_RUN=1
   echo 1 > status_run.log
else
   STATUS_RUN=0
   echo 0 > status_run.log
   echo "ERROR in ELMER RUN $OUTPUT_FILE"
   exit -1
fi

TIME=$( awk '/Time:/ {print $4}' $OUTPUT_FILE | tail -1 )

MELT_RATE=$( awk '/TOTAL_MELT_RATE:/ {print $3}' $OUTPUT_FILE | tail -1 )

THICKNESS=$( awk '/thickness/ {print $6}' $OUTPUT_FILE | tail -1 )

if [ ! -f Run.db ];
then
   echo 'NumRun  ' 'Elmer Time ' 'Total Melt ' 'Glacier thickness ' > Run.db
fi

echo $NRUN $TIME $MELT_RATE $THICKNESS >> Run.db

@echo off
REM USAGE: Coverage.bat [BAM/CRAM file]


REM JAVA_HOME="jdk-14.0.2\bin"
set Ref=References\GRCh38_full_analysis_set_plus_decoy_hla.fa
set codingIntervals=References\GRCh38_codingIntervals.int
set Rfolder=%programfiles%\R\R-4.0.3

echo ######################################################
echo CALC COVERAGE
echo Picard Tools
java.exe -jar picard.jar CollectWgsMetrics -version
"%Rfolder%\bin\Rscript.exe" --version  || exit /b
echo ######################################################


java.exe -jar picard.jar CollectHsMetrics  I=%1 O=%1_coverage.txt R=%Ref% BAIT_INTERVALS=%codingIntervals% TARGET_INTERVALS=%codingIntervals% PER_TARGET_COVERAGE=%1_CovPerTarget.txt  || exit /b


"%Rfolder%\bin\Rscript.exe" --vanilla CollectHsMetrics_to_table.R %1_CovPerTarget.txt %1_CoverageTable.txt





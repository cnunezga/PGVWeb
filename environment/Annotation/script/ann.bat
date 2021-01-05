@echo off
set JAVA_HOME="..\Programs\jdk-14.0.2\bin"
set SNPEFF_HOME="..\Programs\snpEff"
set vcfbed="..\Programs\vcfbed\vcfbed.jar"


set clinvarVCF="..\AnnotationsDB\Clinvar\clinvar.vcf.gz"
set gnomadVCF="..\AnnotationsDB\gnomad\gnomad.exomes.r2.1.1.sites.liftover_grch38.subset.vcf.bgz"
set dbnsfpVCF="..\AnnotationsDB\dbNSFP4.1a\dbNSFP4.3a_predictors.txt.gz"
set dbsnpVCF="..\AnnotationsDB\dbsnp\dbsnp151.vcf.gz"

set geneidbed="..\AnnotationsDB\GENEID\RefSeq_gene_id.bed"
set NCBIRefSeqCuratedExons="..\AnnotationsDB\NCBIRefSeqCuratedExons\NCBIRefSeqCurated.bed"


echo ######################################################
echo VARIANTS ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\snpEff.jar -version
echo ######################################################

echo ######################################################
echo VCF FILTERING REGIONS
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar intervals -i %1 %NCBIRefSeqCuratedExons% > %1_filter.vcf
echo ######################################################

echo ######################################################
echo VCF snpEff ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\snpEff.jar ann -v -canon -no PROTEIN_PROTEIN_INTERACTION_LOCUS -no PROTEIN_STRUCTURAL_INTERACTION_LOCUS -no-downstream -no-intergenic -no-upstream -noStats -ss 10 -noMotif -noNextProt GRCh38.p13.RefSeq %1_filter.vcf > %1_snpeff.vcf
echo ######################################################

echo ######################################################
echo VCF ClinVar ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar annotate -v -noLog -tabix -noId %clinvarVCF% %1_snpeff.vcf > %1_clinvar.vcf
echo ######################################################

echo ######################################################
echo VCF gnomAD ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar annotate -v -noLog -tabix -noId %gnomadVCF% %1_clinvar.vcf > %1_gnomad.vcf
echo ######################################################

echo ######################################################
echo VCF dbNSFP ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar dbnsfp -v -db %dbnsfpVCF% -f SIFT_converted_rankscore,Polyphen2_HVAR_rankscore,MutationTaster_converted_rankscore,CADD_phred,DANN_score,fathmm-MKL_coding_score,GERP++_RS,phyloP30way_mammalian,phastCons30way_mammalian %1_gnomad.vcf > %1_dbnsfp.vcf
echo ######################################################

echo ######################################################
echo VCF dbSNP ANNOTATION
%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar annotate -v -noLog -tabix -id %dbsnpVCF% %1_dbnsfp.vcf> %1_dbsnp.vcf
echo ######################################################

echo ######################################################
echo VCF GENE_ID ANNOTATION
%JAVA_HOME%\java.exe -jar %vcfbed% -B %geneidbed% -T GENE_ID -e "bed.get(3)" %1_dbsnp.vcf -o %1_geneid_FINAL.vcf --fast
echo ######################################################

%JAVA_HOME%\java.exe -jar %SNPEFF_HOME%\SnpSift.jar extractFields -s ", " -e "" %1_geneid_FINAL.vcf CHROM POS REF ALT GEN[*].GT GEN[*].DP QUAL "ANN[*].GENE" GENE_ID "ANN[*].GENEID" "ANN[*].HGVS_C" "ANN[*].HGVS_P" "ANN[*].RANK" "ANN[*].FEATUREID" "ANN[*].EFFECT" ID controls_AF controls_nhomalt CLNSIG CLNREVSTAT ALLELEID dbNSFP_SIFT_converted_rankscore dbNSFP_Polyphen2_HVAR_rankscore dbNSFP_MutationTaster_converted_rankscore dbNSFP_CADD_phred dbNSFP_DANN_score dbNSFP_fathmm_MKL_coding_score dbNSFP_GERP___RS dbNSFP_phyloP30way_mammalian  dbNSFP_phastCons30way_mammalian > %1_TABLA.txt

exit /b
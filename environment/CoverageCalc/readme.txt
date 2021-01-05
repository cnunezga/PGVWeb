Coverage Calculation using Picard Tools CollectHsMetrics
https://broadinstitute.github.io/picard/

INSTRUCTIONS
a) Download reference genome and save in References folder. Example:
ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/reference/GRCh38_reference_genome/GRCh38_full_analysis_set_plus_decoy_hla.fa

b) Generate GRCh38_codingIntervals.int from 
- BAM HEADER
- UCSC Table Browser Genes and Gene Predictions -> RefseqCurated  Output ncbiRefSeqCurated as BED Exons plus 20 bases at each end

c) Run Coverage.bat bamFile or cramFile



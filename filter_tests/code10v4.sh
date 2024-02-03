#!/bin/bash

##ESSARY JOB SPECIFICATIONS
#SBATCH --job-name=ft10v4      
#SBATCH --time=15:00:00          
#SBATCH --nodes=1                  
#SBATCH --ntasks-per-node=48        
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=3GB
#SBATCH --output=Out.%j     

##OPTIONAL JOB SPECIFICATIONS
#SBATCH --account=132714630989
#SBATCH --mail-user=jifanli@tamu.edu    
#SBATCH --mail-type=ALL    

module load GCC/11.2.0  OpenMPI/4.1.1
module load R/4.2.0

Rscript /scratch/user/jifanli/filter_tests10v4.R
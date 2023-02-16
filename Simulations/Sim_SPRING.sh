#!/bin/bash
#SBATCH -J SPRING
#SBATCH -o ./%x.%j.%N.out
#SBATCH -D ./
#SBATCH --get-user-env
#SBATCH --clusters=cm2_tiny
#SBATCH --partition=cm2_tiny
#SBATCH --nodes=1-1
#SBATCH --cpus-per-task=56
# 56 is the maximum reasonable value for CooLMUC-2
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=User mail Change!
#SBATCH --export=NONE
#SBATCH --time=14:00:00
module load slurm_setup

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/lrz/sys/spack/release/22.2.1/opt/x86_64/libjpeg-turbo/2.1.0-gcc-urdhzdt/lib64/

module load r

Rscript Sim_SPRING.R


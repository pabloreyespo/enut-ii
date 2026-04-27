#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J expenditures
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 44
#SBATCH --mem-per-cpu=3000
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 1-0:0:0
#SBATCH -o enut-ii/expenditures_%A_%a.err.out
#SBATCH -e enut-ii/expenditures_%A_%a.err.out

#-----------------Toolchain---------------------------
# ----------------Modulos----------------------------
module load r/4.4.0 
# ----------------Comando--------------------------

cd enut-ii
Rscript data_processing/expenditures.R
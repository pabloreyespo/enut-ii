#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J twins
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 40
#SBATCH --mem-per-cpu=1200
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 0-12:0:0
#SBATCH -o enut-ii/twins_%A_%a.err.out
#SBATCH -e enut-ii/twins_%A_%a.err.out

#-----------------Toolchain---------------------------
ml purge
ml intel/2022.00
# ----------------Modulos----------------------------
ml Python/3.12.3
# ----------------Comando--------------------------

cd enut-ii
source enut-env/bin/activate
python data_processing/gemelos_matriz.py

#!/bin/bash -e
#SBATCH --job-name=langMentalStates    # job name (shows up in queue)
#SBATCH --time=01-00:00:00             # Walltime (DD-HH:MM:SS)
#SBATCH --mem=80G                      # total memory
#SBATCH --cpus-per-task=4              # CPUs
#SBATCH --account=uoa03415             # Project code

# load R
module load R

# run script
Rscript run.R

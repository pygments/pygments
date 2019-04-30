#!/bin/bash
#SBATCH --partition=part
#SBATCH --job-name=job
#SBATCH --mem=1G
#SBATCH --cpus-per-task=8
srun /usr/bin/sleep
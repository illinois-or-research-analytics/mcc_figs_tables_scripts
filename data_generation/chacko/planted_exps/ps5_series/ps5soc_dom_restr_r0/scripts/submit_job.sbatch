#!/bin/bash
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --job-name=MissionControl
#SBATCH --partition=folkvangr
#SBATCH --account=folkvangr
#SBATCH --mem=64GB

source /etc/profile.d/modules.sh
bash ./scripts/batch_script.run




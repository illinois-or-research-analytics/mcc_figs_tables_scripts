#!/bin/bash
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=64
#SBATCH --job-name=MissionControl
#SBATCH --partition=secondary
#SBATCH --exclude=ccc0406,ccc0378
#SBATCH --mem=64GB

source /etc/profile.d/modules.sh


# apptainer exec -B /projects:/projects ${SHARED_MOUNT}/minhyuk2/singularity_images/python_image.sif /bin/bash ./scripts/batch_script.run
apptainer exec -B /projects:/projects /projects/illinois/eng/cs/chackoge/engrit/minhyuk2/singularity_images/python_image.sif /bin/bash ./scripts/batch_script.run
#singularity exec -B /tmp:/tmp -B /home:/home -B /projects:/projects /projects/tallis/minhyuk2/singularity_images/bio.sif /bin/bash ./scripts/batch_script.run

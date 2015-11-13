#!/bin/tcsh
# LTW dec 2013

setenv FREESURFER_HOME /import/monstrum/Applications/freesurfer
setenv SUBJECTS_DIR /import/monstrum/enigma/freesurfer/subjects
source $FREESURFER_HOME/SetUpFreeSurfer.csh
set outfolder="/import/monstrum/enigma/freesurfer/subjects/SURF_BP_HC"
cd $outfolder

# set some variables
set fsgd="$1" # expected to be located in $outfolder
set fwhm="15 30" # typical values, could be changed
set area="white" # could also be pial 
set meas="thickness"
set volume="volume"
set target="fsaverage" 

# run for each hemisphere
foreach hemi (lh rh)

  # thickness

  set out="${outfolder}/${hemi}.${meas}"
  # resample to common space and concatenate 
  mris_preproc --out ${out}.mgh \
	--target ${target} \
	--fsgd ${fsgd}.fsgd \
	--hemi $hemi \
	--meas ${meas} \

  # smooth surface
   foreach tfwhm ($fwhm)
	mri_surf2surf \
		--s $target \
		--sval ${out}.mgh \
		--cortex \
		--fwhm $tfwhm \
		--tval ${out}.${tfwhm}.mgh \
		--hemi $hemi
   end


  # surface area

  set out="${outfolder}/${hemi}.${area}"
  # resample to common space and concatenate 
  mris_preproc --out ${out}.mgh \
	--target ${target} \
	--fsgd ${fsgd}.fsgd \
	--hemi $hemi \
	--area $area \
	#--no-jac # turn off jacobian correction \

  # smooth surface
  foreach afwhm ($fwhm)
	mri_surf2surf \
		--s $target \
		--sval ${out}.mgh \
		--cortex \
		--fwhm $afwhm \
		--tval ${out}.${afwhm}.mgh \
		--hemi $hemi
   end


  # volume 

  set out = "${outfolder}/${hemi}.${volume}"
  # resample to common space and concatenate
  mris_preproc --out ${out}.mgh \
        --target ${target} \
        --fsgd ${fsgd}.fsgd \
        --hemi $hemi \
        --meas $volume \

  # smooth surface
  foreach vfwhm ($fwhm)
        mri_surf2surf \
                --s $target \
                --sval ${out}.mgh \
                --cortex \
                --fwhm $vfwhm \
                --tval ${out}.${vfwhm}.mgh \
                --hemi $hemi
   end

end

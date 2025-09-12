#######################
## surface space reporting
#wb_command -metric-find-clusters stat_map.func.gii 3.1 10 \
#    clusters.func.gii cluster_report.txt
import os
fslr_dir = '/home/clancy/TemplateFlow/BN_Atlas_freesurfer/100307/fsaverage_LR32k'
os.system(
    f'wb_command -volume-to-surface-mapping paired_run01-run02_zvalue.nii.gz \
    {fslr_dir}/100307.L.midthickness.32k_fs_LR.surf.gii \
    lh.stat_map.func.gii \
    -ribbon-constrained {fslr_dir}/100307.L.white.32k_fs_LR.surf.gii {fslr_dir}/100307.L.pial.32k_fs_LR.surf.gii'
)

os.system(
    f'wb_command -volume-to-surface-mapping paired_run01-run02_zvalue.nii.gz \
    {fslr_dir}/100307.R.midthickness.32k_fs_LR.surf.gii \
    rh.stat_map.func.gii \
    -ribbon-constrained {fslr_dir}/100307.R.white.32k_fs_LR.surf.gii {fslr_dir}/100307.R.pial.32k_fs_LR.surf.gii'
)

os.system(
    f'wb_command -cifti-create-dense-scalar \
    zvalue.dscalar.nii \
    -left-metric lh.stat_map.func.gii \
    -right-metric rh.stat_map.func.gii'
)
